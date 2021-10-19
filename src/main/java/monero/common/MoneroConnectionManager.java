package monero.common;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * Manages a collection of prioritized Monero RPC connections.
 */
public class MoneroConnectionManager {
  
  private static final long DEFAULT_TIMEOUT = 5000l;
  private static final long DEFAULT_REFRESH_PERIOD = 10000l;
  
  private long timeoutInMs = DEFAULT_TIMEOUT;
  private boolean autoSwitch;
  private TaskLooper refreshLooper;
  private List<MoneroRpcConnection> connections = new ArrayList<MoneroRpcConnection>();
  private List<MoneroConnectionManagerListener> listeners = new ArrayList<MoneroConnectionManagerListener>();
  private ConnectionComparator connectionComparator = new ConnectionComparator();
  
  /**
   * Add a connection. The connection may have an elevated priority for this manager to use.
   * 
   * @param connection - the connection to add
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager addConnection(MoneroRpcConnection connection) {
    for (MoneroRpcConnection aConnection : connections) {
      if (aConnection.getUri().equals(connection.getUri())) throw new MoneroError("Connection URI already exists");
    }
    connections.add(connection);
    return this;
  }
  
  /**
   * Remove a connection.
   * 
   * @param connection - the connection to remove
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeConnection(MoneroRpcConnection connection) {
    if (!connections.remove(connection)) throw new MoneroError("Monero connection manager does not contain connection to remove");
    connection.setIsCurrentConnection(false);
    return this;
  }
  
  /**
   * Add a listener to receive notifications when the connection changes.
   * 
   * @param listener - the listener to add
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager addListener(MoneroConnectionManagerListener listener) {
    listeners.add(listener);
    return this;
  }
  
  /**
   * Remove a listener.
   * 
   * @param listener - the listener to remove
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeListener(MoneroConnectionManagerListener listener) {
    if (!listeners.remove(listener)) throw new MoneroError("Monero connection manager does not contain listener to remove");
    return this;
  }
  
  /**
   * Set the maximum request time before its connection is considered offline.
   * 
   * @param timeoutInMs - the timeout before the connection is considered offline
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setTimeout(long timeoutInMs) {
    this.timeoutInMs = timeoutInMs;
    return this;
  }
  
  /**
   * Get the request timeout.
   * 
   * @return the request timeout before a connection is considered offline
   */
  public long getTimeout() {
    return timeoutInMs;
  }
  
  /**
   * Get all connections in order of current connection (if applicable), online status, priority, and name.
   * 
   * @return the list of sorted connections
   */
  public List<MoneroRpcConnection> getConnections() {
    List<MoneroRpcConnection> sortedConnections = new ArrayList<MoneroRpcConnection>(connections);
    Collections.sort(sortedConnections, connectionComparator);
    return sortedConnections;
  }
  
  /**
   * Automatically refresh the connection status by polling the server in a fixed period loop.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager startAutoRefresh() {
    startAutoRefresh(null);
    return this;
  }
  
  /**
   * Automatically refresh the connection status by polling the server in a fixed period loop.
   * 
   * @param refreshPeriod is the time between refreshes in milliseconds (default 10000 or 10 seconds)
   * @return this connection manager for chaining
   */
  public synchronized MoneroConnectionManager startAutoRefresh(Long refreshPeriod) {
    if (refreshPeriod == null) refreshPeriod = DEFAULT_REFRESH_PERIOD;
    if (refreshLooper == null) {
      refreshLooper = new TaskLooper(new Runnable() {
        @Override
        public void run() {
          try { if (getConnection() != null) refreshConnection(); }
          catch (Exception e) { e.printStackTrace(); }
        }
      });
    }
    refreshLooper.start(refreshPeriod);
    return this;
  }
  
  /**
   * Stop automatically refreshing the connection status.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager stopAutoRefresh() {
    if (refreshLooper != null) refreshLooper.stop();
    return this;
  }
  
  /**
   * Automatically switch to best available connection if current connection disconnects.
   * 
   * @param autoSwitch specifies if the connection should switch on disconnect
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setAutoSwitch(boolean autoSwitch) {
    this.autoSwitch = autoSwitch;
    return this;
  }
  
  /**
   * Connect to best available connection in order of priority then response time.
   * 
   * @return the selected connection
   */
  public MoneroRpcConnection connect() {
    
    // try connections within each descending priority
    for (List<MoneroRpcConnection> prioritizedConnections : getConnectionsInDescendingPriority()) {
      try {
      
        // check connections in parallel
        ExecutorService pool = Executors.newFixedThreadPool(prioritizedConnections.size());
        CompletionService<MoneroRpcConnection> completionService = new ExecutorCompletionService<MoneroRpcConnection>(pool);
        for (MoneroRpcConnection connection : prioritizedConnections) {
          completionService.submit(new Runnable() {
            @Override
            public void run() {
              connection.refreshConnection(timeoutInMs);
            }
          }, connection);
        }
        
        // use first connectable response
        pool.shutdown();
        for (int i = 0; i < prioritizedConnections.size(); i++) {
          MoneroRpcConnection connection = completionService.take().get();
          if (connection.isOnline() && !Boolean.FALSE.equals(connection.isAuthenticated())) {
            setCurrentConnection(connection);
            return connection;
          }
        }
      } catch (Exception e) {
        throw new MoneroError(e);
      }
    }
    return null;
  }
  
  /**
   * Connect to a given connection.
   * 
   * @param connection is the connection to connect to
   * @return MoneroRpcConnection is the given connection for convenience
   */
  public MoneroRpcConnection connect(MoneroRpcConnection connection) {
    if (!connections.contains(connection)) addConnection(connection);
    connection.refreshConnection(timeoutInMs);
    if (!connection.isOnline()) throw new MoneroError("Connection is not online");
    if (!connection.isAuthenticated()) throw new MoneroError("Connection is not authenticated"); // TODO: test that this throws NPE when connecting to unauthenticated connection
    setCurrentConnection(connection);
    return connection;
  }
  
  /**
   * Indicates if the connection manager is connected to a node.
   * 
   * @return true if the manager is connected to a node
   */
  public boolean isConnected() {
    MoneroRpcConnection connection = getConnection();
    return connection != null && Boolean.TRUE.equals(connection.isOnline()) && !Boolean.FALSE.equals(connection.isAuthenticated());
  }
  
  /**
   * Get the currently used connection.
   * 
   * @return the current connection
   */
  public MoneroRpcConnection getConnection() {
    for (MoneroRpcConnection connection : connections) if (Boolean.TRUE.equals(connection.isCurrentConnection())) return connection;
    return null;
  }
  
  /**
   * Refresh the current connection.
   * 
   * @return the current connection
   */
  public MoneroRpcConnection refreshConnection() {
    MoneroRpcConnection connection = getConnection();
    if (connection == null) throw new MoneroError("There is no current connection");
    if (connection.refreshConnection(timeoutInMs)) onConnectionChanged(connection);
    if (autoSwitch && (!connection.isOnline() || Boolean.FALSE.equals(connection.isAuthenticated()))) return connect();
    return connection;
  }
  
  /**
   * Refresh all managed connections.
   */
  public void refreshAllConnections() {
    MoneroRpcConnection currentConnection = getConnection();
    ExecutorService pool = Executors.newFixedThreadPool(connections.size());
    for (MoneroRpcConnection connection : connections) {
      pool.submit(new Runnable() {
        @Override
        public void run() {
          try {
            if (connection.refreshConnection(timeoutInMs) && connection == currentConnection) onConnectionChanged(connection);
          } catch (MoneroError err) {
            // ignore error
          }
        }
      });
    }
    try {
      pool.awaitTermination(timeoutInMs, TimeUnit.MILLISECONDS);
    } catch (InterruptedException e) {
      throw new MoneroError(e);
    }
  }
  
  /**
   * Collect connectable peers of the managed connections.
   *
   * @return connectable peers
   */
  public List<MoneroRpcConnection> getPeerConnections() {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------------------ PRIVATE HELPERS ---------------------------
  
  private void onConnectionChanged(MoneroRpcConnection connection) {
    for (MoneroConnectionManagerListener listener : listeners) listener.onConnectionChanged(connection);
  }
  
  private List<List<MoneroRpcConnection>> getConnectionsInDescendingPriority() {
    Map<Integer, List<MoneroRpcConnection>> connectionPriorities = new LinkedHashMap<Integer, List<MoneroRpcConnection>>();
    for (MoneroRpcConnection connection : connections) {
      if (!connectionPriorities.containsKey(connection.getPriority())) connectionPriorities.put(connection.getPriority(), new ArrayList<MoneroRpcConnection>());
      connectionPriorities.get(connection.getPriority()).add(connection);
    }
    List<List<MoneroRpcConnection>> prioritizedConnections = new ArrayList<List<MoneroRpcConnection>>();
    for (List<MoneroRpcConnection> priorityConnections : connectionPriorities.values()) prioritizedConnections.add(priorityConnections);
    Collections.reverse(prioritizedConnections);
    return prioritizedConnections;
  }
  
  private void setCurrentConnection(MoneroRpcConnection connection) {
    if (connection == getConnection()) return;
    for (MoneroRpcConnection aConnection : connections) aConnection.setIsCurrentConnection(aConnection == connection);
    onConnectionChanged(connection);
  }
  
  private class ConnectionComparator implements Comparator<MoneroRpcConnection> {
    
    @Override
    public int compare(MoneroRpcConnection c1, MoneroRpcConnection c2) {
      
      // current connection is first
      if (Boolean.TRUE.equals(c1.isCurrentConnection())) return -1;
      if (Boolean.TRUE.equals(c2.isCurrentConnection())) return 1;
      
      // order by availability then priority then by name
      if (c1.isOnline() == c2.isOnline()) {
        if (c1.getPriority() == c2.getPriority()) return c1.getUri().compareTo(c2.getUri());
        else return c1.getPriority() > c2.getPriority() ? -1 : 1;
      } else {
        if (Boolean.TRUE.equals(c1.isOnline())) return -1;
        else if (Boolean.TRUE.equals(c2.isOnline())) return 1;
        else if (c1.isOnline() == null) return -1;
        else return 1; // c1 is offline
      }
    }
  }
}
