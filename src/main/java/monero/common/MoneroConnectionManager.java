package monero.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * <p>Manages a collection of prioritized connections to daemon or wallet RPC endpoints.</p>
 * 
 * <p>Example usage:</p>
 * 
 * <code>
 * // create connection manager<br>
 * MoneroConnectionManager connectionManager = new MoneroConnectionManager();<br><br>
 * 
 * // add managed connections with priorities<br>
 * connectionManager.addConnection(new MoneroRpcConnection("http://localhost:38081").setPriority(1)); // use localhost as first priority<br>
 * connectionManager.addConnection(new MoneroRpcConnection("http://example.com")); // default priority is prioritized last<br><br>
 * 
 * // set current connection<br>
 * connectionManager.setConnection(new MoneroRpcConnection("http://foo.bar", "admin", "password")); // connection is added if new<br><br>
 * 
 * // check connection status<br>
 * connectionManager.checkConnection();<br>
 * System.out.println("Connection manager is connected: " + connectionManager.isConnected());<br>
 * System.out.println("Connection is online: " + connectionManager.getConnection().isOnline());<br>
 * System.out.println("Connection is authenticated: " + connectionManager.getConnection().isAuthenticated());<br><br>
 * 
 * // receive notifications of any changes to current connection<br>
 * connectionManager.addListener(new MoneroConnectionManagerListener() {<br>
 * &nbsp;&nbsp; @Override<br>
 * &nbsp;&nbsp; public void onConnectionChanged(MoneroRpcConnection connection) {<br>
 * &nbsp;&nbsp;&nbsp;&nbsp; System.out.println("Connection changed to: " + connection);<br>
 * &nbsp;&nbsp; }<br>
 * });<br><br>
 * 
 * // check connection status every 10 seconds<br>
 * connectionManager.startCheckingConnection(10000l);<br><br>
 * 
 * // automatically switch to best available connection if disconnected<br>
 * connectionManager.setAutoSwitch(true);<br><br>
 * 
 * // get best available connection in order of priority then response time<br>
 * MoneroRpcConnection bestConnection = connectionManager.getBestAvailableConnection();<br><br>
 * 
 * // check status of all connections<br>
 * connectionManager.checkConnections();<br><br>
 * 
 * // get connections in order of current connection, online status from last check, priority, and name<br>
 * List&lt;MoneroRpcConnection&gt; connections = connectionManager.getConnections();<br><br>
 * 
 * // clear connection manager<br>
 * connectionManager.clear();
 * </code>
 */
public class MoneroConnectionManager {
  
  // static variables
  private static final long DEFAULT_TIMEOUT = 5000l;
  private static final long DEFAULT_CHECK_CONNECTION_PERIOD = 15000l;
  
  // instance variables
  private MoneroRpcConnection currentConnection;
  private List<MoneroRpcConnection> connections = new ArrayList<MoneroRpcConnection>();
  private List<MoneroConnectionManagerListener> listeners = new ArrayList<MoneroConnectionManagerListener>();
  private ConnectionComparator connectionComparator = new ConnectionComparator();
  private long timeoutMs = DEFAULT_TIMEOUT;
  private boolean autoSwitch;
  private TaskLooper checkConnectionLooper;
  
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
   * Remove all listeners.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeListeners() {
    listeners.clear();
    return this;
  }
  
  /**
   * Add a connection. The connection may have an elevated priority for this manager to use.
   * 
   * @param connection - the connection to add
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager addConnection(MoneroRpcConnection connection) {
    for (MoneroRpcConnection aConnection : connections) {
      if (aConnection.getUri().equals(connection.getUri())) throw new MoneroError("Connection URI already exists with connection manager: " + connection.getUri());
    }
    connections.add(connection);
    return this;
  }
  
  /**
   * Remove a connection.
   * 
   * @param uri - uri of the connection to remove
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeConnection(String uri) {
    MoneroRpcConnection connection = getConnectionByUri(uri);
    if (connection == null) throw new MoneroError("No connection exists with URI: " + uri);
    connections.remove(connection);
    if (connection == currentConnection) {
      currentConnection = null;
      onConnectionChanged(currentConnection);
    }
    return this;
  }
  
  /**
   * Indicates if the connection manager is connected to a node.
   * 
   * @return true if the current connection is set, online, and not unauthenticated. false otherwise
   */
  public boolean isConnected() {
    return currentConnection != null && currentConnection.isConnected();
  }
  
  /**
   * Get the current connection.
   * 
   * @return the current connection or null if no connection set
   */
  public MoneroRpcConnection getConnection() {
    return currentConnection;
  }
  
  /**
   * Get a connection by URI.
   * 
   * @param uri - uri of the connection to get
   * @return the connection with the URI or null if no connection with the URI exists
   */
  public MoneroRpcConnection getConnectionByUri(String uri) {
    for (MoneroRpcConnection connection : connections) if (connection.getUri().equals(uri)) return connection;
    return null;
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
   * Get the best available connection in order of priority then response time.
   * 
   * @param excludedConnections - connections to be excluded from consideration (optional)
   * @return the best available connection in order of priority then response time, null if no connections available
   */
  public MoneroRpcConnection getBestAvailableConnection(MoneroRpcConnection... excludedConnections ) {
    
    // try connections within each ascending priority
    for (List<MoneroRpcConnection> prioritizedConnections : getConnectionsInAscendingPriority()) {
      try {
      
        // check connections in parallel
        int numTasks = 0;
        ExecutorService pool = Executors.newFixedThreadPool(prioritizedConnections.size());
        CompletionService<MoneroRpcConnection> completionService = new ExecutorCompletionService<MoneroRpcConnection>(pool);
        for (MoneroRpcConnection connection : prioritizedConnections) {
          if (Arrays.asList(excludedConnections).contains(connection)) continue;
          numTasks++;
          completionService.submit(new Runnable() {
            @Override
            public void run() {
              connection.checkConnection(timeoutMs);
            }
          }, connection);
        }
        
        // use first available connection
        pool.shutdown();
        for (int i = 0; i < numTasks; i++) {
          MoneroRpcConnection connection = completionService.take().get();
          if (connection.isConnected()) return connection;
        }
      } catch (Exception e) {
        throw new MoneroError(e);
      }
    }
    return null;
  }
  
  /**
   * Set the current connection without changing the credentials.
   * Add new connection if URI not previously added.
   * Notify if current connection changes.
   * Does not check the connection.
   * 
   * @param uri identifies the connection to make current
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setConnection(String uri) {
    if (uri == null || "".equals(uri)) return setConnection((MoneroRpcConnection) null);
    MoneroRpcConnection connection = getConnectionByUri(uri);
    return setConnection(connection == null ? new MoneroRpcConnection(uri) : connection);
  }
  
  /**
   * Set the current connection.
   * Update credentials if connection's URI was previously added. Otherwise add new connection.
   * Notify if current connection changes.
   * Does not check the connection.
   * 
   * @param connection is the connection to make current
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setConnection(MoneroRpcConnection connection) {
    if (currentConnection == connection) return this;
    
    // check if setting null connection
    if (connection == null) {
      currentConnection = null;
      onConnectionChanged(null);
      return this;
    }
    
    // must provide uri
    if (connection.getUri() == null || "".equals(connection.getUri())) throw new MoneroError("Connection is missing URI");
    
    // check if adding new connection
    MoneroRpcConnection prevConnection = getConnectionByUri(connection.getUri());
    if (prevConnection == null) {
      addConnection(connection);
      currentConnection = connection;
      onConnectionChanged(currentConnection);
      return this;
    }
    
    // check if updating current connection
    if (prevConnection != currentConnection || !Objects.equals(prevConnection.getUsername(), connection.getUsername()) || !Objects.equals(prevConnection.getPassword(), connection.getPassword()) || !Objects.equals(prevConnection.getPriority(), connection.getPriority())) {
      prevConnection.setCredentials(connection.getUsername(), connection.getPassword());
      prevConnection.setPriority(connection.getPriority());
      currentConnection = prevConnection;
      onConnectionChanged(currentConnection);
    }
    
    return this;
  }
  
  /**
   * Check the current connection. If disconnected and auto switch enabled, switches to best available connection.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager checkConnection() {
    boolean connectionChanged = false;
    MoneroRpcConnection connection = getConnection();
    if (connection != null && connection.checkConnection(timeoutMs)) connectionChanged = true;
    if (autoSwitch && !isConnected()) {
      MoneroRpcConnection bestConnection = getBestAvailableConnection(connection);
      if (bestConnection != null) {
        setConnection(bestConnection);
        return this;
      }
    }
    if (connectionChanged) onConnectionChanged(connection);
    return this;
  }
  
  /**
   * Check all managed connections.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager checkConnections() {
    
    // collect tasks to check connections
    MoneroRpcConnection currentConnection = getConnection();
    ExecutorService pool = Executors.newFixedThreadPool(connections.size());
    for (MoneroRpcConnection connection : connections) {
      pool.submit(new Runnable() {
        @Override
        public void run() {
          try {
            if (connection.checkConnection(timeoutMs) && connection == currentConnection) onConnectionChanged(connection);
          } catch (MoneroError err) {
            // ignore error
          }
        }
      });
    }
    try {
      
      // check connections in parallel
      pool.shutdown();
      pool.awaitTermination(timeoutMs, TimeUnit.MILLISECONDS);
      
      // auto switch to best connection
      if (autoSwitch && !isConnected()) {
        for (List<MoneroRpcConnection> prioritizedConnections : getConnectionsInAscendingPriority()) {
          MoneroRpcConnection bestConnection = null;
          for (MoneroRpcConnection prioritizedConnection : prioritizedConnections) {
            if (prioritizedConnection.isConnected() && (bestConnection == null || prioritizedConnection.getResponseTime() < bestConnection.getResponseTime())) {
              bestConnection = prioritizedConnection;
            }
          }
          if (bestConnection != null) {
            setConnection(bestConnection);
            break;
          }
        }
      }
    } catch (InterruptedException e) {
      throw new MoneroError(e);
    }
    return this;
  }
  
  /**
   * Check the connection and start checking the connection periodically.
   * 
   * @return this connection manager for chaining (after first checking the connection)
   */
  public MoneroConnectionManager startCheckingConnection() {
    startCheckingConnection(null);
    return this;
  }
  
  /**
   * Check the connection and start checking the connection periodically.
   * 
   * @param periodMs is the time between checks in milliseconds (default 10000 ms or 10 seconds)
   * @return this connection manager for chaining (after first checking the connection)
   */
  public synchronized MoneroConnectionManager startCheckingConnection(Long periodMs) {
    checkConnection();
    if (periodMs == null) periodMs = DEFAULT_CHECK_CONNECTION_PERIOD;
    if (checkConnectionLooper != null) return this;
    checkConnectionLooper = new TaskLooper(new Runnable() {
      boolean isFirstCheck = true;
      @Override
      public void run() {
        if (isFirstCheck) {
          isFirstCheck = false; // skip first check
          return;
        }
        try { checkConnection(); }
        catch (Exception e) { e.printStackTrace(); }
      }
    });
    checkConnectionLooper.start(periodMs);
    return this;
  }
  
  /**
   * Stop checking the connection status periodically.
   * 
   * @return this connection manager for chaining
   */
  public synchronized MoneroConnectionManager stopCheckingConnection() {
    if (checkConnectionLooper != null) checkConnectionLooper.stop();
    checkConnectionLooper = null;
    return this;
  }
  
  /**
   * Automatically switch to best available connection if current connection is disconnected after being checked.
   * 
   * @param autoSwitch specifies if the connection should switch on disconnect
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setAutoSwitch(boolean autoSwitch) {
    this.autoSwitch = autoSwitch;
    return this;
  }
  
  /**
   * Get if auto switch is enabled or disabled.
   * 
   * @return true if auto switch enabled, false otherwise
   */
  public boolean getAutoSwitch() {
    return autoSwitch;
  }
  
  /**
   * Set the maximum request time before a connection is considered offline.
   * 
   * @param timeoutInMs is the timeout before a connection is considered offline
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setTimeout(long timeoutInMs) {
    this.timeoutMs = timeoutInMs;
    return this;
  }
  
  /**
   * Get the request timeout.
   * 
   * @return the request timeout before a connection is considered offline
   */
  public long getTimeout() {
    return timeoutMs;
  }
  
  /**
   * Collect connectable peers of the managed connections.
   *
   * @return connectable peers
   */
  public List<MoneroRpcConnection> getPeerConnections() {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Disconnect from the current connection.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager disconnect() {
    setConnection((String) null);
    return this;
  }
  
  /**
   * Remove all connections.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager clear() {
    connections.clear();
    if (currentConnection != null) {
      currentConnection = null;
      onConnectionChanged(null);
    }
    return this;
  }
  
  /**
   * Reset to default state.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager reset() {
    removeListeners();
    stopCheckingConnection();
    clear();
    timeoutMs = DEFAULT_TIMEOUT;
    autoSwitch = false;
    return this;
  }
  
  // ------------------------------ PRIVATE HELPERS ---------------------------
  
  private void onConnectionChanged(MoneroRpcConnection connection) {
    for (MoneroConnectionManagerListener listener : listeners) listener.onConnectionChanged(connection);
  }
  
  private List<List<MoneroRpcConnection>> getConnectionsInAscendingPriority() {
    Map<Integer, List<MoneroRpcConnection>> connectionPriorities = new TreeMap<Integer, List<MoneroRpcConnection>>();
    for (MoneroRpcConnection connection : connections) {
      if (!connectionPriorities.containsKey(connection.getPriority())) connectionPriorities.put(connection.getPriority(), new ArrayList<MoneroRpcConnection>());
      connectionPriorities.get(connection.getPriority()).add(connection);
    }
    List<List<MoneroRpcConnection>> prioritizedConnections = new ArrayList<List<MoneroRpcConnection>>();
    for (List<MoneroRpcConnection> priorityConnections : connectionPriorities.values()) prioritizedConnections.add(priorityConnections);
    if (connectionPriorities.containsKey(0)) prioritizedConnections.add(prioritizedConnections.remove(0)); // move priority 0 to end
    return prioritizedConnections;
  }
  
  private class ConnectionComparator implements Comparator<MoneroRpcConnection> {
    
    @Override
    public int compare(MoneroRpcConnection c1, MoneroRpcConnection c2) {
      
      // current connection is first
      if (c1 == currentConnection) return -1;
      if (c2 == currentConnection) return 1;
      
      // order by availability then priority then by name
      if (c1.isOnline() == c2.isOnline()) {
        if (c1.getPriority() == c2.getPriority()) return c1.getUri().compareTo(c2.getUri());
        else return c1.getPriority() == 0 ? 1 : c2.getPriority() == 0 ? -1 : c1.getPriority() - c2.getPriority();
      } else {
        if (Boolean.TRUE.equals(c1.isOnline())) return -1;
        else if (Boolean.TRUE.equals(c2.isOnline())) return 1;
        else if (c1.isOnline() == null) return -1;
        else return 1; // c1 is offline
      }
    }
  }
}
