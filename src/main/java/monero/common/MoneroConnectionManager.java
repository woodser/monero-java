package monero.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

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
 * // start polling for best connection every 10 seconds and automatically switch<br>
 * connectionManager.startPolling(10000l);<br><br>
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
  private static final long DEFAULT_POLL_PERIOD = 20000l;
  private static final boolean DEFAULT_AUTO_SWITCH = true;
  private static final int MIN_BETTER_RESPONSES = 3;
  private static ConnectionPriorityComparator priorityComparator = new ConnectionPriorityComparator();

  // instance variables
  private MoneroRpcConnection currentConnection;
  private List<MoneroRpcConnection> connections = new ArrayList<MoneroRpcConnection>();
  private List<MoneroConnectionManagerListener> listeners = new ArrayList<MoneroConnectionManagerListener>();
  private ConnectionComparator connectionComparator = new ConnectionComparator();
  private boolean autoSwitch = DEFAULT_AUTO_SWITCH;
  private long timeoutMs = DEFAULT_TIMEOUT;
  private TaskLooper poller;
  private Map<MoneroRpcConnection, List<Long>> responseTimes = new HashMap<MoneroRpcConnection, List<Long>>();

  /**
   * Specify behavior when polling.
   * 
   * One of PRIORITIZED (poll connections in order of priority until connected; default), CURRENT (poll current connection), or ALL (poll all connections).
   */
  public enum PollType {
    PRIORITIZED,
    CURRENT,
    ALL
  }
  
  /**
   * Add a listener to receive notifications when the connection changes.
   * 
   * @param listener - the listener to add
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager addListener(MoneroConnectionManagerListener listener) {
    synchronized (listeners) {
      listeners.add(listener);
      return this;
    }
  }
  
  /**
   * Remove a listener.
   * 
   * @param listener - the listener to remove
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeListener(MoneroConnectionManagerListener listener) {
    synchronized (listeners) {
      if (!listeners.remove(listener)) throw new MoneroError("Monero connection manager does not contain listener to remove");
      return this;
    }
  }
  
  /**
   * Remove all listeners.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeListeners() {
    synchronized (listeners) {
      listeners.clear();
      return this;
    }
  }
  
  /**
   * Get all listeners.
   * 
   * @return all listeners
   */
  public List<MoneroConnectionManagerListener> getListeners() {
    return listeners;
  }

  /**
   * Add a connection URI.
   * 
   * @param uri - uri of the connection to add
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager addConnection(String uri) {
    return addConnection(new MoneroRpcConnection(uri));
  }
  
  /**
   * Add a connection. The connection may have an elevated priority for this manager to use.
   * 
   * @param connection - the connection to add
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager addConnection(MoneroRpcConnection connection) {
    synchronized (connections) {
      for (MoneroRpcConnection aConnection : connections) {
        if (aConnection.getUri().equals(connection.getUri())) throw new MoneroError("Connection URI already exists with connection manager: " + connection.getUri());
      }
      connections.add(connection);
      return this;
    }
  }
  
  /**
   * Remove a connection.
   * 
   * @param uri - uri of the connection to remove
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager removeConnection(String uri) {
    synchronized (connections) {
      MoneroRpcConnection connection = getConnectionByUri(uri);
      if (connection == null) throw new MoneroError("No connection exists with URI: " + uri);
      connections.remove(connection);
      responseTimes.remove(connection);
      if (connection == currentConnection) {
        currentConnection = null;
        onConnectionChanged(currentConnection);
      }
      return this;
    }
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
   * Replace connection if its URI was previously added. Otherwise add new connection.
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

    // add or replace connection
    synchronized (connections) {
      MoneroRpcConnection prevConnection = getConnectionByUri(connection.getUri());
      if (prevConnection != null) connections.remove(prevConnection);
      addConnection(connection);
      currentConnection = connection;
      onConnectionChanged(currentConnection);
      return this;
    }
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
   * Indicates if this manager has a connection with the given URI.
   * 
   * @param uri - URI of the connection to check
   * @return true if this manager has a connection with the given URI, false otherwise
   */
  public boolean hasConnection(String uri) {
    return getConnectionByUri(uri) != null;
  }
  
  /**
   * Get a connection by URI.
   * 
   * @param uri - URI of the connection to get
   * @return the connection with the URI or null if no connection with the URI exists
   */
  public MoneroRpcConnection getConnectionByUri(String uri) {
    synchronized (connections) {
      for (MoneroRpcConnection connection : connections) if (connection.getUri().equals(uri)) return connection;
      return null;
    }
  }

  /**
   * Get all connections in order of current connection (if applicable), online status, priority, and name.
   * 
   * @return the list of sorted connections
   */
  public List<MoneroRpcConnection> getConnections() {
    synchronized (connections) {
      List<MoneroRpcConnection> sortedConnections = new ArrayList<MoneroRpcConnection>(connections);
      Collections.sort(sortedConnections, connectionComparator);
      return sortedConnections;
    }
  }

  /**
   * Indicates if the connection manager is connected to a node.
   * 
   * @return true if the current connection is set, online, and not unauthenticated, null if unknown, false otherwise
   */
  public Boolean isConnected() {
    if (currentConnection == null) return false;
    return currentConnection.isConnected();
  }

  /**
   * Start polling connections. Automatically connects to the best available connection based on priority, response time, and consistency.
   */
  public void startPolling() {
    startPolling(null, null, null, null, null);
  }

  /**
   * Start polling connections. Automatically connects to the best available connection based on priority, response time, and consistency.
   * 
   * @param periodMs poll period in milliseconds (default 20s)
   */
  public void startPolling(Long periodMs) {
    startPolling(periodMs, null, null, null, null);
  }

  /**
   * Start polling connections.
   * 
   * @param periodMs poll period in milliseconds (default 20s)
   * @param autoSwitch specifies to automatically switch to the best connection (default true unless changed)
   * @param timeoutMs specifies the timeout to poll a single connection (default 5s unless changed)
   * @param pollType one of PRIORITIZED (poll connections in order of priority until connected; default), CURRENT (poll current connection), or ALL (poll all connections)
   * @param excludedConnections connections excluded from being polled
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager startPolling(Long periodMs, Boolean autoSwitch, Long timeoutMs, PollType pollType, Collection<MoneroRpcConnection> excludedConnections) {

    // apply defaults
    if (periodMs == null) periodMs = DEFAULT_POLL_PERIOD;
    if (autoSwitch != null) setAutoSwitch(autoSwitch);
    if (timeoutMs != null) setTimeout(timeoutMs);
    if (pollType == null) pollType = PollType.PRIORITIZED;

    // stop polling
    stopPolling();

    // start polling
    switch (pollType) {
      case CURRENT:
        startPollingConnection(periodMs);
        break;
      case ALL:
        startPollingConnections(periodMs);
        break;
      case PRIORITIZED:
      default:
        startPollingPrioritizedConnections(periodMs, excludedConnections);
    }
    return this;
  }

  /**
   * Stop polling connections.
   * 
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager stopPolling() {
    if (poller != null) poller.stop();
    poller = null;
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
    if (connection != null) {
      if (connection.checkConnection(timeoutMs)) connectionChanged = true;
      if (processResponses(Arrays.asList(connection)) != null) return this; // done if connection set from responses
    }
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
    checkConnections(getConnections(), null);
    return this;
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
          completionService.submit(() -> {
            connection.checkConnection(timeoutMs);
            return connection;
          });
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
   * Automatically switch to the best available connection as connections are polled, based on priority, response time, and consistency.
   * 
   * @param autoSwitch specifies if the connection should auto switch to a better connection
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
   * @param timeoutMs is the timeout before a connection is considered offline
   * @return this connection manager for chaining
   */
  public MoneroConnectionManager setTimeout(long timeoutMs) {
    this.timeoutMs = timeoutMs;
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
    stopPolling();
    clear();
    timeoutMs = DEFAULT_TIMEOUT;
    autoSwitch = DEFAULT_AUTO_SWITCH;
    return this;
  }
  
  // ----------------------------- PRIVATE HELPERS ----------------------------
  
  private void onConnectionChanged(MoneroRpcConnection connection) {
    synchronized (listeners) {
      for (MoneroConnectionManagerListener listener : listeners) listener.onConnectionChanged(connection);
    }
  }
  
  private List<List<MoneroRpcConnection>> getConnectionsInAscendingPriority() {
    synchronized (connections) {
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
        return priorityComparator.compare(c1.getPriority(), c2.getPriority()) * -1; // order by priority in descending order
      } else {
        if (Boolean.TRUE.equals(c1.isOnline())) return -1;
        else if (Boolean.TRUE.equals(c2.isOnline())) return 1;
        else if (c1.isOnline() == null) return -1;
        else return 1; // c1 is offline
      }
    }
  }
  
  private static class ConnectionPriorityComparator implements Comparator<Integer> {

    @Override
    public int compare(Integer p1, Integer p2) {
      if (p1 == p2) return 0;
      if (p1 == 0) return -1;
      if (p2 == 0) return 1;
      return p2 - p1;
    }
  }

  private void startPollingConnection(long periodMs) {
    poller = new TaskLooper(() -> {
      try { checkConnection(); }
      catch (Exception e) { e.printStackTrace(); }
    });
    poller.start(periodMs);
  }

  private void startPollingConnections(long periodMs) {
    poller = new TaskLooper(() -> {
      try { checkConnections(); }
      catch (Exception e) { e.printStackTrace(); }
    });
    poller.start(periodMs);
  }

    private void startPollingPrioritizedConnections(long periodMs, Collection<MoneroRpcConnection> excludedConnections) {
    poller = new TaskLooper(() -> {
      try { checkPrioritizedConnections(excludedConnections); }
      catch (Exception e) { e.printStackTrace(); }
    });
    poller.start(periodMs);
  }

  private void checkPrioritizedConnections(Collection<MoneroRpcConnection> excludedConnections) {
    for (List<MoneroRpcConnection> prioritizedConnections : getConnectionsInAscendingPriority()) {
      boolean hasConnection = checkConnections(prioritizedConnections, excludedConnections);
      if (hasConnection) return;
    }
  }

  private boolean checkConnections(Collection<MoneroRpcConnection> connections, Collection<MoneroRpcConnection> excludedConnections) {
    synchronized (connections) {
      try {

        // start checking connections in parallel
        int numTasks = 0;
        ExecutorService pool = Executors.newFixedThreadPool(connections.size());
        CompletionService<MoneroRpcConnection> completionService = new ExecutorCompletionService<MoneroRpcConnection>(pool);
        for (MoneroRpcConnection connection : connections) {
          if (excludedConnections != null && excludedConnections.contains(connection)) continue;
          numTasks++;
          completionService.submit(() -> {
            boolean change = connection.checkConnection(timeoutMs);
            if (change && connection == getConnection()) onConnectionChanged(connection);
            return connection;
          });
        }

        // wait for responses
        pool.shutdown();
        boolean hasConnection = false;
        for (int i = 0; i < numTasks; i++) {
          MoneroRpcConnection connection = completionService.take().get();
          if (Boolean.TRUE.equals(connection.isConnected()) && !hasConnection) {
            hasConnection = true;
            if (!Boolean.TRUE.equals(isConnected()) && autoSwitch) setConnection(connection); // set first available connection if disconnected
          }
        }

        // process responses
        processResponses(connections);
        return hasConnection;
      } catch (Exception e) {
        throw new MoneroError(e);
      }
    }
  }

  private MoneroRpcConnection processResponses(Collection<MoneroRpcConnection> responses) {

    // add new connections
    for (MoneroRpcConnection connection : responses) {
      if (!responseTimes.containsKey(connection)) responseTimes.put(connection, new ArrayList<Long>());
    }

    // insert response times or null
    for (Entry<MoneroRpcConnection, List<Long>> responseTime : responseTimes.entrySet()) {
      responseTime.getValue().add(0, responses.contains(responseTime.getKey()) ? responseTime.getKey().getResponseTime() : null);

      // remove old response times
      if (responseTime.getValue().size() > MIN_BETTER_RESPONSES) responseTime.getValue().remove(responseTime.getValue().size() - 1);
    }

    // update best connection based on responses and priority
    return updateBestConnectionInPriority();
  }

  private MoneroRpcConnection updateBestConnectionInPriority() {
    if (!autoSwitch) return null;
    for (List<MoneroRpcConnection> prioritizedConnections : getConnectionsInAscendingPriority()) {
      MoneroRpcConnection bestConnectionFromResponses = getBestConnectionFromPrioritizedResponses(prioritizedConnections);
      if (bestConnectionFromResponses != null) {
        setConnection(bestConnectionFromResponses);
        return bestConnectionFromResponses;
      }
    }
    return null;
  }

  /**
   * Get the best connection from the given responses.
   * 
   * @param responses are connection responses to update from
   * @return MoneroRpcConnection is the best response among the given responses or null if none are best
   */
  private MoneroRpcConnection getBestConnectionFromPrioritizedResponses(Collection<MoneroRpcConnection> responses) {

    // get best response
    MoneroRpcConnection bestResponse = null;
    for (MoneroRpcConnection connection : responses) {
      if (Boolean.TRUE.equals(connection.isConnected()) && (bestResponse == null || connection.getResponseTime() < bestResponse.getResponseTime())) bestResponse = connection;
    }
    
    // no update if no responses
    if (bestResponse == null) return null;
    
    // use best response if disconnected
    MoneroRpcConnection bestConnection = getConnection();
    if (bestConnection == null || !Boolean.TRUE.equals(bestConnection.isConnected())) return bestResponse;
    
    // use best response if different priority (assumes being called in descending priority)
    if (priorityComparator.compare(bestResponse.getPriority(), bestConnection.getPriority()) != 0) return bestResponse;

    // keep best connection if not enough data
    if (!responseTimes.containsKey(bestConnection)) return bestConnection;
    
    // check if a connection is consistently better
    for (MoneroRpcConnection connection : responses) {
      if (connection == bestConnection) continue;
      if (!responseTimes.containsKey(connection) || responseTimes.get(connection).size() < MIN_BETTER_RESPONSES) continue;
      boolean better = true;
      for (int i = 0; i < MIN_BETTER_RESPONSES; i++) {
        if (responseTimes.get(connection).get(i) == null || responseTimes.get(bestConnection).get(i) == null || responseTimes.get(connection).get(i) > responseTimes.get(bestConnection).get(i)) {
          better = false;
          break;
        }
      }
      if (better) bestConnection = connection;
    }
    return bestConnection;
  }
}
