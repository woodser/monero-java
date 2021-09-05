package monero.common;

/**
 * Default connection manager listener which takes no action on notifications.
 */
public interface MoneroConnectionManagerListener {

  /**
   * Notified on connection change events.
   */
  public void onConnectionChanged(MoneroRpcConnection connection);
}
