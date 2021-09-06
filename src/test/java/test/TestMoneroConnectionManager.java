package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import common.utils.GenUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroConnectionManager;
import monero.common.MoneroConnectionManagerListener;
import monero.wallet.MoneroWalletRpc;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Test the Monero RPC connection manager.
 */
public class TestMoneroConnectionManager {
  
  @Test
  public void testMoneroRpcConnectionManager() throws InterruptedException {
    List<MoneroWalletRpc> walletRpcs = new ArrayList<MoneroWalletRpc>();
    try {
      
      // start monero-wallet-rpc instances as test servers (can also use monerod servers)
      try {
        for (int i = 0; i < 5; i++) walletRpcs.add(TestUtils.startWalletRpcProcess());
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
      
      // create connection manager
      MoneroConnectionManager connectionManager = new MoneroConnectionManager()
              .startAutoRefresh(TestUtils.SYNC_PERIOD_IN_MS)
              .setAutoSwitch(false);
      
      // listen for changes
      ConnectionChangeCollector listener = new ConnectionChangeCollector();
      connectionManager.addListener(listener);
      
      // add prioritized connections
      connectionManager.addConnection(walletRpcs.get(0).getRpcConnection());
      connectionManager.addConnection(new MoneroRpcConnection(walletRpcs.get(1).getRpcConnection().getUri())); // test unauthenticated
      connectionManager.addConnection(walletRpcs.get(2).getRpcConnection().setPriority(1));
      connectionManager.addConnection(walletRpcs.get(3).getRpcConnection().setPriority(1));
      connectionManager.addConnection(walletRpcs.get(4).getRpcConnection().setPriority(2));
      
      // test connections and order
      List<MoneroRpcConnection> orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      for (MoneroRpcConnection connection : orderedConnections) assertNull(connection.isOnline());
      
      // connect in order of priority and response time
      MoneroRpcConnection connection = connectionManager.connect();
      assertTrue(connection == walletRpcs.get(4).getRpcConnection());
      assertTrue(connection.isOnline());
      assertTrue(connection.isAuthenticated());
      assertEquals(1, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(0) == connection);
      
      // test connections and order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      for (int i = 1; i < orderedConnections.size(); i++) assertNull(orderedConnections.get(i).isOnline());
      
      // test auto refresh by shutting down connected instance
      TestUtils.stopWalletRpcProcess(walletRpcs.get(4));
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100); // allow time to poll
      assertFalse(connectionManager.isConnected());
      assertFalse(connectionManager.getConnection().isOnline());
      assertNull(connectionManager.getConnection().isAuthenticated());
      assertEquals(2, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(1) == connectionManager.getConnection());
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      
      // refresh all connections
      connectionManager.refreshAllConnections();
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      
      // test online and authentication status
      for (int i = 0; i < orderedConnections.size(); i++) {
        Boolean isOnline = orderedConnections.get(i).isOnline();
        Boolean isAuthenticated = orderedConnections.get(i).isAuthenticated();
        if (i == 0) assertFalse(isOnline);
        else assertTrue(isOnline);
        if (i == 0) assertNull(isAuthenticated);
        else if (i == 4) assertFalse(isAuthenticated);
        else assertTrue(isAuthenticated);
      }
      
      // test auto reconnect when disconnected
      connectionManager.setAutoSwitch(true);
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100);
      assertTrue(connectionManager.isConnected());
      connection = connectionManager.getConnection();
      assertTrue(connection.isOnline());
      assertTrue(connection == walletRpcs.get(2).getRpcConnection() || connection == walletRpcs.get(3).getRpcConnection());
      assertEquals(3, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(2) == connection);
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == connection);
      assertTrue(connection == walletRpcs.get(2).getRpcConnection() ? orderedConnections.get(1) == walletRpcs.get(3).getRpcConnection() : orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(0).getRpcConnection()); // assumes uri is first alphabetically
      assertEquals(orderedConnections.get(3).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(orderedConnections.get(4) == walletRpcs.get(4).getRpcConnection());
      for (int i = 0; i < orderedConnections.size() - 1; i++) assertTrue(orderedConnections.get(i).isOnline());
      assertFalse(orderedConnections.get(4).isOnline());
      
      // connect to specific endpoint without authentication
      try {
        connectionManager.connect(orderedConnections.get(3));
      } catch (MoneroError e) {
        assertEquals(e.getMessage(), "Connection is not authenticated");
      }
      
      // connect to specific endpoint with authentication
      orderedConnections.get(3).setCredentials("rpc_user", "abc123");
      connectionManager.setAutoSwitch(false);
      connection = connectionManager.connect(orderedConnections.get(3));
      assertEquals(connection.getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(connection.isOnline());
      assertTrue(connection.isAuthenticated());
      assertEquals(4, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(3) == connection);
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == connectionManager.getConnection());
      assertEquals(orderedConnections.get(0).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertTrue(orderedConnections.get(4) == walletRpcs.get(4).getRpcConnection());
      for (int i = 0; i < orderedConnections.size() - 1; i++) assertTrue(orderedConnections.get(i).isOnline());
      assertFalse(orderedConnections.get(4).isOnline());
      
      // stop polling connection
      connectionManager.stopAutoRefresh();
    } finally {
      
      // stop monero-wallet-rpc instances
      for (MoneroWalletRpc walletRpc : walletRpcs) {
        try { TestUtils.stopWalletRpcProcess(walletRpc); }
        catch (Exception e2) { }
      }
    }
  }
  
  private class ConnectionChangeCollector implements MoneroConnectionManagerListener {
    List<MoneroRpcConnection> changedConnections = new ArrayList<MoneroRpcConnection>();
    @Override
    public void onConnectionChanged(MoneroRpcConnection connection) {
      changedConnections.add(connection);
    }
  }
}
