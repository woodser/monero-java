package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import common.utils.GenUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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
  public void testConnectionManager() throws InterruptedException, IOException {
    List<MoneroWalletRpc> walletRpcs = new ArrayList<MoneroWalletRpc>();
    try {
      
      // start monero-wallet-rpc instances as test server connections (can also use monerod servers)
      for (int i = 0; i < 5; i++) walletRpcs.add(TestUtils.startWalletRpcProcess());
      
      // create connection manager
      MoneroConnectionManager connectionManager = new MoneroConnectionManager();
      
      // listen for changes
      ConnectionChangeCollector listener = new ConnectionChangeCollector();
      connectionManager.addListener(listener);
      
      // add prioritized connections
      connectionManager.addConnection(walletRpcs.get(4).getRpcConnection().setPriority(1));
      connectionManager.addConnection(walletRpcs.get(2).getRpcConnection().setPriority(2));
      connectionManager.addConnection(walletRpcs.get(3).getRpcConnection().setPriority(2));
      connectionManager.addConnection(walletRpcs.get(0).getRpcConnection()); // default priority is lowest
      connectionManager.addConnection(new MoneroRpcConnection(walletRpcs.get(1).getRpcConnection().getUri())); // test unauthenticated
      
      // test connections and order
      List<MoneroRpcConnection> orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      for (MoneroRpcConnection connection : orderedConnections) assertNull(connection.isOnline());
      
      // auto connect to best available connection
      connectionManager.setAutoSwitch(true);
      connectionManager.startCheckingConnection(TestUtils.SYNC_PERIOD_IN_MS);
      assertTrue(connectionManager.isConnected());
      MoneroRpcConnection connection = connectionManager.getConnection();
      assertTrue(connection.isOnline());
      assertTrue(connection == walletRpcs.get(4).getRpcConnection());
      assertEquals(1, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(0) == connection);
      connectionManager.setAutoSwitch(false);
      connectionManager.stopCheckingConnection();
      connectionManager.disconnect();
      assertEquals(2, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(1) == null);
      
      // start periodically checking connection
      connectionManager.startCheckingConnection(TestUtils.SYNC_PERIOD_IN_MS);
      
      // connect to best available connection in order of priority and response time
      connection = connectionManager.getBestAvailableConnection();
      connectionManager.setConnection(connection);
      assertTrue(connection == walletRpcs.get(4).getRpcConnection());
      assertTrue(connection.isOnline());
      assertTrue(connection.isAuthenticated());
      assertEquals(3, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(2) == connection);
      
      // test connections and order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      for (int i = 1; i < orderedConnections.size(); i++) assertNull(orderedConnections.get(i).isOnline());
      
      // test auto connect by shutting down connected instance
      TestUtils.stopWalletRpcProcess(walletRpcs.get(4));
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100); // allow time to poll
      assertFalse(connectionManager.isConnected());
      assertFalse(connectionManager.getConnection().isOnline());
      assertNull(connectionManager.getConnection().isAuthenticated());
      assertEquals(4, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(3) == connectionManager.getConnection());
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      
      // check all connections
      connectionManager.checkConnections();
      
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
      
      // test auto switch when disconnected
      connectionManager.setAutoSwitch(true);
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100);
      assertTrue(connectionManager.isConnected());
      connection = connectionManager.getConnection();
      assertTrue(connection.isOnline());
      assertTrue(connection == walletRpcs.get(2).getRpcConnection() || connection == walletRpcs.get(3).getRpcConnection());
      assertEquals(5, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(4) == connection);
      
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
      connection = orderedConnections.get(3);
      assertFalse(connection.isAuthenticated());
      connectionManager.setConnection(connection);
      assertFalse(connectionManager.isConnected());
      assertEquals(6, listener.changedConnections.size());
      
      // connect to specific endpoint with authentication
      connectionManager.setAutoSwitch(false);
      connection.setCredentials("rpc_user", "abc123");
      connectionManager.checkConnection();
      assertEquals(connection.getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(connection.isOnline());
      assertTrue(connection.isAuthenticated());
      assertEquals(7, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(6) == connection);
      
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
      
      // set connection to new uri
      String uri = "http://localhost:49999";
      connectionManager.setConnection(uri);
      assertEquals(uri, connectionManager.getConnection().getUri());
      
      // test no available connection
      connectionManager.setConnection(connectionManager.getBestAvailableConnection());
      assertEquals(9, listener.changedConnections.size());
      connectionManager.setAutoSwitch(true);
      for (MoneroWalletRpc walletRpc : walletRpcs) TestUtils.stopWalletRpcProcess(walletRpc);
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100);
      assertEquals(10, listener.changedConnections.size());
      
      // stop polling connection
      connectionManager.stopCheckingConnection();
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
