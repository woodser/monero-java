

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import common.utils.GenUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroConnectionManager.PollType;
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
    MoneroConnectionManager connectionManager = null;
    try {
      
      // start monero-wallet-rpc instances as test server connections (can also use monerod servers)
      for (int i = 0; i < 5; i++) walletRpcs.add(TestUtils.startWalletRpcProcess());
      
      // create connection manager
      connectionManager = new MoneroConnectionManager();
      
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

      // test getting connection by uri
      assertTrue(connectionManager.hasConnection(walletRpcs.get(0).getRpcConnection().getUri()));
      assertTrue(connectionManager.getConnectionByUri(walletRpcs.get(0).getRpcConnection().getUri()) == walletRpcs.get(0).getRpcConnection());
      
      // test unknown connection
      int numExpectedChanges = 0;
      connectionManager.setConnection(orderedConnections.get(0));
      assertEquals(null, connectionManager.isConnected());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());

      // auto connect to best available connection
      connectionManager.startPolling(TestUtils.SYNC_PERIOD_IN_MS);
      GenUtils.waitFor(TestUtils.AUTO_CONNECT_TIMEOUT_MS);
      assertTrue(connectionManager.isConnected());
      MoneroRpcConnection connection = connectionManager.getConnection();
      assertTrue(connection.isOnline());
      assertTrue(connection == walletRpcs.get(4).getRpcConnection());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == connection);
      connectionManager.setAutoSwitch(false);
      connectionManager.stopPolling();
      connectionManager.disconnect();
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == null);
      
      // start periodically checking connection without auto switch
      connectionManager.startPolling(TestUtils.SYNC_PERIOD_IN_MS, false, null, null, null);
      
      // connect to best available connection in order of priority and response time
      connection = connectionManager.getBestAvailableConnection();
      connectionManager.setConnection(connection);
      assertTrue(connection == walletRpcs.get(4).getRpcConnection());
      assertTrue(connection.isOnline());
      assertTrue(connection.isAuthenticated());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == connection);
      
      // test connections and order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(3).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(4).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      for (int i = 1; i < orderedConnections.size(); i++) assertNull(orderedConnections.get(i).isOnline());
      
      // shut down prioritized servers
      TestUtils.stopWalletRpcProcess(walletRpcs.get(2));
      TestUtils.stopWalletRpcProcess(walletRpcs.get(3));
      TestUtils.stopWalletRpcProcess(walletRpcs.get(4));
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100); // allow time to poll
      assertFalse(connectionManager.isConnected());
      assertFalse(connectionManager.getConnection().isOnline());
      assertNull(connectionManager.getConnection().isAuthenticated());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == connectionManager.getConnection());
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(2).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(4) == walletRpcs.get(3).getRpcConnection());
      
      // check all connections
      connectionManager.checkConnections();
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(2).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(4) == walletRpcs.get(3).getRpcConnection());
      
      // test online and authentication status
      for (int i = 0; i < orderedConnections.size(); i++) {
        Boolean isOnline = orderedConnections.get(i).isOnline();
        Boolean isAuthenticated = orderedConnections.get(i).isAuthenticated();
        if (i == 1 || i == 2) assertTrue(isOnline);
        else assertFalse(isOnline);
        if (i == 1) assertTrue(isAuthenticated);
        else if (i == 2) assertFalse(isAuthenticated);
        else assertNull(isAuthenticated);
      }
      
      // test auto switch when disconnected
      connectionManager.setAutoSwitch(true);
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100);
      assertTrue(connectionManager.isConnected());
      connection = connectionManager.getConnection();
      assertTrue(connection.isOnline());
      assertTrue(connection == walletRpcs.get(0).getRpcConnection());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == connection);
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == connection);
      assertTrue(orderedConnections.get(0) == walletRpcs.get(0).getRpcConnection());
      assertEquals(orderedConnections.get(1).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(4) == walletRpcs.get(3).getRpcConnection());
      
      // connect to specific endpoint without authentication
      connection = orderedConnections.get(1);
      assertFalse(connection.isAuthenticated());
      connectionManager.setConnection(connection);
      assertFalse(connectionManager.isConnected());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      
      // connect to specific endpoint with authentication
      orderedConnections.get(1).setCredentials("rpc_user", "abc123");
      connectionManager.checkConnection();
      assertEquals(connectionManager.getConnection().getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(connection.isOnline());
      assertTrue(connection.isAuthenticated());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == connection);
      
      // test connection order
      orderedConnections = connectionManager.getConnections();
      assertTrue(orderedConnections.get(0) == connectionManager.getConnection());
      assertEquals(orderedConnections.get(0).getUri(), walletRpcs.get(1).getRpcConnection().getUri());
      assertTrue(orderedConnections.get(1) == walletRpcs.get(0).getRpcConnection());
      assertTrue(orderedConnections.get(2) == walletRpcs.get(4).getRpcConnection());
      assertTrue(orderedConnections.get(3) == walletRpcs.get(2).getRpcConnection());
      assertTrue(orderedConnections.get(4) == walletRpcs.get(3).getRpcConnection());
      for (int i = 0; i < orderedConnections.size() - 1; i++) assertTrue(i <= 1 ? orderedConnections.get(i).isOnline() : !orderedConnections.get(i).isOnline());
      assertFalse(orderedConnections.get(4).isOnline());
      
      // set connection to existing uri
      connectionManager.setConnection(walletRpcs.get(0).getRpcConnection().getUri());
      assertTrue(connectionManager.isConnected());
      assertTrue(walletRpcs.get(0).getRpcConnection() == connectionManager.getConnection());
      assertEquals(TestUtils.WALLET_RPC_USERNAME, connectionManager.getConnection().getUsername());
      assertEquals(TestUtils.WALLET_RPC_PASSWORD, connectionManager.getConnection().getPassword());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == walletRpcs.get(0).getRpcConnection());
      
      // set connection to new uri
      connectionManager.stopPolling();
      String uri = "http://localhost:49999";
      connectionManager.setConnection(uri);
      assertEquals(uri, connectionManager.getConnection().getUri());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertEquals(uri, listener.changedConnections.get(listener.changedConnections.size() - 1).getUri());
      
      // set connection to empty string
      connectionManager.setConnection("");
      assertEquals(null, connectionManager.getConnection());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      
      // check all connections and test auto switch
      connectionManager.checkConnections();
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(connectionManager.isConnected());

      // remove current connection and test auto switch
      connectionManager.removeConnection(connectionManager.getConnection().getUri());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertFalse(connectionManager.isConnected());
      connectionManager.checkConnections();
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(connectionManager.isConnected());

      // test polling current connection
      connectionManager.setConnection((String) null);
      assertFalse(connectionManager.isConnected());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      connectionManager.startPolling(TestUtils.SYNC_PERIOD_IN_MS, null, null, PollType.CURRENT, null);
      GenUtils.waitFor(TestUtils.AUTO_CONNECT_TIMEOUT_MS);
      assertTrue(connectionManager.isConnected());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());

      // test polling all connections
      connectionManager.setConnection((String) null);
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      connectionManager.startPolling(TestUtils.SYNC_PERIOD_IN_MS, null, null, PollType.ALL, null);
      GenUtils.waitFor(TestUtils.AUTO_CONNECT_TIMEOUT_MS);
      assertTrue(connectionManager.isConnected());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());

      // shut down all connections
      connection = connectionManager.getConnection();
      for (MoneroWalletRpc walletRpc : walletRpcs) TestUtils.stopWalletRpcProcess(walletRpc);
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS + 100);
      assertFalse(connection.isOnline());
      assertEquals(++numExpectedChanges, listener.changedConnections.size());
      assertTrue(listener.changedConnections.get(listener.changedConnections.size() - 1) == connection);
      
      // reset
      connectionManager.reset();
      assertEquals(0, connectionManager.getConnections().size());
      assertEquals(null, connectionManager.getConnection());
    } finally {

      // stop connection manager
      if (connectionManager != null) connectionManager.reset();
      
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
