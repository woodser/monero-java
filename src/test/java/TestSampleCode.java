

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.math.BigInteger;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import monero.common.MoneroConnectionManager;
import monero.common.MoneroConnectionManagerListener;
import monero.common.MoneroRpcConnection;
import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Test the sample code for README.md.
 */
public class TestSampleCode {
  
  private static boolean FUNDS_RECEIVED = false;
  
  @BeforeAll
  public static void setUpBeforeClass() throws Exception {
    
    // pre-create test wallet
    MoneroWalletRpc wallet = TestUtils.getWalletRpc();
    wallet.close();

    // create directory for test wallets if it doesn't exist
    File testWalletsDir = new File(TestUtils.TEST_WALLETS_DIR);
    if (!testWalletsDir.exists()) testWalletsDir.mkdirs();
  }
  
  // Sample code demonstration
  @SuppressWarnings("unused")
  @Test
  public void testSampleCode() throws InterruptedException {
    
    // connect to daemon
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:28081", "", "");
    long height = daemon.getHeight();                       // 1523651
    List<MoneroTx> txsInPool = daemon.getTxPool();          // get transactions in the pool
    
    // create wallet from seed using JNI bindings to monero-project
    MoneroWalletFull walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
            .setPath("./test_wallets/" + UUID.randomUUID().toString())  // *** CHANGE README TO "sample_wallet_full" ***
            .setPassword("supersecretpassword123")
            .setNetworkType(MoneroNetworkType.TESTNET)
            .setServerUri("http://localhost:28081")
            .setServerUsername("superuser")
            .setServerPassword("abctesting123")
            .setSeed(TestUtils.SEED)                        // *** REPLACE WITH SEED IN README ***
            .setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT)); // *** REPLACE WITH FIRST RECEIVE HEIGHT IN README ***
    
    // synchronize the wallet and receive progress notifications
    walletFull.sync(new MoneroWalletListener() {
      @Override
      public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
        // feed a progress bar?
      }
    });
    
    // synchronize in the background every 5 seconds
    walletFull.startSyncing(5000l);
    
    // receive notifications when funds are received, confirmed, and unlocked
    walletFull.addListener(new MoneroWalletListener() {
      @Override
      public void onOutputReceived(MoneroOutputWallet output) {
        BigInteger amount = output.getAmount();
        String txHash = output.getTx().getHash();
        Boolean isConfirmed = output.getTx().isConfirmed();
        Boolean isLocked = output.getTx().isLocked();
        FUNDS_RECEIVED = true;
      }
    });

    // connect to wallet RPC and open wallet
    MoneroWalletRpc walletRpc = new MoneroWalletRpc(TestUtils.WALLET_RPC_URI, TestUtils.WALLET_RPC_USERNAME, TestUtils.WALLET_RPC_PASSWORD); // *** REPLACE WITH CONSTANTS IN README ***
    walletRpc.openWallet("test_wallet_1", "supersecretpassword123");  // *** CHANGE README TO "sample_wallet_rpc" ***
    String primaryAddress = walletRpc.getPrimaryAddress();  // 555zgduFhmKd2o8rPUz...
    BigInteger balance = walletRpc.getBalance();            // 533648366742
    List<MoneroTxWallet> txs = walletRpc.getTxs();          // get transactions containing transfers to/from the wallet
    
    // send funds from RPC wallet to full wallet
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(walletRpc);                                     // *** REMOVE FROM README SAMPLE ***
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(walletRpc, 0, null, new BigInteger("250000000000")); // *** REMOVE FROM README SAMPLE ***
    MoneroTxWallet createdTx = walletRpc.createTx(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(walletFull.getAddress(1, 0))
            .setAmount("250000000000") // send 0.25 XMR (denominated in atomic units)
            .setRelay(false)); // create transaction and relay to the network if true
    BigInteger fee = createdTx.getFee(); // "Are you sure you want to send... ?"
    walletRpc.relayTx(createdTx); // relay the transaction
    
    // recipient receives unconfirmed funds within 5 seconds
    TimeUnit.SECONDS.sleep(5);
    assertTrue(FUNDS_RECEIVED);
    
    // save and close full wallet
    walletFull.close(true);
  }
  
  // Connection manager demonstration
  @SuppressWarnings("unused")
  @Test
  public void testConnectionManagerDemo() {
    
    // create connection manager
    MoneroConnectionManager connectionManager = new MoneroConnectionManager();
    
    // add managed connections with priorities
    connectionManager.addConnection(new MoneroRpcConnection("http://localhost:28081").setPriority(1)); // use localhost as first priority
    connectionManager.addConnection(new MoneroRpcConnection("http://example.com")); // default priority is prioritized last
    
    // set current connection
    connectionManager.setConnection(new MoneroRpcConnection("http://foo.bar", "admin", "password")); // connection is added if new

    // create or open wallet governed by connection manager
    MoneroWalletFull walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPath("./test_wallets/" + UUID.randomUUID().toString())  // *** CHANGE README TO "sample_wallet_full" ***
        .setPassword("supersecretpassword123")
        .setNetworkType(MoneroNetworkType.TESTNET)
        .setConnectionManager(connectionManager)
        .setSeed(TestUtils.SEED)                        // *** REPLACE WITH SEED IN README ***
        .setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT)); // *** REPLACE WITH FIRST RECEIVE HEIGHT IN README ***
    
    // check connection status
    connectionManager.checkConnection();
    System.out.println("Connection manager is connected: " + connectionManager.isConnected());
    System.out.println("Connection is online: " + connectionManager.getConnection().isOnline());
    System.out.println("Connection is authenticated: " + connectionManager.getConnection().isAuthenticated());
    
    // receive notifications of any changes to current connection
    connectionManager.addListener(new MoneroConnectionManagerListener() {
      @Override
      public void onConnectionChanged(MoneroRpcConnection connection) {
        System.out.println("Connection changed to: " + connection);
      }
    });
    
    // check connections every 10 seconds (in order of priority) and switch to the best
    connectionManager.startPolling(10000l);
    
    // get best available connection in order of priority then response time
    MoneroRpcConnection bestConnection = connectionManager.getBestAvailableConnection();
    
    // check status of all connections
    connectionManager.checkConnections();
    
    // get connections in order of current connection, online status from last check, priority, and name
    List<MoneroRpcConnection> connections = connectionManager.getConnections();
    
    // clear connection manager
    connectionManager.clear();
  }

  @SuppressWarnings("unused")
  private static void proxySampleCodeMonerod1() {

    // create client connected to monerod with tor config
    MoneroRpcConnection monerodConnection = new MoneroRpcConnection("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081")
        .setCredentials("superuser", "abctesting123")
        .setProxyUri("<tor proxy uri>");
    MoneroDaemonRpc monerod = new MoneroDaemonRpc(monerodConnection);
  }

  @SuppressWarnings("unused")
  private static void proxySampleCodeMonerod2() {

    // connect to monerod with tor address
    MoneroDaemonRpc monerod = new MoneroDaemonRpc("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081"); // can add username and password

    // set tor's proxy uri
    monerod.setProxyUri("<tor proxy uri>");
  }

  @SuppressWarnings("unused")
  private static void proxySampleCodeWalletRpc() {

      // create client connected to your monero-wallet-rpc instance
      MoneroWallet walletRpc = new MoneroWalletRpc("http://localhost:38084"); // can add username and password

      // open or create a wallet
      // ...

      // set connection to monerod with tor config
      MoneroRpcConnection monerodConnection = new MoneroRpcConnection("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081")
              .setCredentials("superuser", "abctesting123")
              .setProxyUri("<tor proxy uri>");
      walletRpc.setDaemonConnection(monerodConnection);
  }

  @SuppressWarnings("unused")
  private static void proxySampleCodeWalletFull() {

      // option 1: get or create full wallet with tor config
      MoneroWallet walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
              .setPath("sample_wallet_full")
              .setPassword("supersecretpassword123")
              .setNetworkType(MoneroNetworkType.MAINNET)
              .setSeed("hefty value scenic...")
              .setRestoreHeight(573936l)
              .setServerUri("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081")
              .setServerUsername("superuser")
              .setServerPassword("abctesting123")
              .setServerProxyUri("<tor proxy uri>"));

      // option 2: set connection to monerod with tor config
      MoneroRpcConnection monerodConnection = new MoneroRpcConnection("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081")
              .setCredentials("superuser", "abctesting123")
              .setProxyUri("<tor proxy uri>");
      walletFull.setDaemonConnection(monerodConnection);
  }
}
