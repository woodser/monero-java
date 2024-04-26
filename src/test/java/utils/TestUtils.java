package utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroRpcError;
import monero.common.MoneroUtils;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;

/**
 * Test utilities and constants.
 */
public class TestUtils {
  
  // c++ log configuration
  public static boolean CPP_LOG_ENABLED = false;
  public static String CPP_LOG_PATH = "log_java_tests.txt";
  public static int CPP_LOG_LEVEL = 1;
  public static boolean CPP_LOG_CONSOLE = true;
  static {
    if (CPP_LOG_ENABLED) {
      MoneroUtils.loadNativeLibrary();
      MoneroUtils.configureNativeLogging(CPP_LOG_PATH, CPP_LOG_CONSOLE);
      MoneroUtils.setLogLevel(CPP_LOG_LEVEL);
    }
  }
  
  // directory with monero binaries to test (monerod and monero-wallet-rpc)
  public static final String MONERO_BINS_DIR = "";
  
  // monero daemon rpc endpoint configuration (change per your configuration)
  public static final String DAEMON_RPC_URI = "localhost:28081";
  public static final String DAEMON_RPC_USERNAME = "";
  public static final String DAEMON_RPC_PASSWORD = "";
  public static final String DAEMON_LOCAL_PATH = MONERO_BINS_DIR + "/monerod";
  
  // monero wallet rpc configuration (change per your configuration)
  public static final int WALLET_RPC_PORT_START = 28084; // test wallet executables will bind to consecutive ports after these
  public static final boolean WALLET_RPC_ZMQ_ENABLED = false;
  public static final int WALLET_RPC_ZMQ_PORT_START = 58083;
  public static final int WALLET_RPC_ZMQ_BIND_PORT_START = 48083;  // TODO: zmq bind port necessary?
  public static final String WALLET_RPC_USERNAME = "rpc_user";
  public static final String WALLET_RPC_PASSWORD = "abc123";
  public static final String WALLET_RPC_ZMQ_DOMAIN = "127.0.0.1";
  public static final String WALLET_RPC_DOMAIN = "localhost";
  public static final String WALLET_RPC_URI = WALLET_RPC_DOMAIN + ":" + WALLET_RPC_PORT_START;
  public static final String WALLET_RPC_ZMQ_URI = "tcp://" + WALLET_RPC_ZMQ_DOMAIN + ":" + WALLET_RPC_ZMQ_PORT_START;
  public static final String WALLET_RPC_LOCAL_PATH = MONERO_BINS_DIR + "/monero-wallet-rpc";
  public static final String WALLET_RPC_LOCAL_WALLET_DIR = MONERO_BINS_DIR;
  public static final String WALLET_RPC_ACCESS_CONTROL_ORIGINS = "http://localhost:8080"; // cors access from web browser
  
  // test wallet config
  public static final String WALLET_NAME = "test_wallet_1";
  public static final String WALLET_PASSWORD = "supersecretpassword123";
  public static final String TEST_WALLETS_DIR = "./test_wallets";
  public static final String WALLET_FULL_PATH = TEST_WALLETS_DIR + "/" + WALLET_NAME;
  
  // test wallet constants
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  public static final MoneroNetworkType NETWORK_TYPE = MoneroNetworkType.TESTNET;
  public static final String LANGUAGE = "English";
  public static final String SEED = "silk mocked cucumber lettuce hope adrenalin aching lush roles fuel revamp baptism wrist long tender teardrop midst pastry pigment equip frying inbound pinched ravine frying";
  public static final String ADDRESS = "A1y9sbVt8nqhZAVm3me1U18rUVXcjeNKuBd1oE2cTs8biA9cozPMeyYLhe77nPv12JA3ejJN3qprmREriit2fi6tJDi99RR";
  public static final long FIRST_RECEIVE_HEIGHT = 171; // NOTE: this value must be the height of the wallet's first tx for tests
  public static final long SYNC_PERIOD_IN_MS = 5000; // period between wallet syncs in milliseconds
  public static final String OFFLINE_SERVER_URI = "offline_server_uri"; // dummy server uri to remain offline because wallet2 connects to default if not given
  public static final long AUTO_CONNECT_TIMEOUT_MS = 3000;
  
  // logger configuration
  public static final Logger LOGGER = Logger.getLogger(TestUtils.class.getName());
  static {
    try {
      InputStream is = TestUtils.class.getClassLoader().getResourceAsStream("logger.properties");
      if (is != null) LogManager.getLogManager().readConfiguration(is);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
  
  // used to track wallet txs for tests
  public static WalletTxTracker WALLET_TX_TRACKER = new WalletTxTracker();
  
  /**
   * Get a singleton instance of a monerod client.
   */
  private static MoneroDaemonRpc daemonRpc;
  public static MoneroDaemonRpc getDaemonRpc() {
    if (daemonRpc == null) {
      MoneroRpcConnection rpc = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
      daemonRpc = new MoneroDaemonRpc(rpc);
    }
    return daemonRpc;
  }
  
  /**
   * Get a singleton instance of a monero-wallet-rpc client.
   */
  private static MoneroWalletRpc walletRpc;
  public static MoneroWalletRpc getWalletRpc() {
    if (walletRpc == null) {
      
      // construct wallet rpc instance with daemon connection
      MoneroRpcConnection rpc = new MoneroRpcConnection(WALLET_RPC_URI, WALLET_RPC_USERNAME, WALLET_RPC_PASSWORD, WALLET_RPC_ZMQ_ENABLED ? WALLET_RPC_ZMQ_URI : null);
      walletRpc = new MoneroWalletRpc(rpc);
    }
    
    // attempt to open test wallet
    try {
      walletRpc.openWallet(WALLET_NAME, WALLET_PASSWORD);
    } catch (MoneroRpcError e) {
      
      // -1 returned when wallet does not exist or fails to open e.g. it's already open by another application
      if (e.getCode() == -1) {
        
        // create wallet
        walletRpc.createWallet(new MoneroWalletConfig().setPath(WALLET_NAME).setPassword(WALLET_PASSWORD).setSeed(SEED).setRestoreHeight(FIRST_RECEIVE_HEIGHT));
      } else {
        throw e;
      }
    }
    
    // ensure we're testing the right wallet
    assertEquals(TestUtils.SEED, walletRpc.getSeed());
    assertEquals(TestUtils.ADDRESS, walletRpc.getPrimaryAddress());
    
    // sync and save wallet
    walletRpc.sync();
    walletRpc.save();
    walletRpc.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    
    // return cached wallet rpc
    return walletRpc;
  }
  
  /**
   * Start a monero-wallet-rpc process bound to the next available port.
   */
  public static Map<MoneroWalletRpc, Integer> WALLET_PORT_OFFSETS = new HashMap<MoneroWalletRpc, Integer>();
  public static MoneroWalletRpc startWalletRpcProcess() { return TestUtils.startWalletRpcProcess(false); }
  public static MoneroWalletRpc startWalletRpcProcess(boolean offline) {
    
    // get next available offset of ports to bind to
    int portOffset = 1;
    while (WALLET_PORT_OFFSETS.values().contains(portOffset)) portOffset++;
    
    // create command to start client with internal monero-wallet-rpc process
    List<String> cmd = new ArrayList<String>(Arrays.asList(
        TestUtils.WALLET_RPC_LOCAL_PATH,
        "--" + TestUtils.NETWORK_TYPE.toString().toLowerCase(),
        "--rpc-bind-port", "" + (TestUtils.WALLET_RPC_PORT_START + portOffset),
        "--rpc-login", TestUtils.WALLET_RPC_USERNAME + ":" + TestUtils.WALLET_RPC_PASSWORD,
        "--wallet-dir", TestUtils.WALLET_RPC_LOCAL_WALLET_DIR,
        "--rpc-access-control-origins", TestUtils.WALLET_RPC_ACCESS_CONTROL_ORIGINS));
    if (offline) cmd.add("--offline");
    else cmd.addAll(Arrays.asList("--daemon-address", TestUtils.DAEMON_RPC_URI));
    if (TestUtils.DAEMON_RPC_USERNAME != null && !TestUtils.DAEMON_RPC_USERNAME.equals("")) cmd.addAll(Arrays.asList("--daemon-login", TestUtils.DAEMON_RPC_USERNAME + ":" + TestUtils.DAEMON_RPC_PASSWORD));
    
    // start with zmq if enabled
    if (WALLET_RPC_ZMQ_ENABLED) {
      cmd.addAll(Arrays.asList("--zmq-rpc-bind-port", "" + (TestUtils.WALLET_RPC_ZMQ_BIND_PORT_START + portOffset)));
      cmd.addAll(Arrays.asList("--zmq-pub", "tcp://" + WALLET_RPC_ZMQ_DOMAIN + ":" + (TestUtils.WALLET_RPC_ZMQ_PORT_START + portOffset)));
    } else {
      //cmd.add("--no-zmq"); // TODO: enable this when zmq supported in monero-wallet-rpc
    }
    
    // register wallet with port offset
    try {
      MoneroWalletRpc wallet = new MoneroWalletRpc(cmd);
      WALLET_PORT_OFFSETS.put(wallet, portOffset);
      return wallet;
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Stop a monero-wallet-rpc process and release its associated port.
   * 
   * @param walletRpc - wallet created with internal monero-wallet-rpc process
   */
  public static void stopWalletRpcProcess(MoneroWalletRpc walletRpc) throws InterruptedException {
    WALLET_PORT_OFFSETS.remove(walletRpc);
    walletRpc.stopProcess();
  }
  
  /**
   * Get a singleton instance of a wallet supported by JNI bindings to monero-project's wallet2.
   */
  private static MoneroWalletFull walletFull;
  public static MoneroWalletFull getWalletFull() {
    if (walletFull == null || walletFull.isClosed()) {
      
      // create wallet from seed if it doesn't exist
      if (!MoneroWalletFull.walletExists(WALLET_FULL_PATH)) {
        
        // create directory for test wallets if it doesn't exist
        File testWalletsDir = new File(TestUtils.TEST_WALLETS_DIR);
        if (!testWalletsDir.exists()) testWalletsDir.mkdirs();
        
        // create wallet with connection
        MoneroRpcConnection daemonConnection = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
        walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_FULL_PATH).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(NETWORK_TYPE).setSeed(TestUtils.SEED).setServer(daemonConnection).setRestoreHeight(FIRST_RECEIVE_HEIGHT));
        assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletFull.getRestoreHeight());
        assertEquals(daemonConnection, walletFull.getDaemonConnection());
      }
      
      // otherwise open existing wallet and update daemon connection
      else {
        walletFull = MoneroWalletFull.openWallet(WALLET_FULL_PATH, WALLET_PASSWORD, TestUtils.NETWORK_TYPE);
        walletFull.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
      }

      // sync and save wallet
      walletFull.sync(new WalletSyncPrinter());
      walletFull.save();
      walletFull.startSyncing(TestUtils.SYNC_PERIOD_IN_MS); // start background synchronizing with sync period
    }
    
    // ensure we're testing the right wallet
    assertEquals(TestUtils.SEED, walletFull.getSeed());
    assertEquals(TestUtils.ADDRESS, walletFull.getPrimaryAddress());
    return walletFull;
  }
  
  /**
   * Creates a new wallet considered to be "ground truth".
   * 
   * @param networkType is the ground truth wallet's network type
   * @param seed is the ground truth wallet's seed
   * @param startHeight is the height to start syncing from
   * @param restoreHeight is the ground truth wallet's restore height
   * @return the created wallet
   */
  public static MoneroWalletFull createWalletGroundTruth(MoneroNetworkType networkType, String seed, Long startHeight, Long restoreHeight) {
    
    // create directory for test wallets if it doesn't exist
    File testWalletsDir = new File(TestUtils.TEST_WALLETS_DIR);
    if (!testWalletsDir.exists()) testWalletsDir.mkdirs();
    
    // create ground truth wallet
    MoneroRpcConnection daemonConnection = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
    String path = TestUtils.TEST_WALLETS_DIR + "/gt_wallet_" + System.currentTimeMillis();
    MoneroWalletFull gtWallet = MoneroWalletFull.createWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(networkType).setSeed(seed).setServer(daemonConnection).setRestoreHeight(restoreHeight));
    assertEquals(restoreHeight == null ? 0 : (long) restoreHeight, gtWallet.getRestoreHeight());
    gtWallet.sync(startHeight, new WalletSyncPrinter());
    gtWallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    
    // close the full wallet when the runtime is shutting down to release resources
    Runtime.getRuntime().addShutdownHook(new Thread() {
      @Override
      public void run() {
        gtWallet.close();
      }
    });
    
    return gtWallet;
  }
  
  public static void testUnsignedBigInteger(BigInteger num) {
    testUnsignedBigInteger(num, null);
  }
  
  public static void testUnsignedBigInteger(BigInteger num, Boolean nonZero) {
    assertNotNull(num);
    int comparison = num.compareTo(BigInteger.valueOf(0));
    assertTrue(comparison >= 0);
    if (Boolean.TRUE.equals(nonZero)) assertTrue(comparison > 0);
    if (Boolean.FALSE.equals(nonZero)) assertTrue(comparison == 0);
  }
  
  // TODO: switch to local wallet (like js version) if/when it can generate addresses
  public static String getExternalWalletAddress() {
    MoneroNetworkType networkType = getDaemonRpc().getInfo().getNetworkType();
    switch (networkType) {
      case STAGENET:
        return "78Zq71rS1qK4CnGt8utvMdWhVNMJexGVEDM2XsSkBaGV9bDSnRFFhWrQTbmCACqzevE8vth9qhWfQ9SUENXXbLnmMVnBwgW"; // subaddress
      case TESTNET:
        return "BhsbVvqW4Wajf4a76QW3hA2B3easR5QdNE5L8NwkY7RWXCrfSuaUwj1DDUsk3XiRGHBqqsK3NPvsATwcmNNPUQQ4SRR2b3V"; // subaddress
      case MAINNET:
        return "87a1Yf47UqyQFCrMqqtxfvhJN9se3PgbmU7KUFWqhSu5aih6YsZYoxfjgyxAM1DztNNSdoYTZYn9xa3vHeJjoZqdAybnLzN"; // subaddress
      default:
        throw new RuntimeException("Invalid network type: " + networkType);
    }
  }
  
  public static boolean txsMergeable(MoneroTxWallet tx1, MoneroTxWallet tx2) {
    try {
      MoneroTxWallet copy1 = tx1.copy();
      MoneroTxWallet copy2 = tx2.copy();
      if (copy1.isConfirmed()) copy1.setBlock(tx1.getBlock().copy().setTxs(Arrays.asList(copy1)));
      if (copy2.isConfirmed()) copy2.setBlock(tx2.getBlock().copy().setTxs(Arrays.asList(copy2)));
      copy1.merge(copy2);
      return true;
    } catch (Exception | AssertionError e) {
      e.printStackTrace();
      return false;
    }
  }
}
