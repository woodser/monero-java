package utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import monero.common.MoneroRpcConnection;
import monero.common.MoneroRpcError;
import monero.common.MoneroUtils;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWalletJni;
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
      System.loadLibrary("monero-java");
      MoneroUtils.initJniLogging(CPP_LOG_PATH, CPP_LOG_LEVEL, CPP_LOG_CONSOLE);
    }
  }
  
  // monero daemon rpc endpoint configuration (adjust per your configuration)
  public static final String DAEMON_RPC_URI = "http://localhost:38081";
  public static final String DAEMON_RPC_USERNAME = "superuser";
  public static final String DAEMON_RPC_PASSWORD = "abctesting123";  
  
  // monero wallet rpc configuration (adjust per your configuration)
  public static final String WALLET_RPC_URI = "http://localhost:38083";
  public static final String WALLET_RPC_USERNAME = "rpc_user";
  public static final String WALLET_RPC_PASSWORD = "abc123";

  // test wallet config
  public static final String WALLET_NAME = "test_wallet_1";
  public static final String WALLET_PASSWORD = "supersecretpassword123";
  public static final String TEST_WALLETS_DIR = "./test_wallets";
  public static final String WALLET_JNI_PATH = TEST_WALLETS_DIR + "/" + WALLET_NAME;
  
  // test wallet constants
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  public static final MoneroNetworkType NETWORK_TYPE = MoneroNetworkType.STAGENET;
  public static final String LANGUAGE = "English";
  public static final String MNEMONIC = "niece cube almost phase zeal ultimate pyramid tapestry hickory bulb bifocals festival always wayside sphere kept upwards wagtail invoke radar pager flippant sensible stunning kept"; 
  public static final String ADDRESS = "59dF9pSotECe1Fn4dBGZXWHYyNdo53rbZ7YYseu9jBKCf4c2cUzhuFVRH8HuD4wyaKTqtD3VF3F4eQe3Kzq342F5U8R4jeq";
  public static final long FIRST_RECEIVE_HEIGHT = 201; // NOTE: this value MUST be the height of the wallet's first tx for tests
  
  // logger configuration
  public static final Logger LOGGER = Logger.getLogger(TestUtils.class.getName());
  static {
    try {
      InputStream is = TestUtils.class.getClassLoader().getResourceAsStream("logger.properties");
      LogManager.getLogManager().readConfiguration(is);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
  
  // used to track which wallets are in sync with pool so associated txs in the pool do not need to be waited on
  public static TxPoolWalletTracker TX_POOL_WALLET_TRACKER = new TxPoolWalletTracker();
  
  /**
   * Get a daemon RPC singleton instance shared among tests.
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
   * Get a singleton instance of a wallet supported by RPC.
   */
  private static MoneroWalletRpc walletRpc;
  public static MoneroWalletRpc getWalletRpc() {
    if (walletRpc == null) {
      
      // construct wallet rpc instance with daemon connection
      MoneroRpcConnection rpc = new MoneroRpcConnection(WALLET_RPC_URI, WALLET_RPC_USERNAME, WALLET_RPC_PASSWORD);
      walletRpc = new MoneroWalletRpc(rpc);
    }
    
    // attempt to open test wallet
    try {
      walletRpc.openWallet(WALLET_NAME, WALLET_PASSWORD);
    } catch (MoneroRpcError e) {
      
      // -1 returned when wallet does not exist or fails to open e.g. it's already open by another application
      if (e.getCode() == -1) {
        
        // create wallet
        walletRpc.createWallet(new MoneroWalletConfig().setPath(WALLET_NAME).setPassword(WALLET_PASSWORD).setMnemonic(MNEMONIC).setRestoreHeight(FIRST_RECEIVE_HEIGHT));
      } else {
        throw e;
      }
    }
    
    // ensure we're testing the right wallet
    assertEquals(TestUtils.MNEMONIC, walletRpc.getMnemonic());
    assertEquals(TestUtils.ADDRESS, walletRpc.getPrimaryAddress());
    
    // sync and save the wallet
    walletRpc.sync();
    walletRpc.save();
    
    // return cached wallet rpc
    return walletRpc;
  }
  
  /**
   * Get a singleton instance of a wallet supported by JNI.
   */
  private static MoneroWalletJni walletJni;
  public static MoneroWalletJni getWalletJni() {
    if (walletJni == null || walletJni.isClosed()) {
      
      // create wallet from mnemonic phrase if it doesn't exist
      if (!MoneroWalletJni.walletExists(WALLET_JNI_PATH)) {
        
        // create directory for test wallets if it doesn't exist
        File testWalletsDir = new File(TestUtils.TEST_WALLETS_DIR);
        if (!testWalletsDir.exists()) testWalletsDir.mkdirs();
        
        // create wallet with connection
        MoneroRpcConnection daemonConnection = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
        walletJni = MoneroWalletJni.createWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_JNI_PATH).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(NETWORK_TYPE).setMnemonic(TestUtils.MNEMONIC).setServer(daemonConnection).setRestoreHeight(FIRST_RECEIVE_HEIGHT));
        assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletJni.getSyncHeight());
        walletJni.sync(new WalletSyncPrinter());
        walletJni.save();
        walletJni.startSyncing();
      }
      
      // otherwise open existing wallet and update daemon connection
      else {
        walletJni = MoneroWalletJni.openWallet(WALLET_JNI_PATH, WALLET_PASSWORD, TestUtils.NETWORK_TYPE);
        walletJni.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
        walletJni.sync(new WalletSyncPrinter());
        walletJni.startSyncing();
      }
      
      // Save and close the JNI wallet when the runtime is shutting down in order
      // to preserve local wallet data (e.g. destination addresses and amounts).
      // This is not necessary in the rpc wallet which saves automatically.
      Runtime.getRuntime().addShutdownHook(new Thread() {
        public void run() {
          walletJni.close(true);
        }
      });
    }
    
    // ensure we're testing the right wallet
    assertEquals(TestUtils.MNEMONIC, walletJni.getMnemonic());
    assertEquals(TestUtils.ADDRESS, walletJni.getPrimaryAddress());
    return walletJni;
  }
  
  /**
   * Creates a new wallet considered to be "ground truth".
   * 
   * @param networkType is the ground truth wallet's network type
   * @param mnemonic is the ground truth wallet's mnemonic
   * @param restoreHeight is the ground truth wallet's restore height
   * @return the created wallet
   */
  public static MoneroWalletJni createWalletGroundTruth(MoneroNetworkType networkType, String mnemonic, Long restoreHeight) {
    MoneroRpcConnection daemonConnection = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
    String path = TestUtils.TEST_WALLETS_DIR + "/gt_wallet_" + System.currentTimeMillis();
    MoneroWalletJni gtWallet = MoneroWalletJni.createWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(networkType).setMnemonic(mnemonic).setServer(daemonConnection).setRestoreHeight(restoreHeight));
    assertEquals(restoreHeight == null ? 0 : (long) restoreHeight, gtWallet.getSyncHeight());
    gtWallet.sync(new WalletSyncPrinter());
    gtWallet.startSyncing();
    
    // close the JNI wallet when the runtime is shutting down to release resources
    Runtime.getRuntime().addShutdownHook(new Thread() {
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
        //return "59bc81VNoucPsbJSH648GLLssjSMT9K1vLwNv63edf3fLUHBMt7FHtQdVA2XhDSRwi5uBwXWUkUBg29pouuBbvw98XDXjFy"; // primary
        return "78Zq71rS1qK4CnGt8utvMdWhVNMJexGVEDM2XsSkBaGV9bDSnRFFhWrQTbmCACqzevE8vth9qhWfQ9SUENXXbLnmMVnBwgW"; // subaddress
      case TESTNET:
        //return "9ufu7oz6G1d7bfJgRfwtEh3iYYqxNABraPLaYXhAK1TiPjuH8tWfomALhTfeWUCnDwjFqmgrtqccKGcwYqyCgdzY1dA5Tda"; // primary
        return "BhsbVvqW4Wajf4a76QW3hA2B3easR5QdNE5L8NwkY7RWXCrfSuaUwj1DDUsk3XiRGHBqqsK3NPvsATwcmNNPUQQ4SRR2b3V"; // subaddress
      case MAINNET:
        //return "47Pf4v4sG6H5CDcAY1sY6qQjYYe2w7UxrBR7j54DYTQBXxFjTtuQtjBdDZV3q8n4hTC8PKjXVGnFTTZJWB7wBipYNc47XwP"; // primary
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
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }
}
