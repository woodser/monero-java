package utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpcConnection;
import monero.rpc.MoneroRpcException;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;

/**
 * Test utilities and constants.
 */
public class TestUtils {
  
  // monero daemon rpc endpoint configuration (adjust per your configuration)
  public static final String DAEMON_RPC_URI = "http://localhost:38081";
  public static final String DAEMON_RPC_USERNAME = null;
  public static final String DAEMON_RPC_PASSWORD = null;  
  
  // monero wallet rpc configuration (adjust per your configuration)
  public static final String WALLET_RPC_URI = "http://localhost:38083";
  public static final String WALLET_RPC_USERNAME = "rpc_user";
  public static final String WALLET_RPC_PASSWORD = "abc123";
  public static final String WALLET_RPC_NAME_1 = "test_wallet_1";
  public static final String WALLET_RPC_NAME_2 = "test_wallet_2";
  public static final String WALLET_RPC_PW = "supersecretpassword123";
  
  // wallet jni configuration (adjust per your configuration)
  public static final String TEST_WALLETS_DIR = "./test_wallets";
  public static final String WALLET_JNI_PATH_1 = TEST_WALLETS_DIR + "/test_wallet_1";
  public static final String WALLET_JNI_PATH_2 = TEST_WALLETS_DIR + "/test_wallet_2";
  public static final String WALLET_JNI_PW = "supersecretpassword123";
  
  // test wallet constants
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  public static final MoneroNetworkType NETWORK_TYPE = MoneroNetworkType.STAGENET;
  public static final String LANGUAGE = "English";
  public static final String MNEMONIC = "veteran inwardly films myriad godfather feel odds legion rarest dating adopt onward wetsuit atrium drying ruined relic refer bamboo voted baffles agnostic cycling dexterity relic"; 
  public static final String ADDRESS = "57QzhZAYSHeWG8EpUT8iNTcYATUoMv7hf6MoVCWge6HoV1vGTor1ZkSGa6pH1Xv3xMRh53m5KWFdbSnmmMmzSgpQCrVUyTL";
  public static final long RESTORE_HEIGHT = 370987;
  
  // logger configuration
  public static final Logger LOGGER = Logger.getLogger(TestUtils.class);
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }
  
  // used to track which wallets are in sync with pool so associated txs in the pool do not need to be waited on
  public static TxPoolWalletTracker TX_POOL_WALLET_TRACKER = new TxPoolWalletTracker();
  
  // test constants
  public static final int MIXIN = 11;
  
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
      
      // open rpc test wallet
      try {
        walletRpc.openWallet(WALLET_RPC_NAME_1, WALLET_RPC_PW);
      } catch (MoneroRpcException e) {
        if (-1 != e.getCode()) throw e; // -1 indicates wallet is already open, which is ok
      }
      
      // ensure we're testing the right wallet
      assertEquals(TestUtils.MNEMONIC, walletRpc.getMnemonic());
      assertEquals(TestUtils.ADDRESS, walletRpc.getPrimaryAddress());
    }
    
    // return cached wallet rpc
    return walletRpc;
  }
  
  /**
   * Get a singleton instance of a wallet supported by JNI.
   */
  private static MoneroWalletJni walletJni;
  public static MoneroWalletJni getWalletJni() {
    if (walletJni == null) {
      
      // create wallet from mnemonic phrase if it doesn't exist
      if (!MoneroWalletJni.walletExists(WALLET_JNI_PATH_1)) {
        MoneroRpcConnection daemonConnection = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
        walletJni = MoneroWalletJni.createWalletFromMnemonic(TestUtils.WALLET_JNI_PATH_1, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, NETWORK_TYPE, daemonConnection, RESTORE_HEIGHT);
        assertEquals(TestUtils.RESTORE_HEIGHT, walletJni.getRestoreHeight());
        walletJni.sync(new WalletSyncPrinter());
        walletJni.startSyncing();
      }
      
      // otherwise open existing wallet and update daemon connection
      else {
        walletJni = MoneroWalletJni.openWallet(WALLET_JNI_PATH_1, WALLET_JNI_PW, MoneroNetworkType.STAGENET);
        walletJni.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
        walletJni.sync(new WalletSyncPrinter());
        walletJni.startSyncing();
      }
      
      // Save and close the JNI wallet when the runtime is shutting down in order
      // to preserve local wallet data (e.g. destination addresses and amounts).
      // This is not necessary in the rpc wallet which saves automatically.
      Runtime.getRuntime().addShutdownHook(new Thread() {
        public void run() {
          walletJni.save();
          walletJni.close();
        }
      });
    }
    
    // return cached wallet jni
    return walletJni;
  }
  
  /**
   * Creates a new wallet which is considered to be "ground truth".
   * 
   * The best wallet known to serve as ground truth is a freshly created jni wallet restored from mnemonic.
   * By observation, it is consistent with a freshly restored wallet using cli even when persisted rpc and
   * jni wallets become inconsistent.
   * 
   * The caller of this method should close the returned wallet when done using it. // TODO: need to confirm close at end, but double .close() will seg fault, so need wallet.getIsClosed() check
   * 
   * @param networkType is the ground truth wallet's network type
   * @param mnemonic is the ground truth wallet's mnemonic
   * @param restoreHeight is the ground truth wallet's restore height
   */
  public static MoneroWalletJni createWalletGroundTruth(MoneroNetworkType networkType, String mnemonic, Long restoreHeight) {
    MoneroRpcConnection daemonConnection = new MoneroRpcConnection(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
    String path = TestUtils.TEST_WALLETS_DIR + "/gt_wallet_" + System.currentTimeMillis();
    MoneroWalletJni gtWallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, mnemonic, networkType, daemonConnection, restoreHeight);
    assertEquals(restoreHeight == null ? 0 : (long) restoreHeight, gtWallet.getRestoreHeight());
    gtWallet.sync(new WalletSyncPrinter());
    gtWallet.startSyncing();
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
  public static String getRandomWalletAddress() {
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
}
