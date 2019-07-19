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
  
  // test constants
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  public static final MoneroNetworkType NETWORK_TYPE = MoneroNetworkType.STAGENET;
  public static final String TEST_LANGUAGE = "English";
  public static final String TEST_MNEMONIC = "dilute arrow koala gorilla loudly uttered bygones stockpile problems efficient alerts lordship sighting launching hire tycoon hence ultimate fetches napkin cube usage agony army efficient"; 
  public static final String TEST_ADDRESS = "55PEGdaqz7iW3M4Puddfup1LumsrF2hHc4cQrVnFLVJ2eXexULWwYEUDRKHTRb6uouM9ybJs7GYcEXFht25jw3HZKHcULwp";
  public static final long TEST_RESTORE_HEIGHT = 363492l;
  
  // logger configuration
  public static final Logger LOGGER = Logger.getLogger(TestUtils.class);
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }
  
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
      assertEquals(TestUtils.TEST_MNEMONIC, walletRpc.getMnemonic());
      assertEquals(TestUtils.TEST_ADDRESS, walletRpc.getPrimaryAddress());
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
        walletJni = new MoneroWalletJni(TestUtils.WALLET_JNI_PATH_1, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, NETWORK_TYPE, daemonConnection, TEST_RESTORE_HEIGHT);
        walletJni.sync(new WalletSyncPrinter());
        walletJni.setAutoSync(true);
      }
      
      // otherwise open existing wallet and update daemon connection
      else {
        walletJni = new MoneroWalletJni(WALLET_JNI_PATH_1, WALLET_JNI_PW, MoneroNetworkType.STAGENET);
        walletJni.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
        walletJni.sync((long) 0, new WalletSyncPrinter());
        walletJni.setAutoSync(true);
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
