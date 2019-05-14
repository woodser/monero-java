package utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpc;
import monero.rpc.MoneroRpcException;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;

/**
 * Test utilities and constants.
 */
public class TestUtils {
  
  // monero daemon rpc endpoint configuration (adjust per your configuration)
  private static final String DAEMON_RPC_URI = "http://localhost:38081";
  private static final String DAEMON_RPC_USERNAME = null;
  private static final String DAEMON_RPC_PASSWORD = null;  
  
  // monero wallet rpc configuration (adjust per your configuration)
  private static final String WALLET_RPC_URI = "http://localhost:38083";
  private static final String WALLET_RPC_USERNAME = "rpc_user";
  private static final String WALLET_RPC_PASSWORD = "abc123";
  public static final String WALLET_RPC_NAME_1 = "test_wallet_1";
  public static final String WALLET_RPC_NAME_2 = "test_wallet_2";
  public static final String WALLET_RPC_PW = "supersecretpassword123";
  
  // wallet jni configuration (adjust per your configuration)
  public static final String WALLET_JNI_PATH_1 = "test_wallet_1";
  public static final String WALLET_JNI_PATH_2 = "test_wallet_2";
  public static final String WALLET_JNI_PW = "supersecretpassword123";
  
  // test constants
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  public static final MoneroNetworkType NETWORK_TYPE = MoneroNetworkType.STAGENET;
  public static final String TEST_LANGUAGE = "English";
  public static final String TEST_MNEMONIC = "nagged giddy virtual bias spying arsenic fowls hexagon oars frying lava dialect copy gasp utensils muffin tattoo ritual exotic inmate kisses either sprig sunken sprig";
  public static final String TEST_ADDRESS = "59aZULsUF3YNSKGiHz4JPMfjGYkm1S4TB3sPsTr3j85HhXb9crZqGa7jJ8cA87U48kT5wzi2VzGZnN2PKojEwoyaHqtpeZh";
  
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
      MoneroRpc rpc = new MoneroRpc(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
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
      
      // construct wallet
      MoneroRpc rpc = new MoneroRpc(WALLET_RPC_URI, WALLET_RPC_USERNAME, WALLET_RPC_PASSWORD);
      walletRpc = new MoneroWalletRpc(rpc);
      
      // create rpc wallet file if necessary
      try {
        walletRpc.createWallet(WALLET_RPC_NAME_1, WALLET_RPC_PW, "English");
      } catch (MoneroRpcException e) {
        assertEquals(-21, (int) e.getCode());  // exception is ok if wallet already created
      }
      
      // open rpc wallet file
      try {
        walletRpc.openWallet(WALLET_RPC_NAME_1, WALLET_RPC_PW);
      } catch (MoneroRpcException e) {
        assertEquals(-1, (int) e.getCode()); // TODO (monero-wallet-rpc): -1: Failed to open wallet if wallet is already open; better code and message
      }
      
      // refresh wallet
      try {
        walletRpc.rescanSpent();
      } catch (MoneroRpcException e) {
        e.printStackTrace();
        assertEquals((int) -38, (int) e.getCode());  // TODO: (monero-wallet-rpc) sometimes getting -38: no connection to daemon on rescan call (after above calls)
        System.out.println("WARNING: received -38: no connection to daemon on rescan call after create/open, ignoring...");
      }
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
        MoneroRpc daemonConnection = new MoneroRpc(DAEMON_RPC_URI, DAEMON_RPC_USERNAME, DAEMON_RPC_PASSWORD);
        walletJni = MoneroWalletJni.createWalletFromMnemonic(NETWORK_TYPE, daemonConnection, TestUtils.TEST_MNEMONIC, 30000);
        walletJni.save(WALLET_JNI_PATH_1, WALLET_JNI_PW); 
      }
      
      // otherwise open existing wallet and update daemon connection
      else {
        walletJni = MoneroWalletJni.openWallet(WALLET_JNI_PATH_1, WALLET_JNI_PW);
        walletJni.setDaemonConnection(TestUtils.getDaemonRpc().getRpc());
      }
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
