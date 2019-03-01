package utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

import monero.daemon.MoneroDaemonRpc;
import monero.rpc.MoneroRpc;
import monero.rpc.MoneroRpcException;
import monero.wallet.MoneroWalletRpc;

/**
 * Test utilities and constants.
 */
public class TestUtils {
  
  // monero-daemon-rpc endpoint configuration (adjust per your configuration)
  private static final String DAEMON_RPC_URI = "http://localhost:28081";
  private static final String DAEMON_RPC_USERNAME = null;
  private static final String DAEMON_RPC_PASSWORD = null;  
  
  // monero-wallet-rpc endpoint configuration (adjust per your configuration)
  private static final String WALLET_RPC_URI = "http://localhost:28083";
  private static final String WALLET_RPC_USERNAME = "rpc_user";
  private static final String WALLET_RPC_PASSWORD = "abc123";
  
  // names of test wallets
  public static final String WALLET_RPC_NAME_1 = "test_wallet_1";
  public static final String WALLET_RPC_NAME_2 = "test_wallet_2";
  public static final String WALLET_RPC_PW = "supersecretpassword123";
  
  // test constants
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  public static final String TEST_MNEMONIC = "cage moon giant fall library framed adrenalin yawning ledge voice tell jingle gusts kangaroo paddles boldly hydrogen ripped dangerous gleeful jeers cell sequence spud giant";
  public static final String TEST_ADDRESS = "9ygBCoxDMKBW9Tasxcsk29cLB54QsKkYb2m3Sne9c74ziUwepWQiXfKBoF42K9Xd38VBQWswF5nuf2QcRdJd2Dn69tyiTi2";
  
  // logger configuration
  public static final Logger LOGGER = Logger.getLogger(TestUtils.class);
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }
  
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
   * Get a wallet RPC singleton instance shared among tests.
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
        assertEquals(-21, e.getRpcCode());  // exception is ok if wallet already created
      }
      
      // open rpc wallet file
      try {
        walletRpc.openWallet(WALLET_RPC_NAME_1, WALLET_RPC_PW);
      } catch (MoneroRpcException e) {
        assertEquals(-1, e.getRpcCode()); // TODO (monero-wallet-rpc): -1: Failed to open wallet if wallet is already open; better code and message
      }
      
      // refresh wallet
      try {
        walletRpc.rescanSpent();
      } catch (MoneroRpcException e) {
        assertEquals((int) -38, (int) e.getRpcCode());  // TODO: (monero-wallet-rpc) sometimes getting -38: no connection to daemon on rescan call (after above calls) which causes mocha "before all" hook problem
        System.out.println("WARNING: received -38: no connection to daemon on rescan call after create/open, ignoring...");
      }
    }
    
    // return cached wallet rpc
    return walletRpc;
  }
  
  public static void testUnsignedBigInteger(BigInteger num) {
    testUnsignedBigInteger(num, null);
  }
  
  public static void testUnsignedBigInteger(BigInteger num, Boolean nonZero) {
    assertNotNull(num);
    assertTrue(num.intValue() >= 0);
    if (nonZero == true) assertTrue(num.intValue() > 0);
    if (nonZero == false) assertTrue(num.intValue() == 0);
  }
}
