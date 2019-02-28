package test;

import org.junit.Before;
import org.junit.BeforeClass;

import monero.daemon.MoneroDaemon;
import monero.wallet.MoneroWallet;
import utils.TestUtils;

/**
 * Tests a Monero daemon.
 */
public class TestMoneroDaemon {
  
  // classes to test
  private static MoneroDaemon daemon;
  private static MoneroWallet wallet;
  
  // test configuration
  private static boolean testNonRelays = true;
  private static boolean testRelays = true; // creates and relays outgoing txs
  private static boolean testNotifications = true;
  
  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daemon = TestUtils.getDaemonRpc();
    wallet = TestUtils.getWalletRpc();
  }
  
  @Before
  public static void before() {
    
  }
}
