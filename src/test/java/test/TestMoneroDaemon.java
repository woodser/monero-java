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
  
  private MoneroDaemon daemon;
  private MoneroWallet wallet;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daemon = TestUtils.getDaemonRpc();
    wallet = TestUtils.getWalletRpc();
  }
  
  @Before
  public static void before() {
    
  }
  
  
}
