package test;

import monero.daemon.MoneroDaemonRpc;
import monero.wallet.MoneroWallet;
import utils.TestUtils;

/**
 * Test base for subclasses to supply the wallet or daemon to test.
 */
public abstract class TestMoneroBase {

  /**
   * Get the daemon to test.
   * 
   * @return the daemon to test
   */
  protected MoneroDaemonRpc getTestDaemon() {
    return TestUtils.getDaemonRpc();
  }
  
  /**
   * Get the wallet to test.
   * 
   * @return the wallet to test
   */
  protected abstract MoneroWallet getTestWallet();
}
