package test;

import static org.junit.Assert.fail;

import org.junit.BeforeClass;
import org.junit.Test;

import monero.wallet.MoneroWallet;

/**
 * Runs common tests that every Monero wallet implementation should support.
 * 
 * TODO: test filtering with not relayed
 */
public abstract class TestMoneroWalletCommon<T extends MoneroWallet> {
  
  // wallet to test
  private MoneroWallet wallet;
  
  /**
   * Subclasseses return wallet instance to test.
   */
  protected abstract T initWallet();

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }

  @Test
  public void test() {
    fail("Not yet implemented");
  }
}
