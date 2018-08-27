package test;

import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import utils.TestUtils;
import wallet.MoneroWallet;

/**
 * Tests a Monero wallet excluding sending transactions.
 */
public class TestMoneroWalletSends {
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  @Test
  public void testSend() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendSplit() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepAll() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepDust() {
    fail("Not yet implemented");
  }
}
