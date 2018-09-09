package test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import utils.TestUtils;
import wallet.MoneroWallet;
import wallet.model.MoneroTx;
import wallet.model.MoneroTxConfig;

/**
 * Tests sweeping all wallet funds.
 * 
 * These tests are separated because they invalidate the other write tests because it uses all unlocked funds.
 */
public class TestMoneroWalletSweepAll {
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }

  @Test
  public void testSweepAll() {
    MoneroTxConfig config = new MoneroTxConfig(wallet.getPrimaryAddress(), null, null);
    List<MoneroTx> txs = wallet.sweepAll(config);
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertNotNull(tx.getKey());
      assertNotNull(tx.getBlob());
      assertNotNull(tx.getMetadata());
      TestUtils.testTx(tx);
    }
  }
  
  @Test
  public void testSweepAllFromSubaddresses() {
    throw new RuntimeException("Not implemented");
  }
}
