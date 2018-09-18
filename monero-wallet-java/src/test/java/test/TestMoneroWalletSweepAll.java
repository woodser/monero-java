package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import utils.TestUtils;
import wallet.MoneroWallet;
import wallet.model.MoneroSubaddress;
import wallet.model.MoneroTx;
import wallet.model.MoneroTxConfig;

/**
 * Tests sweeping all wallet funds.
 * 
 * These tests are separated because they use all unlocked funds which invalidates other write tests.
 */
public class TestMoneroWalletSweepAll {
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }

  @Test
  public void testSweepAllDefault() {
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
  public void testSweepAllSubaddresses() {
    
    // collect subaddress indices which contain unlocked balance
    List<Integer> subaddressIndices = new ArrayList<Integer>();
    List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(0);
    for (MoneroSubaddress subaddress : subaddresses) {
      if (subaddress.getUnlockedBalance().longValue() > 0) {
        subaddressIndices.add(subaddress.getIndex());
      }
    }
    
    // build sweep configuration
    MoneroTxConfig config = new MoneroTxConfig(wallet.getPrimaryAddress(), null, null);
    config.setAccountIdx(0);
    config.setSubaddressIndices(subaddressIndices);
    
    // sweep all
    List<MoneroTx> txs = wallet.sweepAll(config);
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertNotNull(tx.getKey());
      assertNotNull(tx.getBlob());
      assertNotNull(tx.getMetadata());
      TestUtils.testTx(tx);
    }
    
    // verify all balances no subaddresses contain unlocked balance
    subaddresses = wallet.getSubaddresses(0);
    for (MoneroSubaddress subaddress : subaddresses) {
      assertEquals(0, subaddress.getUnlockedBalance().longValue());
    }
  }
}
