package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Before;
import org.junit.Test;

import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxConfig;
import utils.TestUtils;

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
  public void testSweepAllFromAddresses() {
    
    // collect coordinates of all subaddresses that have unlocked balance
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue("testSweepAllFromAddresses() requires multiple accounts; run testSendFan()", accounts.size() > 1);
    Map<Integer, List<Integer>> subaddressIndexMap = new HashMap<Integer, List<Integer>>();
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      for (MoneroSubaddress subaddress : subaddresses) {
        if (subaddress.getUnlockedBalance().longValue() > 0) {
          List<Integer> subaddressIndices = subaddressIndexMap.get(account.getIndex());
          if (subaddressIndices == null) {
            subaddressIndices = new ArrayList<Integer>();
            subaddressIndexMap.put(account.getIndex(), subaddressIndices);
          }
          subaddressIndices.add(subaddress.getIndex());
        }
      }
    }
    
    // assert unlocked balance across accounts
    assertTrue("Test requires multiple accounts with unlocked balance; run testSendFan() and wait for funds to unlock", subaddressIndexMap.size() > 1);
    
    // sweep from each account
    for (Entry<Integer, List<Integer>> entry : subaddressIndexMap.entrySet()) {
      
      // build sweep configuration
      MoneroTxConfig config = new MoneroTxConfig(wallet.getPrimaryAddress(), null, null);
      config.setAccountIdx(entry.getKey());
      config.setSubaddressIndices(entry.getValue());
      
      // sweep all
      List<MoneroTx> txs = wallet.sweepAll(config);
      assertFalse(txs.isEmpty());
      for (MoneroTx tx : txs) {
        assertNotNull(tx.getKey());
        assertNotNull(tx.getBlob());
        assertNotNull(tx.getMetadata());
        TestUtils.testTx(tx);
      }
      
      // verify no unlocked balances
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(entry.getKey());
      for (MoneroSubaddress subaddress : subaddresses) {
        assertEquals(0, subaddress.getUnlockedBalance().longValue());
      }
    }
  }
}
