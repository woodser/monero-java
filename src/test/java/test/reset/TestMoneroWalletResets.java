package test.reset;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import common.types.Pair;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxConfig;
import utils.PrintBalances;
import utils.TestUtils;

/**
 * Runs tests that reset the wallet state including sweeping all funds to the primary account and
 * rescanning the blockchain which resets the local wallet cache (e.g. send destinations).
 * 
 * These tests are separated because they use all unlocked funds which invalidates other tests until
 * testSendToMultiple() is run.
 */
public class TestMoneroWalletResets {
  
  private static MoneroWallet wallet;

  @BeforeClass
  public static void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  @AfterClass
  public static void teardown() {
    PrintBalances.printBalances();
  }
  
  @Ignore // disabled so tests don't delete local cache
  @Test
  public void testRescanBlockchain() {
    wallet.rescanBlockchain();
    for (MoneroTx tx : wallet.getTxs()) {
      TestUtils.testGetTx(tx, false, wallet);
    }
  }
  
  @Test
  public void testSweepSubaddress() {
    fail("No implemented");
  }

  @Test
  public void testSweepAccount() {
    int NUM_ACCOUNTS_TO_SWEEP = 1;
    
    // collect accounts with balance and unlocked balance
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    List<MoneroAccount> balanceAccounts = new ArrayList<MoneroAccount>();
    List<MoneroAccount> unlockedAccounts = new ArrayList<MoneroAccount>();
    for (MoneroAccount account : accounts) {
      if (account.getBalance().longValue() != 0) balanceAccounts.add(account);
      if (account.getUnlockedBalance().longValue() != 0) unlockedAccounts.add(account);
    }
    
    // test requires at least one more account than the number being swept to verify it does not change
    assertTrue("Test requires balance in at least " + (NUM_ACCOUNTS_TO_SWEEP + 1) + " accounts; run testSendToMultiple() first", balanceAccounts.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    assertTrue("Test is waiting on unlocked funds", unlockedAccounts.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    
    // sweep from first unlocked accounts
    for (int i = 0; i < NUM_ACCOUNTS_TO_SWEEP; i++) {
      
      // sweep unlocked account
      MoneroAccount unlockedAccount = unlockedAccounts.get(i);
      List<MoneroTx> txs = wallet.sweepAccount(unlockedAccount.getIndex(), wallet.getPrimaryAddress());
      
      // test transactions
      assertFalse(txs.isEmpty());
      for (MoneroTx tx : txs) {
        MoneroTxConfig config = new MoneroTxConfig(wallet.getPrimaryAddress(), null, null);
        config.setAccountIndex(unlockedAccount.getIndex());
        TestUtils.testSendTx(tx, config, true, false, wallet);
      }
      
      // assert no unlocked funds in account
      MoneroAccount account = wallet.getAccount(unlockedAccount.getIndex());
      assertEquals(0, account.getUnlockedBalance().longValue());
    }
    
    // test accounts after sweeping
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    for (int i = 0; i < accounts.size(); i++) {
      MoneroAccount accountBefore = accounts.get(i);
      MoneroAccount accountAfter = accountsAfter.get(i);
      
      // determine if account was swept
      boolean swept = false;
      for (int j = 0; j < NUM_ACCOUNTS_TO_SWEEP; j++) {
        if (unlockedAccounts.get(j).getIndex() == accountBefore.getIndex()) {
          swept = true;
          break;
        }
      }
      
      // test that unlocked balance is 0 if swept, unchanged otherwise
      if (swept) {
        assertEquals(0, accountAfter.getUnlockedBalance().longValue());
      } else {
        assertEquals(accountBefore, accountAfter);
      }
    }
  }
  
  @Test
  public void testSweepWallet() {
    
    // sweep destination
    String destination = wallet.getPrimaryAddress();
    
    // verify 2 accounts with unlocked balance
    Pair<Map<Integer, List<Integer>>, Map<Integer, List<Integer>>> balances = getBalances();
    assertTrue("Test requires multiple accounts with a balance; run testSendToMultiple() first", balances.getFirst().size() >= 2);
    assertTrue("Wallet is waiting on unlocked funds", balances.getSecond().size() >= 2);
    
    // sweep
    List<MoneroTx> txs = wallet.sweepWallet(destination);
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      MoneroTxConfig config = new MoneroTxConfig(destination, null, null);
      config.setAccountIndex(tx.getSrcAccountIndex());  // TODO: this is to game testSendTx(); should not assert account equivalency there?
      TestUtils.testSendTx(tx, config, true, false, wallet);
    }
    
    // assert no unlocked funds across subaddresses
    balances = getBalances();
    assertTrue("Wallet should have no unlocked funds after sweeping all", balances.getSecond().isEmpty());
  }
  
  @Test
  public void testSweepCustom() {
    fail("No implemented");
  }
  
  /**
   * Collect indices of subaddresses with a non-zero balance / unlocked balance.
   * 
   * @return Pair<Map<Integer, List<Integer>>, Map<Integer, List<Integer>>> are indices of subaddresses with a non-zero balance / unlocked balance
   */
  private Pair<Map<Integer, List<Integer>>, Map<Integer, List<Integer>>> getBalances() {
    Map<Integer, List<Integer>> balances = new HashMap<Integer, List<Integer>>();
    Map<Integer, List<Integer>> unlockedBalances = new HashMap<Integer, List<Integer>>();
    for (MoneroAccount account : wallet.getAccounts()) {
      for (MoneroSubaddress subaddress : wallet.getSubaddresses(account.getIndex())) {
        if (subaddress.getBalance().longValue() > 0) {
          List<Integer> subaddressIndices = balances.get(account.getIndex());
          if (subaddressIndices == null) {
            subaddressIndices = new ArrayList<Integer>();
            balances.put(account.getIndex(), subaddressIndices);
          }
          subaddressIndices.add(subaddress.getSubaddrIndex());
        }
        if (subaddress.getUnlockedBalance().longValue() > 0) {
          List<Integer> subaddressIndices = unlockedBalances.get(account.getIndex());
          if (subaddressIndices == null) {
            subaddressIndices = new ArrayList<Integer>();
            unlockedBalances.put(account.getIndex(), subaddressIndices);
          }
          subaddressIndices.add(subaddress.getSubaddrIndex());
        }
      }
    }
    return new Pair<Map<Integer, List<Integer>>, Map<Integer, List<Integer>>>(balances, unlockedBalances);
  }
}