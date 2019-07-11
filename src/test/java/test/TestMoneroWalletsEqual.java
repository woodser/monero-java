package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroTxRequest;
import utils.TestUtils;

/**
 * Compares two wallets for equality using only on-chain data.
 * 
 * This test will sync the two wallets until their height is equal to guarantee equal state.
 * 
 * The RPC and JNI wallets are tested by default unless overriden by subclassing or using the setters.
 */
public class TestMoneroWalletsEqual {
  
  private MoneroWallet w1;
  private MoneroWallet w2;

  public MoneroWallet getWallet1() {
    return w1;
  }
  
  public TestMoneroWalletsEqual setWallet1(MoneroWallet w1) {
    this.w1 = w1;
    return this;
  }
  
  public MoneroWallet getWallet2() {
    return w2;
  }
  
  public TestMoneroWalletsEqual setWallet2(MoneroWallet w2) {
    this.w2 = w2;
    return this;
  }
  
  @Test
  public void testWalletsEqualOnChain() {
    
    // default to rpc and jni wallets
    if (w1 == null) w1 = TestUtils.getWalletRpc();
    if (w2 == null) w2 = TestUtils.getWalletJni();
    
    // sync the wallets until same height
    while (w1.getHeight() != w2.getHeight()) {
      w1.sync();
      w2.sync();
    }
    
    // test that wallets are equal based using only on-chain data
    assertEquals(w1.getHeight(), w2.getHeight());
    assertEquals(w1.getMnemonic(), w2.getMnemonic());
    assertEquals(w1.getPrimaryAddress(), w2.getPrimaryAddress());
    assertEquals(w1.getPrivateViewKey(), w2.getPrivateViewKey());
    assertEquals(w1.getPrivateSpendKey(), w2.getPrivateSpendKey());
    assertEquals(w1.getBalance(), w2.getBalance());
    assertEquals(w1.getUnlockedBalance(), w2.getUnlockedBalance());
    testAccountsEqualOnChain(w1.getAccounts(true), w2.getAccounts(true));
    MoneroTxRequest noFailedTxs = new MoneroTxRequest().setIsFailed(false);
    testTxWalletsEqualOnChain(w1.getTxs(noFailedTxs), w2.getTxs(noFailedTxs));
    // TOOD: compare outputs, etc
  }
  
  protected void testAccountsEqualOnChain(List<MoneroAccount> accounts1, List<MoneroAccount> accounts2) {
    for (int i = 0; i < Math.max(accounts1.size(), accounts2.size()); i++) {
      if (i < accounts1.size() && i < accounts2.size()) {
        testAccountsEqualOnChain(accounts1.get(i), accounts2.get(i));
      } else if (i >= accounts1.size()) {
        for (int j = i; j < accounts2.size(); j++) {
          assertEquals(BigInteger.valueOf(0), accounts2.get(j).getBalance());
          assertTrue(accounts2.get(j).getSubaddresses().size() >= 1);
          for (MoneroSubaddress subaddress : accounts2.get(j).getSubaddresses()) assertFalse(subaddress.getIsUsed());
        }
        return;
      } else {
        for (int j = i; j < accounts1.size(); j++) {
          assertEquals(BigInteger.valueOf(0), accounts1.get(j).getBalance());
          assertTrue(accounts1.get(j).getSubaddresses().size() >= 1);
          for (MoneroSubaddress subaddress : accounts1.get(j).getSubaddresses()) assertFalse(subaddress.getIsUsed());
        }
        return;
      }
    }
  }
  
  protected void testAccountsEqualOnChain(MoneroAccount account1, MoneroAccount account2) {
    
    // nullify off-chain data for comparison
    List<MoneroSubaddress> subaddresses1 = account1.getSubaddresses();
    List<MoneroSubaddress> subaddresses2 = account2.getSubaddresses();
    account1.setSubaddresses(null);
    account2.setSubaddresses(null);
    account1.setLabel(null);
    account2.setLabel(null);
    account1.setTag(null);
    account2.setTag(null);
    
    // test account equality
    assertEquals(account1, account2);
    testSubaddressesEqualOnChain(subaddresses1, subaddresses2);
  }
  
  protected void testSubaddressesEqualOnChain(List<MoneroSubaddress> subaddresses1, List<MoneroSubaddress> subaddresses2) {
    for (int i = 0; i < Math.max(subaddresses1.size(), subaddresses2.size()); i++) {
      if (i < subaddresses1.size() && i < subaddresses2.size()) {
        testSubaddressesEqualOnChain(subaddresses1.get(i), subaddresses2.get(i));
      } else if (i >= subaddresses1.size()) {
        for (int j = i; j < subaddresses2.size(); j++) {
          assertEquals(BigInteger.valueOf(0), subaddresses2.get(j).getBalance());
          assertFalse(subaddresses2.get(j).getIsUsed());
        }
        return;
      } else {
        for (int j = i; j < subaddresses1.size(); j++) {
          assertEquals(BigInteger.valueOf(0), subaddresses1.get(i).getBalance());
          assertFalse(subaddresses1.get(j).getIsUsed());
        }
        return;
      }
    }
  }
  
  protected void testSubaddressesEqualOnChain(MoneroSubaddress subaddress1, MoneroSubaddress subaddress2) {
    subaddress1.setLabel(null); // nullify off-chain data for comparison
    subaddress2.setLabel(null);
    assertEquals(subaddress1, subaddress2);
  }
  
  protected void testTxWalletsEqualOnChain(List<MoneroTxWallet> txs1, List<MoneroTxWallet> txs2) {
    assertEquals(txs1.size(), txs2.size());
    
    // nullify off-chain data for comparison
    List<MoneroTxWallet> allTxs = new ArrayList<MoneroTxWallet>(txs1);
    allTxs.addAll(txs2);
    for (MoneroTxWallet tx : allTxs) {
      tx.setNote(null);
      if (tx.getOutgoingTransfer() != null) tx.getOutgoingTransfer().setAddresses(null);
    }
    
    // separate unconfirmed txs which are not ordered
    List<MoneroTxWallet> txs1Confirmed;
    List<MoneroTxWallet> txs1Unconfirmed = new ArrayList<MoneroTxWallet>();
    List<MoneroTxWallet> txs2Confirmed;
    List<MoneroTxWallet> txs2Unconfirmed = new ArrayList<MoneroTxWallet>();
    Integer firstUnconfirmedIdx;
    if ((firstUnconfirmedIdx = getFirstUnconfirmedIdx(txs1)) != null) {
      txs1Unconfirmed = txs1.subList(firstUnconfirmedIdx, txs1.size());
      txs1Confirmed = txs1.subList(0, firstUnconfirmedIdx);
    } else {
      txs1Confirmed = new ArrayList<MoneroTxWallet>(txs1);
    }
    if ((firstUnconfirmedIdx = getFirstUnconfirmedIdx(txs2)) != null) {
      txs2Unconfirmed = txs2.subList(firstUnconfirmedIdx, txs2.size());
      txs2Confirmed = txs2.subList(0, firstUnconfirmedIdx);
    } else {
      txs2Confirmed = new ArrayList<MoneroTxWallet>(txs2);
    }
    
    // test confirmed tx equality which is ordered
    assertEquals(txs1Confirmed.size(), txs2Confirmed.size());
    for (int i = 0; i < txs1Confirmed.size(); i++) {
      testTxWalletsOnChain(txs1.get(i), txs2.get(i));
    }
    
    // test unconfirmed tx equality which is not ordered
    assertEquals(txs1Unconfirmed.size(), txs2Unconfirmed.size());
    for (MoneroTxWallet tx1 : txs1) {
      boolean found = false;
      for (MoneroTxWallet tx2 : txs2) {
        if (tx1.getId().equals(tx2.getId())) {
          assertEquals(tx1, tx2);
          found = true;
          break;
        }
      }
      assertTrue(found);  // each tx must have one and only one match
    }
  }
  
  private static Integer getFirstUnconfirmedIdx(List<MoneroTxWallet> txs) {
    for (int i = 0; i < txs.size(); i++) {
      if (!txs.get(i).getIsConfirmed()) return i;
    }
    return null;
  }
  
  protected void testTxWalletsOnChain(MoneroTxWallet tx1, MoneroTxWallet tx2) {
    
    // handle unconfirmed blocks which are known to daemon
    if (tx1.getBlock() == null || tx2.getBlock() == null) {
      assertNull(tx1.getBlock());
      assertNull(tx2.getBlock());
      assertEquals(tx1, tx2);
    }
    
    // otherwise compare tx's blocks which compare entire trees
    else {
      assertTrue(tx1.getBlock().getTxs().contains(tx1));
      assertTrue(tx2.getBlock().getTxs().contains(tx2));
      assertEquals(tx1.getBlock(), tx2.getBlock());
    }
  }
}
