package utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc.IncomingTransferComparator;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroOutgoingTransfer;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;

/**
 * Utilities to deep compare wallets.
 */
public class WalletEqualityUtils {

  /**
   * Compares two wallets for equality using only on-chain data.
   * 
   * This test will sync the two wallets until their height is equal to guarantee equal state.
   * 
   * @param w1 a wallet to compare
   * @param w2 a wallet to compare
   */
  public static void testWalletEqualityOnChain(MoneroWallet w1, MoneroWallet w2) {
    
    // wait for relayed txs associated with wallets to clear pool
    assertEquals(w1.isConnectedToDaemon(), w2.isConnectedToDaemon());
    if (w1.isConnectedToDaemon()) {

      // sync wallets until same height
      w1.sync();
      w2.sync();
      while (w1.getHeight() != w2.getHeight()) {
        w1.sync();
        w2.sync();
      }

      // wait for txs to clear the pool
      TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(w1, w2);
    }
    
    // test that wallets are equal using only on-chain data
    assertEquals(w1.getHeight(), w2.getHeight());
    assertEquals(w1.getSeed(), w2.getSeed());
    assertEquals(w1.getPrimaryAddress(), w2.getPrimaryAddress());
    assertEquals(w1.getPrivateViewKey(), w2.getPrivateViewKey());
    assertEquals(w1.getPrivateSpendKey(), w2.getPrivateSpendKey());
    MoneroTxQuery txQuery = new MoneroTxQuery().setIsConfirmed(true);
    testTxWalletsEqualOnChain(w1.getTxs(txQuery), w2.getTxs(txQuery));
    txQuery.setIncludeOutputs(true);
    testTxWalletsEqualOnChain(w1.getTxs(txQuery), w2.getTxs(txQuery)); // fetch and compare outputs
    testAccountsEqualOnChain(w1.getAccounts(true), w2.getAccounts(true));
    assertEquals(w1.getBalance(), w2.getBalance());
    assertEquals(w1.getUnlockedBalance(), w2.getUnlockedBalance());
    MoneroTransferQuery transferQuery = new MoneroTransferQuery().setTxQuery(new MoneroTxQuery().setIsConfirmed(true));
    testTransfersEqualOnChain(w1.getTransfers(transferQuery), w2.getTransfers(transferQuery));
    MoneroOutputQuery outputQuery = new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setIsConfirmed(true));
    testOutputWalletsEqualOnChain(w1.getOutputs(outputQuery), w2.getOutputs(outputQuery));
  }
  
  private static void testAccountsEqualOnChain(List<MoneroAccount> accounts1, List<MoneroAccount> accounts2) {
    for (int i = 0; i < Math.max(accounts1.size(), accounts2.size()); i++) {
      if (i < accounts1.size() && i < accounts2.size()) {
        testAccountEqualOnChain(accounts1.get(i), accounts2.get(i));
      } else if (i >= accounts1.size()) {
        for (int j = i; j < accounts2.size(); j++) {
          assertEquals(BigInteger.valueOf(0), accounts2.get(j).getBalance());
          assertTrue(accounts2.get(j).getSubaddresses().size() >= 1);
          for (MoneroSubaddress subaddress : accounts2.get(j).getSubaddresses()) assertFalse(subaddress.isUsed());
        }
        return;
      } else {
        for (int j = i; j < accounts1.size(); j++) {
          assertEquals(BigInteger.valueOf(0), accounts1.get(j).getBalance());
          assertTrue(accounts1.get(j).getSubaddresses().size() >= 1);
          for (MoneroSubaddress subaddress : accounts1.get(j).getSubaddresses()) assertFalse(subaddress.isUsed());
        }
        return;
      }
    }
  }
  
  private static void testAccountEqualOnChain(MoneroAccount account1, MoneroAccount account2) {
    
    // nullify off-chain data for comparison
    List<MoneroSubaddress> subaddresses1 = account1.getSubaddresses();
    List<MoneroSubaddress> subaddresses2 = account2.getSubaddresses();
    account1.setSubaddresses(null);
    account2.setSubaddresses(null);
    account1.setTag(null);
    account2.setTag(null);
    
    // test account equality
    assertEquals(account1, account2);
    testSubaddressesEqualOnChain(subaddresses1, subaddresses2);
  }
  
  private static void testSubaddressesEqualOnChain(List<MoneroSubaddress> subaddresses1, List<MoneroSubaddress> subaddresses2) {
    for (int i = 0; i < Math.max(subaddresses1.size(), subaddresses2.size()); i++) {
      if (i < subaddresses1.size() && i < subaddresses2.size()) {
        testSubaddressesEqualOnChain(subaddresses1.get(i), subaddresses2.get(i));
      } else if (i >= subaddresses1.size()) {
        for (int j = i; j < subaddresses2.size(); j++) {
          assertEquals(BigInteger.valueOf(0), subaddresses2.get(j).getBalance());
          assertFalse(subaddresses2.get(j).isUsed());
        }
        return;
      } else {
        for (int j = i; j < subaddresses1.size(); j++) {
          assertEquals(BigInteger.valueOf(0), subaddresses1.get(i).getBalance());
          assertFalse(subaddresses1.get(j).isUsed());
        }
        return;
      }
    }
  }
  
  private static void testSubaddressesEqualOnChain(MoneroSubaddress subaddress1, MoneroSubaddress subaddress2) {
    subaddress1.setLabel(null); // nullify off-chain data for comparison
    subaddress2.setLabel(null);
    assertEquals(subaddress1, subaddress2);
  }
  
  public static void testTxWalletsEqualOnChain(List<MoneroTxWallet> txs1, List<MoneroTxWallet> txs2) {

    // remove pool or failed txs for comparison
    txs1 = new ArrayList<MoneroTxWallet>(txs1);
    Set<MoneroTxWallet> toRemove = new HashSet<MoneroTxWallet>();
    for (MoneroTxWallet tx : txs1) {
      if (Boolean.TRUE.equals(tx.inTxPool()) || Boolean.TRUE.equals(tx.isFailed())) toRemove.add(tx);
    }
    txs1.removeAll(toRemove);
    txs2 = new ArrayList<MoneroTxWallet>(txs2);
    toRemove.clear();
    for (MoneroTxWallet tx : txs2) {
      if (Boolean.TRUE.equals(tx.inTxPool()) || Boolean.TRUE.equals(tx.isFailed())) toRemove.add(tx);
    }
    txs2.removeAll(toRemove);
    
    // nullify off-chain data for comparison
    List<MoneroTxWallet> allTxs = new ArrayList<MoneroTxWallet>(txs1);
    allTxs.addAll(txs2);
    for (MoneroTxWallet tx : allTxs) {
      tx.setNote(null);
      if (tx.getOutgoingTransfer() != null) {
        tx.getOutgoingTransfer().setAddresses(null);
      }
    }
    
    // compare txs
    assertEquals(txs1.size(), txs2.size());
    for (MoneroTxWallet tx1 : txs1) {
      boolean found = false;
      for (MoneroTxWallet tx2 : txs2) {
        if (tx1.getHash().equals(tx2.getHash())) {
          
          // transfer cached info if known for comparison
          if (tx1.getOutgoingTransfer() != null && tx1.getOutgoingTransfer().getDestinations() != null) {
            if (tx2.getOutgoingTransfer() == null || tx2.getOutgoingTransfer().getDestinations() == null) transferCachedInfo(tx1, tx2);
          } else if (tx2.getOutgoingTransfer() != null && tx2.getOutgoingTransfer().getDestinations() != null) {
            transferCachedInfo(tx2, tx1);
          }
          
          // test tx equality
          assertTrue(TestUtils.txsMergeable(tx1, tx2), "Txs are not mergeable");
          assertEquals(tx1, tx2);
          found = true;
          
          // test block equality except txs to ignore order
          List<MoneroTx> blockTxs1 = tx1.getBlock().getTxs();
          List<MoneroTx> blockTxs2 = tx2.getBlock().getTxs();
          tx1.getBlock().setTxs();
          tx2.getBlock().setTxs();
          assertEquals(tx1.getBlock(), tx2.getBlock());
          tx1.getBlock().setTxs(blockTxs1);
          tx2.getBlock().setTxs(blockTxs2);
        }
      }
      assertTrue(found);  // each tx must have one and only one match
    }
  }
  
  private static void transferCachedInfo(MoneroTxWallet src, MoneroTxWallet tgt) {
    
    // fill in missing incoming transfers when sending from/to the same account
    if (src.getIncomingTransfers() != null) {
      for (MoneroIncomingTransfer inTransfer : src.getIncomingTransfers()) {
        if (inTransfer.getAccountIndex() == src.getOutgoingTransfer().getAccountIndex()) {
          tgt.getIncomingTransfers().add(inTransfer);
        }
      }
      Collections.sort(tgt.getIncomingTransfers(), new IncomingTransferComparator());
    }
    
    // transfer info to outgoing transfer
    if (tgt.getOutgoingTransfer() == null) tgt.setOutgoingTransfer(src.getOutgoingTransfer());
    else {
      tgt.getOutgoingTransfer().setDestinations(src.getOutgoingTransfer().getDestinations());
      tgt.getOutgoingTransfer().setAmount(src.getOutgoingTransfer().getAmount());
    }
    
    // transfer payment id if outgoing // TODO: monero-wallet-rpc does not provide payment id for outgoing transfer when cache missing https://github.com/monero-project/monero/issues/8378
    if (tgt.getOutgoingTransfer() != null) tgt.setPaymentId(src.getPaymentId());
  }
  
  private static void testTransfersEqualOnChain(List<MoneroTransfer> transfers1, List<MoneroTransfer> transfers2) {
    assertEquals(transfers1.size(), transfers2.size());
    
    // test and collect transfers per transaction
    Map<String, List<MoneroTransfer>> txsTransfers1 = new HashMap<String, List<MoneroTransfer>>();
    Map<String, List<MoneroTransfer>> txsTransfers2 = new HashMap<String, List<MoneroTransfer>>();
    Long lastHeight = null;
    MoneroTxWallet lastTx1 = null;
    MoneroTxWallet lastTx2 = null;
    for (int i = 0; i < transfers1.size(); i++) {
      MoneroTransfer transfer1 = transfers1.get(i);
      MoneroTransfer transfer2 = transfers2.get(i);
      
      // transfers must have same height even if they don't belong to same tx (because tx ordering within blocks is not currently provided by wallet2)
      assertEquals((long) transfer1.getTx().getHeight(), (long) transfer2.getTx().getHeight());
      
      // transfers must be in ascending order by height
      if (lastHeight == null) lastHeight = transfer1.getTx().getHeight();
      else assertTrue(lastHeight <= transfer1.getTx().getHeight());
      
      // transfers must be consecutive per transaction
      if (lastTx1 != transfer1.getTx()) {
        assertFalse(txsTransfers1.containsKey(transfer1.getTx().getHash()));  // cannot be seen before
        lastTx1 = transfer1.getTx();
      }
      if (lastTx2 != transfer2.getTx()) {
        assertFalse(txsTransfers2.containsKey(transfer2.getTx().getHash()));  // cannot be seen before
        lastTx2 = transfer2.getTx();
      }
      
      // collect tx1 transfer
      List<MoneroTransfer> txTransfers1 = txsTransfers1.get(transfer1.getTx().getHash());
      if (txTransfers1 == null) {
        txTransfers1 = new ArrayList<MoneroTransfer>();
        txsTransfers1.put(transfer1.getTx().getHash(), txTransfers1);
      }
      txTransfers1.add(transfer1);
      
      // collect tx2 transfer
      List<MoneroTransfer> txTransfers2 = txsTransfers2.get(transfer2.getTx().getHash());
      if (txTransfers2 == null) {
        txTransfers2 = new ArrayList<MoneroTransfer>();
        txsTransfers2.put(transfer2.getTx().getHash(), txTransfers2);
      }
      txTransfers2.add(transfer2);
    }
    
    // compare collected transfers per tx for equality
    for (String txHash : txsTransfers1.keySet()) {
      List<MoneroTransfer> txTransfers1 = txsTransfers1.get(txHash);
      List<MoneroTransfer> txTransfers2 = txsTransfers2.get(txHash);
      assertEquals(txTransfers1.size(), txTransfers2.size());
      
      // normalize and compare transfers
      for (int i = 0; i < txTransfers1.size(); i++) {
        MoneroTransfer transfer1 = txTransfers1.get(i);
        MoneroTransfer transfer2 = txTransfers2.get(i);
        
        // normalize outgoing transfers
        if (transfer1 instanceof MoneroOutgoingTransfer) {
          MoneroOutgoingTransfer ot1 = (MoneroOutgoingTransfer) transfer1;
          MoneroOutgoingTransfer ot2 = (MoneroOutgoingTransfer) transfer2;
    
          // transfer destination info if known for comparison
          if (ot1.getDestinations() != null) {
            if (ot2.getDestinations() == null) transferCachedInfo(ot1.getTx(), ot2.getTx());
          } else if (ot2.getDestinations() != null) {
            transferCachedInfo(ot2.getTx(), ot1.getTx());
          }
          
          // nullify other local wallet data
          ot1.setAddresses(null);
          ot2.setAddresses(null);
        }
        
        // normalize incoming transfers
        else {
          MoneroIncomingTransfer it1 = (MoneroIncomingTransfer) transfer1;
          MoneroIncomingTransfer it2 = (MoneroIncomingTransfer) transfer2;
          it1.setAddress(null);
          it2.setAddress(null);
        }
        
        // compare transfer equality
        assertEquals(transfer1, transfer2);
      }
    }
  }
  
  private static void testOutputWalletsEqualOnChain(List<MoneroOutputWallet> outputs1, List<MoneroOutputWallet> outputs2) {
    assertEquals(outputs1.size(), outputs2.size());
    
    // test and collect outputs per transaction
    Map<String, List<MoneroOutputWallet>> txsOutputs1 = new HashMap<String, List<MoneroOutputWallet>>();
    Map<String, List<MoneroOutputWallet>> txsOutputs2 = new HashMap<String, List<MoneroOutputWallet>>();
    Long lastHeight = null;
    MoneroTxWallet lastTx1 = null;
    MoneroTxWallet lastTx2 = null;
    for (int i = 0; i < outputs1.size(); i++) {
      MoneroOutputWallet output1 = outputs1.get(i);
      MoneroOutputWallet output2 = outputs2.get(i);
      
      // outputs must have same height even if they don't belong to same tx (because tx ordering within blocks is not currently provided by wallet2)
      assertEquals((long) output1.getTx().getHeight(), (long) output2.getTx().getHeight());
      
      // outputs must be in ascending order by height
      if (lastHeight == null) lastHeight = output1.getTx().getHeight();
      else assertTrue(lastHeight <= output1.getTx().getHeight());
      
      // outputs must be consecutive per transaction
      if (lastTx1 != output1.getTx()) {
        assertFalse(txsOutputs1.containsKey(output1.getTx().getHash()));  // cannot be seen before
        lastTx1 = output1.getTx();
      }
      if (lastTx2 != output2.getTx()) {
        assertFalse(txsOutputs2.containsKey(output2.getTx().getHash()));  // cannot be seen before
        lastTx2 = output2.getTx();
      }
      
      // collect tx1 output
      List<MoneroOutputWallet> txOutputs1 = txsOutputs1.get(output1.getTx().getHash());
      if (txOutputs1 == null) {
        txOutputs1 = new ArrayList<MoneroOutputWallet>();
        txsOutputs1.put(output1.getTx().getHash(), txOutputs1);
      }
      txOutputs1.add(output1);
      
      // collect tx2 output
      List<MoneroOutputWallet> txOutputs2 = txsOutputs2.get(output2.getTx().getHash());
      if (txOutputs2 == null) {
        txOutputs2 = new ArrayList<MoneroOutputWallet>();
        txsOutputs2.put(output2.getTx().getHash(), txOutputs2);
      }
      txOutputs2.add(output2);
    }
    
    // compare collected outputs per tx for equality
    for (String txHash : txsOutputs1.keySet()) {
      List<MoneroOutputWallet> txOutputs1 = txsOutputs1.get(txHash);
      List<MoneroOutputWallet> txOutputs2 = txsOutputs2.get(txHash);
      assertEquals(txOutputs1.size(), txOutputs2.size());
      
      // normalize and compare outputs
      for (int i = 0; i < txOutputs1.size(); i++) {
        MoneroOutput output1 = txOutputs1.get(i);
        MoneroOutput output2 = txOutputs2.get(i);
        assertEquals(output1.getTx().getHash(), output2.getTx().getHash());
        assertEquals(output1, output2);
      }
    }
  }
}
