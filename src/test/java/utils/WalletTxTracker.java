package utils;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;

/**
 * Tracks wallets which are in sync with the tx pool and therefore whose txs in the pool
 * do not need to be waited on for up-to-date pool information e.g. to create txs.
 * 
 * This is only necessary because txs relayed outside wallets are not fully incorporated
 * into the wallet state until confirmed.
 * 
 * TODO monero-project: sync txs relayed outside wallet so this class is unecessary
 */
public class WalletTxTracker {

  /**
   * Wait for pending wallet transactions to clear the pool.
   * 
   * @param wallets may have transactions to clear
   */
  public void waitForTxsToClearPool(MoneroWallet... wallets) {
    waitForTxsToClear(false, wallets);
  }

  /**
   * Wait for pending wallet transasctions to clear from the wallets.
   * 
   * @param wallets may have transactions to clear
   */
  public void waitForTxsToClearWallets(MoneroWallet... wallets) {
    waitForTxsToClear(true, wallets);
  }

  private void waitForTxsToClear(boolean clearFromWallet, MoneroWallet... wallets) {

    // loop until pending txs cleared
    boolean isFirst = true;
    boolean miningStarted = false;
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    while (true) {

      // get pending wallet tx hashes
      Set<String> txHashesWallet = new HashSet<String>();
      for (MoneroWallet wallet : wallets) {
        wallet.sync();
        for (MoneroTxWallet tx : wallet.getTxs(new MoneroTxQuery().setInTxPool(true))) {
          if (!tx.isRelayed()) continue;
          else if (tx.isFailed()) daemon.flushTxPool(tx.getHash()); // flush tx if failed
          else txHashesWallet.add(tx.getHash());
        }
      }

      // get pending txs to wait for
      Set<String> txHashesPool = new HashSet<String>();
      if (clearFromWallet) {
        txHashesPool.addAll(txHashesWallet);
      } else {
        for (MoneroTx tx : daemon.getTxPool()) {
          if (!tx.isRelayed()) continue;
          else if (tx.isFailed()) daemon.flushTxPool(tx.getHash()); // flush tx if failed
          else if (txHashesWallet.contains(tx.getHash())) txHashesPool.add(tx.getHash());
        }
      }
      
      // break if no txs to wait for
      if (txHashesPool.isEmpty()) {
        if (miningStarted) daemon.stopMining(); // stop mining if started mining
        break;
      }

      // log message and start mining if first iteration
      if (isFirst) {
        isFirst = false;
        System.out.println("Waiting for wallet txs to clear from the pool in order to fully sync and avoid double spend attempts: " + txHashesPool);
        MoneroMiningStatus miningStatus = daemon.getMiningStatus();
        if (!miningStatus.isActive()) {
          try {
            StartMining.startMining();
            miningStarted = true;
          } catch (Exception e) { } // no problem
        }
      }
      
      // sleep for sync period
      try { TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS); }
      catch (InterruptedException e) {  throw new RuntimeException(e); } 
    }
  }
  
  public BigInteger waitForUnlockedBalance(MoneroWallet wallet, Integer accountIndex, Integer subaddressIndex, BigInteger minAmount) {
    if (minAmount == null) minAmount = new BigInteger("0");
    
    // check if wallet has balance
    if (wallet.getBalance(accountIndex, subaddressIndex).compareTo(minAmount) < 0) throw new RuntimeException("Wallet does not have enough balance to wait for");
    
    // check if wallet has unlocked balance
    BigInteger unlockedBalance = wallet.getUnlockedBalance(accountIndex, subaddressIndex);
    if (unlockedBalance.compareTo(minAmount) > 0) return unlockedBalance;
   
    // start mining
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    boolean miningStarted = false;
    if (!daemon.getMiningStatus().isActive()) {
      try {
        StartMining.startMining();
        miningStarted = true;
      } catch (Exception err) { }
    }
    
    // wait for unlocked balance // TODO: promote to MoneroWallet interface?
    System.out.println("Waiting for unlocked balance");
    while (unlockedBalance.compareTo(minAmount) < 0) {
      unlockedBalance = wallet.getUnlockedBalance(accountIndex, subaddressIndex);
      try { TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS); }
      catch (InterruptedException e) { throw new RuntimeException(e); }
    }
    
    // stop mining if started
    if (miningStarted) daemon.stopMining();
    return unlockedBalance;
  }
}