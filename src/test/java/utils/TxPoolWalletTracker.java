package utils;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import monero.common.MoneroUtils;
import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroTxWallet;

/**
 * Tracks wallets which are in sync with the tx pool and therefore whose txs in the pool
 * do not need to be waited on for up-to-date pool information e.g. to create txs.
 * 
 * This is only necessary because txs relayed outside wallets are not fully incorporated
 * into the wallet state until confirmed.
 * 
 * TODO monero core: sync txs relayed outside wallet so this class is unecessary
 */
public class TxPoolWalletTracker {

  private Set<MoneroWallet> clearedWallets;
  
  public TxPoolWalletTracker() {
    clearedWallets = new HashSet<MoneroWallet>();
  }
  
  public void reset() {
    clearedWallets.clear();
  }
  
//  /**
//   * Reset the tracker such that all wallets except the given sending wallet will
//   * need to wait for pool txs to confirm in order to reliably sync.
//   * 
//   * @param sendingWallet is the wallet which sent the tx and therefore should not cause txs to be waited on
//   */
//  public void resetExcept(MoneroWallet sendingWallet) {
//    boolean found = clearedWallets.contains(sendingWallet);
//    clearedWallets.clear();
//    if (found) clearedWallets.add(sendingWallet);
//  }
  
  /**
   * Waits for transactions in the pool belonging to the given wallets to clear.
   * 
   * @param wallets have transactions to wait on if in the pool
   */
  public void waitForWalletTxsToClearPool(MoneroWallet... wallets) {
    
    // get wallet tx hashes
    Set<String> txHashesWallet = new HashSet<String>();
    for (MoneroWallet wallet : wallets) {
      if (!clearedWallets.contains(wallet)) {
        wallet.sync();
        for (MoneroTxWallet tx : wallet.getTxs()) {
          txHashesWallet.add(tx.getHash());
        }
      }
    }
    
    // loop until all wallet txs clear from pool
    boolean isFirst = true;
    boolean miningStarted = false;
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    while (true) {
      
      // get hashes of relayed, non-failed txs in the pool
      Set<String> txHashesPool = new HashSet<String>();
      for (MoneroTx tx : daemon.getTxPool()) {
        if (!tx.isRelayed()) continue;
        else if (tx.isFailed()) daemon.flushTxPool(tx.getHash());  // flush tx if failed
        else txHashesPool.add(tx.getHash());
      }
      
      // get hashes to wait for as intersection of wallet and pool txs
      txHashesPool.retainAll(txHashesWallet);
      
      // break if no txs to wait for
      if (txHashesPool.isEmpty()) break;

      // if first time waiting, log message and start mining
      if (isFirst) {
        isFirst = false;
        System.out.println("Waiting for wallet txs to clear from the pool in order to fully sync and avoid double spend attempts (known issue)");
        MoneroMiningStatus miningStatus = daemon.getMiningStatus();
        if (!miningStatus.isActive()) {
          try {
            StartMining.startMining();
            miningStarted = true;
          } catch (Exception e) { } // no problem
        }
      }
      
      // sleep for a moment
      try { TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL); }
      catch (InterruptedException e) {  throw new RuntimeException(e); } 
    }
    
    // stop mining if started mining
    if (miningStarted) daemon.stopMining();
    
    // sync wallets with the pool
    for (MoneroWallet wallet : wallets) {
      wallet.sync();
      clearedWallets.add(wallet);
    }
  }
}
