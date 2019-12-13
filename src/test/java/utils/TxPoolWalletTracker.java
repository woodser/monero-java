package utils;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroUtils;
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
  
  /**
   * Reset the tracker such that all wallets except the given sending wallet will
   * need to wait for pool txs to confirm in order to reliably sync.
   * 
   * @param sendingWallet is the wallet which sent the tx and therefore should not cause txs to be waited on
   */
  public void resetExcept(MoneroWallet sendingWallet) {
    boolean found = clearedWallets.contains(sendingWallet);
    clearedWallets.clear();
    if (found) clearedWallets.add(sendingWallet);
  }
  
  public void waitForWalletTxsToClearPool(MoneroWallet... wallets) {
    
    // get hashes of txs in the pool
    Set<String> txHashesPool = new HashSet<String>();
    for (MoneroTx tx : TestUtils.getDaemonRpc().getTxPool()) {
      if (!tx.isRelayed() || tx.isFailed()) continue;
      txHashesPool.add(tx.getHash());
    }
    
    // get hashes of txs from wallets to wait for
    Set<String> txHashesWallet = new HashSet<String>();
    for (MoneroWallet wallet : wallets) {
      if (!clearedWallets.contains(wallet)) {
        wallet.sync();
        for (MoneroTxWallet tx : wallet.getTxs()) {
          txHashesWallet.add(tx.getHash());
        }
      }
    }
    
    // wait for txs to clear pool
    txHashesPool.retainAll(txHashesWallet);
    waitForTxsToClearPool(txHashesPool.toArray(new String[txHashesPool.size()]));
    
    // sync wallets with the pool
    for (MoneroWallet wallet : wallets) {
      wallet.sync();
      clearedWallets.add(wallet);
    }
  }
  
  private static void waitForTxsToClearPool(String... txHashes) {
    MoneroDaemon daemon = TestUtils.getDaemonRpc(); 
      
    // attempt to start mining to push the network along
    boolean startedMining = false;
    MoneroMiningStatus miningStatus = daemon.getMiningStatus();
    if (!miningStatus.isActive()) {
      try {
        StartMining.startMining();
        startedMining = true;
      } catch (Exception e) { } // no problem
    }
    
    // loop until txs are not in pool
    boolean isFirst = true;
    while (txsInPool(txHashes)) {
      
      // print debug messsage one time
      if (isFirst) {  
        System.out.println("Waiting for wallet txs to clear from the pool in order to fully sync and avoid double spend attempts (known issue)");
        isFirst = false;
      }
      
      // sleep for a moment
      try { TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL); }
      catch (InterruptedException e) {  throw new RuntimeException(e); } 
    }
    
    // stop mining at end of test
    if (startedMining) daemon.stopMining();
  }
  
  private static boolean txsInPool(String...txHashes) {
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    List<MoneroTx> txsPool = daemon.getTxPool();
    for (MoneroTx txPool : txsPool) {
      for (String txHash : txHashes) {
        if (txHash.equals(txPool.getHash())) return true;
      }
    }
    return false;
  }
}
