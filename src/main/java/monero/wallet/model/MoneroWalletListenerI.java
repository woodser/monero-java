package monero.wallet.model;

import java.math.BigInteger;

/**
 * Interface to receive wallet notifications.
 */
public interface MoneroWalletListenerI {
  
  /**
   * Invoked as the wallet is synchronized.
   * 
   * @param height - height of the synced block 
   * @param startHeight - starting height of the sync request
   * @param endHeight - ending height of the sync request
   * @param percentDone - sync progress as a percentage
   * @param message is a human-readable description of the current progress
   */
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message);
  
  /**
   * Invoked when a new block is added to the chain.
   * 
   * @param height - the height of the new block (i.e. the number of blocks before it).
   */
  public void onNewBlock(long height);

  /**
   * Invoked when the wallet's balances change.
   * 
   * @param newBalance - new wallet balance
   * @param newUnlockedBalance - new unlocked wallet balance
   */
  public void onBalancesChanged(BigInteger newBalance, BigInteger newUnlockedBalance);
  
  /**
   * Invoked 3 times per received output: once when unconfirmed, once when confirmed, and
   * once when unlocked.
   * 
   * @param output - the received output
   */
  public void onOutputReceived(MoneroOutputWallet output);
  
  /**
   * Invoked twice per spent output: once when confirmed and once when unlocked.
   * 
   * @param output - the spent output
   */
  public void onOutputSpent(MoneroOutputWallet output); // TODO (monero-project): support notification of unconfirmed spent funds
}