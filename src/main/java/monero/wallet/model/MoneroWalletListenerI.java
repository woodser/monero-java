package monero.wallet.model;

/**
 * Interface to receive wallet notifications.
 */
public interface MoneroWalletListenerI {
  
  /**
   * Invoked as the wallet is synchronized.
   * 
   * @param height is the height of the synced block 
   * @param startHeight is the starting height of the sync request
   * @param endHeight is the ending height of the sync request
   * @param percentDone is the sync progress as a percentage
   * @param message is a human-readable description of the current progress
   */
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message);
  
  /**
   * Invoked when a new block is added to the chain.
   * 
   * @param height - the height of the block added to the chain
   */
  public void onNewBlock(long height);
  
  /**
   * Invoked when the wallet receives an output.
   * 
   * @param output - the received output
   */
  public void onOutputReceived(MoneroOutputWallet output);
  
  /**
   * Invoked when the wallet spends an output.
   * 
   * @param output - the spent output
   */
  public void onOutputSpent(MoneroOutputWallet output);
}