package monero.wallet.model;

/**
 * Interface to receive wallet notifications.
 */
public interface MoneroWalletListenerI extends MoneroSyncListener {
  
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