package monero.wallet.model;

/**
 * Interface to receive wallet notifications.
 */
public interface MoneroWalletListenerI extends MoneroSyncListener {
  
  /**
   * Invoked when a new block is added to the chain.
   * 
   * @param height is the height of the block added to the chain
   */
  public void onNewBlock(long height);
  
  /**
   * Invoked when the wallet receives an incoming transfer.
   * 
   * @param transfer is the incoming transfer to the wallet
   */
  public void onIncomingTransfer(MoneroIncomingTransfer transfer);
  
  /**
   * Invoked when the wallet sends an outgoing transfer.
   * 
   * @param transfer is the outgoing transfer from the wallet
   */
  public void onOutgoingTransfer(MoneroOutgoingTransfer transfer);
}