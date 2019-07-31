package monero.wallet.model;

import monero.daemon.model.MoneroBlockHeader;

/**
 * Interface to receive wallet notifications.
 */
public interface MoneroWalletListener extends MoneroSyncListener {
  
  /**
   * Invoked when a new block is added to the chain.
   * 
   * @param header is the header of the block added to the chain
   */
  public void onNewBlock(MoneroBlockHeader header);
  
  /**
   * Invoked when the wallet observes a transaction with incoming or outgoing transfers
   * to or from the wallet.
   * 
   * @param tx is the transaction with incoming or outgoing transfers to or from the wallet
   */
  public void onNewTx(MoneroTxWallet tx);
  
  /**
   * Invoked when the wallet receives one or more incoming transfers as part of
   * a transaction.
   * 
   * @param tx is the transaction with incoming transfers
   */
  public void onMoneyReceived(MoneroTxWallet tx);
  
  /**
   * Invoked when the wallet sends an outgoing transfer.
   * 
   * @param tx is the transaction with an outgoing transfer
   */
  public void onMoneySent(MoneroTxWallet tx);
}