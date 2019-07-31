package monero.wallet.model;

import monero.daemon.model.MoneroBlockHeader;

/**
 * Interface to receive wallet notifications.
 */
public interface MoneroWalletListener extends MoneroSyncListener {
  
  public void onNewBlock(MoneroBlockHeader header);
}