package monero.wallet.model;

import monero.daemon.model.MoneroBlockHeader;

/**
 * Receives notifications as a wallet is updated and provides default handling.
 */
public class MoneroWalletListener implements MoneroSyncListener {
  
  public void onNewBlock(MoneroBlockHeader header) { }

  @Override
  public void onProgress(int numBlocksDone, int numBlocksTotal, float percentDone, String message){ }
}
