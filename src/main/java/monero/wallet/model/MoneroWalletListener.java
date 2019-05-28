package monero.wallet.model;

import monero.daemon.model.MoneroBlockHeader;

/**
 * Receives notifications as a wallet is updated and provides default handling.
 */
public class MoneroWalletListener implements MoneroSyncListener {
  
  public void onNewBlock(MoneroBlockHeader header) { }

  @Override
  public void onSyncProgress(long startHeight, long numBlocksDone, long numBlocksTotal, double percentDone, String message) { }
}
