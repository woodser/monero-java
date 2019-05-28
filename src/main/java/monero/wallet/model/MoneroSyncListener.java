package monero.wallet.model;

/**
 * Receives progress notifications as a wallet is synchronized.
 */
public interface MoneroSyncListener {

  /**
   * Invoked when sync progress is made.
   * 
   * @param startHeight is the starting height of the sync request
   * @param numBlocksDone is the number of blocks synced
   * @param numBlocksTotal is the total number of blocks to sync
   * @param percentDone is the sync progress as a percentage
   * @param message is a human-readable description of the current progress
   */
  public void onSyncProgress(long startHeight, long numBlocksDone, long numBlocksTotal, double percentDone, String message);
}
