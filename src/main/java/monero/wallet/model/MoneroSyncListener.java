package monero.wallet.model;

/**
 * Receive updates as sync progress is made.
 */
public interface MoneroSyncListener {

  /**
   * Invoked with info as sync progress is made.
   * 
   * @param numBlocksDone is the number of blocks synced
   * @param numBlocksTotal is the total number of blocks to sync
   * @param percentDone is the sync progress as a percentage
   * @param message is a human-readable description of the current progress
   */
  public void onProgress(int numBlocksDone, int numBlocksTotal, float percentDone, String message);
}
