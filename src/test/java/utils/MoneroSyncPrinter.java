package utils;

import monero.wallet.model.MoneroSyncListener;

/**
 * Print sync progress every X blocks.
 */
public class MoneroSyncPrinter implements MoneroSyncListener {
  
  private long blockResolution;
  
  public MoneroSyncPrinter() {
    this(25000l);
  }
  
  public MoneroSyncPrinter(long blockResolution) {
    this.blockResolution = blockResolution;
  }
  
  @Override
  public void onSyncProgress(long startHeight, long numBlocksDone, long numBlocksTotal, double percentDone, String message) {
    if (numBlocksDone % blockResolution == 0) {
      System.out.println("onSyncProgress(" + startHeight + ", " + numBlocksDone + ", " + numBlocksTotal + ", " + percentDone + ", " + message + ")");
    }
  }
}