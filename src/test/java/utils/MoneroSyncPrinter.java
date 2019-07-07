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
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    if ((startHeight - height) % blockResolution == 0) {
      System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message + ")");
    }
  }
}