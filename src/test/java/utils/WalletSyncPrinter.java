package utils;

import monero.wallet.model.MoneroSyncListener;

/**
 * Print sync progress every X blocks.
 */
public class WalletSyncPrinter implements MoneroSyncListener {
  
  private long blockResolution;
  
  public WalletSyncPrinter() {
    this(25000l);
  }
  
  public WalletSyncPrinter(long blockResolution) {
    this.blockResolution = blockResolution;
  }
  
  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    if ((startHeight - height) % blockResolution == 0) {
      System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message + ")");
    }
  }
}