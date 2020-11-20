package utils;

import monero.wallet.model.MoneroWalletListener;

/**
 * Print sync progress every X blocks.
 */
public class WalletSyncPrinter extends MoneroWalletListener {
  
  private double nextIncrement;
  private double syncResolution;
  
  public WalletSyncPrinter() {
    this(0.05);
  }
  
  public WalletSyncPrinter(double syncResolution) {
    this.nextIncrement = 0;
    this.syncResolution = syncResolution;
  }
  
  @Override
  public synchronized void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    if (percentDone == 1.0 || percentDone >= this.nextIncrement) {
      System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message + ")");
      this.nextIncrement += this.syncResolution;
    }
  }
}