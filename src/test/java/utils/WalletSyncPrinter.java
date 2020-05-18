package utils;

import monero.wallet.model.MoneroWalletListener;

/**
 * Print sync progress every X blocks.
 */
public class WalletSyncPrinter extends MoneroWalletListener {
  
  private double syncResolution;
  private double lastIncrement;
  
  public WalletSyncPrinter() {
    this(0.05);
  }
  
  public WalletSyncPrinter(double syncResolution) {
    this.lastIncrement = 0;
    this.syncResolution = syncResolution;
  }
  
  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    if (percentDone == 1.0 || percentDone >= this.lastIncrement) {
      System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message + ")");
      this.lastIncrement += this.syncResolution;
    }
  }
}