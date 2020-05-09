package utils;

import monero.wallet.model.MoneroWalletListener;

/**
 * Print sync progress every X blocks.
 */
public class WalletSyncPrinter extends MoneroWalletListener {
  
  private long blockResolution;
  
  public WalletSyncPrinter() {
    this(5000l);
  }
  
  public WalletSyncPrinter(long blockResolution) {
    this.blockResolution = blockResolution;
  }
  
  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    if (percentDone == 1 || (startHeight - height) % blockResolution == 0) {
      System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message + ")");
    }
  }
}