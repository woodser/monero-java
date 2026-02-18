package utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import monero.wallet.MoneroWallet;

public class SyncProgressTester extends WalletSyncPrinter {
  
  protected MoneroWallet wallet;
  protected Long prevHeight;
  protected long startHeight;
  protected long prevEndHeight;
  protected Long prevCompleteHeight;
  protected boolean isDone;
  protected Boolean onSyncProgressAfterDone;

  public SyncProgressTester(MoneroWallet wallet, long startHeight, long endHeight) {
    this.wallet = wallet;
    assertTrue(startHeight >= 0);
    assertTrue(endHeight >= 0);
    this.startHeight = startHeight;
    this.prevEndHeight = endHeight;
    this.isDone = false;
  }
  
  @Override
  public synchronized void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    super.onSyncProgress(height, startHeight, endHeight, percentDone, message);
    
    // registered wallet listeners will continue to get sync notifications after the wallet's initial sync
    if (isDone) {
      assertTrue(wallet.getListeners().contains(this), "Listener has completed and is not registered so should not be called again");
      onSyncProgressAfterDone = true;
    }
    
    // update tester's start height if new sync session
    if (prevCompleteHeight != null && startHeight == prevCompleteHeight) this.startHeight = startHeight;
    
    // if sync is complete, record completion height for subsequent start heights
    if (Double.compare(percentDone, 1) == 0) prevCompleteHeight = endHeight;
    
    // otherwise start height is equal to previous completion height
    else if (prevCompleteHeight != null) assertEquals((long) prevCompleteHeight, startHeight);
    
    assertTrue(endHeight > startHeight, "end height > start height");
    assertEquals(this.startHeight, startHeight);
    assertTrue(endHeight >= prevEndHeight);  // chain can grow while syncing
    prevEndHeight = endHeight;
    assertTrue(height >= startHeight);
    assertTrue(height < endHeight);
    double expectedPercentDone = (double) (height - startHeight + 1) / (double) (endHeight - startHeight);
    assertTrue(Double.compare(expectedPercentDone, percentDone) == 0);
    
    if (prevHeight == null) assertEquals(startHeight, height);
    else assertEquals(height, prevHeight + 1);
    
    prevHeight = height;
  }
  
  public void onDone(long chainHeight) {
    assertFalse(isDone);
    this.isDone = true;
    if (prevHeight == null) {
      assertNull(prevCompleteHeight);
      assertEquals(chainHeight, startHeight);
    } else {
      assertEquals(chainHeight - 1, (long) prevHeight);  // otherwise last height is chain height - 1
      assertEquals(chainHeight, (long) prevCompleteHeight);
    }
  }
  
  public Boolean isNotified() {
    return prevHeight != null;
  }
  
  public Boolean getOnSyncProgressAfterDone() {
    return onSyncProgressAfterDone;
  }
}
