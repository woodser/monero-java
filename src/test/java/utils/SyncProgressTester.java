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
  protected boolean sessionStart = true;
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
    if (sessionStart && prevCompleteHeight != null && startHeight >= prevCompleteHeight) this.startHeight = startHeight;
    
    // progress notifications are throttled, so heights may skip, and the start height may rebase
    // down to report progress while the wallet skips hashes below the sync start
    assertTrue(endHeight > startHeight, "end height > start height");
    assertTrue(startHeight <= this.startHeight, "start height only rebases down");
    this.startHeight = startHeight;
    assertTrue(endHeight >= prevEndHeight);  // chain can grow while syncing
    prevEndHeight = endHeight;
    if (prevHeight != null) assertTrue(height >= prevHeight, "heights advance monotonically");
    prevHeight = height;
    if (height < startHeight) {
      assertTrue(sessionStart, "hash-skip notification only at the start of a sync session");
      assertEquals(0.0, percentDone); // initial notification while the wallet skips ahead to the sync start
    } else {
      assertTrue(height < endHeight);
      double expectedPercentDone = (double) (height - startHeight + 1) / (double) (endHeight - startHeight);
      assertTrue(Double.compare(expectedPercentDone, percentDone) == 0);
      if (Double.compare(percentDone, 1) == 0) prevCompleteHeight = endHeight; // record completion height for subsequent sync sessions
    }
    sessionStart = Double.compare(percentDone, 1) == 0; // completion starts a new session
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
