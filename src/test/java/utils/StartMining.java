package utils;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Mines regtest blocks on demand or at random intervals in the background.
 */
public class StartMining {

  private static ScheduledThreadPoolExecutor miner;

  public static void main(String[] args) throws InterruptedException {
    startMining();
    Thread.currentThread().join();
  }

  public static void generateBlocks(long numBlocks) {
    generateBlocks(TestUtils.MINING_ADDRESS, numBlocks);
  }

  public static void generateBlocks(String address, long numBlocks) {
    if (numBlocks <= 0) return;
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("wallet_address", address);
    params.put("amount_of_blocks", numBlocks);
    TestUtils.getDaemonRpc().getRpcConnection().sendJsonRequest("generateblocks", params);
  }

  public static void mineToHeight(long height) {
    generateBlocks(height - TestUtils.getDaemonRpc().getHeight());
  }

  public static synchronized void startMining() {
    if (miner != null) return;
    ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, runnable -> {
      Thread thread = new Thread(runnable);
      thread.setDaemon(true);
      return thread;
    });
    executor.setExecuteExistingDelayedTasksAfterShutdownPolicy(false); // stopping skips the wait for the next block
    executor.execute(() -> mineBlock(executor));
    miner = executor;
  }

  private static void mineBlock(ScheduledThreadPoolExecutor executor) {
    try { generateBlocks(1); }
    catch (Exception e) { System.err.println("Failed to mine block: " + e.getMessage()); }
    long delay = (long) (-Math.log(1 - Math.random()) * TestUtils.BLOCK_PERIOD_IN_MS); // exponential intervals like mined blocks
    try { executor.schedule(() -> mineBlock(executor), delay, TimeUnit.MILLISECONDS); }
    catch (RejectedExecutionException e) { } // mining stopped
  }

  public static synchronized void stopMining() {
    if (miner == null) return;
    miner.shutdown(); // a block being mined completes first
    try { miner.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS); }
    catch (InterruptedException e) { throw new RuntimeException(e); }
    miner = null;
  }

  public static synchronized boolean isMining() {
    return miner != null;
  }
}
