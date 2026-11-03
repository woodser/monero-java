package utils;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Mines regtest blocks on demand or one per block period in the background.
 */
public class StartMining {

  private static ScheduledExecutorService miner;

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
    miner = Executors.newSingleThreadScheduledExecutor(runnable -> {
      Thread thread = new Thread(runnable);
      thread.setDaemon(true);
      return thread;
    });
    miner.scheduleWithFixedDelay(() -> {
      try { generateBlocks(1); }
      catch (Exception e) { System.err.println("Failed to mine block: " + e.getMessage()); }
    }, 0, TestUtils.BLOCK_PERIOD_IN_MS, TimeUnit.MILLISECONDS);
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
