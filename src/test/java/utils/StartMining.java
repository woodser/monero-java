package utils;

import common.utils.GenUtils;
import monero.daemon.MoneroDaemon;

/**
 * Utility class to start mining.
 */
public class StartMining {

  public static void main(String[] args) {
    startMining();
  }

  public static void mineToHeight(long height) {
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    if (daemon.getHeight() >= height) return;
    boolean startedMining = false;
    if (!daemon.getMiningStatus().isActive()) {
      try {
        startMining();
        startedMining = true;
      } catch (Exception e) { }
    }
    try {
      while (daemon.getHeight() < height) GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS);
    } finally {
      if (startedMining) daemon.stopMining();
    }
  }
  
  public static void startMining() {
    startMining(1);
  }
  
  public static void startMining(long numThreads) {
    TestUtils.getDaemonRpc().startMining(TestUtils.MINING_ADDRESS, numThreads, false, false);
  }
}
