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
    //TestUtils.getWalletRpc().startMining(numThreads, false, true);
    //TestUtils.getDaemonRpc().startMining("59dF9pSotECe1Fn4dBGZXWHYyNdo53rbZ7YYseu9jBKCf4c2cUzhuFVRH8HuD4wyaKTqtD3VF3F4eQe3Kzq342F5U8R4jeq", numThreads, false, false); // stagenet
    TestUtils.getDaemonRpc().startMining("9tsUiG9bwcU7oTbAdBwBk2PzxFtysge5qcEsHEpetmEKgerHQa1fDqH7a4FiquZmms7yM22jdifVAD7jAb2e63GSJMuhY75", numThreads, false, false); // testnet
  }
}
