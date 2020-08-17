package utils;

/**
 * Utility class to start mining.
 */
public class StartMining {

  public static void main(String[] args) {
    startMining();
  }
  
  public static void startMining() {
    long numThreads = 1;
    //TestUtils.getWalletRpc().startMining(numThreads, false, true);
    TestUtils.getDaemonRpc().startMining("55TQb2k8LKmWgEqpsWxA2sH18bQ9oK1gDDfCkqivyP6V8F289fyVEPgSuvaPtjswyfKuXn9AcgS7LfbJvKoGSJFKMDGx5Vr", numThreads, false, false);  // random subaddress
  }
}
