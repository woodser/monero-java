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
    TestUtils.getDaemonRpc().startMining("59dF9pSotECe1Fn4dBGZXWHYyNdo53rbZ7YYseu9jBKCf4c2cUzhuFVRH8HuD4wyaKTqtD3VF3F4eQe3Kzq342F5U8R4jeq", numThreads, false, false);
  }
}
