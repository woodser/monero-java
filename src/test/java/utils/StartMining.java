package utils;

/**
 * Utility class to start mining.
 */
public class StartMining {

  public static void main(String[] args) {
    startMining();
  }
  
  public static void startMining() {
    long numThreads = 16;
    //TestUtils.getWalletRpc().startMining(numThreads, false, true);
    TestUtils.getDaemonRpc().startMining("586ehiDDayx273iusViduzEFF2ACgkiBFEBuJUkLyACX9svXeWtxCZXBviT6WRaYoWFspnXN3rsazGNW8i3ah6nwV4RmAjA", numThreads, false, false);  // random subaddress
  }
}
