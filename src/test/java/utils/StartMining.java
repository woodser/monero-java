package utils;

public class StartMining {

  public static void main(String[] args) {
    int numThreads = 8;
    //TestUtils.getWalletRpc().startMining(numThreads, false, true);
    TestUtils.getDaemonRpc().startMining("59HdmrdyhUEMyuzhLGLVTjPEm4kHaHQc5gGVXRBjxaXDEU4M6G2mRv7KZipYyK2QS9G7FLCg4bGc2Wqv5H86zjEMF1UhbzZ", numThreads, false, false);
  }
}
