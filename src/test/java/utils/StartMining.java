package utils;

public class StartMining {

  public static void main(String[] args) {
    TestUtils.getWalletRpc().startMining(8, false, true);
  }
}
