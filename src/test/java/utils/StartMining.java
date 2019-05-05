package utils;

public class StartMining {

  public static void main(String[] args) {
    TestUtils.getWalletRpc().startMining(7, false, true);
  }
}
