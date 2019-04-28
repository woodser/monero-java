package utils;

public class StopMining {

  public static void main(String[] args) {
    TestUtils.getDaemonRpc().stopMining();
  }
}
