package utils;

import monero.wallet.MoneroWallet;

/**
 * Prints the height of the wallet.
 */
public class PrintHeightOld {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Height: " + wallet.getHeight());
  }
}
