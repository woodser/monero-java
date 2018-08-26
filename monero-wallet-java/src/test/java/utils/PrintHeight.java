package utils;

import wallet.MoneroWallet;

/**
 * Prints the height of the wallet.
 */
public class PrintHeight {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Height: " + wallet.getHeight());
  }
}
