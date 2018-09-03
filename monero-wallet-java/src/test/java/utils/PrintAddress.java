package utils;

import wallet.MoneroWallet;

/**
 * Prints the balances of the wallet.
 */
public class PrintAddress {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Primary address: " + wallet.getPrimaryAddress());
  }
}
