package utils;

import monero.wallet.MoneroWallet;

/**
 * Prints the height of the wallet.
 */
public class PrintHeight {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWalletRpc();
    System.out.println("Height: " + wallet.getHeight());
  }
}
