package utils;

import service.MoneroWallet;

/**
 * Prints the balances of the wallet.
 */
public class PrintBalances {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Balance: " + wallet.getBalance(0));
    System.out.println("Unlocked: " + wallet.getUnlockedBalance(0));
  }
}
