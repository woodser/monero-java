package utils;

import service.MoneroAccount;

/**
 * Prints the balances of the wallet.
 */
public class PrintBalances {

  public static void main(String[] args) {
    MoneroAccount wallet = TestUtils.getWallet();
    System.out.println("Balance: " + wallet.getBalance());
    System.out.println("Unlocked: " + wallet.getUnlockedBalance());
  }
}
