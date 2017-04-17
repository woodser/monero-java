package utils;

import wallet.MoneroWallet;

/**
 * Prints the balances of the wallet.
 * 
 * @author woodser
 */
public class PrintBalances {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Balance: " + wallet.getBalance());
    System.out.println("Unlocked: " + wallet.getUnlockedBalance());
  }
}
