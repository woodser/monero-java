package utils;

import model.MoneroTx;
import wallet.MoneroWallet;

/**
 * Prints transactions in a wallet.
 */
public class PrintTransactions {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    for (MoneroTx tx : wallet.getTxs()) {
      System.out.println(tx);
    }
  }
}
