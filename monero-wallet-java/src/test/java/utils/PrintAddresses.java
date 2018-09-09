package utils;

import java.util.List;

import wallet.MoneroWallet;
import wallet.model.MoneroAccount;
import wallet.model.MoneroSubaddress;

/**
 * Prints the balances of the wallet.
 */
public class PrintAddresses {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Primary address: " + wallet.getPrimaryAddress());
    List<MoneroAccount> accounts = wallet.getAccounts();
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      for (MoneroSubaddress subaddress : subaddresses) {
        System.out.println("[" + account.getIndex() + ", " + subaddress.getIndex() + "] " + subaddress.getAddress());
      }
    }
  }
}
