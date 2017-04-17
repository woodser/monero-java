package utils;

import wallet.MoneroWallet;
import wallet.MoneroWalletRpc;

/**
 * Collection of test utilities.
 * 
 * @author woodser
 */
public class TestUtils {
  
  private static String DOMAIN = "localhost";
  private static int PORT = 18082;
  private static MoneroWallet wallet;

  public static MoneroWallet getWallet() {
    if (wallet == null) {
      try {
        wallet = new MoneroWalletRpc(DOMAIN, PORT);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    return wallet;
  }
}
