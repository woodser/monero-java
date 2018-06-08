package utils;

import org.apache.log4j.PropertyConfigurator;

import wallet.MoneroWallet;
import wallet.MoneroWalletRpc;

/**
 * Collection of test utilities.
 * 
 * @author woodser
 */
public class TestUtils {
  
  private static final String DOMAIN = "localhost";
  private static final int PORT = 38083;
  private static final String USERNAME = "rpc_user";
  private static final String PASSWORD = "abc123";
  private static MoneroWallet wallet;
  
  // initialize log4j configuration
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }

  public static MoneroWallet getWallet() {
    if (wallet == null) {
      try {
        wallet = new MoneroWalletRpc(DOMAIN, PORT, USERNAME, PASSWORD);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    return wallet;
  }
}
