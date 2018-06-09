package utils;

import org.apache.log4j.PropertyConfigurator;

import service.MoneroAccount;
import service.rpc.MoneroWalletRpc;

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
  private static MoneroAccount wallet;
  
  // initialize log4j configuration
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }

  public static MoneroAccount getWallet() {
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
