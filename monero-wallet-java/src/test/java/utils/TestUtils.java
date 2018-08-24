package utils;

import org.apache.log4j.PropertyConfigurator;

import service.MoneroAccount;
import service.MoneroWallet;
import service.rpc.MoneroWalletRpcOld;

/**
 * Collection of test utilities.
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

  public static MoneroWallet getWallet() {
    if (wallet == null) {
      try {
        wallet = new MoneroWalletRpcOld(DOMAIN, PORT, USERNAME, PASSWORD);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    return wallet;
  }
}
