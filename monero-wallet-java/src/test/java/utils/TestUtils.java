package utils;

import org.apache.log4j.PropertyConfigurator;

import wallet.MoneroWallet;
import wallet.rpc.MoneroWalletRpc;

/**
 * Test utilities and constants.
 */
public class TestUtils {
  
  // monero-wallet-rpc endpoint configuration (adjust per your configuration)
  private static final String DOMAIN = "localhost";
  private static final int PORT = 38083;
  private static final String USERNAME = "rpc_user";
  private static final String PASSWORD = "abc123";
  
  // log4j configuration
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }
  
  // singleton instance of the wallet to be tested
  private static MoneroWallet wallet;
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
  
  // names of test wallets
  public static final String WALLET_PW = "supersecretpassword123";
  public static final String WALLET_NAME_1 = "test_wallet_1";
  public static final String WALLET_NAME_2 = "test_wallet_2";
}
