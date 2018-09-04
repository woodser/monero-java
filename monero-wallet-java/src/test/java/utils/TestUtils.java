package utils;

import java.net.URISyntaxException;

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
  
  // names of test wallets
  public static final String WALLET_NAME_1 = "test_wallet_1";
  public static final String WALLET_NAME_2 = "test_wallet_2";
  public static final String WALLET_PW = "supersecretpassword123";
  
  // log4j configuration
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }
  
  // singleton instance of the wallet to be tested
  private static MoneroWallet wallet;
  public static MoneroWallet getWallet() {
    if (wallet == null) {
      
      // connect to wallet
      try {
        wallet = new MoneroWalletRpc(DOMAIN, PORT, USERNAME, PASSWORD);
      } catch (URISyntaxException e1) {
        throw new RuntimeException(e1);
      }
      
//      // create test wallet if necessary
//      try {
//        wallet.createWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW, "English");
//      } catch (MoneroRpcException e) {
//        assertEquals((int) -21, (int) e.getRpcCode());  // exception is ok if wallet already created
//      }
//      
//      // open test wallet
//      wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
    }
    return wallet;
  }
}
