package utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.net.URISyntaxException;

import org.apache.log4j.PropertyConfigurator;

import wallet.MoneroWallet;
import wallet.MoneroWalletRpc;
import wallet.model.MoneroAccount;
import wallet.model.MoneroAddressBookEntry;
import wallet.model.MoneroOutput;
import wallet.model.MoneroPayment;
import wallet.model.MoneroSubaddress;
import wallet.model.MoneroTx;
import wallet.model.MoneroTx.MoneroTxType;

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
  
  public static void testAccount(MoneroAccount account) {
    assertTrue(account.getIndex() >= 0);
    assertNotNull(account.getPrimaryAddress());
    assertTrue(account.getBalance().doubleValue() >= 0);
    assertTrue(account.getUnlockedBalance().doubleValue() >= 0);
    assertFalse(account.isMultisigImportNeeded());
    if (account.getSubaddresses() != null) {
      for (int i = 0; i < account.getSubaddresses().size(); i++) {
        testSubaddress(account.getSubaddresses().get(i));
      }
    }
  }
  
  public static void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getIndex() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getBalance().doubleValue() >= 0);
    assertTrue(subaddress.getUnlockedBalance().doubleValue() >= 0);
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    assertFalse(subaddress.isMultisigImportNeeded());
    if (subaddress.getBalance().doubleValue() > 0) assertTrue(subaddress.isUsed());
  }
  
  public static void testTx(MoneroTx tx) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getType());
    if (tx.getType() == MoneroTxType.OUTGOING) {
      assertNotNull(tx.getAddress());
      assertNotNull(tx.getAmount());
      assertFalse(tx.isDoubleSpend());
      assertNotNull(tx.getAccountIndex());
      assertNotNull(tx.getSubaddressIndex());
      assertTrue(tx.getAmount().longValue() >= 0);  // TODO: seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
      if (tx.getPayments() != null) {
        assertFalse(tx.getPayments().isEmpty());
        for (MoneroPayment payment : tx.getPayments()) {
          assertNotNull(payment.getAmount());
          assertNotNull(payment.getAddress());
          assertTrue(payment.getAmount().longValue() > 0);
        }
      }
    } else if (tx.getType() == MoneroTxType.INCOMING) {
      assertNotNull(tx.getOutputs());
      assertNotNull(tx.getKey());
      assertFalse(tx.getOutputs().isEmpty());
      for (MoneroOutput output : tx.getOutputs()) {
        assertNotNull(output.getAmount());
        assertNotNull(output.isSpent());
      }
    }
  }
  
  public static void testAddressBookEntry(MoneroAddressBookEntry entry) {
    assertTrue(entry.getIndex() >= 0);
    assertNotNull(entry.getAddress());
    assertNotNull(entry.getDescription());
  }
}
