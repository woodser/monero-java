package utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URISyntaxException;
import java.util.Collection;

import org.apache.log4j.PropertyConfigurator;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.rpc.MoneroRpc;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroOutput;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTx.MoneroTxType;

/**
 * Test utilities and constants.
 */
public class TestUtils {
  
  // monero-daemon-rpc endpoint configuration (adjust per your configuration)
  private static final String DAEMON_RPC_DOMAIN = "localhost";
  private static final int DAEMON_RPC_PORT = 38081;
  
  // monero-wallet-rpc endpoint configuration (adjust per your configuration)
  private static final String WALLET_RPC_DOMAIN = "localhost";
  private static final int WALLET_RPC_PORT = 38083;
  private static final String WALLET_RPC_USERNAME = "rpc_user";
  private static final String WALLET_RPC_PASSWORD = "abc123";
  
  // names of test wallets
  public static final String WALLET_NAME_1 = "test_wallet_1";
  public static final String WALLET_NAME_2 = "test_wallet_2";
  public static final String WALLET_PW = "supersecretpassword123";
  
  // test constants
  public static final Integer MIXIN = 6;
  
  // log4j configuration
  static {
    PropertyConfigurator.configure("src/main/resources/log4j.properties");
  }
  
  // singleton instance of a Daemon to test
  private static MoneroDaemon daemon;
  public static MoneroDaemon getDaemon() {
    if (daemon == null) {
      
      // connect to daemon
      try {
        MoneroRpc rpc = new MoneroRpc(DAEMON_RPC_DOMAIN, DAEMON_RPC_PORT);
        daemon = new MoneroDaemonRpc(rpc);
      } catch (URISyntaxException e) {
        throw new RuntimeException(e);
      }
    }
    return daemon;
  }
  
  // singleton instance of the wallet to test
  private static MoneroWallet wallet;
  public static MoneroWallet getWallet() {
    if (wallet == null) {
      
      // connect to wallet
      try {
        wallet = new MoneroWalletRpc(WALLET_RPC_DOMAIN, WALLET_RPC_PORT, WALLET_RPC_USERNAME, WALLET_RPC_PASSWORD);
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
      
      // refresh wallet
//      wallet.rescanSpent();
//      wallet.rescanBlockchain();
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
    
    // test all transactions
    assertNotNull(tx.getId());
    assertNotNull(tx.getType());
    assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());  // default payment should be converted to null
    
    // test incoming or outgoing
    if (tx.getType() == MoneroTxType.OUTGOING || tx.getType() == MoneroTxType.INCOMING) {
      assertNotNull(tx.getAccountIndex());
      assertNotNull(tx.getSubaddressIndex());
    }
    
    // test outgoing
    if (tx.getType() == MoneroTxType.OUTGOING) {
      if (tx.getAddress() == null) {
        assertNotNull(tx.getPayments());
        assertFalse(tx.getPayments().isEmpty());
      }
      if (tx.getPayments() == null || tx.getPayments().isEmpty()) {
        assertNotNull(tx.getAddress());
      }
      assertNotNull(tx.getAmount());
      assertFalse(tx.getIsDoubleSpend());
      assertNotNull(tx.getAccountIndex());
      assertTrue(tx.getAmount().longValue() >= 0);  // TODO: seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
      if (tx.getPayments() != null) {
        assertFalse(tx.getPayments().isEmpty());
        for (MoneroPayment payment : tx.getPayments()) {
          assertNotNull(payment.getAmount());
          assertNotNull(payment.getAddress());
          assertTrue(payment.getAmount().longValue() > 0);
        }
      }
    }
    
    // test incoming
    if (tx.getType() == MoneroTxType.INCOMING) {
      assertNotNull(tx.getOutputs());
      assertNotNull(tx.getKey());
      assertFalse(tx.getOutputs().isEmpty());
      for (MoneroOutput output : tx.getOutputs()) {
        assertNotNull(output.getAmount());
        assertNotNull(output.isSpent());
      }
    }
  }
  
  public static void testSendTx(MoneroTx tx, boolean canSplit) {
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNull(tx.getSize());
    assertNotNull(tx.getPayments());
    assertFalse(tx.getPayments().isEmpty());
    assertEquals(MoneroTxType.OUTGOING, tx.getType());
    assertNull(tx.getHeight());
    assertEquals((Integer) 0, tx.getUnlockTime());
    assertNotNull(tx.getBlob());
    assertNotNull(tx.getMetadata());
    if (canSplit) {
      assertNull(tx.getKey());
    } else {
      assertNotNull(tx.getKey());
    }
  }
  
  public static void testAddressBookEntry(MoneroAddressBookEntry entry) {
    assertTrue(entry.getIndex() >= 0);
    assertNotNull(entry.getAddress());
    assertNotNull(entry.getDescription());
  }
  
  /**
   * Get an account by index.
   * 
   * @param accounts is a collection of accounts to search
   * @param accountIdx is the index of the account to return
   * @return MoneroAccount is the account with the given index, null if not found
   */
  public static MoneroAccount getAccountById(Collection<MoneroAccount> accounts, int accountIdx) {
    for (MoneroAccount account : accounts) {
      if (account.getIndex() == accountIdx) return account;
    }
    return null;
  }
}
