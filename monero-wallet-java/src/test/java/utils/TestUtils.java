package utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.net.URISyntaxException;
import java.util.Collection;

import org.apache.log4j.PropertyConfigurator;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.rpc.MoneroRpc;
import monero.rpc.MoneroRpcException;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTx.MoneroTxType;
import monero.wallet.model.MoneroTxConfig;

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
      
      // create test wallet if necessary
      try {
        wallet.createWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW, "English");
      } catch (MoneroRpcException e) {
        assertEquals((int) -21, (int) e.getRpcCode());  // exception is ok if wallet already created
      }
      
      // open test wallet
      wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
      
      // refresh wallet
      wallet.rescanSpent();
      //wallet.rescanBlockchain();
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
  
  public static void testCommonTx(MoneroTx tx) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getType());
    assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());  // default payment should be converted to null
  }
  
  public static void testGetTx(MoneroTx tx, Boolean hasOutgoingPayments) {
    testCommonTx(tx);
    
    // test outgoing
    if (tx.getType() == MoneroTxType.OUTGOING || tx.getType() == MoneroTxType.PENDING || tx.getType() == MoneroTxType.FAILED) {
      assertNotNull("hasOutgoingPayments must be specified", hasOutgoingPayments);
      assertNotNull(tx.getId());
      assertNotNull(tx.getSrcAddress());
      assertNotNull(tx.getSrcAccountIdx());
      assertNotNull(tx.getSrcSubaddressIdx());
      assertNotNull(tx.getTotalAmount());
      assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      assertNotNull(tx.getFee());
      assertNull(tx.getMixin());
      assertNotNull(tx.getSize());
      assertNotNull(tx.getNote());
      assertNotNull(tx.getTimestamp());
      assertEquals((Integer) 0, tx.getUnlockTime());
      assertNotNull(tx.getIsDoubleSpend());
      assertFalse(tx.getIsDoubleSpend());
      assertNull(tx.getKey());
      assertNull(tx.getBlob());
      assertNull(tx.getMetadata());
      if (tx.getType() == MoneroTxType.OUTGOING) {
        assertNotNull(tx.getHeight());
      } else if (tx.getType() == MoneroTxType.PENDING || tx.getType() == MoneroTxType.FAILED) {
        assertNull(tx.getHeight());
      }
      if (hasOutgoingPayments) {
        assertNotNull(tx.getPayments());
        assertFalse(tx.getPayments().isEmpty());
        BigInteger totalAmount = BigInteger.valueOf(0);
        for (MoneroPayment payment : tx.getPayments()) {
          assertTrue(tx == payment.getTx());
          assertNotNull(payment.getAddress());
          assertNotNull(payment.getAmount());
          assertTrue(payment.getAmount().longValue() > 0);  // TODO: seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
          assertNull(payment.getAccountIdx());
          assertNull(payment.getSubaddressIdx());
          assertNull(payment.getIsSpent());
          totalAmount = totalAmount.add(payment.getAmount());
        }
        assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
      }
    }
    
    // test incoming
    if (tx.getType() == MoneroTxType.INCOMING || tx.getType() == MoneroTxType.MEMPOOL) {
      assertNotNull(tx.getId());
      assertNull(tx.getSrcAddress());
      assertNull(tx.getSrcAccountIdx());
      assertNull(tx.getSrcSubaddressIdx());
      assertNotNull(tx.getTotalAmount());
      assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      assertNotNull(tx.getFee());
      assertNull(tx.getMixin());
      assertNotNull(tx.getSize());
      assertNotNull(tx.getNote());
      assertNotNull(tx.getTimestamp());
      assertNotNull(tx.getUnlockTime());
      assertNotNull(tx.getIsDoubleSpend());
      assertFalse(tx.getIsDoubleSpend());
      assertNotNull(tx.getKey());
      assertNull(tx.getBlob());
      assertNull(tx.getMetadata());
      if (tx.getType() == MoneroTxType.INCOMING) {
        assertNotNull(tx.getHeight());
      } else if (tx.getType() == MoneroTxType.MEMPOOL) {
        assertNull(tx.getHeight());
      }
      assertNotNull(tx.getPayments());
      assertFalse(tx.getPayments().isEmpty());
      BigInteger totalAmount = BigInteger.valueOf(0);
      for (MoneroPayment payment : tx.getPayments()) {
        assertTrue(tx == payment.getTx());
        if (payment.getAddress() == null) {
          System.out.println("address null");
          System.out.println(tx);
        }
        //assertNotNull(payment.getAddress());  // TODO: re-enable but need to fetch
        assertNotNull(payment.getAmount());
        assertTrue(payment.getAmount().longValue() > 0);  // TODO: seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
        assertNotNull(payment.getAccountIdx());
        assertNotNull(payment.getSubaddressIdx());
        assertNotNull(payment.getIsSpent());
        totalAmount = totalAmount.add(payment.getAmount());
      }
      assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
    }
  }
  
  public static void testSendTx(MoneroTx tx, MoneroTxConfig config) {
    testCommonTx(tx);
    assertEquals(MoneroTxType.PENDING, tx.getType());
    assertNotNull(tx.getId());
    assertNotNull(tx.getSrcAddress());
    assertEquals(config.getAccountIndex(), tx.getSrcAccountIdx());
    assertEquals((Integer) 0, tx.getSrcSubaddressIdx());
    assertNotNull(tx.getTotalAmount());
    if (config.getPaymentId().equals(MoneroTx.DEFAULT_PAYMENT_ID)) assertNull(tx.getPaymentId());
    else assertEquals(config.getPaymentId(), tx.getPaymentId());
    assertNotNull(tx.getFee());
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getSize());
    assertNotNull(tx.getNote());
    assertNotNull(tx.getTimestamp());
    assertEquals((Integer) 0, tx.getUnlockTime());
    assertNotNull(tx.getIsDoubleSpend());
    assertFalse(tx.getIsDoubleSpend());
//    if (canSplit) {
//      assertNull(tx.getKey());
//    } else {
      assertNotNull(tx.getKey());
//    }
    assertNotNull(tx.getBlob());
    assertNotNull(tx.getMetadata());
    assertNull(tx.getHeight());
    assertNotNull(tx.getPayments());
    assertFalse(tx.getPayments().isEmpty());
    BigInteger totalAmount = BigInteger.valueOf(0);
    assertEquals(config.getDestinations(), tx.getPayments());
    for (MoneroPayment payment : tx.getPayments()) {
      assertTrue(tx == payment.getTx());
      assertNotNull(payment.getAddress());
      assertNotNull(payment.getAmount());
      assertTrue(payment.getAmount().longValue() > 0);  // TODO: seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
      assertNull(payment.getAccountIdx());
      assertNull(payment.getSubaddressIdx());
      assertNull(payment.getIsSpent());
      totalAmount = totalAmount.add(payment.getAmount());
    }
    assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
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
