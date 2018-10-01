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

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.rpc.MoneroRpc;
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
  public static final BigInteger MAX_FEE = BigInteger.valueOf(5000000).multiply(BigInteger.valueOf(10000));
  
  // logger configuration
  public static final Logger LOGGER = Logger.getLogger(TestUtils.class);
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
//      
//      // refresh wallet
//      wallet.rescanSpent();
    }
    return wallet;
  }
  
  public static void testAccount(MoneroAccount account) {
    assertTrue(account.getIndex() >= 0);
    assertNotNull(account.getPrimaryAddress());
    assertTrue(account.getBalance().doubleValue() >= 0);
    assertTrue(account.getUnlockedBalance().doubleValue() >= 0);
    assertFalse(account.getIsMultisigImportNeeded());
    if (account.getSubaddresses() != null) {
      BigInteger balance = BigInteger.valueOf(0);
      BigInteger unlockedBalance = BigInteger.valueOf(0);
      for (int i = 0; i < account.getSubaddresses().size(); i++) {
        testSubaddress(account.getSubaddresses().get(i));
        assertEquals((Integer) i, account.getSubaddresses().get(i).getIndex());
        balance = balance.add(account.getSubaddresses().get(i).getBalance());
        unlockedBalance = unlockedBalance.add(account.getSubaddresses().get(i).getUnlockedBalance());
      }
      assertEquals(account.getBalance(), balance);
      assertEquals(account.getUnlockedBalance(), unlockedBalance);
    }
  }
  
  public static void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getIndex() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getBalance().doubleValue() >= 0);
    assertTrue(subaddress.getUnlockedBalance().doubleValue() >= 0);
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    if (subaddress.getBalance().doubleValue() > 0) assertTrue(subaddress.isUsed());
  }
  
  public static void testCommonTx(MoneroTx tx) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getId(), tx.getType());
    assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());  // default payment should be converted to null
  }
  
  public static void testGetTx(MoneroTx tx, Boolean hasOutgoingPayments) {
    testCommonTx(tx);
    
    // test outgoing
    if (tx.getType() == MoneroTxType.OUTGOING || tx.getType() == MoneroTxType.PENDING || tx.getType() == MoneroTxType.FAILED) {
      assertNotNull(tx.getId());
      //assertNotNull(tx.getId(), tx.getSrcAddress());   // TODO: need to fetch
      assertNotNull(tx.getId(), tx.getSrcAccountIdx());
      assertNotNull(tx.getId(), tx.getSrcSubaddressIdx());
      assertNotNull(tx.getId(), tx.getTotalAmount());
      assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      assertNotNull(tx.getId(), tx.getFee());
      assertNull(tx.getId(), tx.getMixin());
      assertNull(tx.getId(), tx.getSize());
      assertNotNull(tx.getId(), tx.getNote());
      assertNotNull(tx.getId(), tx.getTimestamp());
      assertEquals(tx.getId(), (Integer) 0, tx.getUnlockTime());
      assertNotNull(tx.getId(), tx.getIsDoubleSpend());
      assertFalse(tx.getId(), tx.getIsDoubleSpend());
      assertNull(tx.getId(), tx.getKey());
      assertNull(tx.getId(), tx.getBlob());
      assertNull(tx.getId(), tx.getMetadata());
      if (tx.getType() == MoneroTxType.OUTGOING) assertNotNull(tx.getId(), tx.getHeight());
      else if (tx.getType() == MoneroTxType.PENDING || tx.getType() == MoneroTxType.FAILED) assertNull(tx.getId(), tx.getHeight());
      if (Boolean.TRUE.equals(hasOutgoingPayments)) assertNotNull(tx.getId(), tx.getPayments());
      else if (Boolean.FALSE.equals(hasOutgoingPayments)) assertNull(tx.getId(), tx.getPayments());
      if (tx.getPayments() != null) {
        assertFalse(tx.getId(), tx.getPayments().isEmpty());
        BigInteger totalAmount = BigInteger.valueOf(0);
        for (MoneroPayment payment : tx.getPayments()) {
          assertTrue(tx == payment.getTx());
          totalAmount = totalAmount.add(payment.getAmount());
          assertNotNull(tx.getId(), payment.getAddress());
          assertNotNull(tx.getId(), payment.getAmount());
          assertTrue(tx.getId(), payment.getAmount().longValue() >= 0);  // TODO (monero-wallet-rpc): seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
          assertNull(tx.getId(), payment.getAccountIdx());
          assertNull(tx.getId(), payment.getSubaddressIdx());
          assertNull(tx.getId(), payment.getIsSpent());
        }
        // TODO (monero-wallet-rpc): incoming_transfers d59fe775249465621d7736b53c0cb488e03e4da8dae647a13492ea51d7685c62 totalAmount is 0?
        if (totalAmount.compareTo(tx.getTotalAmount()) != 0 && tx.getTotalAmount().compareTo(BigInteger.valueOf(0)) == 0) {
          LOGGER.warn("Total amount is not sum of payments: " + tx.getTotalAmount() + " vs " + totalAmount + " for TX " + tx.getId());
        } else {
          assertTrue("Total amount is not sum of payments: " + tx.getTotalAmount() + " vs " + totalAmount + " for TX " + tx.getId(), totalAmount.compareTo(tx.getTotalAmount()) == 0);
        }
      }
    }
    
    // test incoming
    if (tx.getType() == MoneroTxType.INCOMING || tx.getType() == MoneroTxType.MEMPOOL) {
      assertNotNull(tx.getId(), tx.getId());
      assertNull(tx.getId(), tx.getSrcAddress());
      assertNull(tx.getId(), tx.getSrcAccountIdx());
      assertNull(tx.getSrcSubaddressIdx());
      assertNotNull(tx.getId(), tx.getTotalAmount());
      assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      if (tx.getFee() == null) LOGGER.warn("Incoming transaction is missing fee: " + tx.getId()); // TODO (monero-wallet-rpc): show on incoming_transfers or fix bug where incoming txs don't return on account 0
      assertNull(tx.getId(), tx.getMixin());
      //assertNotNull(tx.getId(), tx.getSize());
      assertNull(tx.getId(), tx.getNote());
      if (tx.getTimestamp() == null) LOGGER.warn("Incoming transaction is missing timestamp: " + tx.getId());
      if (tx.getUnlockTime() == null) LOGGER.warn("Incoming transaction is missing unlock_time: " + tx.getId());
      if (tx.getIsDoubleSpend() == null) LOGGER.warn("Incoming transaction is missing is_double_spend: " + tx.getId());
      if (tx.getIsDoubleSpend() != null) assertFalse(tx.getId(), tx.getIsDoubleSpend());
      if (tx.getType() == MoneroTxType.MEMPOOL) {
        assertNull(tx.getId(), tx.getKey());
        assertNull(tx.getId(), tx.getHeight());
      } else {
        assertNotNull(tx.getId(), tx.getKey());
        //assertNotNull(tx.getId(), tx.getHeight());
        if (tx.getHeight() == null) LOGGER.warn("Incoming transaction is missing height: " + tx.getId());
      }
      assertNull(tx.getId(), tx.getBlob());
      assertNull(tx.getId(), tx.getMetadata());
      assertNotNull(tx.getId(), tx.getPayments());
      assertFalse(tx.getId(), tx.getPayments().isEmpty());
      BigInteger totalAmount = BigInteger.valueOf(0);
      for (MoneroPayment payment : tx.getPayments()) {
        assertTrue(tx == payment.getTx());
        totalAmount = totalAmount.add(payment.getAmount());
        //assertNotNull(payment.getAddress());  // TODO: re-enable but need to fetch
        assertNotNull(tx.getId(), payment.getAmount());
        assertTrue(tx.getId(), payment.getAmount().longValue() > 0);  // TODO (monero-wallet-rpc): seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
        assertNotNull(tx.getId(), payment.getAccountIdx());
        assertNotNull(tx.getId(), payment.getSubaddressIdx());
        assertNotNull(tx.getId(), payment.getIsSpent());
      }
      assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
    }
  }
  
  public static void testSendTx(MoneroTx tx, MoneroTxConfig config, boolean hasKey, boolean hasPayments) {
    testCommonTx(tx);
    assertNotNull(tx.getId());
    assertEquals(tx.getId(), MoneroTxType.PENDING, tx.getType());
    assertNotNull(tx.getId(), tx.getSrcAddress());
    assertEquals(tx.getId(), config.getAccountIndex(), tx.getSrcAccountIdx());
    assertEquals(tx.getId(), (Integer) 0, tx.getSrcSubaddressIdx());
    assertNotNull(tx.getId(), tx.getTotalAmount());
    if (MoneroTx.DEFAULT_PAYMENT_ID.equals(config.getPaymentId())) assertNull(tx.getId(), tx.getPaymentId());
    else assertEquals(tx.getId(), config.getPaymentId(), tx.getPaymentId());
    assertNotNull(tx.getId(), tx.getFee());
    assertEquals(tx.getId(), config.getMixin(), tx.getMixin());
    assertNull(tx.getId(), tx.getSize());
    assertNotNull(tx.getId(), tx.getNote());
    assertNotNull(tx.getId(), tx.getTimestamp());
    assertEquals(tx.getId(), (Integer) 0, tx.getUnlockTime());
    assertNotNull(tx.getId(), tx.getIsDoubleSpend());
    assertFalse(tx.getId(), tx.getIsDoubleSpend());
    if (hasKey) assertNotNull(tx.getKey());
    else assertNull(tx.getKey());
    if (hasPayments) {
      assertNotNull(tx.getPayments());
      assertFalse(tx.getPayments().isEmpty());
      assertEquals(tx.getId(), config.getDestinations(), tx.getPayments());
    }
    assertNotNull(tx.getId(), tx.getBlob());
    assertNotNull(tx.getId(), tx.getMetadata());
    assertNull(tx.getId(), tx.getHeight());
    if (tx.getPayments() != null) {
      BigInteger totalAmount = BigInteger.valueOf(0);
      for (MoneroPayment payment : tx.getPayments()) {
        assertTrue(tx.getId(), tx == payment.getTx());
        totalAmount = totalAmount.add(payment.getAmount());
        assertNotNull(tx.getId(), payment.getAddress());
        assertNotNull(tx.getId(), payment.getAmount());
        assertTrue(tx.getId(), payment.getAmount().longValue() > 0);  // TODO (monero-wallet-rpc): seems amount = 0 is a bug in monero-wallet-rpc since destination amounts are > 0
        assertNull(tx.getId(), payment.getAccountIdx());
        assertNull(tx.getId(), payment.getSubaddressIdx());
        assertNull(tx.getId(), payment.getIsSpent());
      }
      assertTrue("Total amount is not sum of payments: " + tx.getTotalAmount() + " vs " + totalAmount + " for TX " + tx.getId(), totalAmount.compareTo(tx.getTotalAmount()) == 0);
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
