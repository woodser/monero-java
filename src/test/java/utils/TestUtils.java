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
import monero.rpc.MoneroRpcException;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTx.MoneroTxType;
import monero.wallet.model.MoneroTxCheck;
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
  public static final BigInteger MAX_FEE = BigInteger.valueOf(7500000).multiply(BigInteger.valueOf(10000));
  
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
      
      // create test wallet if necessary
      try {
        wallet.createWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW, "English");
      } catch (MoneroRpcException e) {
        assertEquals((int) -21, (int) e.getRpcCode());  // exception is ok if wallet already created
      }
      
      // open test wallet
      try {
        wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
      } catch (MoneroRpcException e) {
        assertEquals((int) -1, (int) e.getRpcCode()); // TODO (monero-wallet-rpc): -1: Failed to open wallet if wallet is already open; better code and message
      }
      
      // refresh wallet
      wallet.rescanSpent();
    }
    return wallet;
  }
  
  public static void testAccount(MoneroAccount account) {
    assertTrue(account.getIndex() >= 0);
    assertNotNull(account.getPrimaryAddress());
    assertTrue(account.getBalance().doubleValue() >= 0);
    assertTrue(account.getUnlockedBalance().doubleValue() >= 0);
    if (account.getSubaddresses() != null) {
      BigInteger balance = BigInteger.valueOf(0);
      BigInteger unlockedBalance = BigInteger.valueOf(0);
      for (int i = 0; i < account.getSubaddresses().size(); i++) {
        testSubaddress(account.getSubaddresses().get(i));
        assertEquals(account.getIndex(), account.getSubaddresses().get(i).getAccountIndex());
        assertEquals((Integer) i, account.getSubaddresses().get(i).getSubaddrIndex());
        balance = balance.add(account.getSubaddresses().get(i).getBalance());
        unlockedBalance = unlockedBalance.add(account.getSubaddresses().get(i).getUnlockedBalance());
      }
      assertEquals(account.getBalance(), balance);
      assertEquals(account.getUnlockedBalance(), unlockedBalance);
    }
  }
  
  public static void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getAccountIndex() >= 0);
    assertTrue(subaddress.getSubaddrIndex() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getBalance().doubleValue() >= 0);
    assertTrue(subaddress.getUnlockedBalance().doubleValue() >= 0);
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    if (subaddress.getBalance().doubleValue() > 0) assertTrue(subaddress.getIsUsed());
  }
  
  public static void testCommonTx(MoneroTx tx) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getId(), tx.getType());
    assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());  // default payment should be converted to null
  }
  
  /**
   * Test a single transasction.
   * 
   * @param tx is the transaction to test
   * @param hasOutgoingPayments specifies if the tx has outgoing payents, null if doesn't matter
   * @param wallet is a wallet reference to test the tx
   */
  public static void testGetTx(MoneroTx tx, Boolean hasOutgoingPayments, MoneroWallet wallet) {
    testCommonTx(tx);
    
    // test outgoing
    if (tx.getType() == MoneroTxType.OUTGOING || tx.getType() == MoneroTxType.PENDING) {
      assertNotNull(tx.getId());
      assertNotNull(tx.getId(), tx.getSrcSubaddress());
      assertNotNull(tx.getId(), tx.getSrcSubaddress().getAccountIndex());
      assertNotNull(tx.getId(), tx.getSrcSubaddress().getSubaddrIndex());
      assertNotNull(tx.getId(), tx.getSrcSubaddress().getAddress());
      assertEquals(wallet.getAddress(tx.getSrcSubaddress().getAccountIndex(), tx.getSrcSubaddress().getSubaddrIndex()), tx.getSrcSubaddress().getAddress());
      assertNotNull(tx.getId(), tx.getTotalAmount());
      assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      assertNotNull(tx.getId(), tx.getFee());
      assertNull(tx.getId(), tx.getMixin());
      assertNull(tx.getId(), tx.getSize()); // TODO (monero-wallet-rpc): add tx_size to get_transfers and get_transfer_by_txid
      assertTrue(tx.getNote() == null || !tx.getNote().isEmpty());
      assertNotNull(tx.getId(), tx.getTimestamp());
      assertEquals(tx.getId(), (Integer) 0, tx.getUnlockTime());
      assertNotNull(tx.getId(), tx.getIsDoubleSpend());
      assertFalse(tx.getId(), tx.getIsDoubleSpend());
      assertNull(tx.getId(), tx.getKey());   // TODO (monero-wallet-rpc): cannot get tx keys after sending
      assertNull(tx.getId(), tx.getBlob());
      assertNull(tx.getId(), tx.getMetadata());
      if (tx.getType() == MoneroTxType.OUTGOING) {
        assertNotNull(tx.getId(), tx.getHeight());
        assertNotNull(tx.getId(), tx.getNumConfirmations());
        assertTrue(tx.getId(), tx.getNumConfirmations() > 0);
        assertNull(tx.getId(), tx.getNumEstimatedBlocksUntilConfirmed());
      } else if (tx.getType() == MoneroTxType.PENDING) {
        assertNull(tx.getId(), tx.getHeight());
        assertEquals(tx.getId(), 0, (int) tx.getNumConfirmations());
        assertTrue(tx.getId(), tx.getNumEstimatedBlocksUntilConfirmed() > 0);
      }
      if (Boolean.TRUE.equals(hasOutgoingPayments)) assertNotNull(tx.getId(), tx.getPayments());
      else if (Boolean.FALSE.equals(hasOutgoingPayments)) assertNull(tx.getId(), tx.getPayments()); // TODO (monero-wallet-rpc): outgoing destinations only known after first confirmation
      if (tx.getPayments() != null) {
        assertFalse(tx.getId(), tx.getPayments().isEmpty());
        BigInteger totalAmount = BigInteger.valueOf(0);
        for (MoneroPayment payment : tx.getPayments()) {
          assertTrue(tx == payment.getTx());
          totalAmount = totalAmount.add(payment.getAmount());
          assertNotNull(tx.getId(), payment.getDestination().getAddress());
          assertNotNull(tx.getId(), payment.getAmount());
          assertTrue(tx.getId(), payment.getAmount().longValue() > 0);
          assertNull(tx.getId(), payment.getDestination().getAccountIndex());
          assertNull(tx.getId(), payment.getDestination().getSubaddrIndex());
          assertNull(tx.getId(), payment.getIsSpent());
          assertNull(tx.getId(), payment.getKeyImage());
        }
        // TODO: incoming_transfers d59fe775249465621d7736b53c0cb488e03e4da8dae647a13492ea51d7685c62 totalAmount is 0?
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
      assertNull(tx.getId(), tx.getSrcSubaddress());
      assertNotNull(tx.getId(), tx.getTotalAmount());
      assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      assertNull(tx.getId(), tx.getMixin());
      assertNull(tx.getId(), tx.getSize());  // TODO (monero-wallet-rpc): add tx_size to "in" get_transfers and incoming_transfers
      assertNull(tx.getId(), tx.getNote());
      if (tx.getFee() == null) LOGGER.warn("Incoming transaction is missing fee: " + tx.getId());    // TODO (monero-wallet-rpc): incoming txs are occluded by outgoing counterparts from same account ()https://github.com/monero-project/monero/issues/4500) and then incoming_transfers need address, fee, timestamp, unlock_time, is_double_spend, height, tx_size 
      if (tx.getTimestamp() == null) LOGGER.warn("Incoming transaction is missing timestamp: " + tx.getId());
      if (tx.getUnlockTime() == null) LOGGER.warn("Incoming transaction is missing unlock_time: " + tx.getId());
      if (tx.getIsDoubleSpend() == null) LOGGER.warn("Incoming transaction is missing is_double_spend: " + tx.getId());
      if (tx.getIsDoubleSpend() != null) assertFalse(tx.getId(), tx.getIsDoubleSpend());
      if (tx.getType() == MoneroTxType.INCOMING) {
        assertNull(tx.getId(), tx.getKey());
        if (tx.getHeight() == null) LOGGER.warn("Incoming transaction is missing height: " + tx.getId());
        if (tx.getNumConfirmations() == null) LOGGER.warn("Incoming transaction is missing confirmations: " + tx.getId());
        else assertTrue(tx.getId(), tx.getNumConfirmations() > 0);
        assertNull(tx.getId(), tx.getNumEstimatedBlocksUntilConfirmed());
      } else {
        assertNull(tx.getId(), tx.getKey());
        assertNull(tx.getId(), tx.getHeight());
        assertEquals(tx.getId(), 0, (int) tx.getNumConfirmations());
        assertTrue(tx.getId(), tx.getNumEstimatedBlocksUntilConfirmed() > 0);
      }
      assertNull(tx.getId(), tx.getBlob());
      assertNull(tx.getId(), tx.getMetadata());
      assertNotNull(tx.getId(), tx.getPayments());
      assertFalse(tx.getId(), tx.getPayments().isEmpty());
      BigInteger totalAmount = BigInteger.valueOf(0);
      for (MoneroPayment payment : tx.getPayments()) {
        assertTrue(tx == payment.getTx());
        totalAmount = totalAmount.add(payment.getAmount());
        MoneroSubaddress subaddress = payment.getDestination();
        assertNotNull(tx.getId(), subaddress);
        assertNotNull(tx.getId(), subaddress.getAccountIndex());
        assertNotNull(tx.getId(), subaddress.getSubaddrIndex());
        assertNotNull(tx.getId(), subaddress.getAddress());
        assertEquals(tx.getId(), wallet.getAddress(subaddress.getAccountIndex(), subaddress.getSubaddrIndex()), subaddress.getAddress());
        assertNotNull(tx.getId(), payment.getAmount());
        assertTrue(tx.getId(), payment.getAmount().longValue() > 0);
        assertNotNull(tx.getId(), payment.getIsSpent());
        if (tx.getType() == MoneroTxType.INCOMING) assertNotNull(tx.getId(), payment.getKeyImage());
        else assertNull(tx.getId(), payment.getKeyImage()); // TODO (monero-wallet-rpc): mempool transactions do not have key_images
      }
      assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
    }
    
    // test failed
    if (tx.getType() == MoneroTxType.FAILED) {
      assertNotNull(tx.getId());
      MoneroSubaddress srcSubaddress = tx.getSrcSubaddress();
      assertNotNull(tx.getId(), srcSubaddress);
      assertNotNull(tx.getId(), srcSubaddress.getAccountIndex());
      assertNotNull(tx.getId(), srcSubaddress.getSubaddrIndex());
      assertNotNull(tx.getId(), srcSubaddress.getAddress());
      assertEquals(tx.getId(), wallet.getAddress(srcSubaddress.getAccountIndex(), srcSubaddress.getSubaddrIndex()), srcSubaddress.getAddress());
      assertNotNull(tx.getId(), tx.getTotalAmount());
      assertNotEquals(tx.getId(), MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId());
      assertNotNull(tx.getId(), tx.getFee());
      assertNull(tx.getId(), tx.getMixin());
      assertNull(tx.getId(), tx.getSize()); // TODO (monero-wallet-rpc): add tx_size to get_transfers and get_transfer_by_txid
      assertNull(tx.getId(), tx.getNote());
      assertNotNull(tx.getId(), tx.getTimestamp());
      assertEquals(tx.getId(), (Integer) 0, tx.getUnlockTime());
      assertNotNull(tx.getId(), tx.getIsDoubleSpend());
      assertFalse(tx.getId(), tx.getIsDoubleSpend());
      assertNull(tx.getId(), tx.getKey());
      assertNull(tx.getId(), tx.getBlob());
      assertNull(tx.getId(), tx.getMetadata());
      assertNull(tx.getId(), tx.getHeight());
      assertNotNull(tx.getId(), tx.getNumConfirmations());
      assertEquals(tx.getId(), 0, (int) tx.getNumConfirmations());
      assertNull(tx.getId(), tx.getNumEstimatedBlocksUntilConfirmed());
      assertNull(tx.getId(), tx.getPayments());
    }
  }
  
  public static void testSendTx(MoneroTx tx, MoneroTxConfig config, boolean hasKey, boolean hasPayments, MoneroWallet wallet) {
    testCommonTx(tx);
    assertNotNull(tx.getId());
    assertEquals(tx.getId(), MoneroTxType.PENDING, tx.getType());
    MoneroSubaddress srcSubaddress = tx.getSrcSubaddress();
    assertNotNull(tx.getId(), srcSubaddress);
    assertNotNull(tx.getId(), srcSubaddress.getAccountIndex());
    assertNotNull(tx.getId(), srcSubaddress.getSubaddrIndex());
    assertEquals(tx.getId(), 0, (int) srcSubaddress.getSubaddrIndex());
    assertNotNull(tx.getId(), srcSubaddress.getAddress());
    assertEquals(tx.getId(), wallet.getAddress(srcSubaddress.getAccountIndex(), srcSubaddress.getSubaddrIndex()), srcSubaddress.getAddress());
    assertNotNull(tx.getId(), tx.getTotalAmount());
    if (MoneroTx.DEFAULT_PAYMENT_ID.equals(config.getPaymentId())) assertNull(tx.getId(), tx.getPaymentId());
    else assertEquals(tx.getId(), config.getPaymentId(), tx.getPaymentId());
    assertNotNull(tx.getId(), tx.getFee());
    assertEquals(tx.getId(), config.getMixin(), tx.getMixin());
    assertNull(tx.getId(), tx.getSize());
    assertNotEquals(tx.getId(), "", tx.getNote());  // notes can be non-zero length or null
    assertNotNull(tx.getId(), tx.getTimestamp());
    assertEquals(tx.getId(), (Integer) 0, tx.getUnlockTime());
    assertNotNull(tx.getId(), tx.getIsDoubleSpend());
    assertFalse(tx.getId(), tx.getIsDoubleSpend());
    if (hasKey) assertNotNull(tx.getKey());
    else assertNull(tx.getKey());
    if (hasPayments) {
      assertNotNull(tx.getPayments());
      assertFalse(tx.getPayments().isEmpty());
      assertEquals(tx.getId(), config.getPayments(), tx.getPayments());
    }
    assertNotNull(tx.getId(), tx.getBlob());
    assertNotNull(tx.getId(), tx.getMetadata());
    assertNull(tx.getId(), tx.getHeight());
    if (tx.getPayments() != null) {
      BigInteger totalAmount = BigInteger.valueOf(0);
      for (MoneroPayment payment : tx.getPayments()) {
        assertTrue(tx.getId(), tx == payment.getTx());
        totalAmount = totalAmount.add(payment.getAmount());
        MoneroSubaddress subaddress = payment.getDestination();
        assertNotNull(tx.getId(), subaddress);
        assertNotNull(tx.getId(), subaddress.getAddress());
        assertNull(tx.getId(), subaddress.getAccountIndex());
        assertNull(tx.getId(), subaddress.getSubaddrIndex());
        assertNotNull(tx.getId(), payment.getAmount());
        assertTrue(tx.getId(), payment.getAmount().longValue() > 0);
        assertNull(tx.getId(), payment.getIsSpent());
        assertNull(tx.getId(), payment.getKeyImage());
      }
      assertTrue("Total amount is not sum of payments: " + tx.getTotalAmount() + " vs " + totalAmount + " for TX " + tx.getId(), totalAmount.compareTo(tx.getTotalAmount()) == 0);
    }
  }
  
  public static void testSendTxDoNotRelay(MoneroTx tx, MoneroTxConfig config, boolean hasKey, boolean hasPayments, MoneroWallet wallet) {
    testCommonTx(tx);
    assertNotNull(tx.getId());
    assertEquals(tx.getId(), MoneroTxType.NOT_RELAYED, tx.getType());
    MoneroSubaddress srcSubaddress = tx.getSrcSubaddress();
    assertNotNull(tx.getId(), srcSubaddress);
    assertNotNull(tx.getId(), srcSubaddress.getAccountIndex());
    assertNotNull(tx.getId(), srcSubaddress.getSubaddrIndex());
    assertEquals(tx.getId(), 0, (int) srcSubaddress.getSubaddrIndex());
    assertNotNull(tx.getId(), srcSubaddress.getAddress());
    assertEquals(tx.getId(), wallet.getAddress(srcSubaddress.getAccountIndex(), srcSubaddress.getSubaddrIndex()), srcSubaddress.getAddress());
    assertEquals(tx.getId(), config.getAccountIndex(), tx.getSrcSubaddress().getAccountIndex());
    assertNotNull(tx.getId(), tx.getTotalAmount());
    if (MoneroTx.DEFAULT_PAYMENT_ID.equals(config.getPaymentId())) assertNull(tx.getId(), tx.getPaymentId());
    else assertEquals(tx.getId(), config.getPaymentId(), tx.getPaymentId());
    assertNotNull(tx.getId(), tx.getFee());
    assertEquals(tx.getId(), config.getMixin(), tx.getMixin());
    assertNull(tx.getId(), tx.getSize());
    assertNull(tx.getId(), tx.getNote());
    assertNull(tx.getId(), tx.getTimestamp());
    assertNull(tx.getId(), tx.getUnlockTime());
    assertNull(tx.getId(), tx.getIsDoubleSpend());
    if (hasKey) assertNotNull(tx.getKey());
    else assertNull(tx.getKey());
    if (hasPayments) {
      assertNotNull(tx.getPayments());
      assertFalse(tx.getPayments().isEmpty());
      assertEquals(tx.getId(), config.getPayments(), tx.getPayments());
    }
    assertNotNull(tx.getId(), tx.getBlob());
    assertNotNull(tx.getId(), tx.getMetadata());
    assertNull(tx.getId(), tx.getHeight());
    if (tx.getPayments() != null) {
      BigInteger totalAmount = BigInteger.valueOf(0);
      for (MoneroPayment payment : tx.getPayments()) {
        assertTrue(tx.getId(), tx == payment.getTx());
        totalAmount = totalAmount.add(payment.getAmount());
        MoneroSubaddress subaddress = payment.getDestination();
        assertNotNull(tx.getId(), subaddress);
        assertNotNull(tx.getId(), subaddress.getAddress());
        assertNull(tx.getId(), subaddress.getAccountIndex());
        assertNull(tx.getId(), subaddress.getSubaddrIndex());
        assertNotNull(tx.getId(), payment.getAmount());
        assertTrue(tx.getId(), payment.getAmount().longValue() > 0);
        assertNull(tx.getId(), payment.getIsSpent());
        assertNull(tx.getId(), payment.getKeyImage());
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
  
  public static void testTxCheck(MoneroTx tx, MoneroTxCheck check) {
    assertNotNull(tx.getId(), check.getNumConfirmations());
    assertNotNull(tx.getId(), check.getIsInPool());
    assertNotNull(tx.getId(), check.getAmountReceived());
    assertTrue(tx.getId(), check.getAmountReceived().compareTo(BigInteger.valueOf(0)) >= 0);
    if (check.getIsInPool()) assertEquals(tx.getId(), 0, (int) check.getNumConfirmations());
    else assertTrue(tx.getId(), check.getNumConfirmations() > 0);
  }
}
