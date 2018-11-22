package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.rpc.MoneroRpcException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImage;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTx.MoneroTxType;
import monero.wallet.model.MoneroTxFilter;
import monero.wallet.model.MoneroUri;
import utils.TestUtils;

/**
 * Tests a Monero wallet excluding sending transactions.
 * 
 * TODO: incoming_transfers d59fe775249465621d7736b53c0cb488e03e4da8dae647a13492ea51d7685c62 totalAmount is 0?
 */
public class TestMoneroWallet {
  
  private static MoneroWallet wallet;
  private static final String SAMPLE_ADDRESS = "58bf9MfrBNDXSqCzK6snxSXaJHehLTnvx3BdS6qMkYAsW8P5kvRVq8ePbGQ7mfAeYfC7QELPhpQBe2C9bqCrqeesUsifaWw";
  private static List<MoneroTx> txCache;
  private static Integer MAX_TX_PROOFS = 25;   // maximum number of transactions to check for each proof, null to check all

  @BeforeClass
  public static void setup() throws Exception {
    wallet = TestUtils.getWallet();
    txCache = null;
  }
  
  @AfterClass
  public static void teardown() {
    //PrintBalances.printBalances();
  }

  @Test
  public void testGetHeight() {
    int height = wallet.getHeight();
    assertTrue(height >= 0);
  }

  @Test
  public void testGetMnemonicSeed() {
    String seed = wallet.getMnemonicSeed();
    MoneroUtils.validateMnemonicSeed(seed);
  }

  @Test
  public void testGetViewKey() {
    String viewKey = wallet.getViewKey();
    MoneroUtils.validateViewKey(viewKey);
  }

  @Test
  public void testGetIntegratedAddress() {
    
    // save address for later comparison
    String address = wallet.getSubaddress(0, 0).getAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    assertEquals(address, integratedAddress.getStandardAddress());
    assertEquals(paymentId, integratedAddress.getPaymentId());
    
    // test invalid payment id
    try {
      String invalidPaymentId = "invalid_payment_id_123456";
      integratedAddress = wallet.getIntegratedAddress(invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown a RPC exception");
    } catch (MoneroRpcException e) {
      assertEquals((int) -5, (int) e.getRpcCode());
      assertEquals("Invalid payment ID", e.getRpcMessage());
    }
    
    // test null payment id which generates a new one
    integratedAddress = wallet.getIntegratedAddress(null);
    assertEquals(address, integratedAddress.getStandardAddress());
    assertNotNull(integratedAddress.getPaymentId());
  }
  
  @Test
  public void testGetPrimaryAddress() {
    String primaryAddress = wallet.getPrimaryAddress();
    assertEquals(wallet.getSubaddress(0, 0).getAddress(), primaryAddress);
  }
  
  @Test
  public void testDecodeIntegratedAddress() {
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress("03284e41c342f036");
    MoneroIntegratedAddress decodedAddress = wallet.decodeIntegratedAddress(integratedAddress.toString());
    assertEquals(integratedAddress, decodedAddress);
  }

  @Test
  public void testGetAccounts() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      assertNull(account.getSubaddresses());
      TestUtils.testAccount(account);
    }
  }
  
  @Test
  public void testGetAccountsWithSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      assertNotNull(account.getSubaddresses());
      assertFalse(account.getSubaddresses().isEmpty());
      TestUtils.testAccount(account);
    }
  }

  @Test
  public void testGetAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      MoneroAccount retrieved = wallet.getAccount(account.getIndex(), false);
      assertNull(retrieved.getSubaddresses());
      TestUtils.testAccount(retrieved);
    }
  }
  
  @Test
  public void testGetAccountWithSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      MoneroAccount retrieved = wallet.getAccount(account.getIndex(), true);
      assertNotNull(retrieved.getSubaddresses());
      assertFalse(retrieved.getSubaddresses().isEmpty());
      TestUtils.testAccount(retrieved);
    }
  }

  @Test
  public void testGetSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        TestUtils.testSubaddress(subaddress);
      }
    }
  }

  @Test
  public void testGetSubaddressesByIndices() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      Collection<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroSubaddress subaddress : subaddresses) {
        subaddressIndices.add(subaddress.getSubaddrIndex());
      }
      assertEquals(subaddresses, wallet.getSubaddresses(account.getIndex(), subaddressIndices));
    }
  }
  
  @Test
  public void testGetSubaddressByIndex() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (int i = 0; i < subaddresses.size(); i++) {
        MoneroSubaddress subaddress = wallet.getSubaddress(account.getIndex(), subaddresses.get(i).getSubaddrIndex());
        assertEquals(subaddresses.get(i), subaddress);
      }
    }
  }
  
  @Test
  public void testGetAddress() {
    assertEquals(wallet.getPrimaryAddress(), wallet.getSubaddress(0, 0).getAddress());
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : wallet.getSubaddresses(account.getIndex())) {
        assertEquals(subaddress.getAddress(), wallet.getAddress(account.getIndex(), subaddress.getSubaddrIndex()));
      }
    }
  }
  
  @Test
  public void testGetBalance() {
    BigInteger balanceWallet = wallet.getBalance();
    assertTrue(balanceWallet.compareTo(BigInteger.valueOf(0)) > 0); // wallet must have balance
    BigInteger balanceAccounts = BigInteger.valueOf(0);
    for (MoneroAccount account : wallet.getAccounts()) {
      balanceAccounts = balanceAccounts.add(account.getBalance());
    }
    assertTrue(balanceWallet.compareTo(balanceAccounts) == 0);
  }
  
  @Test
  public void testGetUnlockedBalance() {
    BigInteger unlockedBalanceWallet = wallet.getUnlockedBalance();
    BigInteger unlockedBalanceAccounts = BigInteger.valueOf(0);
    for (MoneroAccount account : wallet.getAccounts()) {
      unlockedBalanceAccounts = unlockedBalanceAccounts.add(account.getUnlockedBalance());
    }
    assertTrue(unlockedBalanceWallet.compareTo(unlockedBalanceAccounts) == 0);
  }
  
  @Test
  public void testMultisigImportNeeded() {
    assertFalse(wallet.isMultisigImportNeeded());
  }
  
  @Test
  public void testGetTxsWallet() {
    boolean nonDefaultIncoming = false;
    List<MoneroTx> txs1 = getCachedTxs();
    List<MoneroTx> txs2 = wallet.getTxs();  // fetch transactions twice to ensure results are the same
    assertEquals(txs1.size(), txs2.size());
    for (int i = 0; i < txs1.size(); i++) {
      TestUtils.testGetTx(txs1.get(i), null, wallet);
      TestUtils.testGetTx(txs2.get(i), null, wallet);
      assertEquals(txs1.get(i), txs2.get(i));
      if (!MoneroUtils.isOutgoing(txs1.get(i).getType())) {
        for (MoneroPayment payment : txs1.get(i).getPayments()) {
         if (payment.getAccountIndex() != 0 && payment.getSubaddrIndex() != 0) nonDefaultIncoming = true;
        }
      }
    }
    assertTrue("No incoming transactions found in non-default account and subaddress; run testSendToMultiple() first", nonDefaultIncoming);
  }
  
  @Test
  public void testGetTxsAccount() {
    boolean nonDefaultIncoming = false;
    for (MoneroAccount account : wallet.getAccounts()) {
      List<MoneroTx> txs = wallet.getTxs(account.getIndex());
      for (MoneroTx tx : txs) {
        TestUtils.testGetTx(tx, null, wallet);
        if (MoneroUtils.isOutgoing(tx.getType())) {
          assertEquals(account.getIndex(), tx.getSrcAccountIndex());
        } else {
          for (MoneroPayment payment : tx.getPayments()) {
            assertEquals(account.getIndex(), payment.getAccountIndex());
            if (payment.getAccountIndex() != 0 && payment.getSubaddrIndex() != 0) nonDefaultIncoming = true;
          }
        }
      }
    }
    assertTrue("No incoming transactions found in non-default account and subaddress; run testSendToMultiple() first", nonDefaultIncoming);
  }
  
  @Test
  public void testGetTxsSubaddress() {
    boolean nonDefaultIncoming = false;
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    for (int accountIdx = 0; accountIdx < Math.min(accounts.size(), 3); accountIdx++) {
      for (int subaddressIdx = 0; subaddressIdx < Math.min(accounts.get(accountIdx).getSubaddresses().size(), 5); subaddressIdx++) {
        for (MoneroTx tx : wallet.getTxs(accountIdx, subaddressIdx)) {
          TestUtils.testGetTx(tx, null, wallet);
          if (MoneroUtils.isOutgoing(tx.getType()))  {
            assertEquals(accountIdx, (int) tx.getSrcAccountIndex());
          } else {
            for (MoneroPayment payment : tx.getPayments()) {
              assertEquals(accountIdx, (int) payment.getAccountIndex());
              assertEquals(subaddressIdx, (int) payment.getSubaddrIndex());
              if (payment.getAccountIndex() != 0 && payment.getSubaddrIndex() != 0) nonDefaultIncoming = true;
            }
          }
        }
      }
    }
    assertTrue("No incoming transactions found in non-default account and subaddress; run testSendToMultiple() first", nonDefaultIncoming);
  }
  
  @Test
  public void testGetTxsHasPayments() {
    
    // filter on having payments
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setAccountIndex(0);
    filter.setHasPayments(true);
    List<MoneroTx> txs = wallet.getTxs(filter);
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : wallet.getTxs(filter)) {
      assertNotNull(tx.getPayments());
      assertFalse(tx.getPayments().isEmpty());
    }
    
    // filter on not having payments
    filter.setHasPayments(false);
    txs = wallet.getTxs(filter);
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : wallet.getTxs(filter)) {
      assertNull(tx.getPayments());
    }
    
    // filter on no preference
    filter.setHasPayments(null);
    txs = wallet.getTxs(filter);
    boolean foundPayments = false;
    boolean foundNoPayments = false;
    for (MoneroTx tx : wallet.getTxs(filter)) {
      if (tx.getPayments() != null && !tx.getPayments().isEmpty()) foundPayments = true;
      if (tx.getPayments() == null) foundNoPayments = true;
    }
    assertTrue(foundPayments);
    assertTrue(foundNoPayments);
  }
  
  @Test
  public void testGetTxsIncomingSumToBalance() {
    
    // test each account balance
    for (MoneroAccount account : wallet.getAccounts()) {
      
      // get transactions
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setAccountIndex(account.getIndex());
      List<MoneroTx> txs = wallet.getTxs(filter);
      if (account.getIndex() == 0) assertFalse(txs.isEmpty());
      
      // sum balances of incoming payments and pending deductions
      BigInteger incomingBalance = BigInteger.valueOf(0);
      BigInteger pendingBalance = BigInteger.valueOf(0);
      BigInteger mempoolBalance = BigInteger.valueOf(0);
      for (MoneroTx tx : txs) {
        if (tx.getType() == MoneroTxType.PENDING) {
          pendingBalance = pendingBalance.add(tx.getTotalAmount()); // TODO: test pending balance
        }
        if (!MoneroUtils.isOutgoing(tx.getType())) {
          assertFalse(tx.getPayments().isEmpty());
          for (MoneroPayment payment : tx.getPayments()) {
            if (!payment.getIsSpent()) {
              if (MoneroUtils.isConfirmed(tx.getType())) incomingBalance = incomingBalance.add(payment.getAmount());
              else mempoolBalance = mempoolBalance.add(payment.getAmount());
            }
          }
        }
      }
      
      // wallet balance must equal sum of unspent incoming txs
      BigInteger walletBalance = wallet.getAccount(account.getIndex()).getBalance();
      BigInteger expectedBalance = incomingBalance;  // TODO (monero-wallet-rpc): unconfirmed balance may not add up because of https://github.com/monero-project/monero/issues/4500
//      System.out.println("Wallet    : " + walletBalance);
//      System.out.println("Incoming  : " + incomingBalance);
//      System.out.println("Pending   : " + pendingBalance);
//      System.out.println("Mempool   : " + mempoolBalance);
//      System.out.println("Expected  : " + expectedBalance);
      assertEquals("Account " + account.getIndex() + " balance does not add up", walletBalance, expectedBalance);
    }
  }
  
  @Test
  public void testGetTxsByIds() {
    
    // get random transactions
    List<MoneroTx> txs = getRandomTransactions(null, 1, 5);
    
    // fetch transactions by id
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) {
      txIds.add(tx.getId());
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setTxIds(Arrays.asList(tx.getId()));
      List<MoneroTx> filteredTxs = wallet.getTxs(filter);
      assertFalse(filteredTxs.isEmpty());
      for (MoneroTx filteredTx : filteredTxs) {
        assertEquals(tx.getId(), filteredTx.getId());
      }
    }
    
    // fetch transactions by ids
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setTxIds(txIds);
    List<MoneroTx> filteredTxs = wallet.getTxs(filter);
    assertFalse(filteredTxs.isEmpty());
    for (MoneroTx filteredTx : filteredTxs) {
      assertTrue(txIds.contains(filteredTx.getId()));
    }
    for (String txId : txIds) {
      boolean found = false;
      for (MoneroTx filteredTx : filteredTxs) if (filteredTx.getId().equals(txId)) found = true;
      assertTrue("No transaction with id " + txId + " fetched", found);
    }
  }

  // TODO: break this test up
  @Test
  public void testGetTxsMoneroTxFilter() {
    
    // get all transactions for reference
    List<MoneroTx> allTxs = getCachedTxs();
    assertFalse(allTxs.isEmpty());
    for (MoneroTx tx : allTxs) {
      TestUtils.testGetTx(tx, null, wallet);
    }
    
    // test getting transactions by payment ids
    // TODO: this test is very slow, optimize
    Collection<String> paymentIds = new HashSet<String>();
    for (MoneroTx tx : allTxs) paymentIds.add(tx.getPaymentId());
    assertFalse(paymentIds.isEmpty());
    for (String paymentId : paymentIds) {
      Collection<String> filterPaymentIds = new HashSet<String>();
      filterPaymentIds.add(paymentId);
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setPaymentIds(filterPaymentIds);
      List<MoneroTx> txs = wallet.getTxs(filter);
      assertFalse(txs.isEmpty());
      for (MoneroTx tx : txs) {
        TestUtils.testGetTx(tx, null, wallet);
        assertTrue(filter.getPaymentIds().contains(tx.getPaymentId()));
      }
    }
    
    // test getting incoming transactions
    List<MoneroTx> txs = wallet.getTxs(new MoneroTxFilter(true, false, false, false, false, null, null, null, null, null, null, null));
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertTrue(tx.getType() == MoneroTxType.INCOMING || tx.getType() == MoneroTxType.BLOCK);
    }
    
    // test getting outgoing transactions
    txs = wallet.getTxs(new MoneroTxFilter(false, false, true, false, false, null, null, null, null, null, null, null));
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertEquals(MoneroTxType.OUTGOING, tx.getType());
    }
    
    // test block height filtering
    {
      txs = wallet.getTxs(0);
      assertFalse("No transactions; run testSendToMultiple()", txs.isEmpty());
        
      // get and sort block heights in ascending order
      List<Integer> heights = new ArrayList<Integer>();
      for (MoneroTx tx : txs) {
        if (tx.getHeight() != null) heights.add(tx.getHeight());
      }
      Collections.sort(heights);
      
      // pick minimum and maximum heights for filtering
      int minHeight = -1;
      int maxHeight = -1;
      if (heights.size() == 1) {
        minHeight = 0;
        maxHeight = heights.get(0) - 1;
      } else {
        minHeight = heights.get(0) + 1;
        maxHeight = heights.get(heights.size() - 1) - 1;
      }
      
      // assert some transactions filtered
      int unfilteredCount = txs.size();
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setAccountIndex(0);
      filter.setMinHeight(minHeight);
      filter.setMaxHeight(maxHeight);
      txs = wallet.getTxs(filter);
      assertFalse(txs.isEmpty());
      assertTrue(txs.size() < unfilteredCount);
      for (MoneroTx tx : txs) {
        assertTrue(tx.getHeight() >= minHeight && tx.getHeight() <= maxHeight);
      }
    }
    
    // get all subaddresses with balances
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0 || subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) {
          subaddresses.add(subaddress);
        }
      }
    }
    
    // test that unspent tx payments add up to balance
    for (MoneroSubaddress subaddress : subaddresses) {
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setAccountIndex(subaddress.getAccountIndex());
      filter.setSubaddressIndices(Arrays.asList(subaddress.getSubaddrIndex()));
      txs = wallet.getTxs(filter);
      
      // test that unspent tx payments add up to subaddress balance
      BigInteger balance = BigInteger.valueOf(0);
      for (MoneroTx tx : txs) {
        if (MoneroUtils.isIncoming(tx.getType()) && MoneroUtils.isConfirmed(tx.getType())) {
          for (MoneroPayment payment : tx.getPayments()) {
            if (!payment.getIsSpent()) {
              balance = balance.add(payment.getAmount());
            }
          }
        }
      }
      assertEquals(wallet.getSubaddress(subaddress.getAccountIndex(), subaddress.getSubaddrIndex()).getBalance(), balance); // TODO: (monero-wallet-rpc): fails after send tests
    }
    
    // assert that ummet filter criteria has no results
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setAccountIndex(0);
    Collection<Integer> subaddressIndices = new HashSet<Integer>();
    subaddressIndices.add(1234907);
    filter.setSubaddressIndices(subaddressIndices);
    txs = wallet.getTxs(filter);
    assertTrue(txs.isEmpty());
  }
  
  @Test
  public void testGetTxNote() {
    List<MoneroTx> txs = getRandomTransactions(null, 1, 5);
    
    // set notes
    for (int i = 0; i < txs.size(); i++) {
      wallet.setTxNote(txs.get(i).getId(), "Hello " + i);
    }
    
    // get notes
    for (int i = 0; i < txs.size(); i++) {
      assertEquals("Hello " + i, wallet.getTxNote(txs.get(i).getId()));
    }
  }

  @Test
  public void testGetTxNotes() {
    List<MoneroTx> txs = getCachedTxs();
    assertFalse(txs.isEmpty());
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) {
      TestUtils.testGetTx(tx, null, wallet);
      txIds.add(tx.getId());
    }
    List<String> txNotes = wallet.getTxNotes(txIds);
    assertEquals(txs.size(), txNotes.size());
    for (String txNote : txNotes) assertNotNull(txNote);
  }
  
  
  @Test
  public void testSetTxNote() {
    fail("Not implemented");
  }
  
  @Test
  public void testSetTxNotes() {
    
    // set tx notes
    List<MoneroTx> txs = getCachedTxs();
    assertTrue(txs.size() >= 3);
    List<String> txIds = new ArrayList<String>();
    List<String> txNotes = new ArrayList<String>();
    for (int i = 0; i < txIds.size(); i++) {
      txIds.add(txs.get(i).getId());
      txNotes.add("Hello " + i);
    }
    wallet.setTxNotes(txIds, txNotes);
    
    // get tx notes
    txNotes = wallet.getTxNotes(txIds);
    for (int i = 0; i < txIds.size(); i++) {
      assertEquals("Hello " + i, txNotes.get(i));
    }
    
    // TODO: test that get transaction has note
  }
  
  
  @Test
  public void testTxKeyProof() {
    
    // get random txs with outgoing payments
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setIncoming(false);
    filter.setMempool(false);
    filter.setFailed(false);
    filter.setHasPayments(true);
    List<MoneroTx> txs = getRandomTransactions(filter, 1, MAX_TX_PROOFS);
    
    // test good checks
    for (MoneroTx tx : txs) {
      String key = wallet.getTxKey(tx.getId());
      for (MoneroPayment payment : tx.getPayments()) {
        MoneroCheckTx check = wallet.checkTxKey(tx.getId(), key, payment.getAddress());
        assertTrue(check.getIsGood());
        if (payment.getAmount().compareTo(BigInteger.valueOf(0)) > 0) {
//        assertTrue(check.getAmountReceived().compareTo(BigInteger.valueOf(0)) > 0); // TODO (monero-wallet-rpc): indicates amount received amount is 0 despite transaction with payment to this address
          if (check.getAmountReceived().compareTo(BigInteger.valueOf(0)) == 0) {
            TestUtils.LOGGER.warn("Key proof indicates no funds received despite payment (txid=" + tx.getId() + ", key=" + key + ", address=" + payment.getAddress() + ", amount=" + payment.getAmount() + ")");
          }
        }
        else assertTrue(check.getAmountReceived().compareTo(BigInteger.valueOf(0)) == 0);
        TestUtils.testCheckTx(tx, check);
      }
    }
    
    // test get tx key with invalid id
    try {
      wallet.getTxKey("invalid_tx_id");
      fail("Should throw exception for invalid key");
    } catch (MoneroRpcException e) {
      assertEquals(-8, (int) e.getRpcCode());
    }
    
    // test check with invalid tx id
    MoneroTx tx = txs.get(0);
    String key = wallet.getTxKey(tx.getId());
    try {
      wallet.checkTxKey("invalid_tx_id", key, tx.getPayments().get(0).getAddress());
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-8, (int) e.getRpcCode());
    }
    
    // test check with invalid key
    try {
      wallet.checkTxKey(tx.getId(), "invalid_tx_key", tx.getPayments().get(0).getAddress());
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-25, (int) e.getRpcCode());
    }
    
    // test check with invalid address
    try {
      wallet.checkTxKey(tx.getId(), key, "invalid_tx_address");
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-2, (int) e.getRpcCode());
    }
    
    // test check with different address
    String differentAddress = null;
    for (MoneroTx aTx : getCachedTxs()) {
      if (aTx.getPayments() == null) continue;
      for (MoneroPayment aPayment : aTx.getPayments()) {
        if (!aPayment.getAddress().equals(tx.getPayments().get(0).getAddress())) {
          differentAddress = aPayment.getAddress();
          break;
        }
      }
    }
    assertNotNull("Could not get a different address to test", differentAddress);
    MoneroCheckTx check = wallet.checkTxKey(tx.getId(), key, differentAddress);
    assertTrue(check.getIsGood());
    assertTrue(check.getAmountReceived().compareTo(BigInteger.valueOf(0)) >= 0);
    TestUtils.testCheckTx(tx, check);
  }
  
  @Test
  public void testTxProof() {
    
    // get random txs with outgoing payments
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setIncoming(false);
    filter.setMempool(false);
    filter.setFailed(false);
    filter.setHasPayments(true);
    List<MoneroTx> txs = getRandomTransactions(filter, 2, MAX_TX_PROOFS);
    
    // test good checks with messages
    for (MoneroTx tx : txs) {
      for (MoneroPayment payment : tx.getPayments()) {
        String signature = wallet.getTxProof(tx.getId(), payment.getAddress(), "This transaction definitely happened.");
        MoneroCheckTx check = wallet.checkTxProof(tx.getId(), payment.getAddress(), "This transaction definitely happened.", signature);
        TestUtils.testCheckTx(tx, check);
      }
    }
    
    // test good check without message
    MoneroTx tx = txs.get(0);
    String signature = wallet.getTxProof(tx.getId(), tx.getPayments().get(0).getAddress(), null);
    MoneroCheckTx check = wallet.checkTxProof(tx.getId(), tx.getPayments().get(0).getAddress(), null, signature);
    TestUtils.testCheckTx(tx, check);
    
    // test get proof with invalid id
    try {
      wallet.getTxProof("invalid_tx_id", tx.getPayments().get(0).getAddress(), null);
      fail("Should throw exception for invalid key");
    } catch (MoneroRpcException e) {
      assertEquals(-8, (int) e.getRpcCode());
    }
    
    // test check with invalid tx id
    try {
      wallet.checkTxProof("invalid_tx_id", tx.getPayments().get(0).getAddress(), null, signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-8, (int) e.getRpcCode());
    }
    
    // test check with invalid address
    try {
      wallet.checkTxProof(tx.getId(), "invalid_tx_address", null, signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-2, (int) e.getRpcCode());
    }
    
    // test check with wrong message
    signature = wallet.getTxProof(tx.getId(), tx.getPayments().get(0).getAddress(), "This is the right message");
    check = wallet.checkTxProof(tx.getId(), tx.getPayments().get(0).getAddress(), "This is the wrong message", signature);
    assertFalse(check.getIsGood());
    TestUtils.testCheckTx(tx, check);
    
    // test check with wrong signature
    String wrongSignature = wallet.getTxProof(txs.get(1).getId(), txs.get(1).getPayments().get(0).getAddress(), "This is the right message");
    try {
      check = wallet.checkTxProof(tx.getId(), tx.getPayments().get(0).getAddress(), "This is the right message", wrongSignature);  
      assertFalse(check.getIsGood());
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode()); // TODO: sometimes comes back bad, sometimes throws exception.  ensure txs come from different addresses?
    }
  }
  
  @Test
  public void testSpendProof() {
    
    // get random outgoing txs
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setIncoming(false);
    filter.setMempool(false);
    filter.setFailed(false);
    List<MoneroTx> txs = getRandomTransactions(filter, 2, MAX_TX_PROOFS);
    
    // test good checks with messages
    for (MoneroTx tx : txs) {
      String signature = wallet.getSpendProof(tx.getId(), "I am a message.");
      assertTrue(wallet.checkSpendProof(tx.getId(), "I am a message.", signature));
    }
    
    // test good check without message
    MoneroTx tx = txs.get(0);
    String signature = wallet.getSpendProof(tx.getId(), null);
    assertTrue(wallet.checkSpendProof(tx.getId(), null, signature));
    
    // test get proof with invalid id
    try {
      wallet.getSpendProof("invalid_tx_id", null);
      fail("Should throw exception for invalid key");
    } catch (MoneroRpcException e) {
      assertEquals(-8, (int) e.getRpcCode());
    }
    
    // test check with invalid tx id
    try {
      wallet.checkSpendProof("invalid_tx_id", null, signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-8, (int) e.getRpcCode());
    }
    
    // test check with invalid message
    signature = wallet.getSpendProof(tx.getId(), "This is the right message");
    assertFalse(wallet.checkSpendProof(tx.getId(), "This is the wrong message", signature));
    
    // test check with wrong signature
    signature = wallet.getSpendProof(txs.get(1).getId(), "This is the right message");
    assertFalse(wallet.checkSpendProof(tx.getId(), "This is the right message", signature));
  }
  
  @Test
  public void testReserveProofWallet() {
    
    // get proof of entire wallet
    String signature = wallet.getReserveProof("Test message");
    
    // check proof of entire wallet
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", signature);
    assertTrue(check.getIsGood());
    TestUtils.testCheckReserve(check);
    assertEquals(wallet.getBalance(), check.getAmountTotal());  // TODO: fails after send tests
    
    // test different wallet address
    wallet.openWallet(TestUtils.WALLET_NAME_2, TestUtils.WALLET_PW);
    String differentAddress = wallet.getPrimaryAddress();
    wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof(wallet.getSubaddress(0, 1).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
    
    // test wrong message
    check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Wrong message", signature);
    assertFalse(check.getIsGood());  // TODO: specifically test reserve checks, probably separate objects
    TestUtils.testCheckReserve(check);
    
    // test wrong signature
    try {
      wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", "wrong signature");
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
  }
  
  @Test
  public void testReserveProofAccount() {
    
    // test proofs of accounts
    int numNonZeroTests = 0;
    String msg = "Test message";
    List<MoneroAccount> accounts = wallet.getAccounts();
    String signature = null;
    for (MoneroAccount account : accounts) {
      if (account.getBalance().compareTo(BigInteger.valueOf(0)) > 0) {
        BigInteger checkAmount = account.getBalance().divide(BigInteger.valueOf(2));
        signature = wallet.getReserveProof(account.getIndex(), checkAmount, msg);
        MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), msg, signature);
        assertTrue(check.getIsGood());
        TestUtils.testCheckReserve(check);
        assertTrue(check.getAmountTotal().compareTo(checkAmount) >= 0);
        numNonZeroTests++;
      } else {
        try {
          wallet.getReserveProof(account.getIndex(), account.getBalance(), msg);
          fail("Should have thrown exception");
        } catch (MoneroRpcException e) {
          assertEquals(-1, (int) e.getRpcCode());
          try {
            wallet.getReserveProof(account.getIndex(), TestUtils.MAX_FEE, msg);
            fail("Should have thrown exception");
          } catch (MoneroRpcException e2) {
            assertEquals(-1, (int) e2.getRpcCode());
          }
        }
      }
    }
    assertTrue("Must have more than one account with non-zero balance; run testSendToMultiple() first", numNonZeroTests > 1);
    
    // test error when not enough balance for requested minimum reserve amount
    try {
      wallet.getReserveProof(0, accounts.get(0).getBalance().add(TestUtils.MAX_FEE), "Test message");
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
    
    // test different wallet address
    wallet.openWallet(TestUtils.WALLET_NAME_2, TestUtils.WALLET_PW);
    String differentAddress = wallet.getPrimaryAddress();
    wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof(wallet.getSubaddress(0, 1).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
    
    // test wrong message
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Wrong message", signature);
    assertFalse(check.getIsGood()); // TODO: specifically test reserve checks, probably separate objects
    TestUtils.testCheckReserve(check);
    
    // test wrong signature
    try {
      wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", "wrong signature");
      fail("Should have thrown exception");
    } catch (MoneroRpcException e) {
      assertEquals(-1, (int) e.getRpcCode());
    }
  }

  @Test
  public void testGetKeyImages() {
    Collection<MoneroKeyImage> images = wallet.getKeyImages();
    assertFalse(images.isEmpty());
    for (MoneroKeyImage image : images) {
      assertNotNull(image.getKeyImage());
      assertNotNull(image.getSignature());
    }
  }

  @Test
  public void testImportKeyImages() {
    Collection<MoneroKeyImage> images = wallet.getKeyImages();
    assertFalse(images.isEmpty());
    Map<String, BigInteger> results = wallet.importKeyImages(images);
    assertNotNull(results.get("height"));
    assertNotNull(results.get("spent"));
    assertNotNull(results.get("unspent"));
  }

  @Test
  public void testGetLanguages() {
    List<String> languages = wallet.getLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }

  @Test
  public void testCreateOpenWallet() {
    
    // create / open new wallet
    try {
      wallet.createWallet(TestUtils.WALLET_NAME_2, TestUtils.WALLET_PW, "English");
    } catch (MoneroRpcException e) {
      assertEquals((int) -21, (int) e.getRpcCode());  // exception is ok if wallet already created
    }    
    wallet.openWallet(TestUtils.WALLET_NAME_2, TestUtils.WALLET_PW);
    List<MoneroTx> txs = wallet.getTxs();
    assertTrue(txs.isEmpty());  // wallet is unused except tests
    
    // open previous wallet
    wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
    txs = wallet.getTxs();
    assertFalse(txs.isEmpty());  // wallet is used
  }

  @Test
  public void testSignVerify() {
    String msg = "This is a super important message which needs to be signed and verified.";
    String signature = wallet.sign(msg);
    boolean verified = wallet.verify(msg, wallet.getSubaddress(0, 0).getAddress().toString(), signature);
    assertTrue(verified);
    verified = wallet.verify(msg, SAMPLE_ADDRESS, signature);
    assertFalse(verified);
  }

  @Test
  public void testUri() {
    
    // test with optional fields as null
    MoneroUri mUri1 = new MoneroUri();
    mUri1.setAddress(wallet.getSubaddress(0, 0).getAddress());
    URI uri = wallet.toUri(mUri1);
    MoneroUri mUri2 = wallet.toMoneroUri(uri);
    assertTrue(mUri1.equals(mUri2));
    
    // test with all fields
    mUri1.setAmount(BigInteger.valueOf((Long.parseLong("425000000000"))));
    mUri1.setPaymentId("03284e41c342f036");
    mUri1.setRecipientName("John Doe");
    mUri1.setTxDescription("OMZG XMR FTW");
    uri = wallet.toUri(mUri1);
    mUri2 = wallet.toMoneroUri(uri);

    assertTrue(mUri1.equals(mUri2));
    
    // test with address null
    mUri1.setAddress(null);
    mUri1.setPaymentId("bizzup");
    try {
      wallet.toUri(mUri1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroRpcException e) {
      assertEquals((int) -11, (int) e.getRpcCode());
      assertTrue(e.getRpcMessage().contains("Cannot make URI from supplied parameters"));
    }
  }

  @Test
  public void testSaveBlockchain() {
    wallet.saveBlockchain();
  }

  @Test
  public void testRescanSpent() {
    wallet.rescanSpent();
  }

  @Ignore // disabled so tests don't actually stop the wallet
  @Test
  public void testStopWallet() {
    wallet.stopWallet();
  }

  @Test
  public void testMining() {
    wallet.startMining(2, false, true);
    wallet.stopMining();
  }
  
  @Test
  public void testCreateAccount() {
    
    // create account with null label
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount(null);
    TestUtils.testAccount(createdAccount);
    assertNull(createdAccount.getLabel());
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 1);
    
    List<MoneroAccount> newAccounts = new ArrayList<MoneroAccount>();
    
    // create account with label
    accountsBefore = wallet.getAccounts();
    String label = UUID.randomUUID().toString();
    createdAccount = wallet.createAccount(label);
    newAccounts.add(createdAccount);
    assertEquals(label, createdAccount.getLabel());
    TestUtils.testAccount(createdAccount);
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 1);
    
    // create account with same label
    createdAccount = wallet.createAccount(label);
    newAccounts.add(createdAccount);
    assertEquals(label, createdAccount.getLabel());
    TestUtils.testAccount(createdAccount);
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 2);
  }
  
  @Test
  public void testAccountTags() {
    
    // test that null tag returns all accounts
    List<MoneroAccount> accounts1 = wallet.getAccounts();
    List<MoneroAccount> accounts2 = wallet.getAccounts(null);
    assertEquals(accounts1, accounts2);
    
    // test that non-existing tag returns no accounts
    try {
      wallet.getAccounts("non_existing_tag");
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroRpcException e) {
      assertEquals((int) -1, (int) e.getRpcCode());
    }
    
    // create expected tag for test
    MoneroAccountTag expectedTag = new MoneroAccountTag("my_tag_" + UUID.randomUUID().toString(), "my tag label", Arrays.asList(0, 1));
    
    // tag and query accounts
    assertTrue(accounts1.size() >= 3);
    wallet.tagAccounts(expectedTag.getTag(), Arrays.asList(0, 1));
    List<MoneroAccount> accounts = wallet.getAccounts(expectedTag.getTag());
    assertEquals(2, accounts.size());
    assertEquals(accounts1.get(0), accounts.get(0));
    assertEquals(accounts1.get(1), accounts.get(1));
    
    // set tag label
    wallet.setAccountTagLabel(expectedTag.getTag(), expectedTag.getLabel());
    
    // retrieve and find new tag
    List<MoneroAccountTag> tags = wallet.getAccountTags();
    assertTrue(tags.contains(expectedTag));
    
    // untag and query accounts
    wallet.untagAccounts(Arrays.asList(0, 1));
    assertFalse(wallet.getAccountTags().contains(expectedTag));
    try {
      wallet.getAccounts(expectedTag.getTag());
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroRpcException e) {
      assertEquals((int) -1, (int) e.getRpcCode());
    }
  }

  @Test
  public void testCreateSubaddress() {
    
    // create subaddresses across accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    if (accounts.size() < 2) wallet.createAccount();
    accounts = wallet.getAccounts();
    assertTrue(accounts.size() > 1);
    for (int accountIdx = 0; accountIdx < 2; accountIdx++) {
      
      // create subaddress with no label
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(accountIdx);
      MoneroSubaddress subaddress = wallet.createSubaddress(accountIdx, null);
      assertEquals("", subaddress.getLabel());
      TestUtils.testSubaddress(subaddress);
      List<MoneroSubaddress> subaddressesNew = wallet.getSubaddresses(accountIdx);
      assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
      assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
      
      // create subaddress with label
      subaddresses = wallet.getSubaddresses(accountIdx);
      String uuid = UUID.randomUUID().toString();
      subaddress = wallet.createSubaddress(accountIdx, uuid);
      assertEquals(subaddress.getLabel(), uuid);
      TestUtils.testSubaddress(subaddress);
      subaddressesNew = wallet.getSubaddresses(accountIdx);
      assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
      assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
    }
  }
  
  @Test
  public void testAddressBookEntries() {
    
    // initial state
    List<MoneroAddressBookEntry> entries = wallet.getAddressBookEntries();
    int numEntriesStart = entries.size();
    for (MoneroAddressBookEntry entry : entries) TestUtils.testAddressBookEntry(entry);
    
    // test adding standard addresses
    final int NUM_ENTRIES = 5;
    String address = wallet.getSubaddress(0, 0).getAddress();
    List<Integer> indices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_ENTRIES; i++) {
      indices.add(wallet.addAddressBookEntry(address, "hi there!"));
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart + NUM_ENTRIES, entries.size());
    for (Integer idx : indices) {
      boolean found = false;
      for (MoneroAddressBookEntry entry : entries) {
        if (idx == entry.getIndex()) {
          TestUtils.testAddressBookEntry(entry);
          assertEquals(address, entry.getAddress());
          assertEquals("hi there!", entry.getDescription());
          found = true;
          break;
        }
      }
      assertTrue("Index " + idx + " not found in address book indices", found);
    }
    
    // delete entries at starting index
    int deleteIdx = indices.get(0);
    for (int i = 0; i < indices.size(); i++) {
      wallet.deleteAddressBookEntry(deleteIdx);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart, entries.size());
    
    // test adding integrated addresses
    indices.clear();
    String paymentId = "03284e41c342f03"; // payment id less one character
    Map<Integer, MoneroIntegratedAddress> integratedAddresses = new HashMap<Integer, MoneroIntegratedAddress>();
    Map<Integer, String> integratedDescriptions = new HashMap<Integer, String>();
    for (int i = 0; i < NUM_ENTRIES; i++) {
      MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId + i); // create unique integrated address
      String uuid = UUID.randomUUID().toString();
      int idx = wallet.addAddressBookEntry(integratedAddress.toString(), uuid);
      indices.add(idx);
      integratedAddresses.put(idx, integratedAddress);
      integratedDescriptions.put(idx, uuid);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart + NUM_ENTRIES, entries.size());
    for (Integer idx : indices) {
      boolean found = false;
      for (MoneroAddressBookEntry entry : entries) {
        if (idx == entry.getIndex()) {
          TestUtils.testAddressBookEntry(entry);
          assertEquals(integratedDescriptions.get(idx), entry.getDescription());
          assertEquals(integratedAddresses.get(idx).getStandardAddress(), entry.getAddress());
          assertTrue(MoneroUtils.paymentIdsEqual(integratedAddresses.get(idx).getPaymentId(), entry.getPaymentId()));
          found = true;
          break;
        }
      }
      assertTrue("Index " + idx + " not found in address book indices", found);
    }
    
    // delete entries at starting index
    deleteIdx = indices.get(0);
    for (int i = 0; i < indices.size(); i++) {
      wallet.deleteAddressBookEntry(deleteIdx);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart, entries.size());
  }
  
  @Test
  public void testAttributes() {
    
    // set attributes
    Map<String, String> attrs = new HashMap<String, String>();
    for (int i = 0; i < 5; i++) {
      String key = "attr" + i;
      String val = UUID.randomUUID().toString();
      attrs.put(key, val);
      wallet.setAttribute(key, val);
    }
    
    // test attributes
    for (String key : attrs.keySet()) {
      assertEquals(attrs.get(key), wallet.getAttribute(key));
    }
  }
  
  /**
   * Gets random transactions.
   * 
   * @param filter specifies a filter for the transactions.
   * @param minTxs specifies the minimum number of transactions (null for no minimum)
   * @param maxTxs specifies the maximum number of transactions (null for all filtered transactions)
   * @return List<MoneroTx> are the random transactions
   */
  private static List<MoneroTx> getRandomTransactions(MoneroTxFilter filter, Integer minTxs, Integer maxTxs) {
    List<MoneroTx> txs = wallet.getTxs(filter);
    if (minTxs != null) assertTrue(txs.size() >= minTxs);
    Collections.shuffle(txs);
    if (maxTxs == null) return txs;
    else return txs.subList(0, Math.min(maxTxs, txs.size()));
  }
  
  private static List<MoneroTx> getCachedTxs() {
    if (txCache != null) return txCache;
    txCache = wallet.getTxs();
    return txCache;
  }
  
//  private static List<MoneroSubaddress> getRandomSubaddresses(int numSubaddresses) {
//    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
//    for (MoneroAccount account : wallet.getAccounts(true)) subaddresses.addAll(account.getSubaddresses());
//    assertTrue("Not enough subaddresses; run testSendToMultiple()", subaddresses.size() > numSubaddresses);
//    Collections.shuffle(subaddresses);
//    return subaddresses.subList(0,  numSubaddresses);
//  }
}
