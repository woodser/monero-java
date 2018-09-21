package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import monero.rpc.MoneroRpcException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxConfig;
import utils.PrintBalances;
import utils.TestUtils;

/**
 * Tests modifying a Monero wallet (churning funds, creating accounts, tagging accounts, creating subaddresses, setting tx notes, etc).
 * 
 * These tests are separated because they rely on a balance, initiate transactions on the blockchain, and modify a test wallet state (TODO).
 * 
 * TODO: support / test send, sendSplit, and sweepAll from specific accounts and subaddresses
 * TODO: test sending with payment id
 */
public class TestMoneroWalletWrite {
  
  private static final int SEND_DIVISOR = 2;
  private static final BigInteger MAX_FEE = BigInteger.valueOf(5000000).multiply(BigInteger.valueOf(10000));
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  @After
  public void teardown() {
    PrintBalances.printBalances();
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
    
    // tag and query accounts
    assertTrue(accounts1.size() >= 3);
    wallet.tagAccounts("my_tag", Arrays.asList(0, 1));
    List<MoneroAccount> accounts = wallet.getAccounts("my_tag");
    assertEquals(2, accounts.size());
    assertEquals(accounts1.get(0), accounts.get(0));
    assertEquals(accounts1.get(1), accounts.get(1));
    
    // untag and query accounts
    wallet.untagAccounts(Arrays.asList(0, 1));
    try {
      wallet.getAccounts("my_tag");
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
  public void testSendToSingle() {
    testSendToSingle(false);
  }
  
  @Test
  public void testSendSplitToSingle() {
    testSendToSingle(true);
  }
  
  private void testSendToSingle(boolean canSplit) {
    
    // find a non-primary subaddress to send from
    boolean sufficientBalance = false;
    MoneroAccount fromAccount = null;
    MoneroSubaddress fromSubaddress = null;
    List<MoneroAccount> accounts = wallet.getAccounts();
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      for (int i = 1; i < subaddresses.size(); i++) {
        if (subaddresses.get(i).getBalance().compareTo(MAX_FEE) > 0) sufficientBalance = true;
        if (subaddresses.get(i).getUnlockedBalance().compareTo(MAX_FEE) > 0) {
          fromAccount = account;
          fromSubaddress = subaddresses.get(i);
          break;
        }
      }
      if (fromAccount != null) break;
    }
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", sufficientBalance);
    assertTrue("Wallet is waiting on unlocked funds", fromSubaddress != null);
    
    // get balance before send
    BigInteger balanceBefore = fromSubaddress.getBalance();
    BigInteger unlockedBalanceBefore  = fromSubaddress.getBalance();
        
    // send to self
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    MoneroTxConfig config = new MoneroTxConfig(address, null, sendAmount, TestUtils.MIXIN);
    config.setAccountIndex(fromAccount.getIndex());
    config.setSubaddressIndices(Arrays.asList(fromSubaddress.getIndex()));
    if (canSplit) {
      txs.addAll(wallet.sendSplit(config));
    } else {
      txs.add(wallet.send(config));
    }
    
    // test that balance and unlocked balance decreased
    assertTrue(wallet.getBalance(fromAccount.getIndex(), fromSubaddress.getIndex()).compareTo(balanceBefore) < 0);
    assertTrue(wallet.getUnlockedBalance(fromAccount.getIndex(), fromSubaddress.getIndex()).compareTo(unlockedBalanceBefore) < 0);
    
    // test transactions
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      TestUtils.testTx(tx);
      TestUtils.testSendTx(tx, canSplit);
      assertEquals(fromAccount.getIndex(), tx.getAccountIndex());
      assertNotNull(tx.getSubaddressIndex());
      assertEquals((int) 0, (int) tx.getSubaddressIndex()); // TODO: monero-wallet-rpc outgoing transactions do not indicate originating subaddresses
      assertEquals(sendAmount, tx.getAmount());
      
      // test tx payments
      assertEquals(1, tx.getPayments().size());
      for (MoneroPayment payment : tx.getPayments()) {
        assertEquals(address, payment.getAddress());
        assertEquals(sendAmount, payment.getAmount());
        assertTrue(tx == payment.getTransaction());
      }
    }
  }

  @Test
  public void testSendToMultiple() {
    testSendToMultiple(false);
  }
  
  @Test
  public void testSendSplitToMultiple() {
    testSendToMultiple(true);
  }
  
  private void testSendToMultiple(boolean canSplit) {
    
    // test constants
    int NUM_ACCOUNTS = 5;
    int NUM_ADDRESSES_PER_ACCOUNT = 5;
    int TOTAL_ADDRESSES = NUM_ACCOUNTS * NUM_ADDRESSES_PER_ACCOUNT;
    
    // get amount to send per subaddress
    BigInteger balance = wallet.getBalance(0);
    BigInteger unlockedBalance = wallet.getUnlockedBalance(0);
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", balance.longValue() > 0);
    assertTrue("Wallet is waiting on unlocked funds", unlockedBalance.longValue() > 0);
    BigInteger sendAmount = unlockedBalance.divide(BigInteger.valueOf(SEND_DIVISOR));
    BigInteger sendAmountPerSubaddress = sendAmount.divide(BigInteger.valueOf(TOTAL_ADDRESSES));
    
    // create minimum number of accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    for (int i = 0; i < NUM_ACCOUNTS - accounts.size(); i++) {
      wallet.createAccount();
    }
    
    // create minimum number of subaddresses per account and collect destination addresses
    List<String> destinationAddresses = new ArrayList<String>();
    for (int i = 0; i < NUM_ACCOUNTS; i++) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(i);
      for (int j = 0; j < NUM_ADDRESSES_PER_ACCOUNT - subaddresses.size(); j++) wallet.createSubaddress(i);
      subaddresses = wallet.getSubaddresses(i);
      assertTrue(subaddresses.size() >= NUM_ADDRESSES_PER_ACCOUNT);
      for (int j = 0; j < NUM_ADDRESSES_PER_ACCOUNT; j++) destinationAddresses.add(subaddresses.get(j).getAddress());
    }
        
    // send to subaddresses
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    for (int i = 0; i < destinationAddresses.size(); i++) {
      MoneroPayment payment = new MoneroPayment();
      payments.add(payment);
      payment.setAddress(destinationAddresses.get(i));
      payment.setAmount(sendAmountPerSubaddress);
    }
    MoneroTxConfig config = new MoneroTxConfig();
    config.setMixin(TestUtils.MIXIN);
    config.setAccountIndex(0);
    config.setDestinations(payments);
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    if (canSplit) {
      txs.addAll(wallet.sendSplit(config));
    } else {
      txs.add(wallet.send(config));
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance(0).longValue() < balance.longValue());
    assertTrue(wallet.getUnlockedBalance(0).longValue() < unlockedBalance.longValue());
    
    // test transactions
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      TestUtils.testTx(tx);
      TestUtils.testSendTx(tx, canSplit);
      assertNotNull(tx.getSubaddressIndex());
      assertEquals((int) 0, (int) tx.getSubaddressIndex()); // TODO: monero-wallet-rpc outgoing transactions do not indicate originating subaddresses
      if (Math.abs(sendAmount.subtract(tx.getAmount()).longValue()) >= TOTAL_ADDRESSES) { // send amounts may be slightly different
        fail("Tx amounts are too different: " + sendAmount + " - " + tx.getAmount() + " = " + sendAmount.subtract(tx.getAmount()));
      }
      assertEquals(TOTAL_ADDRESSES, tx.getPayments().size());
      assertEquals(payments.size(), tx.getPayments().size());
      for (int i = 0; i < payments.size(); i++) {
        assertEquals(payments.get(i).getAddress(), tx.getPayments().get(i).getAddress());
        assertEquals(payments.get(i).getAmount(), tx.getPayments().get(i).getAmount());
        assertTrue(tx == tx.getPayments().get(i).getTransaction());
      }
    }
  }
  
  @Test
  public void testSendFromMultiple() {
    testSendFromMultiple(4, false);
  }
  
  @Test
  public void testSendSplitFromMultiple() {
    testSendFromMultiple(2, true);
  }

  private void testSendFromMultiple(int accountIdx, boolean canSplit) {
    
    int NUM_SUBADDRESSES = 2; // number of subaddresses to send from
    
    // ensure at least 2 accounts exist and the second account has at least 3 subaddresses with unlocked balances
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue("This test requires at least 2 accounts.  Run testSendToMultiple() first", accounts.size() > accountIdx);
    List<Integer> unlockedBalanceIndices = new ArrayList<Integer>();
    List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(accountIdx);
    int numBalances = 0;
    for (MoneroSubaddress subaddress : subaddresses) {
      if (subaddress.getBalance().longValue() > 0) numBalances++;
      if (subaddress.getUnlockedBalance().longValue() > 0) unlockedBalanceIndices.add(subaddress.getIndex());
    }
    if (unlockedBalanceIndices.size() <= NUM_SUBADDRESSES) {
      if (numBalances >= 2) fail("Wallet is waiting on unlocked funds");
      else fail("This test requires at least " + (NUM_SUBADDRESSES + 1) + " subaddresses with unlocked balances in accounts[" + accountIdx + "].  Run testSendToMultiple() first");
    }
    
    // determine the indices of the first two subaddresses with unlocked balances
    List<Integer> fromSubaddressIndices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_SUBADDRESSES; i++) {
      fromSubaddressIndices.add(unlockedBalanceIndices.get(i));
    }
    
    // determine the amount to send (slightly less than the sum to send from)
    BigInteger sendAmount = BigInteger.valueOf(0);
    for (Integer fromSubaddressIdx : fromSubaddressIndices) {
      sendAmount = sendAmount.add(subaddresses.get(fromSubaddressIdx).getUnlockedBalance());
    }
    sendAmount = sendAmount.subtract(MAX_FEE);
    
    // send from the first subaddresses with unlocked balances
    String address = wallet.getPrimaryAddress();
    MoneroTxConfig config = new MoneroTxConfig(address, null, sendAmount);
    config.setAccountIndex(accountIdx);
    config.setSubaddressIndices(fromSubaddressIndices);
    config.setMixin(TestUtils.MIXIN);
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    if (canSplit) {
      txs.addAll(wallet.sendSplit(config));
    } else {
      txs.add(wallet.send(config));
    }
    
    // test that balances from intended subaddresses decreased
    List<MoneroSubaddress> subaddressesAfter = wallet.getSubaddresses(accountIdx);
    assertEquals(subaddresses.size(), subaddressesAfter.size());
    for (int i = 0; i < subaddresses.size(); i++) {
      if (fromSubaddressIndices.contains(i)) {
        if (subaddressesAfter.get(i).getUnlockedBalance().compareTo(subaddresses.get(i).getUnlockedBalance()) >= 0) {
          fail("Tx amounts should have decreased: " + subaddresses.get(i).getUnlockedBalance() + " vs " + subaddressesAfter.get(i).getUnlockedBalance());
        }
      } else {
        if (subaddressesAfter.get(i).getUnlockedBalance().compareTo(subaddresses.get(i).getUnlockedBalance()) != 0) {
          fail("Tx amounts from unrelated subaddresses should not have changed: " + subaddresses.get(i).getUnlockedBalance() + " vs " + subaddressesAfter.get(i).getUnlockedBalance());
        }
      }
    }
    
    // test the resulting transactions
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      TestUtils.testTx(tx);
      TestUtils.testSendTx(tx, canSplit);
      assertNotNull(tx.getSubaddressIndex());
      assertEquals((int) 0, (int) tx.getSubaddressIndex()); // TODO: monero-wallet-rpc outgoing transactions do not indicate originating subaddresses
      if (Math.abs(sendAmount.subtract(tx.getAmount()).longValue()) >= 10) { // send amounts may be slightly different
        fail("Tx amounts are too different: " + sendAmount + " - " + tx.getAmount() + " = " + sendAmount.subtract(tx.getAmount()));
      }
      assertEquals(1, tx.getPayments().size());
      for (MoneroPayment payment : tx.getPayments()) {
        assertEquals(address, payment.getAddress());
        assertEquals(sendAmount, payment.getAmount());
        assertTrue(tx == payment.getTransaction());
      }
    }
  }
  
  @Test
  public void testSweepDust() {
    List<MoneroTx> txs = wallet.sweepDust();
    for (MoneroTx tx : txs) {
      assertNotNull(tx.getId());
      assertNull(tx.getPayments());
      assertNull(tx.getAmount());
      assertNull(tx.getFee());
      assertNull(tx.getMixin());
      assertNull(tx.getKey());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
      assertNull(tx.getBlob());
      assertNull(tx.getMetadata());
    }
  }
  
  @Test
  public void testSetTxNotes() {
    
    // set tx notes
    List<MoneroTx> txs = wallet.getTxs();
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
}
