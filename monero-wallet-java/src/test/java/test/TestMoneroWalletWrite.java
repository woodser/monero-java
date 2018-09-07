package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import model.MoneroAccount;
import model.MoneroAddressBookEntry;
import model.MoneroIntegratedAddress;
import model.MoneroPayment;
import model.MoneroSubaddress;
import model.MoneroTx;
import model.MoneroTxConfig;
import utils.TestUtils;
import wallet.MoneroWallet;
import wallet.rpc.MoneroRpcException;

/**
 * Tests modifying a Monero wallet (churning funds, creating accounts, tagging accounts, creating subaddresses, setting tx notes, etc).
 * 
 * These tests are separated because they rely on a balance, initiate transactions on the blockchain, and modify a test wallet state (TODO).
 * 
 * TODO: support / test send, sendSplit, and sweepAll from specific accounts and subaddresses
 * TODO: test sending with payment id
 */
public class TestMoneroWalletWrite {
  
  private static final Integer MIXIN = 6;
  private static final int SEND_DIVISOR = 2;
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
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
    
    // create subaddress with no label
    List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(0);
    MoneroSubaddress subaddress = wallet.createSubaddress(0, null);
    assertEquals("", subaddress.getLabel());
    TestUtils.testSubaddress(subaddress);
    List<MoneroSubaddress> subaddressesNew = wallet.getSubaddresses(0);
    assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
    assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
    
    // create subaddress with label
    subaddresses = wallet.getSubaddresses(0);
    String uuid = UUID.randomUUID().toString();
    subaddress = wallet.createSubaddress(0, uuid);
    assertEquals(subaddress.getLabel(), uuid);
    TestUtils.testSubaddress(subaddress);
    subaddressesNew = wallet.getSubaddresses(0);
    assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
    assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
  }
  
  @Test
  public void testSend() {
    
    // get balance before
    BigInteger balanceBefore = wallet.getBalance(0);
    MoneroWallet wallet = TestUtils.getWallet();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance(0);
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", balanceBefore.longValue() > 0);
    assertTrue("Wallet is waiting on unlocked funds", unlockedBalanceBefore.longValue() > 0);
    
    // send to self
    String address = wallet.getSubaddress(0, 0).getAddress();
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(SEND_DIVISOR));
    MoneroTx tx = wallet.send(address, null, sendAmount, MIXIN);
    
    // test transaction
    assertEquals(sendAmount, tx.getAmount());
    assertNotNull(tx.getId());
    assertNotNull(tx.getPayments());
    assertEquals(1, tx.getPayments().size());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getKey());
    assertNull(tx.getSize());
    assertNull(tx.getType());
    assertNull(tx.getHeight());
    assertEquals((Integer) 0, tx.getUnlockTime());
    assertNotNull(tx.getBlob());
    assertNotNull(tx.getMetadata());
    
    // test payments
    for (MoneroPayment payment : tx.getPayments()) {
      assertEquals(address.toString(), payment.getAddress());
      assertEquals(sendAmount, payment.getAmount());
      assertTrue(tx == payment.getTransaction());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance(0).longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance(0).longValue() < unlockedBalanceBefore.longValue());
  }

  @Test
  public void testSendSplit() {
    
    // get balance and address
    BigInteger balanceBefore = wallet.getBalance(0);
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance(0);
    String address = wallet.getPrimaryAddress();
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", balanceBefore.longValue() > 0);
    assertTrue("Wallet is waiting on unlocked funds", unlockedBalanceBefore.longValue() > 0);
    
    // create payments to send
    int numPayments = 3;
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(numPayments + SEND_DIVISOR));
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    for (int i = 0; i < numPayments; i++) {
      payments.add(new MoneroPayment(address, sendAmount));
    }
    
    // send payments
    MoneroTxConfig config = new MoneroTxConfig();
    config.setDestinations(payments);
    config.setMixin(MIXIN);
    List<MoneroTx> txs = wallet.sendSplit(config);
    
    // test transactions
    for (MoneroTx tx : txs) {
      assertNotNull(tx.getId());
      assertNull(tx.getPayments());
      assertTrue(tx.getFee().longValue() > 0);
      assertEquals(MIXIN, tx.getMixin());
      assertNull(tx.getKey());
      assertNull(tx.getHeight());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
      assertEquals((Integer) 0, tx.getUnlockTime());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance(0).longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance(0).longValue() < unlockedBalanceBefore.longValue());
  }

  @Test
  public void testSweepAll() {
    fail("Not yet implemented");
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
    Collection<Integer> indices = new HashSet<Integer>();
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
    
    // delete entries
    for (Integer idx : indices) {
      wallet.deleteAddressBookEntry(idx);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart, entries.size());
    
    // test adding integrated addresses
    indices = new HashSet<Integer>();
    String paymentId = "03284e41c342f03"; // payment id less one character
    Map<Integer, MoneroIntegratedAddress> integratedAddresses = new HashMap<Integer, MoneroIntegratedAddress>();
    for (int i = 0; i < NUM_ENTRIES; i++) {
      MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId + i); // create unique integrated address
      int idx = wallet.addAddressBookEntry(integratedAddress.getIntegratedAddress(), null);
      indices.add(idx);
      integratedAddresses.put(idx, integratedAddress);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart + NUM_ENTRIES, entries.size());
    for (Integer idx : indices) {
      boolean found = false;
      for (MoneroAddressBookEntry entry : entries) {
        if (idx == entry.getIndex()) {
          TestUtils.testAddressBookEntry(entry);
          assertEquals(integratedAddresses.get(idx), entry.getAddress());
          assertEquals("", entry.getDescription());
          found = true;
          break;
        }
      }
      assertTrue("Index " + idx + " not found in address book indices", found);
    }
    
    // delete entries
    for (Integer idx : indices) {
      wallet.deleteAddressBookEntry(idx);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart, entries.size());
  }
}
