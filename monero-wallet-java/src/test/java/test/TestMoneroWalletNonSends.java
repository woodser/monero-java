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

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import model.MoneroAccount;
import model.MoneroAddress;
import model.MoneroAddressBookEntry;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import model.MoneroKeyImage;
import model.MoneroOutput;
import model.MoneroPayment;
import model.MoneroSubaddress;
import model.MoneroTx;
import model.MoneroTx.MoneroTxType;
import model.MoneroTxFilter;
import model.MoneroUri;
import utils.MoneroUtils;
import utils.TestUtils;
import wallet.MoneroWallet;
import wallet.rpc.MoneroRpcException;

/**
 * Tests a Monero wallet excluding sending transactions.
 */
public class TestMoneroWalletNonSends {
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
    List<MoneroTx> txs = wallet.getTxs();
    assertTrue("Test wallet does not have transaction history; load '" + TestUtils.WALLET_NAME_1 + "' with stagenet XMR and run TestMoneroWalletSends a few times", txs.size() >= 3);
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
    MoneroAddress address = wallet.getSubaddress(0, 0).getAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(address.getStandardAddress(), integratedAddress.getStandardAddress());
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
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(address.getStandardAddress(), integratedAddress.getStandardAddress());
    assertNotNull(integratedAddress.getPaymentId());
  }
  
  @Test
  public void testGetPrimaryAddress() {
    MoneroAddress primaryAddress = wallet.getPrimaryAddress();
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
      testAccount(account);
    }
  }

  @Test
  public void testGetAccountsByTag() {
    
    // test that null tag returns all accounts
    List<MoneroAccount> accounts1 = wallet.getAccounts();
    List<MoneroAccount> accounts2 = wallet.getAccounts(null);
    assertEquals(accounts1, accounts2);
    
    // test that non-existing tag returns no accounts
    List<MoneroAccount> accounts = wallet.getAccounts("non_existing_tag");
    assertEquals(0, accounts.size());
  }

  @Test
  public void testGetAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(wallet.getAccount(account.getIndex()));
    }
  }

  @Test
  public void testCreateAccount() {
    
    // test creation with null tag
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount(null);
    testAccount(createdAccount);
    assertNull(createdAccount.getLabel());
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 1);
    
    // test creation with tag
    accountsBefore = wallet.getAccounts();
    String tag = UUID.randomUUID().toString();
    createdAccount = wallet.createAccount(tag);
    assertEquals(tag, createdAccount.getLabel());
    testAccount(createdAccount);
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 1);
    
    // test querying by created tag
    List<MoneroAccount> accountsByTag = wallet.getAccounts(tag);
    assertEquals(1, accountsByTag.size());
    assertEquals(createdAccount, accountsByTag.get(0));
    
    // create another account with the same tag
    createdAccount = wallet.createAccount(tag);
    assertEquals(tag, createdAccount.getLabel());
    accountsByTag = wallet.getAccounts(tag);
    assertEquals(2, accountsByTag.size());
    assertEquals(createdAccount, accountsByTag.get(1));
  }

  @Test
  public void testGetSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        testSubaddress(subaddress);
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
        subaddressIndices.add(subaddress.getIndex());
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
        MoneroSubaddress subaddress = wallet.getSubaddress(account.getIndex(), subaddresses.get(i).getIndex());
        assertEquals(subaddresses.get(i), subaddress);
      }
    }
  }

  @Test
  public void testCreateSubaddress() {
    
    // create subaddress with no label
    List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(0);
    MoneroSubaddress subaddress = wallet.createSubaddress(0, null);
    assertNull(subaddress.getLabel());
    testSubaddress(subaddress);
    List<MoneroSubaddress> subaddressesNew = wallet.getSubaddresses(0);
    assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
    assertEquals(subaddressesNew.get(subaddressesNew.size() - 1), subaddress);
    
    // create subaddress with label
    subaddresses = wallet.getSubaddresses(0);
    String uuid = UUID.randomUUID().toString();
    subaddress = wallet.createSubaddress(0, uuid);
    assertEquals(subaddress.getLabel(), uuid);
    testSubaddress(subaddress);
    subaddressesNew = wallet.getSubaddresses(0);
    assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
    assertEquals(subaddressesNew.get(subaddressesNew.size() - 1), subaddress);
  }

  @Test
  public void testGetBalanceAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      BigInteger balance = wallet.getBalance(account.getIndex());
      assertTrue(balance.longValue() >= 0);
    }
    
    // test getting balance of invalid account
    try {
      wallet.getBalance(-456);
      fail("Should have thrown error on invalid account");
    } catch (MoneroException exception) { }
  }

  @Test
  public void getBalanceSubaddress() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        BigInteger balance = wallet.getBalance(account.getIndex(), subaddress.getIndex());
        assertTrue(balance.longValue() >= 0);
      }
    }
    
    // test getting balance of invalid account
    try {
      wallet.getBalance(-456);
      fail("Should have thrown error on invalid account");
    } catch (MoneroException exception) { }
  }

  @Test
  public void testGetUnlockedBalanceAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      BigInteger unlockedBalance = wallet.getUnlockedBalance(account.getIndex());
      assertTrue(unlockedBalance.longValue() >= 0);
    }
    
    // test getting balance of invalid account
    try {
      wallet.getUnlockedBalance(-456);
      fail("Should have thrown error on invalid account");
    } catch (MoneroException exception) { }
  }

  @Test
  public void testGetUnlockedBalanceSubaddress() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        BigInteger unlockedBalance = wallet.getUnlockedBalance(account.getIndex(), subaddress.getIndex());
        assertTrue(unlockedBalance.longValue() >= 0);
      }
    }
    
    // test getting balance of invalid account
    try {
      wallet.getUnlockedBalance(-456);
      fail("Should have thrown error on invalid account");
    } catch (MoneroException exception) { }
  }

  @Test
  public void testGetTxs() {
    List<MoneroTx> txs = wallet.getTxs();
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      testTransaction(tx);
    }
  }

  @Test
  public void testGetTxsMoneroTxFilter() {
    
    // get all transactions for reference
    List<MoneroTx> allTxs = wallet.getTxs();
    assertFalse(allTxs.isEmpty());
    for (MoneroTx tx : allTxs) {
      testTransaction(tx);
    }
    
    // test getting transactions by payment ids
    Collection<String> paymentIds = new HashSet<String>();
    for (MoneroTx tx : allTxs) paymentIds.add(tx.getPaymentId());
    assertFalse(paymentIds.isEmpty());
    for (String paymentId : paymentIds) {
      Collection<String> filterPaymentIds = new HashSet<String>();
      filterPaymentIds.add(paymentId);
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setPaymentIds(paymentIds);
      List<MoneroTx> txs = wallet.getTxs(filter);
      assertFalse(txs.isEmpty());
      for (MoneroTx tx : txs) {
        testTransaction(tx);
      }
    }
    
    // test getting incoming transactions
    List<MoneroTx> txs = wallet.getTxs(new MoneroTxFilter(true, false, false, false, false, null, null, null, null, null, null));
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertEquals(MoneroTxType.INCOMING, tx.getType());
    }
    
    // test getting outgoing transactions
    txs = wallet.getTxs(new MoneroTxFilter(false, true, false, false, false, null, null, null, null, null, null));
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertEquals(MoneroTxType.OUTGOING, tx.getType());
    }
    
    // test balance equals spendable outputs for each account
    for (MoneroAccount account : wallet.getAccounts()) {
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setAccountIdx(account.getIndex());
      txs = wallet.getTxs(filter);
      assertFalse(txs.isEmpty());
      BigInteger balance = BigInteger.valueOf(0);
      for (MoneroTx tx : txs) {
        if (tx.getOutputs() == null) continue;
        for (MoneroOutput output : tx.getOutputs()) {
          if (!output.getIsSpent()) {
            balance = balance.add(output.getAmount());
          }
        }
      }
      assertEquals(wallet.getBalance(account.getIndex()), balance);
    }
    
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
    
    // assert at least some transactions filtered
    int unfilteredCount = txs.size();
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setMinHeight(minHeight);
    filter.setMaxHeight(maxHeight);
    txs = wallet.getTxs(filter);
    assertFalse(txs.isEmpty());
    assertTrue(txs.size() < unfilteredCount);
    for (MoneroTx tx : txs) {
      assertTrue(tx.getHeight() >= minHeight && tx.getHeight() <= maxHeight);
    }
    
    // test filtering by subaddress
    for (MoneroAccount account : wallet.getAccounts()) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      for (MoneroSubaddress subaddress : subaddresses) {
        filter = new MoneroTxFilter();
        filter.setAccountIdx(account.getIndex());
        filter.setSubaddressIndices(Arrays.asList(subaddress.getIndex()));
        txs = wallet.getTxs(filter);
        
        // test that tx amounts add up to subaddress balance
        BigInteger balance = BigInteger.valueOf(0);
        for (MoneroTx tx : txs) {
          if (tx.getOutputs() == null) continue;
          for (MoneroOutput output : tx.getOutputs()) {
            if (!output.getIsSpent()) {
              balance = balance.add(output.getAmount());
            }
          }
        }
        assertEquals(wallet.getBalance(account.getIndex(), subaddress.getIndex()), balance);
      }
    }
    
    // assert that ummet filter criteria has no results
    filter = new MoneroTxFilter();
    filter.setAccountIdx(0);
    Collection<Integer> subaddressIndices = new HashSet<Integer>();
    subaddressIndices.add(1234907);
    filter.setSubaddressIndices(subaddressIndices);
    txs = wallet.getTxs(filter);
    assertTrue(txs.isEmpty());
    
    // test getting transactions by transaction ids
    Collection<String> txIds = new HashSet<String>();
    for (MoneroTx tx : allTxs) txIds.add(tx.getId());
    assertFalse(txIds.isEmpty());
    filter = new MoneroTxFilter();
    filter.setTxIds(txIds);    
    txs = wallet.getTxs(filter);
    assertEquals(allTxs.size(), txs.size());
    for (String txId : txIds) {
      filter = new MoneroTxFilter();
      filter.setTxIds(Arrays.asList(txId));
      txs = wallet.getTxs(filter);
      assertEquals(1, txs.size());
      assertEquals(txId, txs.get(0).getId());
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
  public void testGetTxNotes() {
    List<MoneroTx> txs = wallet.getTxs();
    assertFalse(txs.isEmpty());
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) txIds.add(tx.getId());
    List<String> txNotes = wallet.getTxNotes(txIds);
    assertEquals(txs.size(), txNotes.size());
    for (String txNote : txNotes) assertNotNull(txNote);
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
  public void testAddressBookEntries() {
    
    // initial state
    List<MoneroAddressBookEntry> entries = wallet.getAddressBookEntries();
    int numEntriesStart = entries.size();
    for (MoneroAddressBookEntry entry : entries) testAddressBookEntry(entry);
    
    // test adding standard addresses
    final int NUM_ENTRIES = 5;
    MoneroAddress address = wallet.getSubaddress(0, 0).getAddress();
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
          testAddressBookEntry(entry);
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
      int idx = wallet.addAddressBookEntry(integratedAddress, null);
      indices.add(idx);
      integratedAddresses.put(idx, integratedAddress);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart + NUM_ENTRIES, entries.size());
    for (Integer idx : indices) {
      boolean found = false;
      for (MoneroAddressBookEntry entry : entries) {
        if (idx == entry.getIndex()) {
          testAddressBookEntry(entry);
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

  @Test
  public void testGetLanguages() {
    List<String> languages = wallet.getLanguages();
    assertFalse(languages.isEmpty());
    System.out.println(languages);
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
    assertTrue(wallet.getSubaddresses(0).size() > 1);
    verified = wallet.verify(msg, wallet.getSubaddress(0, 1).getAddress().toString(), signature);
    assertFalse(verified);
  }

  @Test
  public void testUri() {
    
    // test with optional fields as null
    MoneroUri mUri1 = new MoneroUri();
    mUri1.setAddress(wallet.getSubaddress(0, 0).getAddress().getStandardAddress());
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
  public void testRescanBlockchain() {
    wallet.rescanBlockchain();
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
  
  // --------------------------------- PRIVATE --------------------------------
  
  private static void testAccount(MoneroAccount account) {
    assertTrue(account.getIndex() >= 0);
    assertTrue(account.getBalance().doubleValue() >= 0);
    assertTrue(account.getUnlockedBalance().doubleValue() >= 0);
    assertFalse(account.isMultisigImportNeeded());
    if (account.getSubaddresses() != null) {
      for (int i = 0; i < account.getSubaddresses().size(); i++) {
        testSubaddress(account.getSubaddresses().get(i));
      }
    }
  }
  
  private static void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getIndex() >= 0);
    assertTrue(subaddress.getBalance().doubleValue() >= 0);
    assertTrue(subaddress.getUnlockedBalance().doubleValue() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    if (subaddress.getBalance().doubleValue() >= 0) assertTrue(subaddress.isUsed());
  }
  
  private static void testTransaction(MoneroTx tx) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getType());
    if (tx.getType() == MoneroTxType.OUTGOING) {
      assertNotNull(tx.getAmount());
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
  }
  
  private static void testAddressBookEntry(MoneroAddressBookEntry entry) {
    assertTrue(entry.getIndex() >= 0);
    assertNotNull(entry.getAddress());
    assertNotNull(entry.getDescription());
  }
}
