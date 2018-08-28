package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import model.MoneroAccount;
import model.MoneroAddress;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import model.MoneroSubaddress;
import model.MoneroTx;
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
    fail("Not yet implemented");
  }

  @Test
  public void testGetTx() {
    fail("Not yet implemented");
  }

  @Test
  public void testSetTxNotes() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetTxNotes() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetKeyImages() {
    fail("Not yet implemented");
  }

  @Test
  public void testImportKeyImages() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetAddressBookEntries() {
    fail("Not yet implemented");
  }

  @Test
  public void testAddAddressBookEntry() {
    fail("Not yet implemented");
  }

  @Test
  public void testDeleteAddressBookEntry() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetLanguages() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateWallet() {
    fail("Not yet implemented");
  }

  @Test
  public void testOpenWallet() {
    fail("Not yet implemented");
  }

  @Test
  public void testSign() {
    fail("Not yet implemented");
  }

  @Test
  public void testVerify() {
    fail("Not yet implemented");
  }

  @Test
  public void testToUri() {
    fail("Not yet implemented");
  }

  @Test
  public void testToMoneroUri() {
    fail("Not yet implemented");
  }

  @Test
  public void testDecodeIntegratedAddress() {
    fail("Not yet implemented");
  }

  @Test
  public void testSaveBlockchain() {
    fail("Not yet implemented");
  }

  @Test
  public void testRescanBlockchain() {
    fail("Not yet implemented");
  }

  @Test
  public void testRescanSpent() {
    fail("Not yet implemented");
  }

  @Test
  public void testStopWallet() {
    fail("Not yet implemented");
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
    assertNotNull(tx.getHash());
    assertNotNull(tx.getType());
  }
}
