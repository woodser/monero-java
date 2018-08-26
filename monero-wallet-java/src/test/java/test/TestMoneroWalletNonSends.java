package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import model.MoneroAccount;
import model.MoneroAddress;
import model.MoneroIntegratedAddress;
import model.MoneroSubaddress;
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
    assertTrue(accounts.size() > 0);
    for (MoneroAccount account : accounts) {
      testAccount(account);
    }
  }

  @Test
  public void testGetAccountsString() {
    List<MoneroAccount> accounts = wallet.getAccounts("fake_tag");
    assertEquals(0, accounts.size());
  }

  @Test
  public void testGetAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue(accounts.size() > 0);
    for (MoneroAccount account : accounts) {
      testAccount(wallet.getAccount(account.getIndex()));
    }
  }

  @Test
  public void testCreateAccount() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetSubaddressesInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetSubaddress() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetSubaddressesIntCollectionOfInteger() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateSubaddress() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBalanceInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBalanceIntInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetUnlockedBalanceInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetUnlockedBalanceIntInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testSend() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendSplit() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepAll() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepDust() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetTxs() {
    fail("Not yet implemented");
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
  public void testStartMining() {
    fail("Not yet implemented");
  }

  @Test
  public void testStopMining() {
    fail("Not yet implemented");
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
}
