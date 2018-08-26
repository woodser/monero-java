package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import model.MoneroAddress;
import model.MoneroIntegratedAddress;
import service.MoneroWallet;
import service.rpc.MoneroRpcException;
import utils.MoneroUtils;
import utils.TestUtils;

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
    fail("Not yet implemented");
  }

  @Test
  public void testGetAccountsString() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetAccount() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateAccount() {
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

  @Test
  public void testGetSubaddressesInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetSubaddress() {
    fail("Not yet implemented");
  }

}
