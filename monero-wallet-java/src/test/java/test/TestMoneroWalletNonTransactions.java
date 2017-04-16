package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.google.common.primitives.UnsignedInteger;

import utils.MoneroUtils;
import wallet.MoneroAddress;
import wallet.MoneroIntegratedAddress;
import wallet.MoneroRpcException;
import wallet.MoneroUri;
import wallet.MoneroWallet;
import wallet.MoneroWalletRpc;

/**
 * Tests a Monero wallet excluding sending transactions.
 * 
 * TODO: why are nulls getting serialized?
 * 
 * @author woodser
 */
public class TestMoneroWalletNonTransactions {
  
  private static String DOMAIN = "localhost";
  private static int PORT = 18082;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = new MoneroWalletRpc(DOMAIN, PORT);
  }
  
  @Test
  public void testGetHeight() {
    int height = wallet.getHeight();
    assertTrue(height >= 0);
  }

  @Test
  public void testGetBalance() {
    UnsignedInteger balance = wallet.getBalance();
    assertTrue(balance.longValue() >= 0.0);
  }
  
  @Test
  public void getUnlockedBalance() {
    UnsignedInteger unlocked = wallet.getUnlockedBalance();
    assertTrue(unlocked.longValue() >= 0.0);
  }

  @Test
  public void testGetStandardAddress() {
    MoneroAddress address = wallet.getStandardAddress();
    MoneroUtils.validateAddress(address);
  }

  @Test
  public void testGetIntegratedAddress() {
    
    // save address for later comparison
    MoneroAddress address = wallet.getStandardAddress();
    
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
  public void testSplitIntegratedAddress() {
    
    // cache info to test against
    MoneroAddress address = wallet.getStandardAddress();
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(address.getStandardAddress(), integratedAddress.getStandardAddress());
    assertEquals(paymentId, integratedAddress.getPaymentId());
    
    // test split call
    MoneroIntegratedAddress split = wallet.splitIntegratedAddress(integratedAddress.getIntegratedAddress());
    assertEquals(address.getStandardAddress(), split.getStandardAddress());
    assertEquals(paymentId, split.getPaymentId());
    assertEquals(integratedAddress.getIntegratedAddress(), split.getIntegratedAddress());
    
    // test with invalid integrated address
    try {
      wallet.splitIntegratedAddress(integratedAddress.getIntegratedAddress() + " some invalid characters");
      fail("Splitting invalid integrated address should throw a RPC exception");
    } catch (MoneroRpcException e) {
      assertEquals((int) -2, (int) e.getRpcCode());
      assertEquals("Invalid address", e.getRpcMessage());
    }
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
  public void testUriParsing() {
    
    // test with optional fields as null
    MoneroUri mUri1 = new MoneroUri();
    mUri1.setAddress(wallet.getStandardAddress().getStandardAddress());
    URI uri = wallet.toUri(mUri1);
    MoneroUri mUri2 = wallet.fromUri(uri);
    assertTrue(mUri1.equals(mUri2));
    
    // test with all fields
    mUri1.setAmount(UnsignedInteger.valueOf("4250000000"));
    mUri1.setPaymentId("03284e41c342f036");
    mUri1.setRecipientName("John Doe");
    mUri1.setTxDescription("OMZG XMR FTW");
    uri = wallet.toUri(mUri1);
    mUri2 = wallet.fromUri(uri);

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

  @Ignore
  @Test
  public void testStopWallet() {
    wallet.stopWallet();
  }
}
