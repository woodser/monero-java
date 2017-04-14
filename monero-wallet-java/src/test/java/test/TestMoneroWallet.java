package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import com.google.common.primitives.UnsignedInteger;

import utils.MoneroUtils;
import wallet.MoneroAddress;
import wallet.MoneroIntegratedAddress;
import wallet.MoneroRpcException;
import wallet.MoneroWallet;
import wallet.MoneroWalletRpc;

/**
 * Tests a MoneroWallet implementation.
 * 
 * @author woodser
 */
public class TestMoneroWallet {
  
  private static String DOMAIN = "localhost";
  private static int PORT = 18082;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = new MoneroWalletRpc(DOMAIN, PORT);
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
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown an RPC exception");
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
  public void testSendTransactionStringUnsignedIntegerUnsignedIntegerIntInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendTransactionMoneroPayment() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendTransactionSetOfMoneroPaymentUnsignedIntegerIntInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepDust() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetTransactions() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetMnemonicSeed() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetViewKey() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetUriMoneroUri() {
    fail("Not yet implemented");
  }

  @Test
  public void testParseUri() {
    fail("Not yet implemented");
  }

  @Test
  public void testSaveBlockchain() {
    fail("Not yet implemented");
  }

  @Test
  public void testStopWallet() {
    fail("Not yet implemented");
  }

  @Test
  public void testSplitIntegratedAddress() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendTransactionMoneroPaymentUnsignedIntegerIntInt() {
    fail("Not yet implemented");
  }

}
