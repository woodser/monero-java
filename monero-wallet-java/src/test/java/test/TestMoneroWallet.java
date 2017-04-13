package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import com.google.common.primitives.UnsignedInteger;

import utils.MoneroUtils;
import wallet.MoneroAddress;
import wallet.MoneroException;
import wallet.MoneroIntegratedAddress;
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
  public void testGetAddress() {
    MoneroAddress address = wallet.getAddress();
    MoneroUtils.validateAddress(address);
  }

  @Test
  public void testGetIntegratedAddress() {
    
    // test valid payment id
    String paymentId = "f014b1fc8729374d";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(paymentId, integratedAddress.getPaymentId());
    assertEquals(wallet.getAddress().getStandardAddress(), integratedAddress.getStandardAddress());
    
    // test invalid payment id
    try {
      String invalidPaymentId = "f014b1fc8729374z";
      wallet.getIntegratedAddress(invalidPaymentId);
    } catch (MoneroException e) {
      fail("Success but need to validate exception");
    }
    
    // test null payment id which generates a new one
    integratedAddress = wallet.getIntegratedAddress(null);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(wallet.getAddress().getStandardAddress(), integratedAddress.getStandardAddress());
  }

  @Test
  public void testSendTransactionMoneroAddressUIntegerUIntegerIntInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendTransactionMoneroPayment() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendTransactionSetOfMoneroPaymentUIntegerIntInt() {
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
  public void testGetSpendKey() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetViewKey() {
    fail("Not yet implemented");
  }

  @Test
  public void testSave() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetUri() {
    fail("Not yet implemented");
  }

  @Test
  public void testParseUri() {
    fail("Not yet implemented");
  }
}
