package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import com.google.common.primitives.UnsignedInteger;

import types.Pair;
import utils.MoneroUtils;
import wallet.MoneroException;
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
    String standardAddress = wallet.getStandardAddress();
    MoneroUtils.validateStandardAddress(standardAddress);
  }

  @Test
  public void testGetIntegratedAddress() {
    
    // test valid payment id
    String paymentId = "f014b1fc8729374d";
    String integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateIntegratedAddress(wallet.getStandardAddress(), paymentId, integratedAddress);
    
    // test invalid payment id
    try {
      String invalidPaymentId = "9d7610804e14b911";
      integratedAddress = wallet.getIntegratedAddress(invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have failed");
    } catch (MoneroException e) {
      e.printStackTrace();
      
      
      
      
      
      
      fail("Success but need to validate exception");
    }
    
    // test null payment id which generates a new one
    paymentId = null;
    integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateIntegratedAddress(wallet.getStandardAddress(), paymentId, integratedAddress);
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
