package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import com.google.common.primitives.UnsignedInteger;

import wallet.MoneroStandardAddress;
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
    MoneroStandardAddress address = wallet.getAddress();
    assertNotNull(address.getStandardAddress());
    assertEquals(95, address.getStandardAddress().length());
  }

  @Test
  public void testGetIntegratedAddress() {
    fail("Not yet implemented");
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
