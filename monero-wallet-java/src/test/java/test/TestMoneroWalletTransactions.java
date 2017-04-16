package test;

import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import com.google.common.primitives.UnsignedInteger;

import wallet.MoneroWallet;
import wallet.MoneroWalletRpc;

/**
 * Tests sending transactions within a Monero wallet.
 * 
 * These tests are separated since they rely on a balance and initiate transactions on the blockchain.
 * 
 * @author woodser
 */
public class TestMoneroWalletTransactions {
  
  private static String DOMAIN = "localhost";
  private static int PORT = 18082;
  private static final UnsignedInteger SEND_AMOUNT = UnsignedInteger.valueOf(100);
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = new MoneroWalletRpc(DOMAIN, PORT);
  }

  @Test
  public void testSendTransactionStringUnsignedIntegerUnsignedIntegerIntInt() {
    System.out.println(wallet.getBalance());
  }

  @Test
  public void testSendTransactionMoneroPaymentUnsignedIntegerIntInt() {
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
}
