package test;

import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.junit.Before;
import org.junit.Test;

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
  private static final BigInteger SEND_AMOUNT = BigInteger.valueOf(100);
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = new MoneroWalletRpc(DOMAIN, PORT);
  }

  @Test
  public void testSendTransactionStringBigIntegerBigIntegerIntInt() {
    System.out.println(wallet.getBalance());
  }

  @Test
  public void testSendTransactionMoneroPaymentBigIntegerIntInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testSendTransactionSetOfMoneroPaymentBigIntegerIntInt() {
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
