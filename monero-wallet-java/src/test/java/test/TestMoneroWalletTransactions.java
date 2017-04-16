package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.junit.Before;
import org.junit.Test;

import wallet.MoneroAddress;
import wallet.MoneroPayment;
import wallet.MoneroTransaction;
import wallet.MoneroTransactionType;
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
  
  private static final String DOMAIN = "localhost";
  private static final int PORT = 18082;
  private static final BigInteger SEND_AMOUNT = BigInteger.valueOf(Long.valueOf("100000000000")); // 0.1 XMR
  private static final BigInteger FEE = null;
  private static final int MIXIN = 5;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = new MoneroWalletRpc(DOMAIN, PORT);
  }

  @Test
  public void testSendTransactionStringBigIntegerBigIntegerIntInt() {
    
    // send to self
    MoneroAddress address = wallet.getStandardAddress();
    MoneroTransaction tx = wallet.sendTransaction(address.toString(), SEND_AMOUNT, FEE, MIXIN, 0);
    
    // test response
    assertNotNull(tx.getPayments());
    assertEquals((int) 1, (int) tx.getPayments().size());
    MoneroPayment payment = tx.getPayments().get(0);
    assertEquals(address, payment.getAddress());
    assertEquals(SEND_AMOUNT, payment.getAmount());
    assertTrue(payment.getBlockHeight() > 0);
    assertTrue(tx == payment.getTransaction());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getTxKey());
    assertNotNull(tx.getTxHash());
    assertTrue(tx.getSize() > 0);
    assertEquals(MoneroTransactionType.PENDING, tx.getType());
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
