package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.junit.Before;
import org.junit.Test;

import utils.TestUtils;
import wallet.MoneroAddress;
import wallet.MoneroTransaction;
import wallet.MoneroTransactionType;
import wallet.MoneroWallet;

/**
 * Tests sending transactions within a Monero wallet.
 * 
 * These tests are separated since they rely on a balance and initiate transactions on the blockchain.
 * 
 * @author woodser
 */
public class TestMoneroWalletTransactions {
  
  private static final BigInteger SEND_AMOUNT = BigInteger.valueOf(Long.valueOf("1000000000000")); // 1 XMR
  private static final BigInteger FEE = null;
  private static final int MIXIN = 5;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }

  @Test
  public void testSendTransactionStringBigIntegerBigIntegerIntInt() {
    
    // get balance before
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    
    // send to self
    MoneroAddress address = wallet.getStandardAddress();
    MoneroTransaction tx = wallet.sendTransaction(address.toString(), SEND_AMOUNT, null, FEE, MIXIN, 0);
    
    // test response
    assertNotNull(tx.getPayment());
    assertEquals(address, tx.getPayment().getAddress());
    assertEquals(SEND_AMOUNT, tx.getPayment().getAmount());
    assertTrue(tx.getPayment().getBlockHeight() > 0);
    assertTrue(tx == tx.getPayment().getTransaction());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getTxKey());
    assertNotNull(tx.getTxHash());
    assertTrue(tx.getSize() > 0);
    assertEquals(MoneroTransactionType.PENDING, tx.getType());
    assertTrue(wallet.getBalance().longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance().longValue() < unlockedBalanceBefore.longValue());
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
