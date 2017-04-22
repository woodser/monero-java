package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import utils.TestUtils;
import wallet.MoneroAddress;
import wallet.MoneroPayment;
import wallet.MoneroTransaction;
import wallet.MoneroWallet;

/**
 * Tests sending transactions within a Monero wallet.
 * 
 * These tests are separated since they rely on a balance and initiate transactions on the blockchain.
 * 
 * @author woodser
 */
public class TestMoneroWalletTransactions {
  
  private static final BigInteger FEE = null;
  private static final Integer MIXIN = 6;
  private static final int UNLOCKED_DIVISOR = 20;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }

  @Test
  public void testTransferPayment() {
    
    // get balance before
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    
    // send to self
    MoneroAddress address = wallet.getStandardAddress();
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(UNLOCKED_DIVISOR));
    MoneroTransaction tx = wallet.transfer(address.toString(), sendAmount, null, FEE, MIXIN, 0);
    
    // test transaction
    assertNotNull(tx.getPayments());
    assertEquals(1, tx.getPayments().size());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getKey());
    assertNotNull(tx.getHash());
    assertNull(tx.getSize());
    assertNull(tx.getType());
    assertNull(tx.getHeight());
    
    // test payments
    for (MoneroPayment payment : tx.getPayments()) {
      assertEquals(address.toString(), payment.getAddress());
      assertEquals(sendAmount, payment.getAmount());
      assertTrue(tx == payment.getTransaction());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance().longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance().longValue() < unlockedBalanceBefore.longValue());
  }
  
  @Test
  public void testTransferPayments() {
    
    // get balance and address
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    MoneroAddress address = wallet.getStandardAddress();
    
    // create payments to send
    int numPayments = 3;
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(numPayments + UNLOCKED_DIVISOR));
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    for (int i = 0; i < numPayments; i++) {
      payments.add(new MoneroPayment(address.toString(), sendAmount));
    }
    
    // send payments
    MoneroTransaction tx = wallet.transfer(payments, null, FEE, MIXIN, 0);
    
    // test transaction
    assertNotNull(tx.getPayments());
    assertEquals(numPayments, tx.getPayments().size());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getKey());
    assertNotNull(tx.getHash());
    assertNull(tx.getSize());
    assertNull(tx.getType());
    assertNull(tx.getHeight());
    
    // test payments
    for (MoneroPayment payment : tx.getPayments()) {
      assertEquals(address.toString(), payment.getAddress());
      assertEquals(sendAmount, payment.getAmount());
      assertTrue(tx == payment.getTransaction());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance().longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance().longValue() < unlockedBalanceBefore.longValue());
  }
  
  @Test
  public void testTransferSplit() {
    
    // get balance and address
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    MoneroAddress address = wallet.getStandardAddress();
    
    // create payments to send
    int numPayments = 3;
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(numPayments + UNLOCKED_DIVISOR));
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    for (int i = 0; i < numPayments; i++) {
      payments.add(new MoneroPayment(address.toString(), sendAmount));
    }
    
    // send payments
    List<MoneroTransaction> txs = wallet.transferSplit(payments, null, FEE, MIXIN, 0, true);
    
    // test transactions
    for (MoneroTransaction tx : txs) {
      assertNull(tx.getPayments());
      assertTrue(tx.getFee().longValue() > 0);
      assertEquals(MIXIN, tx.getMixin());
      assertNull(tx.getKey());
      assertNotNull(tx.getHash());
      assertNull(tx.getHeight());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance().longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance().longValue() < unlockedBalanceBefore.longValue());
  }

  @Test
  public void testSweepDust() {
    List<MoneroTransaction> txs = wallet.sweepDust();
    for (MoneroTransaction tx : txs) {
      assertNull(tx.getPayments());
      assertNull(tx.getFee());
      assertNull(tx.getMixin());
      assertNull(tx.getKey());
      assertNotNull(tx.getHash());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
    }
  }

  @Test
  public void testGetTransactions() {
    
    // test all transactions
    List<MoneroTransaction> txs = wallet.getTransactions();
    assertTrue(txs != null);
    assertFalse(txs.isEmpty()); // must test at least one transaction
    for (MoneroTransaction tx : txs) {
      testTransaction(tx);
    }
    
    // get and sort block heights in ascending order
    List<Integer> heights = new ArrayList<Integer>();
    for (MoneroTransaction tx : txs) {
      if (tx.getHeight() != null) heights.add(tx.getHeight());
    }
    Collections.sort(heights);
    
    // pick minimum and maximum heights for filtering
    int minHeight = -1;
    int maxHeight = -1;
    if (heights.size() == 1) {
      minHeight = 0;
      maxHeight = heights.get(0) - 1;
    } else {
      minHeight = heights.get(0) + 1;
      maxHeight = heights.get(heights.size() - 1) - 1;
    }
    
    // assert at least some transactions filtered
    int unfilteredCount = txs.size();
    txs = wallet.getTransactions(minHeight, maxHeight);
    assertTrue(txs.size() < unfilteredCount);
    for (MoneroTransaction tx : txs) {
      assertTrue(tx.getHeight() >= minHeight && tx.getHeight() <= maxHeight);
      testTransaction(tx);
    }
  }
  
  private static void testTransaction(MoneroTransaction tx) {
    assertNotNull(tx.getType());
    switch (tx.getType()) {
      case INCOMING:
        assertNotNull(tx.getAmount());
        assertNotNull(tx.getFee());
        assertNotNull(tx.getHeight());
        assertNotNull(tx.getNote());
        assertNotNull(tx.getPaymentId());
        assertNotNull(tx.getTimestamp());
        assertNotNull(tx.getId());
        assertNotNull(tx.getType());
        assertNull(tx.getPayments());
        assertNull(tx.getKey());
        assertNull(tx.getHash());
        break;
      case OUTGOING:
        assertNotNull(tx.getAmount());
        assertNotNull(tx.getFee());
        assertNotNull(tx.getHeight());
        assertNotNull(tx.getNote());
        assertNotNull(tx.getPaymentId());
        assertNotNull(tx.getTimestamp());
        assertNotNull(tx.getId());
        assertNotNull(tx.getType());
        assertNotNull(tx.getPayments());
        assertNull(tx.getKey());
        assertNull(tx.getHash());
        break;
      case PENDING:
        assertNotNull(tx.getAmount());
        assertNotNull(tx.getFee());
        assertNotNull(tx.getHeight());
        assertNotNull(tx.getNote());
        assertNotNull(tx.getPaymentId());
        assertNotNull(tx.getTimestamp());
        assertNotNull(tx.getId());
        assertNotNull(tx.getType());
        assertNull(tx.getPayments());
        assertNull(tx.getKey());
        assertNull(tx.getHash());
        break;
      case FAILED:
        fail("No test data available to write test for transaction type " + tx.getType() + " even though it's probably working");
        break;
      case MEMPOOL:
        fail("No test data available to write test for transaction type " + tx.getType() + " even though it's probably working");
        break;
      default:
        fail("Unrecognized transaction type: " + tx.getType());
    }
  }
}
