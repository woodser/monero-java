package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
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
  private static final int MIXIN = 6;
  
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
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(5));
    MoneroTransaction tx = wallet.transfer(address.toString(), sendAmount, null, FEE, MIXIN, 0);
    
    // test transaction
    assertNotNull(tx.getPayments());
    assertEquals(1, tx.getPayments().size());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getTxKey());
    assertNotNull(tx.getTxHash());
    assertNull(tx.getBlockHeight());
    
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
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(numPayments + 5));
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
    assertNotNull(tx.getTxKey());
    assertNotNull(tx.getTxHash());
    assertNull(tx.getBlockHeight());
    
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
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(numPayments + 5));
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
      assertNull(tx.getTxKey());
      assertNotNull(tx.getTxHash());
      assertNull(tx.getBlockHeight());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance().longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance().longValue() < unlockedBalanceBefore.longValue());
  }

  @Test
  public void testSendTransactionsSplit() {
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
