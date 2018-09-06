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

import model.MoneroAddress;
import model.MoneroPayment;
import model.MoneroTx;
import model.MoneroTxConfig;
import utils.TestUtils;
import wallet.MoneroWallet;

/**
 * Tests sending transactions using a Monero wallet.
 * 
 * These tests are separated because they rely on a balance and initiate transactions on the blockchain.
 * 
 * TODO: support / test send, sendSplit, and sweepAll from specific accounts and subaddresses
 * TODO: test sending with payment id
 */
public class TestMoneroWalletSends {
  
  private static final Integer MIXIN = 6;
  private static final int SEND_DIVISOR = 2;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    try {
      wallet = TestUtils.getWallet();
    } catch (Exception e) {
      e.printStackTrace();
      throw e;
    }
  }
  
  @Test
  public void testSend() {
    
    // get balance before
    BigInteger balanceBefore = wallet.getBalance(0);
    MoneroWallet wallet = TestUtils.getWallet();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance(0);
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", balanceBefore.longValue() > 0);
    assertTrue("Wallet is waiting on unlocked funds", unlockedBalanceBefore.longValue() > 0);
    
    // send to self
    MoneroAddress address = wallet.getSubaddress(0, 0).getAddress();
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(SEND_DIVISOR));
    System.out.println("Balance: " + wallet.getBalance(0));
    System.out.println("Unlocked: " + wallet.getUnlockedBalance(0));
    System.out.println("Send amount: " + sendAmount);
    MoneroTx tx = wallet.send(address.getStandardAddress(), null, sendAmount, MIXIN);
    
    // test transaction
    assertEquals(sendAmount, tx.getAmount());
    assertNotNull(tx.getId());
    assertNotNull(tx.getPayments());
    assertEquals(1, tx.getPayments().size());
    assertTrue(tx.getFee().longValue() > 0);
    assertEquals(MIXIN, tx.getMixin());
    assertNotNull(tx.getKey());
    assertNull(tx.getSize());
    assertNull(tx.getType());
    assertNull(tx.getHeight());
    assertEquals((Integer) 0, tx.getUnlockTime());
    assertNotNull(tx.getBlob());
    assertNotNull(tx.getMetadata());
    
    // test payments
    for (MoneroPayment payment : tx.getPayments()) {
      assertEquals(address.toString(), payment.getAddress().getStandardAddress());
      assertEquals(sendAmount, payment.getAmount());
      assertTrue(tx == payment.getTransaction());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance(0).longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance(0).longValue() < unlockedBalanceBefore.longValue());
  }

  @Test
  public void testSendSplit() {
    
    // get balance and address
    BigInteger balanceBefore = wallet.getBalance(0);
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance(0);
    MoneroAddress address = wallet.getPrimaryAddress();
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", balanceBefore.longValue() > 0);
    assertTrue("Wallet is waiting on unlocked funds", unlockedBalanceBefore.longValue() > 0);
    
    // create payments to send
    int numPayments = 3;
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(numPayments + SEND_DIVISOR));
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    for (int i = 0; i < numPayments; i++) {
      payments.add(new MoneroPayment(address, sendAmount));
    }
    
    // send payments
    MoneroTxConfig config = new MoneroTxConfig();
    config.setDestinations(payments);
    config.setMixin(MIXIN);
    List<MoneroTx> txs = wallet.sendSplit(config);
    
    // test transactions
    for (MoneroTx tx : txs) {
      assertNotNull(tx.getId());
      assertNull(tx.getPayments());
      assertTrue(tx.getFee().longValue() > 0);
      assertEquals(MIXIN, tx.getMixin());
      assertNull(tx.getKey());
      assertNull(tx.getHeight());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
      assertEquals((Integer) 0, tx.getUnlockTime());
    }
    
    // test wallet balance
    assertTrue(wallet.getBalance(0).longValue() < balanceBefore.longValue());
    assertTrue(wallet.getUnlockedBalance(0).longValue() < unlockedBalanceBefore.longValue());
  }

  @Test
  public void testSweepAll() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepDust() {
    List<MoneroTx> txs = wallet.sweepDust();
    for (MoneroTx tx : txs) {
      assertNotNull(tx.getId());
      assertNull(tx.getPayments());
      assertNull(tx.getAmount());
      assertNull(tx.getFee());
      assertNull(tx.getMixin());
      assertNull(tx.getKey());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
      assertNull(tx.getBlob());
      assertNull(tx.getMetadata());
    }
  }
}
