package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import model.MoneroAddress;
import model.MoneroPayment;
import model.MoneroTx;
import utils.TestUtils;
import wallet.MoneroWallet;

/**
 * Tests sending transactions using a Monero wallet.
 * 
 * These tests are separated because they rely on a balance and initiate transactions on the blockchain.
 */
public class TestMoneroWalletSends {
  
  private static final Integer MIXIN = 6;
  private static final int UNLOCKED_DIVISOR = 20;
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  // TODO: test more than account 0
  @Test
  public void testSend0() {
    
    // get balance before
    BigInteger balanceBefore = wallet.getBalance(0);
    MoneroWallet wallet = TestUtils.getWallet();
    System.out.println("Balance: " + wallet.getBalance(0));
    System.out.println("Unlocked: " + wallet.getUnlockedBalance(0));
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance(0);
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", balanceBefore.longValue() > 0);
    assertTrue("Wallet is waiting on unlocked funds", unlockedBalanceBefore.longValue() > 0);
    
    // send to self
    MoneroAddress address = wallet.getSubaddress(0, 0).getAddress();
    BigInteger sendAmount = unlockedBalanceBefore.divide(BigInteger.valueOf(UNLOCKED_DIVISOR));
    MoneroTx tx = wallet.send(address.getStandardAddress(), null, sendAmount, MIXIN);
    
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
    assertEquals((Integer) 0, tx.getUnlockTime());
    assertEquals(sendAmount, tx.getAmount());
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
    fail("Not yet implemented");
  }

  @Test
  public void testSweepAll() {
    fail("Not yet implemented");
  }

  @Test
  public void testSweepDust() {
    List<MoneroTx> txs = wallet.sweepDust();
    for (MoneroTx tx : txs) {
      assertNull(tx.getPayments());
      assertNull(tx.getAmount());
      assertNull(tx.getFee());
      assertNull(tx.getMixin());
      assertNull(tx.getKey());
      assertNotNull(tx.getHash());
      assertNull(tx.getSize());
      assertNull(tx.getType());
      assertNull(tx.getHeight());
      assertNull(tx.getBlob());
      assertNull(tx.getMetadata());
    }
  }
}
