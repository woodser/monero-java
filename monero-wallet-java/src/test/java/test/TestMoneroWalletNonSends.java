package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import utils.TestUtils;
import wallet.MoneroAddress;
import wallet.MoneroIntegratedAddress;
import wallet.MoneroOutput;
import wallet.MoneroRpcException;
import wallet.MoneroTransaction;
import wallet.MoneroTransaction.MoneroTransactionType;
import wallet.MoneroUri;
import wallet.MoneroUtils;
import wallet.MoneroWallet;

/**
 * Tests a Monero wallet excluding sending transactions.
 * 
 * Monero-wallet-rpc suggested improvements:
 * 
 * - tx_hash in incoming_transfers vs txid in get_transfers; standardize on tx_hash or tx_id
 * - standardize terminology transactions vs transfers
 * - standardize terminology destinations vs outputs vs payments (I like payment which is address + amount)
 * - consistently return all possible fields
 * - get_transfers returns all transactions whereas incoming_transfers returns incoming outputs; clarify terminology or both could return transactions for consistency
 * - why doesn't tx_size get returned on get_transfers
 * - standardize terminology payment vs output; one payment may be fulfilled with multiple outputs
 * - no way to get transaction keys after sending
 * - height vs block_height inconsistent
 * - key is never returned as part of get transactions and their variations
 * 
 * @author woodser
 */
public class TestMoneroWalletNonSends {
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  @Test
  public void testGetHeight() {
    int height = wallet.getHeight();
    assertTrue(height >= 0);
  }

  @Test
  public void testGetBalance() {
    BigInteger balance = wallet.getBalance();
    assertTrue(balance.longValue() >= 0.0);
  }
  
  @Test
  public void getUnlockedBalance() {
    BigInteger unlocked = wallet.getUnlockedBalance();
    assertTrue(unlocked.longValue() >= 0.0);
  }

  @Test
  public void testGetStandardAddress() {
    MoneroAddress address = wallet.getStandardAddress();
    MoneroUtils.validateAddress(address);
  }

  @Test
  public void testGetIntegratedAddress() {
    
    // save address for later comparison
    MoneroAddress address = wallet.getStandardAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(address.getStandardAddress(), integratedAddress.getStandardAddress());
    assertEquals(paymentId, integratedAddress.getPaymentId());
    
    // test invalid payment id
    try {
      String invalidPaymentId = "invalid_payment_id_123456";
      integratedAddress = wallet.getIntegratedAddress(invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown a RPC exception");
    } catch (MoneroRpcException e) {
      assertEquals((int) -5, (int) e.getRpcCode());
      assertEquals("Invalid payment ID", e.getRpcMessage());
    }
    
    // test null payment id which generates a new one
    integratedAddress = wallet.getIntegratedAddress(null);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(address.getStandardAddress(), integratedAddress.getStandardAddress());
    assertNotNull(integratedAddress.getPaymentId());
  }

  @Test
  public void testSplitIntegratedAddress() {
    
    // cache info to test against
    MoneroAddress address = wallet.getStandardAddress();
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    MoneroUtils.validateAddress(integratedAddress);
    assertEquals(address.getStandardAddress(), integratedAddress.getStandardAddress());
    assertEquals(paymentId, integratedAddress.getPaymentId());
    
    // test split call
    MoneroIntegratedAddress split = wallet.splitIntegratedAddress(integratedAddress.getIntegratedAddress());
    assertEquals(address.getStandardAddress(), split.getStandardAddress());
    assertEquals(paymentId, split.getPaymentId());
    assertEquals(integratedAddress.getIntegratedAddress(), split.getIntegratedAddress());
    
    // test with invalid integrated address
    try {
      wallet.splitIntegratedAddress(integratedAddress.getIntegratedAddress() + " some invalid characters");
      fail("Splitting invalid integrated address should throw a RPC exception");
    } catch (MoneroRpcException e) {
      assertEquals((int) -2, (int) e.getRpcCode());
      assertEquals("Invalid address", e.getRpcMessage());
    }
  }

  @Test
  public void testGetMnemonicSeed() {
    String seed = wallet.getMnemonicSeed();
    MoneroUtils.validateMnemonicSeed(seed);
  }

  @Test
  public void testGetViewKey() {
    String viewKey = wallet.getViewKey();
    MoneroUtils.validateViewKey(viewKey);
  }

  @Test
  public void testUriParsing() {
    
    // test with optional fields as null
    MoneroUri mUri1 = new MoneroUri();
    mUri1.setAddress(wallet.getStandardAddress().getStandardAddress());
    URI uri = wallet.toUri(mUri1);
    MoneroUri mUri2 = wallet.toMoneroUri(uri);
    assertTrue(mUri1.equals(mUri2));
    
    // test with all fields
    mUri1.setAmount(BigInteger.valueOf((Long.parseLong("425000000000"))));
    mUri1.setPaymentId("03284e41c342f036");
    mUri1.setRecipientName("John Doe");
    mUri1.setTxDescription("OMZG XMR FTW");
    uri = wallet.toUri(mUri1);
    mUri2 = wallet.toMoneroUri(uri);

    assertTrue(mUri1.equals(mUri2));
    
    // test with address null
    mUri1.setAddress(null);
    mUri1.setPaymentId("bizzup");
    try {
      wallet.toUri(mUri1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroRpcException e) {
      assertEquals((int) -11, (int) e.getRpcCode());
      assertTrue(e.getRpcMessage().contains("Cannot make URI from supplied parameters"));
    }
  }

  @Test
  public void testSaveBlockchain() {
    wallet.saveBlockchain();
  }

  @Ignore // disabled so tests don't actually stop the wallet
  @Test
  public void testStopWallet() {
    wallet.stopWallet();
  }
  
  @Test
  public void testGetAllTransactions() {
    List<MoneroTransaction> txs = wallet.getAllTransactions();
    assertFalse(txs.isEmpty());
    for (MoneroTransaction tx : txs) {
      assertNotNull(tx.getHash());
      assertNotNull(tx.getType());
    }
  }
  
  @Test
  public void testGetAllTransactionsOptions() {
    
    // test getting transactions by payment id
    String paymentId = "0000000000000000";
    List<String> paymentIds = new ArrayList<String>();
    paymentIds.add(paymentId);
    List<MoneroTransaction> txs = wallet.getTransactions(true, true, true, true, true, paymentIds, null, null);
    assertFalse(txs.isEmpty());
    for (MoneroTransaction tx : txs) {
      assertNotNull(tx.getType());
      assertEquals(paymentId, tx.getPaymentId());
    }
    
    // test getting incoming transactions
    txs = wallet.getTransactions(true, false, false, false, false, null, null, null);
    assertFalse(txs.isEmpty());
    for (MoneroTransaction tx : txs) {
      assertEquals(MoneroTransactionType.INCOMING, tx.getType());
    }
    
    // test getting outgoing transactions
    txs = wallet.getTransactions(false, true, false, false, false, null, null, null);
    assertFalse(txs.isEmpty());
    for (MoneroTransaction tx : txs) {
      assertEquals(MoneroTransactionType.OUTGOING, tx.getType());
    }
    
    // test balance equals spendable outputs
    txs = wallet.getAllTransactions();
    assertFalse(txs.isEmpty());
    BigInteger balance = BigInteger.valueOf(0);
    for (MoneroTransaction tx : txs) {
      if (tx.getOutputs() == null) continue;
      for (MoneroOutput output : tx.getOutputs()) {
        if (!output.getIsSpent()) {
          balance = balance.add(output.getAmount());
        }
      }
    }
    assertEquals(wallet.getBalance(), balance);
    
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
    txs = wallet.getTransactions(true, true, true, true, true, null, minHeight, maxHeight);
    assertFalse(txs.isEmpty());
    assertTrue(txs.size() < unfilteredCount);
    for (MoneroTransaction tx : txs) {
      assertTrue(tx.getHeight() >= minHeight && tx.getHeight() <= maxHeight);
    }
  }
}
