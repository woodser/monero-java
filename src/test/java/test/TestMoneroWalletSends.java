package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxConfig;
import utils.PrintBalances;
import utils.TestUtils;

/**
 * Tests sending funds from a Monero wallet.
 * 
 * These tests are separated because they rely on a balance and initiate transactions on the blockchain.
 * 
 * TODO: anyway to get around tx not possible?  need to refresh spend?
 */
public class TestMoneroWalletSends {
  
  private static final int SEND_DIVISOR = 2;
  private static final int SEND_MAX_DIFF = 60;
  private static MoneroWallet wallet;

  @BeforeClass
  public static void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  @AfterClass
  public static void teardown() {
    PrintBalances.printBalances();
  }
  
  @Test
  public void testSendToSingle() {
    testSendToSingle(false, null);
  }
  
  @Test
  public void testSendToSinglePaymentId() {
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(null);
    testSendToSingle(false, integratedAddress.getPaymentId());
  }
  
  @Test
  public void testSendSplitToSingle() {
    testSendToSingle(true, null);
  }
  
  private void testSendToSingle(boolean canSplit, String paymentId) {
    
    // find a non-primary subaddress to send from
    boolean sufficientBalance = false;
    MoneroAccount fromAccount = null;
    MoneroSubaddress fromSubaddress = null;
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = account.getSubaddresses();
      for (int i = 1; i < subaddresses.size(); i++) {
        if (subaddresses.get(i).getBalance().compareTo(TestUtils.MAX_FEE) > 0) sufficientBalance = true;
        if (subaddresses.get(i).getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) {
          fromAccount = account;
          fromSubaddress = subaddresses.get(i);
          break;
        }
      }
      if (fromAccount != null) break;
    }
    assertTrue("No non-primary subaddress found with sufficient balance", sufficientBalance);
    assertTrue("Wallet is waiting on unlocked funds", fromSubaddress != null);
    
    // get balance before send
    BigInteger balanceBefore = fromSubaddress.getBalance();
    BigInteger unlockedBalanceBefore  = fromSubaddress.getBalance();
        
    // send to self
    BigInteger sendAmount = unlockedBalanceBefore.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    MoneroTxConfig config = new MoneroTxConfig(address, paymentId, sendAmount, TestUtils.MIXIN);
    config.setAccountIndex(fromAccount.getIndex());
    config.setSubaddressIndices(Arrays.asList(fromSubaddress.getIndex()));
    if (canSplit) {
      txs.addAll(wallet.sendSplit(config));
    } else {
      txs.add(wallet.send(config));
    }
    
    // test that balance and unlocked balance decreased
    MoneroSubaddress subaddress = wallet.getSubaddress(fromAccount.getIndex(), fromSubaddress.getIndex());
    assertTrue(subaddress.getBalance().compareTo(balanceBefore) < 0);
    assertTrue(subaddress.getUnlockedBalance().compareTo(unlockedBalanceBefore) < 0);
    
    // test transactions
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      TestUtils.testSendTx(tx, config, !canSplit, !canSplit, wallet);
      assertEquals(fromAccount.getIndex(), tx.getSrcAccountIdx());
      assertEquals((Integer) 0, tx.getSrcSubaddressIdx()); // TODO (monero-wallet-rpc): outgoing transactions do not indicate originating subaddresses
      assertEquals(sendAmount, tx.getTotalAmount());
      
      // test tx payments
      if (tx.getPayments() != null) {
        assertEquals(1, tx.getPayments().size());
        for (MoneroPayment payment : tx.getPayments()) {
          assertTrue(tx == payment.getTx());
          assertEquals(address, payment.getAddress());
          assertEquals(sendAmount, payment.getAmount());
          assertNull(payment.getAccountIdx());
          assertNull(payment.getSubaddressIdx());
          assertNull(payment.getIsSpent());
        }
      }
    }
  }
  
  @Test
  public void testSendToMultiple() {
    testSendToMultiple(10, 3, false);
  }
  
  @Test
  public void testSendSplitToMultiple() {
    testSendToMultiple(20, 3, true);
  }
  
  /**
   * Fans funds from the first unlocked account across numerous accounts and subaddresses.
   * 
   * @param numAccounts is the number of accounts to receive funds
   * @param numSubaddressesPerAccount is the number of subaddresses per account to receive funds
   * @param canSplit specifies if the operation can be split into multiple transactions
   */
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit) {
    
    // test constants
    int totalSubaddresses = numAccounts * numSubaddressesPerAccount;
    
    // send funds from first account with unlocked funds
    MoneroAccount srcAccount = null;
    boolean hasBalance = true;
    for (MoneroAccount account : wallet.getAccounts()) {
      if (account.getBalance().subtract(TestUtils.MAX_FEE).compareTo(BigInteger.valueOf(0)) > 0) hasBalance = true;
      if (account.getUnlockedBalance().subtract(TestUtils.MAX_FEE).compareTo(BigInteger.valueOf(0)) > 0) {
        srcAccount = account;
        break;
      }
    }
    assertTrue("Wallet is empty; load '" + TestUtils.WALLET_NAME_1 + "' with XMR in order to test sending", hasBalance);
    assertNotNull("Wallet is waiting on unlocked funds", srcAccount);
    
    // get amount to send per address
    BigInteger balance = srcAccount.getBalance();
    BigInteger unlockedBalance = srcAccount.getUnlockedBalance();
    BigInteger sendAmount = unlockedBalance.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    BigInteger sendAmountPerSubaddress = sendAmount.divide(BigInteger.valueOf(totalSubaddresses));
    
    // create minimum number of accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    for (int i = 0; i < numAccounts - accounts.size(); i++) {
      wallet.createAccount();
    }
    
    // create minimum number of subaddresses per account and collect destination addresses
    List<String> destinationAddresses = new ArrayList<String>();
    for (int i = 0; i < numAccounts; i++) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(i);
      for (int j = 0; j < numSubaddressesPerAccount - subaddresses.size(); j++) wallet.createSubaddress(i);
      subaddresses = wallet.getSubaddresses(i);
      assertTrue(subaddresses.size() >= numSubaddressesPerAccount);
      for (int j = 0; j < numSubaddressesPerAccount; j++) destinationAddresses.add(subaddresses.get(j).getAddress());
    }
        
    // send to subaddresses
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    for (int i = 0; i < destinationAddresses.size(); i++) {
      MoneroPayment payment = new MoneroPayment();
      payments.add(payment);
      payment.setAddress(destinationAddresses.get(i));
      payment.setAmount(sendAmountPerSubaddress);
    }
    MoneroTxConfig config = new MoneroTxConfig();
    config.setMixin(TestUtils.MIXIN);
    config.setAccountIndex(srcAccount.getIndex());
    config.setDestinations(payments);
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    if (canSplit) {
      txs.addAll(wallet.sendSplit(config));
    } else {
      txs.add(wallet.send(config));
    }
    
    // test that wallet balance decreased
    MoneroAccount account = wallet.getAccount(srcAccount.getIndex());
    assertTrue(account.getBalance().compareTo(balance) < 0);
    assertTrue(account.getUnlockedBalance().compareTo(unlockedBalance) < 0);
    
    // test each transaction
    assertFalse(txs.isEmpty());
    BigInteger txSum = BigInteger.valueOf(0);
    for (MoneroTx tx : txs) {
      TestUtils.testSendTx(tx, config, !canSplit, !canSplit, wallet);
      txSum = txSum.add(tx.getTotalAmount());
      if (tx.getPayments() != null) {
        BigInteger paymentSum = BigInteger.valueOf(0);
        for (MoneroPayment payment : tx.getPayments()) {
          assertTrue(tx == payment.getTx());
          assertTrue(destinationAddresses.contains(payment.getAddress()));
          paymentSum = paymentSum.add(payment.getAmount());
        }
        assertEquals(tx.getId(), tx.getTotalAmount(), paymentSum);  // assert that payments sum up to tx amount
      }
    }
    
    // assert that tx amounts sum up the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(txSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      fail("Tx amounts are too different: " + sendAmount + " - " + txSum + " = " + sendAmount.subtract(txSum));
    }
  }
  
  @Test
  public void testSendFromMultiple() {
    testSendFromMultiple(false);
  }
  
  @Test
  public void testSendSplitFromMultiple() {
    testSendFromMultiple(true);
  }

  private void testSendFromMultiple(boolean canSplit) {
    
    int NUM_SUBADDRESSES = 2; // number of subaddresses to send from
    
    // get first account with (NUM_SUBADDRESSES + 1) subaddresses with unlocked balances
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertTrue("This test requires at least 2 accounts.  Run testSendToMultiple() first", accounts.size() >= 2);
    MoneroAccount srcAccount = null;
    List<MoneroSubaddress> unlockedSubaddresses = new ArrayList<MoneroSubaddress>();
    boolean hasBalance = false;
    for (MoneroAccount account : accounts) {
      unlockedSubaddresses.clear();
      int numSubaddressBalances = 0;
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0) numSubaddressBalances++;
        if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) unlockedSubaddresses.add(subaddress);
      }
      if (numSubaddressBalances >= NUM_SUBADDRESSES + 1) hasBalance = true;
      if (unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1) {
        srcAccount = account;
        break;
      }
    }
    assertTrue("Wallet does not have account with " + (NUM_SUBADDRESSES + 1) + " subaddresses with balances.  Run testSendToMultiple() first", hasBalance);
    assertTrue("Wallet is waiting on unlocked funds", unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1);
    
    // determine the indices of the first two subaddresses with unlocked balances
    List<Integer> fromSubaddressIndices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_SUBADDRESSES; i++) {
      fromSubaddressIndices.add(unlockedSubaddresses.get(i).getIndex());
    }
    
    System.out.println("Sending from [" + srcAccount.getIndex() + "," + fromSubaddressIndices + "]");
    
    // determine the amount to send (slightly less than the sum to send from)
    BigInteger sendAmount = BigInteger.valueOf(0);
    for (Integer fromSubaddressIdx : fromSubaddressIndices) {
      sendAmount = sendAmount.add(srcAccount.getSubaddresses().get(fromSubaddressIdx).getUnlockedBalance());
    }
    sendAmount = sendAmount.subtract(TestUtils.MAX_FEE);
    
    // send from the first subaddresses with unlocked balances
    String address = wallet.getPrimaryAddress();
    MoneroTxConfig config = new MoneroTxConfig(address, null, sendAmount);
    config.setAccountIndex(srcAccount.getIndex());
    config.setSubaddressIndices(fromSubaddressIndices);
    config.setMixin(TestUtils.MIXIN);
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    if (canSplit) {
      txs.addAll(wallet.sendSplit(config));
    } else {
      txs.add(wallet.send(config));
    }
    
    for (MoneroTx tx : txs) System.out.println("FEE: " + tx.getFee());
    
    // test that balances of intended subaddresses decreased
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    for (int i = 0; i < accounts.size(); i++) {
      assertEquals(accounts.get(i).getSubaddresses().size(), accountsAfter.get(i).getSubaddresses().size());
      for (int j = 0; j < accounts.get(i).getSubaddresses().size(); j++) {
        MoneroSubaddress subaddressBefore = accounts.get(i).getSubaddresses().get(j);
        MoneroSubaddress subaddressAfter = accountsAfter.get(i).getSubaddresses().get(j);
        if (i == srcAccount.getIndex() && fromSubaddressIndices.contains(j)) {
          assertTrue("Subaddress [" + i + "," + j + "] unlocked balance should have decreased", subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) < 0); // TODO: Subaddress [0,1] unlocked balance should have decreased          
        } else {
          assertTrue("Subaddress [" + i + "," + j + "] unlocked balance should not have changed", subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) == 0);          
        }
      }
    }
    
    // test each transaction
    assertFalse(txs.isEmpty());
    BigInteger txSum = BigInteger.valueOf(0);
    for (MoneroTx tx : txs) {
      TestUtils.testSendTx(tx, config, !canSplit, !canSplit, wallet);
      txSum = txSum.add(tx.getTotalAmount());
      if (tx.getPayments() != null) {
        assertEquals(tx.getId(), 1, tx.getPayments().size());
        BigInteger paymentSum = BigInteger.valueOf(0);
        for (MoneroPayment payment : tx.getPayments()) {
          assertEquals(tx.getId(), address, payment.getAddress());
          assertTrue(tx.getId(), tx == payment.getTx());
          paymentSum = paymentSum.add(payment.getAmount());
        }
        assertEquals(tx.getId(), tx.getTotalAmount(), paymentSum);  // assert that payment amounts sum up to tx amount
      }
    }
    
    // assert that tx amounts sum up the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(txSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      fail("Tx amounts are too different: " + sendAmount + " - " + txSum + " = " + sendAmount.subtract(txSum));
    }
  }
  
  @Ignore // TODO: no dust to sweep to test always fails
  @Test
  public void testSweepDust() {
    List<MoneroTx> txs = wallet.sweepDust();
    assertFalse("No dust to sweep", txs.isEmpty());
    for (MoneroTx tx : txs) {
      System.out.println("SWEEP TRANSACTION\n" + tx);
      TestUtils.testSendTx(tx, null, true, true, wallet);
    }
  }
}
