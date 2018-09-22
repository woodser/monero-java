package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import monero.rpc.MoneroRpcException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroException;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImage;
import monero.wallet.model.MoneroOutput;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTx.MoneroTxType;
import monero.wallet.model.MoneroTxFilter;
import monero.wallet.model.MoneroUri;
import utils.PrintBalances;
import utils.TestUtils;

/**
 * Tests a Monero wallet excluding sending transactions.
 */
public class TestMoneroWalletRead {
  
  private MoneroWallet wallet;
  private static final String SAMPLE_ADDRESS = "58bf9MfrBNDXSqCzK6snxSXaJHehLTnvx3BdS6qMkYAsW8P5kvRVq8ePbGQ7mfAeYfC7QELPhpQBe2C9bqCrqeesUsifaWw";
  private static final Logger LOGGER = Logger.getLogger(TestMoneroWalletRead.class);

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }
  
  @After
  public void teardown() {
    PrintBalances.printBalances();
  }

  @Test
  public void testGetHeight() {
    int height = wallet.getHeight();
    assertTrue(height >= 0);
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
  public void testGetIntegratedAddress() {
    
    // save address for later comparison
    String address = wallet.getSubaddress(0, 0).getAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    assertEquals(address, integratedAddress.getStandardAddress());
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
    assertEquals(address, integratedAddress.getStandardAddress());
    assertNotNull(integratedAddress.getPaymentId());
  }
  
  @Test
  public void testGetPrimaryAddress() {
    String primaryAddress = wallet.getPrimaryAddress();
    assertEquals(wallet.getSubaddress(0, 0).getAddress(), primaryAddress);
  }
  
  @Test
  public void testDecodeIntegratedAddress() {
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress("03284e41c342f036");
    MoneroIntegratedAddress decodedAddress = wallet.decodeIntegratedAddress(integratedAddress.toString());
    assertEquals(integratedAddress, decodedAddress);
  }

  @Test
  public void testGetAccounts() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      assertNull(account.getSubaddresses());
      TestUtils.testAccount(account);
    }
  }
  
  @Test
  public void testGetAccountsWithSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      assertNotNull(account.getSubaddresses());
      assertFalse(account.getSubaddresses().isEmpty());
      TestUtils.testAccount(account);
    }
  }

  @Test
  public void testGetAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      MoneroAccount retrieved = wallet.getAccount(account.getIndex(), true);
      assertNull(retrieved.getSubaddresses());
      TestUtils.testAccount(retrieved);
    }
  }
  
  @Test
  public void testGetAccountWithSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      MoneroAccount retrieved = wallet.getAccount(account.getIndex(), true);
      assertNotNull(retrieved.getSubaddresses());
      assertFalse(retrieved.getSubaddresses().isEmpty());
      TestUtils.testAccount(retrieved);
    }
  }

  @Test
  public void testGetSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        TestUtils.testSubaddress(subaddress);
      }
    }
  }

  @Test
  public void testGetSubaddressesByIndices() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      Collection<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroSubaddress subaddress : subaddresses) {
        subaddressIndices.add(subaddress.getIndex());
      }
      assertEquals(subaddresses, wallet.getSubaddresses(account.getIndex(), subaddressIndices));
    }
  }
  
  @Test
  public void testGetSubaddressByIndex() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (int i = 0; i < subaddresses.size(); i++) {
        MoneroSubaddress subaddress = wallet.getSubaddress(account.getIndex(), subaddresses.get(i).getIndex());
        assertEquals(subaddresses.get(i), subaddress);
      }
    }
  }
  
  @Test
  public void testGetBalance() {
    fail("Not implemented");
//    List<MoneroTx> txs = wallet.getTxs();
//    assertTrue("Test wallet does not have transaction history; load '" + TestUtils.WALLET_NAME_1 + "' with stagenet XMR and run TestMoneroWalletSends a few times", txs.size() >= 3);
  }

  @Test
  public void testGetBalanceAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      BigInteger balance = wallet.getBalance(account.getIndex());
      assertTrue(balance.longValue() >= 0);
    }
    
    // test getting balance of invalid account
    try {
      wallet.getBalance(-456);
      fail("Should have thrown error on invalid account");
    } catch (MoneroException exception) { }
  }

  @Test
  public void getBalanceSubaddress() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        BigInteger balance = wallet.getBalance(account.getIndex(), subaddress.getIndex());
        assertTrue(balance.longValue() >= 0);
      }
    }
    
    // test getting balance of invalid subaddress
    try {
      wallet.getBalance(0, -456);
      fail("Should have thrown error on invalid subaddress");
    } catch (MoneroException exception) { }
  }
  
  @Test
  public void testGetUnlockedBalance() {
    fail("Not implemented");
  }

  @Test
  public void testGetUnlockedBalanceAccount() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      BigInteger unlockedBalance = wallet.getUnlockedBalance(account.getIndex());
      assertTrue(unlockedBalance.longValue() >= 0);
    }
    
    // test getting balance of invalid account
    try {
      wallet.getUnlockedBalance(-456);
      fail("Should have thrown error on invalid account");
    } catch (MoneroException exception) { }
  }

  @Test
  public void testGetUnlockedBalanceSubaddress() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        BigInteger unlockedBalance = wallet.getUnlockedBalance(account.getIndex(), subaddress.getIndex());
        assertTrue(unlockedBalance.longValue() >= 0);
      }
    }
    
    // test getting balance of invalid subaddress
    try {
      wallet.getUnlockedBalance(0, -456);
      fail("Should have thrown error on invalid subaddress");
    } catch (MoneroException exception) { }
  }
  
  @Test
  public void testMultisigImportNeeded() {
    assertFalse(wallet.isMultisigImportNeeded());
  }
  
  @Test
  public void testGetTxsWallet() {
    boolean nonDefaultSubaddressFound = false;
    List<MoneroTx> txs = wallet.getTxs();
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      TestUtils.testTx(tx);
      if (tx.getAccountIndex() != null && tx.getAccountIndex() != 0 && tx.getSubaddressIndex() != null && tx.getSubaddressIndex() != 0) {
        nonDefaultSubaddressFound = true;
      }
    }
    assertTrue("No transactions found in non-default account and subaddress; run testSendMultiple() first", nonDefaultSubaddressFound);
  }
  
  @Test
  public void testGetTxsAccount() {
    boolean nonDefaultSubaddressFound = false;
    for (MoneroAccount account : wallet.getAccounts()) {
      List<MoneroTx> txs = wallet.getTxs(account.getIndex());
      for (MoneroTx tx : txs) {
        TestUtils.testTx(tx);
        assertEquals(account.getIndex(), tx.getAccountIndex());
        if (account.getIndex() != 0 && tx.getSubaddressIndex() != 0) nonDefaultSubaddressFound = true;
      }
    }
    assertTrue("No transactions found in non-default account and subaddress; run testSendMultiple() first", nonDefaultSubaddressFound);
  }
  
  @Test
  public void testGetTxsSubaddress() {
    boolean nonDefaultSubaddressFound = false;
    for (MoneroAccount account : wallet.getAccounts()) {
      for (MoneroSubaddress subaddress : wallet.getSubaddresses(account.getIndex())) {
        for (MoneroTx tx : wallet.getTxs(account.getIndex(), subaddress.getIndex())) {
          TestUtils.testTx(tx);
          assertEquals(account.getIndex(), tx.getAccountIndex());
          //assertEquals(subaddress.getIndex(), tx.getSubaddressIndex());
          if (subaddress.getIndex() != tx.getSubaddressIndex()) {
            LOGGER.warn("Tx subaddress index does not match queried subaddress index because monero-wallet-rpc outgoing transactions do not indicate originating subaddresses");
          }
          if (account.getIndex() != 0 && tx.getSubaddressIndex() != 0) nonDefaultSubaddressFound = true;
        }
      }
    }
    assertTrue("No transactions found in non-default account and subaddress; run testSendMultiple() first", nonDefaultSubaddressFound);
  }

  @Test
  public void testGetTxsMoneroTxFilter() {
    
    // get all transactions for reference
    List<MoneroTx> allTxs = wallet.getTxs();
    assertFalse(allTxs.isEmpty());
    for (MoneroTx tx : allTxs) {
      TestUtils.testTx(tx);
    }
    
    // test getting transactions by payment ids
    Collection<String> paymentIds = new HashSet<String>();
    for (MoneroTx tx : allTxs) paymentIds.add(tx.getPaymentId());
    assertFalse(paymentIds.isEmpty());
    for (String paymentId : paymentIds) {
      Collection<String> filterPaymentIds = new HashSet<String>();
      filterPaymentIds.add(paymentId);
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setPaymentIds(filterPaymentIds);
      List<MoneroTx> txs = wallet.getTxs(filter);
      assertFalse(txs.isEmpty());
      for (MoneroTx tx : txs) {
        TestUtils.testTx(tx);
        assertTrue(filter.getPaymentIds().contains(tx.getPaymentId()));
      }
    }
    
    // test getting incoming transactions
    List<MoneroTx> txs = wallet.getTxs(new MoneroTxFilter(true, false, false, false, false, null, null, null, null, null, null));
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertEquals(MoneroTxType.INCOMING, tx.getType());
    }
    
    // test getting outgoing transactions
    txs = wallet.getTxs(new MoneroTxFilter(false, true, false, false, false, null, null, null, null, null, null));
    assertFalse(txs.isEmpty());
    for (MoneroTx tx : txs) {
      assertEquals(MoneroTxType.OUTGOING, tx.getType());
    }
    
    // test balance equals spendable outputs for each account
    for (MoneroAccount account : wallet.getAccounts()) {
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setAccountIndex(account.getIndex());
      txs = wallet.getTxs(filter);
      if (account.getIndex() == 0) assertFalse(txs.isEmpty());
      BigInteger balance = BigInteger.valueOf(0);
      for (MoneroTx tx : txs) {
        if (tx.getOutputs() == null) continue;
        for (MoneroOutput output : tx.getOutputs()) {
          if (!output.isSpent()) {
            balance = balance.add(output.getAmount());
          }
        }
      }
      assertEquals(wallet.getBalance(account.getIndex()), balance);
    }
    
    // test block height filtering
    {
      txs = wallet.getTxs(0);
      assertFalse("No transactions; run testSendMultiple()", txs.isEmpty());
        
      // get and sort block heights in ascending order
      List<Integer> heights = new ArrayList<Integer>();
      for (MoneroTx tx : txs) {
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
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setAccountIndex(0);
      filter.setMinHeight(minHeight);
      filter.setMaxHeight(maxHeight);
      txs = wallet.getTxs(filter);
      assertFalse(txs.isEmpty());
      assertTrue(txs.size() < unfilteredCount);
      for (MoneroTx tx : txs) {
        assertTrue(tx.getHeight() >= minHeight && tx.getHeight() <= maxHeight);
      }
    }
    
    // test filtering by subaddress
    for (MoneroAccount account : wallet.getAccounts()) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      for (MoneroSubaddress subaddress : subaddresses) {
        MoneroTxFilter filter = new MoneroTxFilter();
        filter.setAccountIndex(account.getIndex());
        filter.setSubaddressIndices(Arrays.asList(subaddress.getIndex()));
        txs = wallet.getTxs(filter);
        
        // test that tx amounts add up to subaddress balance
        BigInteger balance = BigInteger.valueOf(0);
        for (MoneroTx tx : txs) {
          if (tx.getOutputs() == null) continue;
          for (MoneroOutput output : tx.getOutputs()) {
            if (!output.isSpent()) {
              balance = balance.add(output.getAmount());
            }
          }
        }
        assertEquals(wallet.getBalance(account.getIndex(), subaddress.getIndex()), balance);
      }
    }
    
    // assert that ummet filter criteria has no results
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setAccountIndex(0);
    Collection<Integer> subaddressIndices = new HashSet<Integer>();
    subaddressIndices.add(1234907);
    filter.setSubaddressIndices(subaddressIndices);
    txs = wallet.getTxs(filter);
    assertTrue(txs.isEmpty());
    
    // test getting transactions by transaction ids
    Collection<String> txIds = new HashSet<String>();
    for (MoneroTx tx : allTxs) txIds.add(tx.getId());
    assertFalse(txIds.isEmpty());
    filter = new MoneroTxFilter();
    filter.setTxIds(txIds);    
    txs = wallet.getTxs(filter);
    assertEquals(allTxs.size(), txs.size());
    // TODO: this queries every transaction, too much
//    for (String txId : txIds) {
//      filter = new MoneroTxFilter();
//      filter.setTxIds(Arrays.asList(txId));
//      txs = wallet.getTxs(filter);
//      assertFalse(txs.isEmpty());
//      for (MoneroTx tx : txs) {
//        assertEquals(txId, tx.getId());
//      }
//    }
  }

  @Test
  public void testGetTxNotes() {
    List<MoneroTx> txs = wallet.getTxs();
    assertFalse(txs.isEmpty());
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) txIds.add(tx.getId());
    List<String> txNotes = wallet.getTxNotes(txIds);
    assertEquals(txs.size(), txNotes.size());
    for (String txNote : txNotes) assertNotNull(txNote);
  }

  @Test
  public void testGetKeyImages() {
    Collection<MoneroKeyImage> images = wallet.getKeyImages();
    assertFalse(images.isEmpty());
    for (MoneroKeyImage image : images) {
      assertNotNull(image.getKeyImage());
      assertNotNull(image.getSignature());
    }
  }

  @Test
  public void testImportKeyImages() {
    Collection<MoneroKeyImage> images = wallet.getKeyImages();
    assertFalse(images.isEmpty());
    Map<String, BigInteger> results = wallet.importKeyImages(images);
    assertNotNull(results.get("height"));
    assertNotNull(results.get("spent"));
    assertNotNull(results.get("unspent"));
  }

  @Test
  public void testGetLanguages() {
    List<String> languages = wallet.getLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }

  @Test
  public void testCreateOpenWallet() {
    
    // create / open new wallet
    try {
      wallet.createWallet(TestUtils.WALLET_NAME_2, TestUtils.WALLET_PW, "English");
    } catch (MoneroRpcException e) {
      assertEquals((int) -21, (int) e.getRpcCode());  // exception is ok if wallet already created
    }    
    wallet.openWallet(TestUtils.WALLET_NAME_2, TestUtils.WALLET_PW);
    List<MoneroTx> txs = wallet.getTxs();
    assertTrue(txs.isEmpty());  // wallet is unused except tests
    
    // open previous wallet
    wallet.openWallet(TestUtils.WALLET_NAME_1, TestUtils.WALLET_PW);
    txs = wallet.getTxs();
    assertFalse(txs.isEmpty());  // wallet is used
  }

  @Test
  public void testSignVerify() {
    String msg = "This is a super important message which needs to be signed and verified.";
    String signature = wallet.sign(msg);
    boolean verified = wallet.verify(msg, wallet.getSubaddress(0, 0).getAddress().toString(), signature);
    assertTrue(verified);
    verified = wallet.verify(msg, SAMPLE_ADDRESS, signature);
    assertFalse(verified);
  }

  @Test
  public void testUri() {
    
    // test with optional fields as null
    MoneroUri mUri1 = new MoneroUri();
    mUri1.setAddress(wallet.getSubaddress(0, 0).getAddress());
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

  @Test
  public void testRescanBlockchain() {
    wallet.rescanBlockchain();
  }

  @Test
  public void testRescanSpent() {
    wallet.rescanSpent();
  }

  @Ignore // disabled so tests don't actually stop the wallet
  @Test
  public void testStopWallet() {
    wallet.stopWallet();
  }

  @Test
  public void testMining() {
    wallet.startMining(2, false, true);
    wallet.stopMining();
  }
}
