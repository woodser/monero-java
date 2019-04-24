package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import common.types.Filter;
import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.config.MoneroSendConfig;
import monero.wallet.config.MoneroTransferFilter;
import monero.wallet.config.MoneroTxFilter;
import monero.wallet.config.MoneroVoutFilter;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroOutgoingTransfer;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;
import utils.TestUtils;

/**
 * Runs common tests that every Monero wallet implementation should support.
 * 
 * TODO: test filtering with not relayed
 */
public abstract class TestMoneroWalletCommon extends TestMoneroBase {
  
  // test constants
  protected static final boolean LITE_MODE = false;
  protected static final boolean TEST_NON_RELAYS = true;
  protected static final boolean TEST_RELAYS = true;
  protected static final boolean TEST_NOTIFICATIONS = false;
  protected static final boolean TEST_RESETS = false;
  private static final int MAX_TX_PROOFS = 25;   // maximum number of transactions to check for each proof, undefined to check all
  private static final int SEND_MAX_DIFF = 60;
  private static final int SEND_DIVISOR = 2;
  
  // instance variables
  private MoneroWallet wallet;    // wallet instance to test
  private MoneroDaemon daemon;    // daemon instance to test
  private List<MoneroTxWallet> txCache; // local tx cache
  
  public TestMoneroWalletCommon() {
    wallet = getTestWallet();
    daemon = getTestDaemon();
  }

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }
  
  // Can get the current height that the wallet is synchronized to
  @Test
  public void testGetHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int height = wallet.getHeight();
    assertTrue(height >= 0);
  }
  
  // Can get the mnemonic phrase derived from the seed
  @Test
  public void testGetMnemonic() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String mnemonic = wallet.getMnemonic();
    MoneroUtils.validateMnemonic(mnemonic);
    assertEquals(TestUtils.TEST_MNEMONIC, mnemonic);
  }
  
  // Can get a list of supported languages for the mnemonic phrase
  @Test
  public void testGetSupportedLanguages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<String> languages = wallet.getLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }
  
  // Can get the private view key
  @Test
  public void testGetPrivateViewKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String privateViewKey = wallet.getPrivateViewKey();
    MoneroUtils.validatePrivateViewKey(privateViewKey);
  }
  
  // Can get the primary address
  @Test
  public void testGetPrimaryAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String primaryAddress = wallet.getPrimaryAddress();
    MoneroUtils.validateAddress(primaryAddress);
    assertEquals((wallet.getSubaddress(0, 0)).getAddress(), primaryAddress);
  }
  
  // Can get an integrated address given a payment id
  @Test
  public void testGetIntegratedAddressFromPaymentId() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // save address for later comparison
    String address = wallet.getSubaddress(0, 0).getAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    assertEquals(integratedAddress.getStandardAddress(), address);
    assertEquals(integratedAddress.getPaymentId(), paymentId);
    
    // test null payment id which generates a new one
    integratedAddress = wallet.getIntegratedAddress(null);
    assertEquals(integratedAddress.getStandardAddress(), address);
    assertFalse(integratedAddress.getPaymentId().isEmpty());
    
    // test invalid payment id
    try {
      String invalidPaymentId = "invalid_payment_id_123456";
      integratedAddress = wallet.getIntegratedAddress(invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown a RPC exception");
    } catch (MoneroException e) {
      assertEquals(-5, (int) e.getCode());
      assertEquals("Invalid payment ID", e.getDescription());
    }
  }
  
  // Can decode an integrated address
  @Test
  public void testDecodeIntegratedAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress("03284e41c342f036");
    MoneroIntegratedAddress decodedAddress = wallet.decodeIntegratedAddress(integratedAddress.toString());
    assertEquals(integratedAddress, decodedAddress);
  }
  
  // Can sync (without progress)
  // TODO: test syncing from start height
  @Test
  public void testSyncWithoutProgress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int numBlocks = 100;
    int chainHeight = daemon.getHeight();
    assertTrue(chainHeight >= numBlocks);
    MoneroSyncResult result = wallet.sync(chainHeight - numBlocks);  // sync end of chain
    assertTrue(result.getNumBlocksFetched() >= 0);
    assertNotNull(result.getReceivedMoney());
  }
  
  // Can get accounts without subaddresses
  @Test
  public void testGetAccountsWithoutSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      assertNull(account.getSubaddresses());
    }
  }
  
  // Can get accounts with subaddresses
  @Test
  public void testGetAccountsWithSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      assertFalse(account.getSubaddresses().isEmpty());
    }
  }
  
  // Can get an account at a specified index
  @Test
  public void testGetAccount() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      
      // test without subaddresses
      MoneroAccount retrieved = wallet.getAccount(account.getIndex());
      assertNull(retrieved.getSubaddresses());
      
      // test with subaddresses
      retrieved = wallet.getAccount(account.getIndex(), true);
      assertFalse(retrieved.getSubaddresses().isEmpty());
    }
  }
  
  // Can create a new account without a label
  @Test
  public void testCreateAccountWithoutLabel() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount();
    testAccount(createdAccount);
    assertNull(createdAccount.getLabel());
    assertTrue(accountsBefore.size() == (wallet.getAccounts()).size() - 1);
  }
  
  // Can create a new account with a label
  @Test
  public void testCreateAccountWithLabel() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create account with label
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    String label = UUID.randomUUID().toString();
    MoneroAccount createdAccount = wallet.createAccount(label);
    testAccount(createdAccount);
    assertEquals(label, createdAccount.getLabel());
    assertTrue(accountsBefore.size() == (wallet.getAccounts()).size() - 1);

    // create account with same label
    createdAccount = wallet.createAccount(label);
    testAccount(createdAccount);
    assertEquals(label, createdAccount.getLabel());
    assertTrue(accountsBefore.size() == (wallet.getAccounts()).size() - 2);
  }
  
  // Can get subaddresses at a specified account index
  @Test
  public void testGetSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        testSubaddress(subaddress);
        assertEquals(account.getIndex(), subaddress.getAccountIndex());
      }
    }
  }
  
  // Can get subaddresses at specified account and subaddress indices
  @Test
  public void testGetSubaddressesByIndices() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      
      // get subaddresses
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertTrue(subaddresses.size() > 0);
      
      // remove a subaddress for query if possible
      if (subaddresses.size() > 1) subaddresses.remove(0);
      
      // get subaddress indices
      List<Integer> subaddressIndices = new ArrayList<Integer>();
      for (MoneroSubaddress subaddress : subaddresses) subaddressIndices.add(subaddress.getIndex());
      assertTrue(subaddressIndices.size() > 0);
      
      // fetch subaddresses by indices
      List<MoneroSubaddress> fetchedSubaddresses = wallet.getSubaddresses(account.getIndex(), subaddressIndices);
      
      // original subaddresses (minus one removed if applicable) is equal to fetched subaddresses
      assertEquals(subaddresses, fetchedSubaddresses);
    }
  }
  
  // Can get a subaddress at a specified account and subaddress index
  @Test
  public void testGetSubaddressByIndex() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue(accounts.size() > 0);
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertTrue(subaddresses.size() > 0);
      for (MoneroSubaddress subaddress : subaddresses) {
        assertEquals(subaddress, wallet.getSubaddress(account.getIndex(), subaddress.getIndex()));
        assertEquals(subaddress, (wallet.getSubaddresses(account.getIndex(), Arrays.asList(subaddress.getIndex()))).get(0)); // test plural call with single subaddr number
      }
    }
  }
  
  // Can create a subaddress with and without a label
  @Test
  public void testCreateSubaddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create subaddresses across accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    if (accounts.size() < 2) wallet.createAccount();
    accounts = wallet.getAccounts();
    assertTrue(accounts.size() > 1);
    for (int accountIdx = 0; accountIdx < 2; accountIdx++) {
      
      // create subaddress with no label
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(accountIdx);
      MoneroSubaddress subaddress = wallet.createSubaddress(accountIdx);
      assertNull(subaddress.getLabel());
      testSubaddress(subaddress);
      List<MoneroSubaddress> subaddressesNew = wallet.getSubaddresses(accountIdx);
      assertEquals(subaddressesNew.size() - 1, subaddresses.size());
      assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
      
      // create subaddress with label
      subaddresses = wallet.getSubaddresses(accountIdx);
      String uuid = UUID.randomUUID().toString();
      subaddress = wallet.createSubaddress(accountIdx, uuid);
      assertEquals(uuid, subaddress.getLabel());
      testSubaddress(subaddress);
      subaddressesNew = wallet.getSubaddresses(accountIdx);
      assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
      assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
    }
  }
  
  // Can get the address of a subaddress at a specified account and subaddress index
  @Test
  public void testGetSubaddressAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertEquals(wallet.getPrimaryAddress(), (wallet.getSubaddress(0, 0)).getAddress());
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        assertEquals(subaddress.getAddress(), wallet.getAddress(account.getIndex(), subaddress.getIndex()));
      }
    }
    
    // test out of range indices
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size();
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNull(address);
  }
  
  // Can get the account and subaddress indices of an address
  @Test
  public void testGetAddressIndices() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get last subaddress to test
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size() - 1;
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNotNull(address);
    
    // get address index
    MoneroSubaddress subaddress = wallet.getAddressIndex(address);
    assertEquals(accountIdx, (int) subaddress.getAccountIndex());
    assertEquals(subaddressIdx, (int) subaddress.getIndex());

    // test valid but unfound address
    String nonWalletAddress = TestUtils.getRandomWalletAddress();
    try {
      subaddress = wallet.getAddressIndex(nonWalletAddress);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals("Address does not belong to the wallet", e.getMessage());
    }
    
    // test invalid address
    try {
      subaddress = wallet.getAddressIndex("this is definitely not an address");
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals("Address does not belong to the wallet", e.getMessage());
    }
  }
  
  // Can get the locked and unlocked balances of the wallet, accounts, and subaddresses
  @Test
  public void getAllBalances() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch accounts with all info as reference
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    
    // test that balances add up between accounts and wallet
    BigInteger accountsBalance = BigInteger.valueOf(0);
    BigInteger accountsUnlockedBalance = BigInteger.valueOf(0);
    for (MoneroAccount account : accounts) {
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
      
      // test that balances add up between subaddresses and accounts
      BigInteger subaddressesBalance = BigInteger.valueOf(0);
      BigInteger subaddressesUnlockedBalance = BigInteger.valueOf(0);
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        subaddressesBalance = subaddressesBalance.add(subaddress.getBalance());
        subaddressesUnlockedBalance = subaddressesUnlockedBalance.add(subaddress.getUnlockedBalance());
        
        // test that balances are consistent with getAccounts() call
        assertEquals((wallet.getBalance(subaddress.getAccountIndex(), subaddress.getIndex())).toString(), subaddress.getBalance().toString());
        assertEquals((wallet.getUnlockedBalance(subaddress.getAccountIndex(), subaddress.getIndex())).toString(), subaddress.getUnlockedBalance().toString());
      }
      assertEquals((wallet.getBalance(account.getIndex())).toString(), subaddressesBalance.toString());
      assertEquals((wallet.getUnlockedBalance(account.getIndex())).toString(), subaddressesUnlockedBalance.toString());
    }
    TestUtils.testUnsignedBigInteger(accountsBalance);
    TestUtils.testUnsignedBigInteger(accountsUnlockedBalance);
    assertEquals((wallet.getBalance()).toString(), accountsBalance.toString());
    assertEquals((wallet.getUnlockedBalance()).toString(), accountsUnlockedBalance.toString());
  }
  
  // Can get transactions in the wallet
  @Test
  public void testGetTxsWallet() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    boolean nonDefaultIncoming = false;
    List<MoneroTxWallet> txs1 = getCachedTxs();
    List<MoneroTxWallet> txs2 = getAndTestTxs(wallet, null, null, true);
    assertEquals(txs2.size(), txs1.size());
    
    // build test context
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    
    // test each transaction
    Map<Integer, MoneroBlock> blockPerHeight = new HashMap<Integer, MoneroBlock>();
    for (int i = 0; i < txs1.size(); i++) {
      testTxWallet(txs1.get(i), ctx);
      testTxWallet(txs2.get(i), ctx);
      assertEquals(txs1.get(i), txs2.get(i));
      
      // test merging equivalent txs
      MoneroTxWallet copy1 = txs1.get(i).copy();
      MoneroTxWallet copy2 = txs2.get(i).copy();
      if (copy1.getIsConfirmed()) copy1.setBlock(txs1.get(i).getBlock().copy().setTxs(Arrays.asList(copy1)));
      if (copy2.getIsConfirmed()) copy2.setBlock(txs2.get(i).getBlock().copy().setTxs(Arrays.asList(copy2)));
      MoneroTxWallet merged = copy1.merge(copy2);
      testTxWallet(merged, ctx);
      
      // find non-default incoming
      if (txs1.get(i).getIncomingTransfers() != null) { // TODO: txs1.get(i).isIncoming()
        for (MoneroIncomingTransfer transfer : txs1.get(i).getIncomingTransfers()) {
          if (transfer.getAccountIndex() != 0 && transfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
        }
      }
      
      // ensure unique block reference per height
      if (txs2.get(i).getIsConfirmed()) {
        MoneroBlock block = blockPerHeight.get(txs2.get(i).getHeight());
        if (block == null) blockPerHeight.put(txs2.get(i).getHeight(), txs2.get(i).getBlock());
        else {
          assertEquals(block, txs2.get(i).getBlock());
          assertTrue("Block references for same height must be same", block == txs2.get(i).getBlock());
        }
      }
    }
    
    // ensure non-default account and subaddress tested
    assertTrue("No incoming transfers found to non-default account and subaddress; run send-to-multiple tests first", nonDefaultIncoming);
  }
  
  // Can get transactions with additional configuration
  @Test
  public void testGetTxsWithConfiguration() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get random transactions for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) testTxWallet(randomTx, null);
    
    // get transactions by id
    List<String> txIds = new ArrayList<String>();
    for (MoneroTxWallet randomTx : randomTxs) {
      txIds.add(randomTx.getId());
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxFilter().setTxId(randomTx.getId()), null, true);
      assertEquals(txs.size(), 1);
      MoneroTxWallet merged = txs.get(0).merge(randomTx.copy()); // txs change with chain so check mergeability
      testTxWallet(merged, null);
    }
    
    // get transactions by ids
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxFilter().setTxIds(txIds), null, null);
    assertEquals(txs.size(), randomTxs.size());
    for (MoneroTxWallet tx : txs) assertTrue(txIds.contains(tx.getId()));
    
    // get transactions with an outgoing transfer
    TestContext ctx = new TestContext();
    ctx.hasOutgoingTransfer = true;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsOutgoing(true), ctx, true);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIsOutgoing());
      assertNotNull(tx.getOutgoingTransfer());
      testTransfer(tx.getOutgoingTransfer());
    }
    
    // get transactions without an outgoing transfer
    ctx.hasOutgoingTransfer = false;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsOutgoing(false), ctx, true);
    for (MoneroTxWallet tx : txs) assertNull(tx.getOutgoingTransfer());
    
    // get transactions with incoming transfers
    ctx = new TestContext();
    ctx.hasIncomingTransfers = true;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsIncoming(true), ctx, true);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIsIncoming());
      assertTrue(tx.getIncomingTransfers().size() > 0);
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        assertTrue(transfer instanceof MoneroTransfer);
        testTransfer(transfer);
      }
    }
    
    // get transactions without incoming transfers
    ctx.hasIncomingTransfers = false;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsIncoming(false), ctx, true);
    for (MoneroTxWallet tx : txs)  {
      assertFalse(tx.getIsIncoming());
      assertNull(tx.getIncomingTransfers());
    }
    
    // get transactions associated with an account
    int accountIdx = 1;
    txs = wallet.getTxs(new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setAccountIndex(accountIdx)));
    for (MoneroTxWallet tx : txs) {
      boolean found = false;
      if (tx.getIsOutgoing() && tx.getOutgoingTransfer().getAccountIndex() == accountIdx) found = true;
      else if (tx.getIncomingTransfers() != null) {
        for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
          if (transfer.getAccountIndex() == accountIdx) {
            found = true;
            break;
          }
        }
      }
      assertTrue(("Transaction is not associated with account " + accountIdx + ":\n" + tx.toString()), found);
    }
    
    // get transactions with incoming transfers to an account
    txs = wallet.getTxs(new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setIsIncoming(true).setAccountIndex(accountIdx)));
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIncomingTransfers().size() > 0);
      boolean found = false;
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() == accountIdx) {
          found = true;
          break;
        }
      }
      assertTrue("No incoming transfers to account " + accountIdx + " found:\n" + tx.toString(), found);
    }
    
    // get txs with manually built filter that are confirmed and have an outgoing transfer from account 0
    ctx = new TestContext();
    ctx.hasOutgoingTransfer = true;
    MoneroTxFilter txFilter = new MoneroTxFilter();
    txFilter.setIsConfirmed(true);
    txFilter.setTransferFilter(new MoneroTransferFilter().setAccountIndex(0).setIsOutgoing(true));
    txs = getAndTestTxs(wallet, txFilter, ctx, true);
    for (MoneroTxWallet tx : txs) {
      if (!tx.getIsConfirmed()) System.out.println(tx);
      assertEquals(true, tx.getIsConfirmed());
      assertTrue(tx.getIsOutgoing());
      assertEquals(0, (int) tx.getOutgoingTransfer().getAccountIndex());
    }
    
    // get txs with outgoing transfers that have destinations to account 1
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setHasDestinations(true).setAccountIndex(0)), null, null);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIsOutgoing());
      assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
    }
    
    // test block height filtering
    {
      txs = wallet.getTxs(new MoneroTxFilter().setIsConfirmed(true));
      assertTrue("No transactions; run send to multiple test", txs.size() > 0);
        
      // get and sort block heights in ascending order
      List<Integer> heights = new ArrayList<Integer>();
      for (MoneroTxWallet tx : txs) {
        heights.add(tx.getBlock().getHeight());
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
      
      // assert some transactions filtered
      int unfilteredCount = txs.size();
      txs = getAndTestTxs(wallet, new MoneroTxFilter().setMinHeight(minHeight).setMaxHeight(maxHeight), null, true);
      assertTrue(txs.size() < unfilteredCount);
      for (MoneroTx tx : txs) {
        int height = tx.getBlock().getHeight();
        assertTrue(height >= minHeight && height <= maxHeight);
      }
    }
    
    // include vouts with transactions
    ctx = new TestContext();
    ctx.getVouts = true;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIncludeVouts(true), ctx, true);
    boolean found = false;
    for (MoneroTxWallet tx : txs) {
      if (tx.getVouts() != null) {
        assertTrue(tx.getVouts().size() > 0);
        found = true;
      } else {
        assertTrue(tx.getIsOutgoing() || (tx.getIsIncoming() && !tx.getIsConfirmed())); // TODO: monero-wallet-rpc: return vouts for unconfirmed txs
      }
    }
    assertTrue("No vouts found in txs", found);
  }
  
  // NOTE: payment ids are deprecated so this test will require an old wallet to pass
  @Test
  public void testGetTxsWithPaymentIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get random transactions with payment ids for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxFilter().setHasPaymentId(true), 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) {
      assertNotNull(randomTx.getPaymentId());
    }
    
    // get transactions by payment id
    List<String> paymentIds = new ArrayList<String>();
    for (MoneroTxWallet tx : randomTxs) paymentIds.add(tx.getPaymentId());
    assertTrue(paymentIds.size() > 1);
    for (String paymentId : paymentIds) {
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxFilter().setPaymentId(paymentId), null, null);
      assertEquals(1, txs.size());
      assertNotNull(txs.get(0).getPaymentId());
      MoneroUtils.validatePaymentId(txs.get(0).getPaymentId());
    }
    
    // get transactions by payment ids
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxFilter().setPaymentIds(paymentIds), null, null);
    for (MoneroTxWallet tx : txs) {
      assertTrue(paymentIds.contains(tx.getPaymentId()));
    }
  }
  
  // Returns all known fields of txs regardless of filtering
  @Test
  public void testGetTxsFieldsWithFiltering() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch wallet txs
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxFilter().setIsConfirmed(true));
    for (MoneroTxWallet tx : txs) {
      
      // find tx sent to same wallet with incoming transfer in different account than src account
      if (tx.getOutgoingTransfer() == null || tx.getIncomingTransfers() == null) continue;
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() == tx.getOutgoingTransfer().getAccountIndex()) continue;
        
        // fetch tx with filtering
        List<MoneroTxWallet> filteredTxs = wallet.getTxs(new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setIsIncoming(true).setAccountIndex(transfer.getAccountIndex())));
        MoneroTxWallet filteredTx = Filter.apply(new MoneroTxFilter().setTxIds(Arrays.asList(tx.getId())), filteredTxs).get(0);
        
        // txs should be the same (mergeable)
        assertEquals(tx.getId(), filteredTx.getId());
        tx.merge(filteredTx);
        
        // test is done
        return;
      }
    }
    
    // test did not fully execute
    throw new Error("Test requires tx sent from/to different accounts of same wallet but none found; run send tests");
  }
  
  // Validates inputs when getting transactions
  @Test
  public void testGetTxsValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // test with invalid id
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxFilter().setTxId("invalid_id"));
    assertEquals(txs.size(), 0);
    
    // test invalid id in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    txs = wallet.getTxs(new MoneroTxFilter().setTxIds(Arrays.asList(randomTxs.get(0).getId(), "invalid_id")));
    assertEquals(1, txs.size());
    assertEquals(randomTxs.get(0).getId(), txs.get(0).getId());
    
    // TODO: test other input validation here
  }

  // Can get transfers in the wallet, accounts, and subaddresses
  @Test
  public void testGetTransfers() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get all transfers
    getAndTestTransfers(wallet, null, null, true);
    
    // get transfers by account index
    boolean nonDefaultIncoming = false;
    for (MoneroAccount account : wallet.getAccounts(true)) {
      List<MoneroTransfer> accountTransfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(account.getIndex()), null, null);
      for (MoneroTransfer transfer : accountTransfers) assertEquals(transfer.getAccountIndex(), account.getIndex());
      
      // get transfers by subaddress index
      List<MoneroTransfer> subaddressTransfers = new ArrayList<MoneroTransfer>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(subaddress.getAccountIndex()).setSubaddressIndex(subaddress.getIndex()), null, null);
        for (MoneroTransfer transfer : transfers) {
          
          // test account and subaddress indices
          assertEquals(subaddress.getAccountIndex(), transfer.getAccountIndex());
          if (transfer.getIsIncoming()) {
            MoneroIncomingTransfer inTransfer = (MoneroIncomingTransfer) transfer;
            assertEquals(subaddress.getIndex(), inTransfer.getSubaddressIndex());
            if (transfer.getAccountIndex() != 0 && inTransfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          } else {
            MoneroOutgoingTransfer outTransfer = (MoneroOutgoingTransfer) transfer;
            assertTrue(outTransfer.getSubaddressIndices().contains(subaddress.getIndex()));
            if (transfer.getAccountIndex() != 0) {
              for (int subaddrIdx : outTransfer.getSubaddressIndices()) {
                if (subaddrIdx > 0) {
                  nonDefaultIncoming = true;
                  break;
                }
              }
            }
          }
          
          // don't add duplicates TODO monero-wallet-rpc: duplicate outgoing transfers returned for different subaddress indices, way to return outgoing subaddress indices?
          boolean found = false;
          for (MoneroTransfer subaddressTransfer : subaddressTransfers) {
            if (transfer.toString().equals(subaddressTransfer.toString()) && transfer.getTx().getId().equals(subaddressTransfer.getTx().getId())) {
              found = true;
              break;
            }
          }
          if (!found) subaddressTransfers.add(transfer);
        }
      }
      assertEquals(accountTransfers.size(), subaddressTransfers.size());
      
      // get transfers by subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroTransfer transfer : subaddressTransfers) {
        if (transfer.getIsIncoming()) subaddressIndices.add(((MoneroIncomingTransfer) transfer).getSubaddressIndex());
        else subaddressIndices.addAll(((MoneroOutgoingTransfer) transfer).getSubaddressIndices());
      }
      List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), null, null);
      //if (transfers.size() != subaddressTransfers.size()) System.out.println("WARNING: outgoing transfers always from subaddress 0 (monero-wallet-rpc #5171)");
      assertEquals(subaddressTransfers.size(), transfers.size()); // TODO monero-wallet-rpc: these may not be equal because outgoing transfers are always from subaddress 0 (#5171) and/or incoming transfers from/to same account are occluded (#4500)
      for (MoneroTransfer transfer : transfers) {
        assertEquals(transfer.getAccountIndex(), account.getIndex());
        if (transfer.getIsIncoming()) assertTrue(subaddressIndices.contains(((MoneroIncomingTransfer) transfer).getSubaddressIndex()));
        else {
          Set<Integer> intersections = new HashSet<Integer>(subaddressIndices);
          intersections.retainAll(((MoneroOutgoingTransfer) transfer).getSubaddressIndices());
          assertTrue(intersections.size() > 0);  // subaddresses must have overlap
        }
      }
    }
    
    // ensure transfer found with non-zero account and subaddress indices
    assertTrue("No transfers found in non-default account and subaddress; run send-to-multiple tests", nonDefaultIncoming);
  }
  
  // Can get transfers with additional configuration
  @Test
  public void testGetTransfersWithConfiguration() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get incoming transfers
    List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setIsIncoming(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.getIsIncoming());
    
    // get outgoing transfers
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setIsOutgoing(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.getIsOutgoing());
    
    // get confirmed transfers to account 0
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(0).setTxFilter(new MoneroTxFilter().setIsConfirmed(true)), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(0, (int) transfer.getAccountIndex());
      assertTrue(transfer.getTx().getIsConfirmed());
    }
    
    // get confirmed transfers to [1, 2]
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(1).setSubaddressIndex(2).setTxFilter(new MoneroTxFilter().setIsConfirmed(true)), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(1, (int) transfer.getAccountIndex());
      if (transfer.getIsIncoming()) assertEquals(2, (int) ((MoneroIncomingTransfer) transfer).getSubaddressIndex());
      else {
        assertEquals(1, ((MoneroOutgoingTransfer) transfer).getSubaddressIndices().size());
        assertEquals(2, (int) ((MoneroOutgoingTransfer) transfer).getSubaddressIndices().get(0));
      }
      assertTrue(transfer.getTx().getIsConfirmed());
    }
    
    // get transfers in the tx pool
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setInTxPool(true)), null, null);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.getTx().getInTxPool());
    }
    
    // get random transactions
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, null, 3, 5);
    
    // get transfers with a tx id
    List<String> txIds = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) {
      txIds.add(tx.getId());
      transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setId(tx.getId())), null, true);
      for (MoneroTransfer transfer : transfers) assertEquals(tx.getId(), transfer.getTx().getId());
    }
    
    // get transfers with tx ids
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setTxIds(txIds)), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(txIds.contains(transfer.getTx().getId()));
    
    // TODO: test that transfers with the same txId have the same tx reference
    
    // TODO: test transfers destinations
    
    // get transfers with pre-built filter that are confirmed and have outgoing destinations
    MoneroTransferFilter transferFilter = new MoneroTransferFilter();
    transferFilter.setIsOutgoing(true);
    transferFilter.setHasDestinations(true);
    transferFilter.setTxFilter(new MoneroTxFilter().setIsConfirmed(true));
    transfers = getAndTestTransfers(wallet, transferFilter, null, null);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.getIsOutgoing());
      assertTrue(((MoneroOutgoingTransfer) transfer).getDestinations().size() > 0);
      assertEquals(true, transfer.getTx().getIsConfirmed());
    }
  }
  
  // Validates inputs when getting transfers
  @Test
  public void testGetTransfersValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // test with invalid id
    List<MoneroTransfer> transfers = wallet.getTransfers(new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setId("invalid_id")));
    assertEquals(0, transfers.size());
    
    // test invalid id in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    transfers = wallet.getTransfers(new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setTxIds(Arrays.asList(randomTxs.get(0).getId(), "invalid_id"))));
    assertTrue(transfers.size() > 0);
    MoneroTxWallet tx = transfers.get(0).getTx();
    for (MoneroTransfer transfer : transfers) assertTrue(tx == transfer.getTx());
    
    // test unused subaddress indices
    transfers = wallet.getTransfers(new MoneroTransferFilter().setAccountIndex(0).setSubaddressIndices(Arrays.asList(1234907)));
    assertTrue(transfers.size() == 0);
    
    // test invalid subaddress index
    try {
      transfers = wallet.getTransfers(new MoneroTransferFilter().setAccountIndex(0).setSubaddressIndex(-1));
      throw new Error("Should have failed");
    } catch (MoneroException e) {
      assertNotEquals("Should have failed", e.getMessage());
    }
  }
  
  // Can get vouts in the wallet, accounts, and subaddresses
  @Test
  public void testGetVouts() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

    // get all vouts
    getAndTestVouts(wallet, null, true);
    
    // get vouts for each account
    boolean nonDefaultIncoming = false;
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    for (MoneroAccount account : accounts) {
      
      // determine if account is used
      boolean isUsed = false;
      for (MoneroSubaddress subaddress : account.getSubaddresses()) if (subaddress.getIsUsed()) isUsed = true;
      
      // get vouts by account index
      List<MoneroOutputWallet> accountVouts = getAndTestVouts(wallet, new MoneroVoutFilter().setAccountIndex(account.getIndex()), isUsed);
      for (MoneroOutputWallet vout : accountVouts) assertEquals(account.getIndex(), vout.getAccountIndex());
      
      // get vouts by subaddress index
      List<MoneroOutputWallet> subaddressVouts = new ArrayList<MoneroOutputWallet>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroOutputWallet> vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()), subaddress.getIsUsed());
        for (MoneroOutputWallet vout : vouts) {
          assertEquals(subaddress.getAccountIndex(), vout.getAccountIndex());
          assertEquals(subaddress.getIndex(), vout.getSubaddressIndex());
          if (vout.getAccountIndex() != 0 && vout.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          subaddressVouts.add(vout);
        }
      }
      assertEquals(subaddressVouts.size(), accountVouts.size());
      
      // get vouts by subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroOutputWallet vout : subaddressVouts) subaddressIndices.add(vout.getSubaddressIndex());
      List<MoneroOutputWallet> vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), isUsed);
      assertEquals(vouts.size(), subaddressVouts.size());
      for (MoneroOutputWallet vout : vouts) {
        assertEquals(account.getIndex(), vout.getAccountIndex());
        assertTrue(subaddressIndices.contains(vout.getSubaddressIndex()));
      }
    }
    
    // ensure vout found with non-zero account and subaddress indices
    assertTrue("No vouts found in non-default account and subaddress; run send-to-multiple tests", nonDefaultIncoming);
  }

  // TODO: Can get vouts with additional configuration
  @Test
  public void testGetVoutsWithConfiguration() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get unspent vouts to account 0
    List<MoneroOutputWallet> vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setAccountIndex(0).setIsSpent(false), null);
    for (MoneroOutputWallet vout : vouts) {
      assertEquals(0, vout.getAccountIndex(), 0);
      assertEquals(false, vout.getIsSpent());
    }
    
    // get spent vouts to account 1
    vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setAccountIndex(1).setIsSpent(true), true);
    for (MoneroOutputWallet vout : vouts) {
      assertEquals(1, (int) vout.getAccountIndex());
      assertEquals(true, vout.getIsSpent());
    }
    
    // get random transactions
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, new MoneroTxFilter().setIsConfirmed(true), 3, 5);
    
    // get vouts with a tx id
    List<String> txIds = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) {
      txIds.add(tx.getId());
      vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setId(tx.getId())), true);
      for (MoneroOutputWallet vout : vouts) assertEquals(vout.getTx().getId(), tx.getId());
    }
    
    // get vouts with tx ids
    vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setTxIds(txIds)), true);
    for (MoneroOutputWallet vout : vouts) assertTrue(txIds.contains(vout.getTx().getId()));
    
    // get confirmed vouts to specific subaddress with pre-built filter
    int accountIdx = 0;
    int subaddressIdx = 1;
    MoneroVoutFilter voutFilter = new MoneroVoutFilter();
    voutFilter.setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx);
    voutFilter.setTxFilter(new MoneroTxFilter().setIsConfirmed(true));
    vouts = getAndTestVouts(wallet, voutFilter, true);
    for (MoneroOutputWallet vout : vouts) {
      assertEquals(accountIdx, (int) vout.getAccountIndex());
      assertEquals(subaddressIdx, (int) vout.getSubaddressIndex());
      assertEquals(true, vout.getTx().getIsConfirmed());
    }
    
    // get vout by key image
    String keyImage = vouts.get(0).getKeyImage().getHex();
    vouts = wallet.getVouts(new MoneroVoutFilter().setKeyImage(new MoneroKeyImage(keyImage)));
    assertEquals(1, vouts.size());
    assertEquals(keyImage, vouts.get(0).getKeyImage().getHex());
  }
  
  // Validates inputs when getting vouts
  @Test
  public void testGetVoutsValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // test with invalid id
    List<MoneroOutputWallet> vouts = wallet.getVouts(new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setId("invalid_id")));
    assertEquals(0, vouts.size());
    
    // test invalid id in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxFilter().setIsConfirmed(true).setIncludeVouts(true), 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) assertFalse(randomTx.getVouts().isEmpty());
    vouts = wallet.getVouts(new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setTxIds(Arrays.asList(randomTxs.get(0).getId(), "invalid_id"))));
    assertFalse(vouts.isEmpty());
    assertEquals(vouts.size(), randomTxs.get(0).getVouts().size());
    MoneroTxWallet tx = vouts.get(0).getTx();
    for (MoneroOutputWallet vout : vouts) assertTrue(tx == vout.getTx());
  }
  
  // Has correct accounting across accounts, subaddresses, txs, transfers, and vouts
  @Test
  public void testAccounting() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    BigInteger walletBalance = wallet.getBalance();
    BigInteger walletUnlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);  // includes subaddresses
    List<MoneroTxWallet> txs = wallet.getTxs();
    
    // sort txs
    Collections.sort(txs, new MoneroTxComparator());
    
    // test wallet balance
    TestUtils.testUnsignedBigInteger(walletBalance);
    TestUtils.testUnsignedBigInteger(walletUnlockedBalance);
    assertTrue(walletBalance.compareTo(walletUnlockedBalance) >= 0);
    
    // test that wallet balance equals sum of account balances
    BigInteger accountsBalance = BigInteger.valueOf(0);
    BigInteger accountsUnlockedBalance = BigInteger.valueOf(0);
    for (MoneroAccount account : accounts) {
      testAccount(account); // test that account balance equals sum of subaddress balances
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
    }
    assertEquals(0, walletBalance.compareTo(accountsBalance));
    assertEquals(0, walletUnlockedBalance.compareTo(accountsUnlockedBalance));
    
//    // test that wallet balance equals net of wallet's incoming and outgoing tx amounts
//    // TODO monero-wallet-rpc: these tests are disabled because incoming transfers are not returned when sent from the same account, so doesn't balance #4500
//    // TODO: test unlocked balance based on txs, requires e.g. tx.isLocked()
//    // TODO: update this test with new api
//    BigInteger outgoingSum = BigInteger.valueOf(0);
//    BigInteger incomingSum = BigInteger.valueOf(0);
//    for (MoneroTxWallet tx : txs) {
//      if (tx.getOutgoingAmount() != null) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//      if (tx.getIncomingAmount() != null) incomingSum = incomingSum.add(tx.getIncomingAmount());
//    }
//    assertEquals(walletBalance, incomingSum.subtract(outgoingSum));
//    
//    // test that each account's balance equals net of account's incoming and outgoing tx amounts
//    for (MoneroAccount account : accounts) {
//      if (account.getIndex() != 1) continue; // find 1
//      outgoingSum = BigInteger.valueOf(0);
//      incomingSum = BigInteger.valueOf(0);
//      let filter = new MoneroTxFilter();
//      filter.setAccountIndex(account.getIndex());
//      for (let tx of txs.filter(tx => filter.meetsCriteria(tx))) { // normally we'd call wallet.getTxs(filter) but we're using pre-fetched txs
//        if (tx.getId() === "8d3919d98dd5a734da8c52eddc558db3fbf059ad55d432f0052ecd59ef122ecb") console.log(tx.toString(0));
//        
//        //console.log((tx.getOutgoingAmount() ? tx.getOutgoingAmount().toString() : "") + ", " + (tx.getIncomingAmount() ? tx.getIncomingAmount().toString() : ""));
//        if (tx.getOutgoingAmount()) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//        if (tx.getIncomingAmount()) incomingSum = incomingSum.add(tx.getIncomingAmount());
//      }
//      assertEquals(incomingSum.subtract(outgoingSum).toString(), account.getBalance().toString());
//    }
    
    // balance may not equal sum of unspent vouts if if unconfirmed txs
    // TODO monero-wallet-rpc: reason not to return unspent vouts on unconfirmed txs? then this isn't necessary
    boolean hasUnconfirmedTx = false;
    for (MoneroTxWallet tx : txs) if (tx.getInTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent vouts
    BigInteger walletSum = BigInteger.valueOf(0);
    for (MoneroOutputWallet vout : wallet.getVouts(new MoneroVoutFilter().setIsSpent(false))) walletSum = walletSum.add(vout.getAmount());
    if (!walletBalance.equals(walletSum)) assertTrue("Wallet balance must equal sum of unspent vouts if no unconfirmed txs", hasUnconfirmedTx);
    
    // account balances are sum of their unspent vouts
    for (MoneroAccount account : accounts) {
      BigInteger accountSum = BigInteger.valueOf(0);
      List<MoneroOutputWallet> accountVouts = wallet.getVouts(new MoneroVoutFilter().setAccountIndex(account.getIndex()).setIsSpent(false));
      for (MoneroOutputWallet vout : accountVouts) accountSum = accountSum.add(vout.getAmount());
      if (!account.getBalance().equals(accountSum)) assertTrue("Account balance must equal sum of its unspent vouts if no unconfirmed txs", hasUnconfirmedTx);
      
      // subaddress balances are sum of their unspent vouts
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        BigInteger subaddressSum = BigInteger.valueOf(0);
        List<MoneroOutputWallet> subaddressVouts = wallet.getVouts(new MoneroVoutFilter().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()).setIsSpent(false));
        for (MoneroOutputWallet vout : subaddressVouts) subaddressSum = subaddressSum.add(vout.getAmount());
        if (!subaddress.getBalance().equals(subaddressSum)) assertTrue("Subaddress balance must equal sum of its unspent vouts if no unconfirmed txs", hasUnconfirmedTx);
      }
    }
  }
  
  // Can get and set a transaction note
  @Test
  public void testSetTransactionNote() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, null, 1, 5);
    
    // set notes
    String uuid = UUID.randomUUID().toString();
    for (int i = 0; i < txs.size(); i++) {
      wallet.setTxNote(txs.get(i).getId(), uuid + i);
    }
    
    // get notes
    for (int i = 0; i < txs.size(); i++) {
      assertEquals(wallet.getTxNote(txs.get(i).getId()), uuid + i);
    }
  }
  
  // Can get and set multiple transaction notes
  // TODO: why does getting cached txs take 2 seconds when should already be cached?
  @Test
  public void testSetTransactionNotes() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set tx notes
    String uuid = UUID.randomUUID().toString();
    List<MoneroTxWallet> txs = getCachedTxs();
    assertTrue("Test requires 3 or more wallet transactions; run send tests", txs.size() >= 3);
    List<String> txIds = new ArrayList<String>();
    List<String> txNotes = new ArrayList<String>();
    for (int i = 0; i < txIds.size(); i++) {
      txIds.add(txs.get(i).getId());
      txNotes.add(uuid + i);
    }
    wallet.setTxNotes(txIds, txNotes);
    
    // get tx notes
    txNotes = wallet.getTxNotes(txIds);
    for (int i = 0; i < txIds.size(); i++) {
      assertEquals(uuid + i, txNotes.get(i));
    }
    
    // TODO: test that get transaction has note
  }
  
  // Can check a transfer using the transaction's secret key and the destination
  @Test
  public void testCheckTxKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroTxWallet> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxFilter().setIsConfirmed(true).setTransferFilter(new MoneroTransferFilter().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (MoneroException e) {
      throw new Error("No txs with outgoing destinations found; run send tests");
    }
    
    // test good checks
    assertTrue("No transactions found with outgoing destinations", txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      String key = wallet.getTxKey(tx.getId());
      assertFalse(tx.getOutgoingTransfer().getDestinations().isEmpty());
      for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
        MoneroCheckTx check = wallet.checkTxKey(tx.getId(), key, destination.getAddress());
        if (destination.getAmount().compareTo(BigInteger.valueOf(0)) > 0) {
          // TODO monero-wallet-rpc: indicates amount received amount is 0 despite transaction with transfer to this address
          // TODO monero-wallet-rpc: returns 0-4 errors, not consistent
//        assertTrue(check.getReceivedAmount().compareTo(BigInteger.valueOf(0)) > 0);
          if (check.getReceivedAmount().equals(BigInteger.valueOf(0))) {
            System.out.println("WARNING: key proof indicates no funds received despite transfer (txid=" + tx.getId() + ", key=" + key + ", address=" + destination.getAddress() + ", amount=" + destination.getAmount() + ")");
          }
        }
        else assertTrue(check.getReceivedAmount().equals(BigInteger.valueOf(0)));
        testCheckTx(tx, check);
      }
    }
    
    // test get tx key with invalid id
    try {
      wallet.getTxKey("invalid_tx_id");
      fail("Should throw exception for invalid key");
    } catch (MoneroException e) {
      assertEquals(-8, (int) e.getCode());
    }
    
    // test check with invalid tx id
    MoneroTxWallet tx = txs.get(0);
    String key = wallet.getTxKey(tx.getId());
    MoneroDestination destination = tx.getOutgoingTransfer().getDestinations().get(0);
    try {
      wallet.checkTxKey("invalid_tx_id", key, destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-8, (int) e.getCode());
    }
    
    // test check with invalid key
    try {
      wallet.checkTxKey(tx.getId(), "invalid_tx_key", destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-25, (int) e.getCode());
    }
    
    // test check with invalid address
    try {
      wallet.checkTxKey(tx.getId(), key, "invalid_tx_address");
      throw new Error("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-2, (int) e.getCode());
    }
    
    // test check with different address
    String differentAddress = null;
    for (MoneroTxWallet aTx : getCachedTxs()) {
      if (aTx.getOutgoingTransfer() == null || aTx.getOutgoingTransfer().getDestinations() == null) continue;
      for (MoneroDestination aDestination : aTx.getOutgoingTransfer().getDestinations()) {
        if (!aDestination.getAddress().equals(destination.getAddress())) {
          differentAddress = aDestination.getAddress();
          break;
        }
      }
    }
    assertNotNull("Could not get a different address to test", differentAddress);
    MoneroCheckTx check = wallet.checkTxKey(tx.getId(), key, differentAddress);
    assertTrue(check.getIsGood());
    assertTrue(check.getReceivedAmount().compareTo(BigInteger.valueOf(0)) >= 0);
    testCheckTx(tx, check);
  }
  
  // Can prove a transaction by getting its signature
  @Test
  public void testCheckTxProof() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroTxWallet> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxFilter().setIsConfirmed(true).setTransferFilter(new MoneroTransferFilter().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (MoneroException e) {
      throw new Error("No txs with outgoing destinations found; run send tests");
    }
    
    // test good checks with messages
    for (MoneroTxWallet tx : txs) {
      for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
        String signature = wallet.getTxProof(tx.getId(), destination.getAddress(), "This transaction definitely happened.");
        MoneroCheckTx check = wallet.checkTxProof(tx.getId(), destination.getAddress(), "This transaction definitely happened.", signature);
        testCheckTx(tx, check);
      }
    }
    
    // test good check without message
    MoneroTxWallet tx = txs.get(0);
    MoneroDestination destination = tx.getOutgoingTransfer().getDestinations().get(0);
    String signature = wallet.getTxProof(tx.getId(), destination.getAddress());
    MoneroCheckTx check = wallet.checkTxProof(tx.getId(), destination.getAddress(), null, signature);
    testCheckTx(tx, check);
    
    // test get proof with invalid id
    try {
      wallet.getTxProof("invalid_tx_id", destination.getAddress());
      throw new Error("Should throw exception for invalid key");
    } catch (MoneroException e) {
      assertEquals(-8, (int) e.getCode());
    }
    
    // test check with invalid tx id
    try {
      wallet.checkTxProof("invalid_tx_id", destination.getAddress(), null, signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-8, (int) e.getCode());
    }
    
    // test check with invalid address
    try {
      wallet.checkTxProof(tx.getId(), "invalid_tx_address", null, signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-2, (int) e.getCode());
    }
    
    // test check with wrong message
    signature = wallet.getTxProof(tx.getId(), destination.getAddress(), "This is the right message");
    check = wallet.checkTxProof(tx.getId(), destination.getAddress(), "This is the wrong message", signature);
    assertEquals(check.getIsGood(), false);
    testCheckTx(tx, check);
    
    // test check with wrong signature
    String wrongSignature = wallet.getTxProof(txs.get(1).getId(), txs.get(1).getOutgoingTransfer().getDestinations().get(0).getAddress(), "This is the right message");
    try {
      check = wallet.checkTxProof(tx.getId(), destination.getAddress(), "This is the right message", wrongSignature);  
      assertEquals(check.getIsGood(), false);
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode()); // TODO: sometimes comes back bad, sometimes throws exception.  ensure txs come from different addresses?
    }
  }
  
  // Can prove a spend using a generated signature and no destination public address
  @Test
  public void testCheckSpendProof() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get random confirmed outgoing txs
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, new MoneroTxFilter().setIsIncoming(false).setInTxPool(false).setIsFailed(false), 2, MAX_TX_PROOFS);
    for (MoneroTxWallet tx : txs) {
      assertEquals(true, tx.getIsConfirmed());
      assertNull(tx.getIncomingTransfers());
      assertNotNull(tx.getOutgoingTransfer());
    }
    
    // test good checks with messages
    for (MoneroTxWallet tx : txs) {
      String signature = wallet.getSpendProof(tx.getId(), "I am a message.");
      assertTrue(wallet.checkSpendProof(tx.getId(), "I am a message.", signature));
    }
    
    // test good check without message
    MoneroTxWallet tx = txs.get(0);
    String signature = wallet.getSpendProof(tx.getId());
    assertTrue(wallet.checkSpendProof(tx.getId(), null, signature));
    
    // test get proof with invalid id
    try {
      wallet.getSpendProof("invalid_tx_id");
      throw new Error("Should throw exception for invalid key");
    } catch (MoneroException e) {
      assertEquals(-8, (int) e.getCode());
    }
    
    // test check with invalid tx id
    try {
      wallet.checkSpendProof("invalid_tx_id", null, signature);
      throw new Error("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-8, (int) e.getCode());
    }
    
    // test check with invalid message
    signature = wallet.getSpendProof(tx.getId(), "This is the right message");
    assertEquals(false, wallet.checkSpendProof(tx.getId(), "This is the wrong message", signature));
    
    // test check with wrong signature
    signature = wallet.getSpendProof(txs.get(1).getId(), "This is the right message");
    assertEquals(false, wallet.checkSpendProof(tx.getId(), "This is the right message", signature));
  }
  
  // Can prove reserves in the wallet
  @Test
  public void testGetReserveProofWallet() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get proof of entire wallet
    String signature = wallet.getReserveProofWallet("Test message");
    
    // check proof of entire wallet
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", signature);
    assertTrue(check.getIsGood());
    testCheckReserve(check);
    BigInteger balance = wallet.getBalance();
    if (!balance.equals(check.getTotalAmount())) {  // TODO monero-wallet-rpc: this check fails with unconfirmed txs
      List<MoneroTxWallet> unconfirmedTxs = wallet.getTxs(new MoneroTxFilter().setInTxPool(true));
      assertTrue("Reserve amount must equal balance unless wallet has unconfirmed txs", unconfirmedTxs.size() > 0);
    }
    
    // test different wallet address
    String differentAddress = TestUtils.getRandomWalletAddress();
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof((wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test wrong message
    check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Wrong message", signature);
    assertEquals(false, check.getIsGood());  // TODO: specifically test reserve checks, probably separate objects
    testCheckReserve(check);
    
    // test wrong signature
    try {
      wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", "wrong signature");
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
  }
  
  // Can prove reserves in an account
  @Test
  public void testGetReserveProofAccount() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
        
    // test proofs of accounts
    int numNonZeroTests = 0;
    String msg = "Test message";
    List<MoneroAccount> accounts = wallet.getAccounts();
    String signature = null;
    for (MoneroAccount account : accounts) {
      if (account.getBalance().compareTo(BigInteger.valueOf(0)) > 0) {
        BigInteger checkAmount = (account.getBalance()).divide(BigInteger.valueOf(2));
        signature = wallet.getReserveProofAccount(account.getIndex(), checkAmount, msg);
        MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), msg, signature);
        assertTrue(check.getIsGood());
        testCheckReserve(check);
        assertTrue(check.getTotalAmount().compareTo(checkAmount) >= 0);
        numNonZeroTests++;
      } else {
        try {
          wallet.getReserveProofAccount(account.getIndex(), account.getBalance(), msg);
          throw new Error("Should have thrown exception");
        } catch (MoneroException e) {
          assertEquals(-1, (int) e.getCode());
          try {
            wallet.getReserveProofAccount(account.getIndex(), TestUtils.MAX_FEE, msg);
            throw new Error("Should have thrown exception");
          } catch (MoneroException e2) {
            assertEquals(-1, (int) e2.getCode());
          }
        }
      }
    }
    assertTrue("Must have more than one account with non-zero balance; run send-to-multiple tests", numNonZeroTests > 1);
    
    // test error when not enough balance for requested minimum reserve amount
    try {
      wallet.getReserveProofAccount(0, accounts.get(0).getBalance().add(TestUtils.MAX_FEE), "Test message");
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test different wallet address
    String differentAddress = TestUtils.getRandomWalletAddress();
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof((wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test wrong message
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Wrong message", signature);
    assertEquals(check.getIsGood(), false); // TODO: specifically test reserve checks, probably separate objects
    testCheckReserve(check);
    
    // test wrong signature
    try {
      wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", "wrong signature");
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
  }
  
  // Can get outputs in hex format
  @Test
  public void testGetOutputsHex() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String outputsHex = wallet.getOutputsHex();
    assertNotNull(outputsHex);  // TODO: this will fail if wallet has no outputs; run these tests on new wallet
    assertTrue(outputsHex.length() > 0);
  }
  
  // Can import outputs in hex format
  @Test
  public void testImportOutputsHex() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get outputs hex
    String outputsHex = wallet.getOutputsHex();
    
    // import outputs hex
    if (outputsHex != null) {
      int numImported = wallet.importOutputsHex(outputsHex);
      assertTrue(numImported > 0);
    }
  }
  
  // Can get signed key images
  @Test
  public void testGetSignedKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroKeyImage> images = wallet.getKeyImages();
    assertTrue("No signed key images in wallet", images.size() > 0);
    for (MoneroKeyImage image : images) {
      assertTrue(image instanceof MoneroKeyImage);
      assertTrue(image.getHex().length() > 0);
      assertTrue(image.getSignature().length() > 0);
    }
  }
  
  // Can get new key images from the last import
  @Test
  public void testGetNewKeyImagesFromLastImport() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get outputs hex
    String outputsHex = wallet.getOutputsHex();
    
    // import outputs hex
    if (outputsHex != null) {
      int numImported = wallet.importOutputsHex(outputsHex);
      assertTrue(numImported > 0);
    }
    
    // get and test new key images from last import
    List<MoneroKeyImage> images = wallet.getNewKeyImagesFromLastImport();
    assertTrue("No new key images in last import", images.size() > 0);  // TODO: these are already known to the wallet, so no new key images will be imported
    for (MoneroKeyImage image : images) {
      assertTrue(image.getHex().length() > 0);
      assertTrue(image.getSignature().length() > 0);
    }
  }
  
  // Can import key images
  @Test
  public void testImportKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroKeyImage> images = wallet.getKeyImages();
    assertTrue("Wallet does not have any key images; run send tests", images.size() > 0);
    MoneroKeyImageImportResult result = wallet.importKeyImages(images);
    assertTrue(result.getHeight() > 0);
    
    // determine if non-zero spent and unspent amounts are expected
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxFilter().setIsOutgoing(true).setIsConfirmed(true));
    BigInteger balance = wallet.getBalance();
    boolean hasSpent = txs.size() > 0;
    boolean hasUnspent = balance.compareTo(BigInteger.valueOf(0)) > 0;
    
    // test amounts
    TestUtils.testUnsignedBigInteger(result.getSpentAmount(), hasSpent);
    TestUtils.testUnsignedBigInteger(result.getUnspentAmount(), hasUnspent);
  }
  
  // Can sign and verify messages
  @Test
  public void testSignAndVerifyMessages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String msg = "This is a super important message which needs to be signed and verified.";
    String signature = wallet.sign(msg);
    boolean verified = wallet.verify(msg, wallet.getAddress(0, 0), signature);
    assertEquals(true, verified);
    verified = wallet.verify(msg, TestUtils.getRandomWalletAddress(), signature);
    assertEquals(false, verified);
  }
  
  // Can get and set arbitrary key/value attributes
  @Test
  public void testSetKeyValues() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set attributes
    Map<String, String> attrs = new HashMap<String, String>();
    for (int i = 0; i < 5; i++) {
      String key = "attr" + i;
      String val = UUID.randomUUID().toString();
      attrs.put(key, val);
      wallet.setAttribute(key, val);
    }
    
    // test attributes
    for (String key : attrs.keySet()) {
      assertEquals(wallet.getAttribute(key), attrs.get(key));
    }
  }
  
  // Can convert between a tx send config and payment URI
  @Test
  public void testCreatePaymentUri() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test with address and amount
    MoneroSendConfig config1 = new MoneroSendConfig(wallet.getAddress(0, 0), BigInteger.valueOf(0));
    String uri = wallet.createPaymentUri(config1);
    MoneroSendConfig config2 = wallet.parsePaymentUri(uri);
    assertEquals(config1, config2);
    
    // test with all fields3
    config1.getDestinations().get(0).setAmount(new BigInteger("425000000000"));
    config1.setPaymentId("03284e41c342f036");
    config1.setRecipientName("John Doe");
    config1.setNote("OMZG XMR FTW");
    uri = wallet.createPaymentUri(config1);
    config2 = wallet.parsePaymentUri(uri);
    assertEquals(config1, config2);
    
    // test with undefined address
    String address = config1.getDestinations().get(0).getAddress();
    config1.getDestinations().get(0).setAddress(null);
    try {
      wallet.createPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroException e) {
      assertEquals(-11, (int) e.getCode());
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
    config1.getDestinations().get(0).setAddress(address);
    
    // test with invalid payment id
    config1.setPaymentId("bizzup");
    try {
      wallet.createPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroException e) {
      assertEquals(-11, (int) e.getCode());
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
  }
  
  @Test
  public void testMining() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroMiningStatus status = daemon.getMiningStatus();
    if (status.getIsActive()) wallet.stopMining();
    wallet.startMining(2, false, true);
    wallet.stopMining();
  }
  
  // ------------------------------- TEST RELAYS ------------------------------
  
  // Can send from multiple subaddresses in a single transaction
  @Test
  public void testSendFromSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendFromMultiple(null);
  }
  
  // Can send from multiple subaddresses in split transactions
  @Test
  public void testSendFromSubaddressesSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendFromMultiple(new MoneroSendConfig().setCanSplit(true));
  }
  
  private void testSendFromMultiple(MoneroSendConfig sendConfig) {
    if (sendConfig == null) sendConfig = new MoneroSendConfig();
    
    int NUM_SUBADDRESSES = 2; // number of subaddresses to send from
    
    // get first account with (NUM_SUBADDRESSES + 1) subaddresses with unlocked balances
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertTrue("This test requires at least 2 accounts; run send-to-multiple tests", accounts.size() >= 2);
    MoneroAccount srcAccount = null;
    List<MoneroSubaddress> unlockedSubaddresses = new ArrayList<MoneroSubaddress>();
    boolean hasBalance = false;
    for (MoneroAccount account : accounts) {
      unlockedSubaddresses.clear();
      int numSubaddressBalances = 0;
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(TestUtils.MAX_FEE) > 0) numSubaddressBalances++;
        if (subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) unlockedSubaddresses.add(subaddress);
      }
      if (numSubaddressBalances >= NUM_SUBADDRESSES + 1) hasBalance = true;
      if (unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1) {
        srcAccount = account;
        break;
      }
    }
    assertTrue("Wallet does not have account with " + (NUM_SUBADDRESSES + 1) + " subaddresses with balances; run send-to-multiple tests", hasBalance);
    assertTrue("Wallet is waiting on unlocked funds", unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1);
    
    // determine the indices of the first two subaddresses with unlocked balances
    List<Integer> fromSubaddressIndices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_SUBADDRESSES; i++) {
      fromSubaddressIndices.add(unlockedSubaddresses.get(i).getIndex());
    }
    
    // determine the amount to send (slightly less than the sum to send from)
    BigInteger sendAmount = BigInteger.valueOf(0);
    for (int fromSubaddressIdx : fromSubaddressIndices) {
      sendAmount = sendAmount.add(srcAccount.getSubaddresses().get(fromSubaddressIdx).getUnlockedBalance()).subtract(TestUtils.MAX_FEE);
    }
    
    BigInteger fromBalance = BigInteger.valueOf(0);
    BigInteger fromUnlockedBalance = BigInteger.valueOf(0);
    for (int subaddressIdx : fromSubaddressIndices) {
      MoneroSubaddress subaddress = wallet.getSubaddress(srcAccount.getIndex(), subaddressIdx);
      fromBalance = fromBalance.add(subaddress.getBalance());
      fromUnlockedBalance = fromUnlockedBalance.add(subaddress.getUnlockedBalance());
    }
    
    // send from the first subaddresses with unlocked balances
    String address = wallet.getPrimaryAddress();
    sendConfig.setDestinations(Arrays.asList(new MoneroDestination(address, sendAmount)));
    sendConfig.setAccountIndex(srcAccount.getIndex());
    sendConfig.setSubaddressIndices(fromSubaddressIndices);
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (Boolean.TRUE.equals(sendConfig.getCanSplit())) {
      List<MoneroTxWallet> sendTxs = wallet.sendSplit(sendConfig);
      for (MoneroTxWallet tx : sendTxs) txs.add(tx);
    } else {
      txs.add(wallet.send(sendConfig));
    }
    
    // test that balances of intended subaddresses decreased
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    for (int i = 0; i < accounts.size(); i++) {
      assertEquals(accounts.get(i).getSubaddresses().size(), accountsAfter.get(i).getSubaddresses().size());
      for (int j = 0; j < accounts.get(i).getSubaddresses().size(); j++) {
        MoneroSubaddress subaddressBefore = accounts.get(i).getSubaddresses().get(j);
        MoneroSubaddress subaddressAfter = accountsAfter.get(i).getSubaddresses().get(j);
        if (i == srcAccount.getIndex() && fromSubaddressIndices.contains(j)) {
          assertTrue("Subaddress [" + i + "," + j + "] unlocked balance should have decreased but changed from " + subaddressBefore.getUnlockedBalance().toString() + " to " + subaddressAfter.getUnlockedBalance().toString(), subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) < 0); // TODO: Subaddress [0,1] unlocked balance should have decreased          
        } else {
          assertTrue("Subaddress [" + i + "," + j + "] unlocked balance should not have changed", subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) == 0);          
        }
      }
    }
    
    // test context
    TestContext ctx = new TestContext();
    ctx.sendConfig = sendConfig;
    ctx.wallet = wallet;
    ctx.isSendResponse = true;
    
    // test each transaction
    assertTrue(txs.size() > 0);
    BigInteger outgoingSum = BigInteger.valueOf(0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        BigInteger destinationSum = BigInteger.valueOf(0);
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          assertEquals(address, destination.getAddress());
          destinationSum = destinationSum.add(destination.getAmount());
        }
        assertTrue(tx.getOutgoingAmount().equals(destinationSum));  // assert that transfers sum up to tx amount
      }
    }
    
    // assert that tx amounts sum up the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(outgoingSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      throw new Error("Tx amounts are too different: " + sendAmount + " - " + outgoingSum + " = " + sendAmount.subtract(outgoingSum));
    }
  }
  
  // Can send to an address in a single transaction
  @Test
  public void testSend() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(null);
  }
  
  // Can send to an address in a single transaction with a payment id
  // NOTE: this test will be invalid when payment ids are fully removed
  @Test
  public void testSendWithPaymentId() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress();
    String paymentId = integratedAddress.getPaymentId();
    testSendToSingle(new MoneroSendConfig(paymentId + paymentId + paymentId + paymentId));  // 64 character payment id
  }
  
  // Can send to an address in a single transaction with a ring size
  @Test
  public void testSendWithRingSize() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendConfig().setRingSize(8));
  }
  
  // Can send to an address with split transactions
  @Test
  public void testSendSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendConfig().setCanSplit(true));
  }
  
  // Can create then relay a transaction to send to a single address
  @Test
  public void testCreateThenRelay() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendConfig().setDoNotRelay(true));
  }
  
  // Can create then relay split transactions to send to a single address
  @Test
  public void testCreateThenRelaySplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendConfig().setCanSplit(true).setDoNotRelay(true));
  }
  
  private void testSendToSingle(MoneroSendConfig sendConfig) {
    if (sendConfig == null) sendConfig = new MoneroSendConfig();
    
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
    BigInteger unlockedBalanceBefore  = fromSubaddress.getUnlockedBalance();
    
    // init send config
    BigInteger sendAmount = unlockedBalanceBefore.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    sendConfig.setDestinations(Arrays.asList(new MoneroDestination(address, sendAmount)));
    sendConfig.setAccountIndex(fromAccount.getIndex());
    sendConfig.setSubaddressIndices(Arrays.asList(fromSubaddress.getIndex()));
    
    // send to self
    if (Boolean.TRUE.equals(sendConfig.getCanSplit())) {
      List<MoneroTxWallet> sendTxs = wallet.sendSplit(sendConfig);
      for (MoneroTxWallet tx : sendTxs) txs.add(tx);
    } else {
      txs.add(wallet.send(sendConfig));
    }
    
    // handle non-relayed transaction
    if (Boolean.TRUE.equals(sendConfig.getDoNotRelay())) {
      
      // build test context
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendConfig = sendConfig;
      ctx.isSendResponse = true;
      
      // test transactions
      for (MoneroTxWallet tx : txs) {
        testTxWallet(tx, ctx);
      }
      
      // relay txs
      List<String> txMetadatas = new ArrayList<String>();
      for (MoneroTxWallet tx : txs) txMetadatas.add(tx.getMetadata());
      List<String> txIds = wallet.relayTxs(txMetadatas);
      for (String txId : txIds) assertEquals(64, txId.length());
      
      // fetch txs for testing
      txs = wallet.getTxs(new MoneroTxFilter().setTxIds(txIds));
    }
    
    // test that balance and unlocked balance decreased
    // TODO: test that other balances did not decrease
    MoneroSubaddress subaddress = wallet.getSubaddress(fromAccount.getIndex(), fromSubaddress.getIndex());
    assertTrue(subaddress.getBalance().compareTo(balanceBefore) < 0);
    assertTrue(subaddress.getUnlockedBalance().compareTo(unlockedBalanceBefore) < 0);
    
    // build test context
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendConfig = sendConfig;
    ctx.isSendResponse = Boolean.TRUE.equals(sendConfig.getDoNotRelay()) ? false : true;
    
    // test transactions
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      assertEquals(fromAccount.getIndex(), tx.getOutgoingTransfer().getAccountIndex());
      assertEquals(1, tx.getOutgoingTransfer().getSubaddressIndices().size());
      assertEquals(fromSubaddress.getIndex(), tx.getOutgoingTransfer().getSubaddressIndices().get(0));
      assertTrue(sendAmount.equals(tx.getOutgoingAmount()));
      if (sendConfig.getPaymentId() != null) assertEquals(sendConfig.getPaymentId(), tx.getPaymentId());
      
      // test outgoing destinations
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        assertEquals(1, tx.getOutgoingTransfer().getDestinations().size());
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          assertEquals(destination.getAddress(), address);
          assertTrue(sendAmount.equals(destination.getAmount()));
        }
      }
    }
  }
  
  // Can send to multiple addresses in a single transaction
  @Test
  public void testSendToMultiple() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToMultiple(5, 3, false);
  }
  
  // Can send to multiple addresses in split transactions
  @Test
  public void testSendToMultipleSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToMultiple(1, 15, true);
  }
  
  /**
   * Sends funds from the first unlocked account to multiple accounts and subaddresses.
   * 
   * @param numAccounts is the number of accounts to receive funds
   * @param numSubaddressesPerAccount is the number of subaddresses per account to receive funds
   * @param canSplit specifies if the operation can be split into multiple transactions
   * @param useJsConfig specifies if the api should be invoked with a JS object instead of a MoneroSendConfig
   */
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit) {
    
    // test constants
    int totalSubaddresses = numAccounts * numSubaddressesPerAccount;
    BigInteger minAccountAmount = TestUtils.MAX_FEE.multiply(BigInteger.valueOf(totalSubaddresses)).multiply(BigInteger.valueOf(SEND_DIVISOR)).add(TestUtils.MAX_FEE); // account balance must be more than divisor * fee * numAddresses + fee so each destination amount is at least a fee's worth 
    
    // send funds from first account with sufficient unlocked funds
    MoneroAccount srcAccount = null;
    boolean hasBalance = false;
    for (MoneroAccount account : wallet.getAccounts()) {
      if (account.getBalance().compareTo(minAccountAmount) > 0) hasBalance = true;
      if (account.getUnlockedBalance().compareTo(minAccountAmount) > 0) {
        srcAccount = account;
        break;
      }
    }
    assertTrue("Wallet does not have enough balance; load '" + TestUtils.WALLET_RPC_NAME_1 + "' with XMR in order to test sending", hasBalance);
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
    
    // build send config using MoneroSendConfig
    MoneroSendConfig config = new MoneroSendConfig();
    config.setMixin(TestUtils.MIXIN);
    config.setAccountIndex(srcAccount.getIndex());
    config.setDestinations(new ArrayList<MoneroDestination>());
    config.setCanSplit(canSplit);
    for (int i = 0; i < destinationAddresses.size(); i++) {
      config.getDestinations().add(new MoneroDestination(destinationAddresses.get(i), sendAmountPerSubaddress));
    }
    
    // send tx(s) with config
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (canSplit) {
      List<MoneroTxWallet> sendTxs = wallet.sendSplit(config);
      for (MoneroTxWallet tx : sendTxs) txs.add(tx);
    } else {
      txs.add(wallet.send(config));
    }
    
    // test that wallet balance decreased
    MoneroAccount account = wallet.getAccount(srcAccount.getIndex());
    assertTrue(account.getBalance().compareTo(balance) < 0);
    assertTrue(account.getUnlockedBalance().compareTo(unlockedBalance) < 0);
    
    // build test context
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendConfig = config;
    ctx.isSendResponse = true;
    
    // test each transaction
    assertTrue(txs.size() > 0);
    BigInteger outgoingSum = BigInteger.valueOf(0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        BigInteger destinationSum = BigInteger.valueOf(0);
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          assertTrue(destinationAddresses.contains(destination.getAddress()));
          destinationSum = destinationSum.add(destination.getAmount());
        }
        assertTrue(tx.getOutgoingAmount().equals(destinationSum));  // assert that transfers sum up to tx amount
      }
    }
    
    // assert that outgoing amounts sum up to the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(outgoingSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      throw new Error("Actual send amount is too different from requested send amount: " + sendAmount + " - " + outgoingSum + " = " + sendAmount.subtract(outgoingSum));
    }
  }
  
  // Can sweep individual outputs identified by their key images
  @Test
  public void testSweepOutputs() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    
    // test config
    int numVouts = 3;
    
    // get unspent and unlocked vouts to sweep
    List<MoneroOutputWallet> vouts = wallet.getVouts(new MoneroVoutFilter().setIsSpent(false).setIsUnlocked(true));
    assertTrue("Wallet has no unspent vouts; run send tests", vouts.size() >= numVouts);
    vouts = vouts.subList(0, numVouts);
    
    // sweep each vout by key image
    boolean useParams = true; // for loop flips in order to alternate test
    for (MoneroOutputWallet vout : vouts) {
      testVout(vout);
      
      // sweep output to address
      String address = wallet.getAddress(vout.getAccountIndex(), vout.getSubaddressIndex());
      MoneroSendConfig sendConfig = new MoneroSendConfig(address).setKeyImage(vout.getKeyImage().getHex());
      MoneroTxWallet tx;
      if (useParams) tx = wallet.sweepOutput(address, vout.getKeyImage().getHex(), null); // test params
      else tx = wallet.sweepOutput(sendConfig);  // test config
      
      // test resulting tx
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendConfig = sendConfig;
      ctx.isSendResponse = true;
      ctx.isSweepResponse = true;
      testTxWallet(tx, ctx);
      useParams = !useParams;
    }
    
    // get vouts after sweeping
    List<MoneroOutputWallet> afterVouts = wallet.getVouts();
    
    // swept vouts are now spent
    for (MoneroOutputWallet afterVout : afterVouts) {
      for (MoneroOutputWallet vout : vouts) {
        if (vout.getKeyImage().getHex().equals(afterVout.getKeyImage().getHex())) {
          assertTrue("Output should be spent", afterVout.getIsSpent());
        }
      }
    }
  }
  
  // Can sweep dust without relaying
  @Test
  public void testSweepDustNoRelay() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    
    // generate non-relayed transactions to sweep dust
    List<MoneroTxWallet> txs = wallet.sweepDust(true);
    if (txs.isEmpty()) return;  // dust does not exist after rct
    
    // test txs
    TestContext ctx = new TestContext();
    ctx.isSendResponse = true;
    ctx.sendConfig = new MoneroSendConfig().setDoNotRelay(true);
    ctx.isSweepResponse = true;
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
    
    // relay txs
    List<String> metadatas = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) metadatas.add(tx.getMetadata());
    List<String> txIds = wallet.relayTxs(metadatas);
    assertEquals(txIds.size(), txs.size());
    for (String txId : txIds) assertEquals(64, txId.length());
    
    // fetch and test txs
    txs = wallet.getTxs(new MoneroTxFilter().setTxIds(txIds));
    ctx.sendConfig.setDoNotRelay(false);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
  }
  
  // Can sweep dust
  @Test
  public void testSweepDust() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    List<MoneroTxWallet> txs = wallet.sweepDust();
    if (txs.isEmpty()) return;  // dust does not exist after rct
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendConfig = null;
    ctx.isSendResponse = true;
    ctx.isSweepResponse = true;
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
  }
  
  // --------------------------- NOTIFICATION TESTS ---------------------------
  
  // TODO: test sending to multiple accounts
  
  // Can update a locked tx sent from/to the same account as blocks are added to the chain
  @Test
  public void testUpdateLockedSameAccount() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    MoneroSendConfig sendConfig = new MoneroSendConfig(wallet.getPrimaryAddress(), TestUtils.MAX_FEE);
    sendConfig.setAccountIndex(0);
    sendConfig.setUnlockTime(3);
    sendConfig.setCanSplit(false);
    testSendAndUpdateTxs(sendConfig);
  }
  
  // Can update split locked txs sent from/to the same account as blocks are added to the chain
  @Test
  public void testUpdateLockedSameAccountSplit() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroSendConfig sendConfig = new MoneroSendConfig(wallet.getPrimaryAddress(), TestUtils.MAX_FEE);
    sendConfig.setAccountIndex(0);
    sendConfig.setUnlockTime(3);
    sendConfig.setCanSplit(true);
    testSendAndUpdateTxs(sendConfig);
  }
  
  // Can update a locked tx sent from/to different accounts as blocks are added to the chain
  @Test
  public void testUpdateLockedDifferentAccounts() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroSendConfig sendConfig = new MoneroSendConfig((wallet.getSubaddress(1, 0)).getAddress(), TestUtils.MAX_FEE);
    sendConfig.setAccountIndex(0);
    sendConfig.setUnlockTime(3);
    sendConfig.setCanSplit(false);
    testSendAndUpdateTxs(sendConfig);
  }
  
  // Can update a locked tx sent from/to different accounts as blocks are added to the chain
  @Test
  public void testUpdateLockedDifferentAccountsSplit() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroSendConfig sendConfig = new MoneroSendConfig((wallet.getSubaddress(1, 0)).getAddress(), TestUtils.MAX_FEE);
    sendConfig.setAccountIndex(0);
    sendConfig.setUnlockTime(3);
    sendConfig.setCanSplit(true);
    testSendAndUpdateTxs(sendConfig);
  }
  
  /**
   * Tests sending a tx with an unlockTime then tracking and updating it as
   * blocks are added to the chain.
   * 
   * TODO: test wallet accounting throughout this; dedicated method? probably.
   * 
   * Allows sending to and from the same account which is an edge case where
   * incoming txs are occluded by their outgoing counterpart (issue #4500)
   * and also where it is impossible to discern which incoming output is
   * the tx amount and which is the change amount without wallet metadata.
   * 
   * @param sendConfig is the send configuration to send and test
   * @throws InterruptedException 
   */
  private void testSendAndUpdateTxs(MoneroSendConfig sendConfig) {
    
    // unlike js version, this test starts and stops its own mining, so it's wrapped in order to stop mining if anything fails
    try {
      
      // attempt to start mining to push the network along
      boolean startedMining = false;
      MoneroMiningStatus miningStatus = daemon.getMiningStatus();
      if (!miningStatus.getIsActive()) {
        try {
          wallet.startMining(8, false, true);
          startedMining = true;
        } catch (MoneroException e) {
          // no problem
        }
      }
      
      // send transactions
      List<MoneroTxWallet> sentTxs;
      if (sendConfig.getCanSplit()) sentTxs = wallet.sendSplit(sendConfig);
      else sentTxs = Arrays.asList(wallet.send(sendConfig));
      
      // build test context
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendConfig = sendConfig;
      ctx.isSendResponse = true;
      
      // test sent transactions
      for (MoneroTxWallet tx : sentTxs) {
        testTxWallet(tx, ctx);
        assertEquals(false, tx.getIsConfirmed());
        assertEquals(true, tx.getInTxPool());
      }
      
      // track resulting outgoing and incoming txs as blocks are added to the chain
      List<MoneroTxWallet> updatedTxs = null;
      
      // loop to update txs through confirmations
      int numConfirmations = 0;
      int numConfirmationsTotal = 2; // number of confirmations to test
      while (numConfirmations < numConfirmationsTotal) {
        
        // wait for a block
        MoneroBlockHeader header = daemon.getNextBlockHeader();
        System.out.println("*** Block " + header.getHeight() + " added to chain ***");
        
        // give wallet time to catch up, otherwise incoming tx may not appear
        // TODO: this lets new block slip, okay?
        try {
          TimeUnit.SECONDS.sleep(5);
        } catch (InterruptedException e) {
          throw new RuntimeException(e);
        }
        
        // get incoming/outgoing txs with sent ids
        List<String> txIds = new ArrayList<String>();
        for (MoneroTxWallet sentTx : sentTxs) txIds.add(sentTx.getId());  // TODO: convenience methods wallet.getTxById(), getTxsById()?
        MoneroTxFilter filter = new MoneroTxFilter().setTxIds(txIds);
        List<MoneroTxWallet> fetchedTxs = getAndTestTxs(wallet, filter, null, true);
        assertFalse(fetchedTxs.isEmpty());
        
        // test fetched txs
        testOutInPairs(wallet, fetchedTxs, sendConfig, false);

        // merge fetched txs into updated txs and original sent txs
        for (MoneroTxWallet fetchedTx : fetchedTxs) {
          
          // merge with updated txs
          if (updatedTxs == null) updatedTxs = fetchedTxs;
          else {
            for (MoneroTxWallet updatedTx : updatedTxs) {
              if (!fetchedTx.getId().equals(updatedTx.getId())) continue;
              if (fetchedTx.getIsOutgoing() != updatedTx.getIsOutgoing()) continue; // skip if directions are different
              updatedTx.merge(fetchedTx.copy());
              if (updatedTx.getBlock() == null && fetchedTx.getBlock() != null) updatedTx.setBlock(fetchedTx.getBlock().copy().setTxs(Arrays.asList(updatedTx)));  // copy block for testing
            }
          }
          
          // merge with original sent txs
          for (MoneroTxWallet sentTx : sentTxs) {
            if (!fetchedTx.getId().equals(sentTx.getId())) continue;
            if (fetchedTx.getIsOutgoing() != sentTx.getIsOutgoing()) continue; // skip if directions are different
            sentTx.merge(fetchedTx.copy());  // TODO: it's mergeable but tests don't account for extra info from send (e.g. hex) so not tested; could specify in test config
          }
        }
        
        // test updated txs
        testOutInPairs(wallet, updatedTxs, sendConfig, false);
        
        // update confirmations in order to exit loop
        numConfirmations = fetchedTxs.get(0).getNumConfirmations();
      }
      
      // stop mining if it was started by this test
      if (startedMining) wallet.stopMining();
      
    } catch (MoneroException e) {
      throw e;
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
    }
  }
  
  private void testOutInPairs(MoneroWallet wallet, List<MoneroTxWallet> txs, MoneroSendConfig sendConfig, boolean isSendResponse) {
    
    // for each out tx
    for (MoneroTxWallet tx : txs) {
      testUnlockTx(wallet, tx, sendConfig, isSendResponse);
      if (tx.getOutgoingTransfer() != null) continue;
      MoneroTxWallet txOut = tx;
      
      // find incoming counterpart
      MoneroTxWallet txIn = null;
      for (MoneroTxWallet tx2 : txs) {
        if (tx2.getIsIncoming() && tx.getId().equals(tx2.getId())) {
          txIn = tx2;
          break;
        }
      }
      
      // test out / in pair
      // TODO monero-wallet-rpc: incoming txs occluded by their outgoing counterpart #4500
      if (txIn == null) {
        System.out.println("WARNING: outgoing tx " + txOut.getId() + " missing incoming counterpart (issue #4500)");
      } else {
        testOutInPair(txOut, txIn);
      }
    }
  }
  
  private void testOutInPair(MoneroTxWallet txOut, MoneroTxWallet txIn) {
    assertEquals(txOut.getIsConfirmed(), txIn.getIsConfirmed());
    assertEquals(txIn.getIncomingAmount(), txOut.getOutgoingAmount());
  }
  
  private void testUnlockTx(MoneroWallet wallet, MoneroTxWallet tx, MoneroSendConfig sendConfig, boolean isSendResponse) {
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendConfig = sendConfig;
    ctx.isSendResponse = isSendResponse;
    try {
      testTxWallet(tx, ctx);
    } catch (MoneroException e) {
      System.out.println(tx.toString());
      throw e;
    }
    assertEquals(tx.getUnlockTime(), sendConfig.getUnlockTime()); // TODO: send config as part of test, then this fn not necessary
  }
  
  // --------------------------------- RESET TESTS --------------------------------
  
  // Can sweep subaddresses
  @Test
  public void testSweepSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    throw new Error("Not implemented");
  }
  
  // Can sweep accounts
  @Test
  public void testSweepAccounts() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    final int NUM_ACCOUNTS_TO_SWEEP = 1;
    
    // collect accounts with balance and unlocked balance
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    List<MoneroAccount> balanceAccounts = new ArrayList<MoneroAccount>();
    List<MoneroAccount> unlockedAccounts = new ArrayList<MoneroAccount>();
    for (MoneroAccount account : accounts) {
      if (account.getBalance().compareTo(BigInteger.valueOf(0)) > 0) balanceAccounts.add(account);
      if (account.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) unlockedAccounts.add(account);
    }
    
    // test requires at least one more account than the number being swept to verify it does not change
    assertTrue("Test requires balance in at least " + (NUM_ACCOUNTS_TO_SWEEP + 1) + " accounts; run send-to-multiple tests", balanceAccounts.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    assertTrue("Wallet is waiting on unlocked funds", unlockedAccounts.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    
    // sweep from first unlocked accounts
    for (int i = 0; i < NUM_ACCOUNTS_TO_SWEEP; i++) {
      
      // sweep unlocked account
      MoneroAccount unlockedAccount = unlockedAccounts.get(i);
      List<MoneroTxWallet> txs = wallet.sweepAccount(unlockedAccount.getIndex(), wallet.getPrimaryAddress());
      
      // test transactions
      assertTrue(txs.size() > 0);
      for (MoneroTxWallet tx : txs) {
        MoneroSendConfig config = new MoneroSendConfig(wallet.getPrimaryAddress());
        config.setAccountIndex(unlockedAccount.getIndex());
        TestContext ctx = new TestContext();
        ctx.wallet = wallet;
        ctx.sendConfig = config;
        ctx.isSweepResponse = true;
        testTxWallet(tx, ctx);
      }
      
      // assert no unlocked funds in account
      MoneroAccount account = wallet.getAccount(unlockedAccount.getIndex());
      assertTrue(account.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) == 0);
    }
    
    // test accounts after sweeping
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    for (int i = 0; i < accounts.size(); i++) {
      MoneroAccount accountBefore = accounts.get(i);
      MoneroAccount accountAfter = accountsAfter.get(i);
      
      // determine if account was swept
      boolean swept = false;
      for (int j = 0; j < NUM_ACCOUNTS_TO_SWEEP; j++) {
        if (unlockedAccounts.get(j).getIndex().equals(accountBefore.getIndex())) {
          swept = true;
          break;
        }
      }
      
      // test that unlocked balance is 0 if swept, unchanged otherwise
      if (swept) {
        assertTrue(accountAfter.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) == 0);
      } else {
        assertTrue(accountBefore.getUnlockedBalance().compareTo(accountAfter.getUnlockedBalance()) == 0);
      }
    }
  }
  
  // Can rescan the blockchain
  @Test
  @Ignore // disabled so tests don't delete local cache
  public void testRescanBlockchain() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    wallet.rescanBlockchain();
    for (MoneroTxWallet tx : wallet.getTxs()) {
      testTxWallet(tx, null);
    }
  }
  
  // Can sweep the whole wallet
  @Test
  @Ignore // disabled so tests don't sweep the whole wallet
  public void testSweepWallet() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    
    // sweep destination
    String destination = wallet.getPrimaryAddress();
    
    // verify 2 accounts with unlocked balance
    List<MoneroSubaddress> subaddressesBalance = getSubaddressesWithBalance();
    List<MoneroSubaddress> subaddressesUnlockedBalance = getSubaddressesWithUnlockedBalance();
    assertTrue("Test requires multiple accounts with a balance; run send to multiple first", subaddressesBalance.size() >= 2);
    assertTrue("Wallet is waiting on unlocked funds", subaddressesUnlockedBalance.size() >= 2);
    
    // sweep
    List<MoneroTxWallet> txs = wallet.sweepWallet(destination);
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      MoneroSendConfig config = new MoneroSendConfig(destination);
      config.setAccountIndex(tx.getOutgoingTransfer().getAccountIndex());
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendConfig = config;
      ctx.isSweepResponse = true;
      testTxWallet(tx, ctx);
    }
    
    // assert no unlocked funds across subaddresses
    subaddressesUnlockedBalance = getSubaddressesWithUnlockedBalance();
    System.out.println(subaddressesUnlockedBalance);
    assertTrue("Wallet should have no unlocked funds after sweeping all", subaddressesUnlockedBalance.isEmpty());
  }
  
  // --------------------------------- PRIVATE --------------------------------
  
  private List<MoneroTxWallet> getCachedTxs() {
    if (txCache != null) return txCache;
    txCache = wallet.getTxs();
    return txCache;
  }
  
  /**
   * Compares two MoneroTxs by their timestamp.
   */
  private class MoneroTxComparator implements Comparator<MoneroTx> {
    @Override
    public int compare(MoneroTx tx1, MoneroTx tx2) {
      long timestampA = tx1.getIsConfirmed() ? tx1.getBlock().getTimestamp() : tx1.getReceivedTimestamp();
      long timestampB = tx2.getIsConfirmed() ? tx2.getBlock().getTimestamp() : tx2.getReceivedTimestamp();
      if (timestampA < timestampB) return -1;
      if (timestampA > timestampB) return 1;
      return 0;
    }
  }
  
  /**
   * Fetchs and tests transactions according to the given config.
   * 
   * TODO: convert config to filter and ensure each tx passes filter, same with testGetTransfer and getAndTestVouts
   */
  private static List<MoneroTxWallet> getAndTestTxs(MoneroWallet wallet, MoneroTxFilter filter, TestContext ctx, Boolean isExpected) {
    List<MoneroTxWallet> txs = wallet.getTxs(filter);
    assertNotNull(txs);
    if (Boolean.FALSE.equals(isExpected)) assertTrue(txs.isEmpty());
    if (Boolean.TRUE.equals(isExpected)) assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) testTxWallet(tx, ctx);
    return txs;
  }
  
  /**
   * Fetchs and tests transfers according to the given config.
   */
  private static List<MoneroTransfer> getAndTestTransfers(MoneroWallet wallet, MoneroTransferFilter filter, TestContext ctx, Boolean isExpected) {
    List<MoneroTransfer> transfers = wallet.getTransfers(filter);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, transfers.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Transactions were expected but not found; run send tests?", transfers.size() > 0);
    if (ctx == null) ctx = new TestContext();
    ctx.wallet = wallet;
    for (MoneroTransfer transfer : transfers) testTxWallet(transfer.getTx(), ctx);
    return transfers;
  }

  /**
   * Fetchs and tests vouts according to the given config.
   */
  private static List<MoneroOutputWallet> getAndTestVouts(MoneroWallet wallet, MoneroVoutFilter filter, Boolean isExpected) {
    List<MoneroOutputWallet> vouts = wallet.getVouts(filter);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, vouts.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Vouts were expected but not found; run send tests?", vouts.size() > 0);
    for (MoneroOutputWallet vout : vouts) testVout(vout);
    return vouts;
  }
  
  /**
   * Provides context or configuration for test methods to test a type.
   */
  public static class TestContext {
    MoneroWallet wallet;
    MoneroSendConfig sendConfig;
    Boolean hasOutgoingTransfer;
    Boolean hasIncomingTransfers;
    Boolean hasDestinations;
    Boolean isSweepResponse;
    Boolean doNotTestCopy;
    Boolean getVouts;
    Boolean isSendResponse;
    public TestContext() { }
    public TestContext(TestContext ctx) {
      if (ctx != null) {
        this.wallet = ctx.wallet;
        this.sendConfig = ctx.sendConfig;
        this.hasOutgoingTransfer = ctx.hasOutgoingTransfer;
        this.hasIncomingTransfers = ctx.hasIncomingTransfers;
        this.hasDestinations = ctx.hasDestinations;
        this.isSweepResponse = ctx.isSweepResponse;
        this.doNotTestCopy = ctx.doNotTestCopy;
        this.getVouts = ctx.getVouts;
        this.isSendResponse = ctx.isSendResponse;
      }
    }
  }
  
  
  private List<MoneroSubaddress> getSubaddressesWithBalance() {
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0) subaddresses.add(subaddress);
      }
    }
    return subaddresses;
  }

  private List<MoneroSubaddress> getSubaddressesWithUnlockedBalance() {
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) subaddresses.add(subaddress);
      }
    }
    return subaddresses;
  }
  
  // ------------------------------ PRIVATE STATIC ----------------------------

   private static void testAccount(MoneroAccount account) {
    
    // test account
    assertNotNull(account);
    assertTrue(account.getIndex() >= 0);
    assertNotNull(account.getPrimaryAddress());
    TestUtils.testUnsignedBigInteger(account.getBalance());
    TestUtils.testUnsignedBigInteger(account.getUnlockedBalance());
    assertTrue(account.getLabel() == null || !account.getLabel().isEmpty());
    
    // if given, test subaddresses and that their balances add up to account balances
    if (account.getSubaddresses() != null) {
      BigInteger balance = BigInteger.valueOf(0);
      BigInteger unlockedBalance = BigInteger.valueOf(0);
      for (int i = 0; i < account.getSubaddresses().size(); i++) {
        testSubaddress(account.getSubaddresses().get(i));
        assertEquals(account.getIndex(), account.getSubaddresses().get(i).getAccountIndex());
        assertEquals(i, (int) account.getSubaddresses().get(i).getIndex());
        balance = balance.add(account.getSubaddresses().get(i).getBalance());
        unlockedBalance = unlockedBalance.add(account.getSubaddresses().get(i).getUnlockedBalance());
      }
      assertTrue("Subaddress balances " + balance + " does not equal account balance " + account.getBalance(), account.getBalance().equals(balance));
      assertTrue("Subaddress unlocked balances " + unlockedBalance + " does not equal account unlocked balance " + account.getUnlockedBalance(), account.getUnlockedBalance().equals(unlockedBalance));
    }
  }

  private static void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getAccountIndex() >= 0);
    assertTrue(subaddress.getIndex() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getLabel() == null || !subaddress.getLabel().isEmpty());
    TestUtils.testUnsignedBigInteger(subaddress.getBalance());
    TestUtils.testUnsignedBigInteger(subaddress.getUnlockedBalance());
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    assertNotNull(subaddress.getIsUsed());
    if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0) assertTrue(subaddress.getIsUsed());
    assertTrue(subaddress.getNumBlocksToUnlock() >= 0);
  }
  
  /**
   * Tests a wallet transaction with a test configuration.
   * 
   * @param tx is the wallet transaction to test
   * @param ctx provides test context
   *        ctx.wallet is used to cross reference tx info if available
   *        testConfig.sendConfig specifies the tx's originating send configuration
   *        testConfig.isSendResponse indicates if the tx is built from a send response, which contains additional fields (e.g. key)
   *        ctx.hasDestinations specifies if the tx has an outgoing transfer with destinations, undefined if doesn't matter
   *        ctx.getVouts specifies if vouts were fetched and should therefore be expected with incoming transfers
   */
  protected static void testTxWallet(MoneroTxWallet tx, TestContext ctx) {
    
    // validate / sanitize inputs
    ctx = new TestContext(ctx);
    ctx.wallet = null;  // TODO: re-enable
    assertNotNull(tx);
    if (ctx.isSendResponse == null || ctx.sendConfig == null) {
      assertNull("if either sendConfig or isSendResponse is defined, they must both be defined", ctx.isSendResponse);
      assertNull("if either sendConfig or isSendResponse is defined, they must both be defined", ctx.sendConfig);
    }
    
    // test common field types
    testTxWalletTypes(tx);
    
    // test confirmed
    if (tx.getIsConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeight() > 0);
      assertTrue(tx.getBlock().getTimestamp() > 0);
      assertEquals(true, tx.getIsRelayed());
      assertEquals(false, tx.getIsFailed());
      assertEquals(false, tx.getInTxPool());
      assertEquals(false, tx.getDoNotRelay());
      assertTrue(tx.getNumConfirmations() > 0);
      assertEquals(false, tx.getIsDoubleSpend());
    } else {
      assertNull(tx.getBlock());
      assertEquals(0, (int) tx.getNumConfirmations());
    }
    
    // test in tx pool
    if (tx.getInTxPool()) {
      assertEquals(false, tx.getIsConfirmed());
      assertEquals(false, tx.getDoNotRelay());
      assertEquals(true, tx.getIsRelayed());
      assertEquals(false, tx.getIsDoubleSpend()); // TODO: test double spend attempt
      
      // these should be initialized unless a response from sending
      if (!Boolean.TRUE.equals(ctx.isSendResponse)) {
        assertTrue(tx.getReceivedTimestamp() > 0);
        assertTrue(tx.getNumSuggestedConfirmations() > 0);
      }
    } else {
      assertNull(tx.getNumSuggestedConfirmations());
      assertNull(tx.getLastRelayedTimestamp());
    }
    
    // test coinbase tx
    if (tx.getIsCoinbase()) {
      assertEquals(BigInteger.valueOf(0), tx.getFee());
      assertTrue(tx.getIncomingTransfers().size() > 0);
    }
    
    // test failed  // TODO: what else to test associated with failed
    if (tx.getIsFailed()) {
      assertTrue(tx.getOutgoingTransfer() instanceof MoneroTransfer);
      assertTrue(tx.getReceivedTimestamp() > 0);
    } else {
      if (tx.getIsRelayed()) assertEquals(tx.getIsDoubleSpend(), false);
      else {
        assertEquals(false, tx.getIsRelayed());
        assertEquals(true, tx.getDoNotRelay());
        assertNull(tx.getIsDoubleSpend());
      }
    }
    assertNull(tx.getLastFailedHeight());
    assertNull(tx.getLastFailedId());
    
    // received time only for tx pool or failed txs
    if (tx.getReceivedTimestamp() != null) {
      assertTrue(tx.getInTxPool() || tx.getIsFailed());
    }
    
    // test relayed tx
    if (tx.getIsRelayed()) assertEquals(tx.getDoNotRelay(), false);
    if (tx.getDoNotRelay()) assertTrue(!tx.getIsRelayed());
    
    // test outgoing transfer per configuration
    if (Boolean.FALSE.equals(ctx.hasOutgoingTransfer)) assertNull(tx.getOutgoingTransfer());
    if (Boolean.TRUE.equals(ctx.hasDestinations)) assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
    
    // test outgoing transfer
    if (tx.getOutgoingTransfer() != null) {
      assertTrue(tx.getIsOutgoing());
      testTransfer(tx.getOutgoingTransfer());
      if (Boolean.TRUE.equals(ctx.isSweepResponse)) assertEquals(tx.getOutgoingTransfer().getDestinations().size(), 1);
      
      // TODO: handle special cases
    } else {
      assertTrue(tx.getIncomingTransfers().size() > 0);
      assertNull(tx.getOutgoingAmount());
      assertNull(tx.getOutgoingTransfer());
      assertNull(tx.getMixin());
      assertNull(tx.getFullHex());
      assertNull(tx.getMetadata());
      assertNull(tx.getKey());
    }
    
    // test incoming transfers
    if (tx.getIncomingTransfers() != null) {
      assertTrue(tx.getIsIncoming());
      assertTrue(tx.getIncomingTransfers().size() > 0);
      TestUtils.testUnsignedBigInteger(tx.getIncomingAmount());      
      assertEquals(tx.getIsFailed(), false);
      
      // test each transfer and collect transfer sum
      BigInteger transferSum = BigInteger.valueOf(0);
      for (MoneroIncomingTransfer transfer : tx.getIncomingTransfers()) {
        testTransfer(transfer);
        assertNotNull(transfer.getAddress());
        assertTrue(transfer.getAccountIndex() >= 0);
        assertTrue(transfer.getSubaddressIndex() >= 0);
        transferSum = transferSum.add(transfer.getAmount());
        if (ctx.wallet != null) assertEquals(ctx.wallet.getAddress(transfer.getAccountIndex(), transfer.getSubaddressIndex()), transfer.getAddress());
        
        // TODO special case: transfer amount of 0
      }
      
      // incoming transfers add up to incoming tx amount
      assertEquals(tx.getIncomingAmount(), transferSum);
    } else {
      assertNotNull(tx.getOutgoingTransfer());
      assertNull(tx.getIncomingAmount());
      assertNull(tx.getIncomingTransfers());
    }
    
    // test tx results from send or relay
    if (Boolean.TRUE.equals(ctx.isSendResponse)) {
      
      // test common attributes
      MoneroSendConfig sendConfig = ctx.sendConfig;
      assertEquals(false, tx.getIsConfirmed());
      testTransfer(tx.getOutgoingTransfer());
      assertEquals(sendConfig.getMixin(), tx.getMixin());
      assertEquals(sendConfig.getUnlockTime() != null ? sendConfig.getUnlockTime() : 0, (int) tx.getUnlockTime());
      assertNull(tx.getBlock());
      if (Boolean.TRUE.equals(sendConfig.getCanSplit())) assertNull(tx.getKey());  // tx key unknown if from split response
      else assertTrue(tx.getKey().length() > 0);
      assertNotNull(tx.getFullHex());
      assertTrue(tx.getFullHex().length() > 0);
      assertNotNull(tx.getMetadata());
      assertNull(tx.getReceivedTimestamp());
      
      // test destinations of sent tx
      assertEquals(sendConfig.getDestinations().size(), tx.getOutgoingTransfer().getDestinations().size());
      for (int i = 0; i < sendConfig.getDestinations().size(); i++) {
        assertEquals(sendConfig.getDestinations().get(i).getAddress(), tx.getOutgoingTransfer().getDestinations().get(i).getAddress());
        if (Boolean.TRUE.equals(ctx.isSweepResponse)) {
          assertEquals(1, sendConfig.getDestinations().size());
          assertNull(sendConfig.getDestinations().get(i).getAmount());
          assertEquals(tx.getOutgoingTransfer().getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        } else {
          assertEquals(sendConfig.getDestinations().get(i).getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        }
      }
      
      // test relayed txs
      if (!Boolean.TRUE.equals(sendConfig.getDoNotRelay())) {
        assertEquals(true, tx.getInTxPool());
        assertEquals(false, tx.getDoNotRelay());
        assertEquals(true, tx.getIsRelayed());
        assertTrue(tx.getLastRelayedTimestamp() > 0);
        assertEquals(false, tx.getIsDoubleSpend());
      }
      
      // test non-relayed txs
      else {
        assertEquals(false, tx.getInTxPool());
        assertEquals(true, tx.getDoNotRelay());
        assertEquals(false, tx.getIsRelayed());
        assertNull(tx.getLastRelayedTimestamp());
        assertNull(tx.getIsDoubleSpend());
      }
    }
    
    // test tx result query
    else {
      assertNull(tx.getMixin());
      assertNull(tx.getKey());
      assertNull(tx.getFullHex());
      assertNull(tx.getMetadata());
      assertNull(tx.getLastRelayedTimestamp());
    }
    
    // test vouts
    if (tx.getIsIncoming() && Boolean.TRUE.equals(ctx.getVouts)) {
      if (tx.getIsConfirmed()) {
        assertNotNull(tx.getVouts());
        assertTrue(tx.getVouts().size() > 0);
      } else {
        assertNull(tx.getVouts());
      }
    }
    if (tx.getVouts() != null) for (MoneroOutputWallet vout : tx.getVoutsWallet()) testVout(vout);
    
    // test deep copy
    if (!Boolean.TRUE.equals(ctx.doNotTestCopy)) testTxWalletCopy(tx, ctx);
  }

  /**
   * Tests that common tx field types are valid regardless of tx state.
   * 
   * @param tx is the tx to test
   */
  private static void testTxWalletTypes(MoneroTxWallet tx) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getIsConfirmed());
    assertNotNull(tx.getIsCoinbase());
    assertNotNull(tx.getIsFailed());
    assertNotNull(tx.getIsRelayed());
    assertNotNull(tx.getInTxPool());
    TestUtils.testUnsignedBigInteger(tx.getFee());
    assertNull(tx.getVins());
    if (tx.getPaymentId() != null) assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId()); // default payment id converted to null
    if (tx.getNote() != null) assertTrue(tx.getNote().length() > 0);  // empty notes converted to undefined
    assertTrue(tx.getUnlockTime() >= 0);
    assertNull(tx.getSize());   // TODO (monero-wallet-rpc): add tx_size to get_transfers and get_transfer_by_txid
    assertNull(tx.getWeight());
  }

  private static void testTxWalletCopy(MoneroTxWallet tx, TestContext ctx) {
    
    // copy tx and assert deep equality
    MoneroTxWallet copy = tx.copy();
    assertTrue(copy instanceof MoneroTxWallet);
    assertEquals(copy, tx);
    
    // test different references
    if (tx.getOutgoingTransfer() != null) {
      assertTrue(tx.getOutgoingTransfer() != copy.getOutgoingTransfer());
      assertTrue(tx.getOutgoingTransfer().getTx() != copy.getOutgoingTransfer().getTx());
      if (tx.getOutgoingTransfer().getDestinations() != null) {
        assertTrue(tx.getOutgoingTransfer().getDestinations() != copy.getOutgoingTransfer().getDestinations());
        for (int i = 0; i < tx.getOutgoingTransfer().getDestinations().size(); i++) {
          assertEquals(copy.getOutgoingTransfer().getDestinations().get(i), tx.getOutgoingTransfer().getDestinations().get(i));
          assertTrue(tx.getOutgoingTransfer().getDestinations().get(i) != copy.getOutgoingTransfer().getDestinations().get(i));
        }
      }
    }
    if (tx.getIncomingTransfers() != null) {
      for (int i = 0; i < tx.getIncomingTransfers().size(); i++) {
        assertEquals(copy.getIncomingTransfers().get(i), tx.getIncomingTransfers().get(i));
        assertTrue(tx.getIncomingTransfers().get(i) != copy.getIncomingTransfers().get(i));
      }
    }
    if (tx.getVouts() != null) {
      for (int i = 0; i < tx.getVouts().size(); i++) {
        assertEquals(copy.getVouts().get(i), tx.getVouts().get(i));
        assertTrue(tx.getVouts().get(i) != copy.getVouts().get(i));
      }
    }
    
    // test copied tx
    ctx = new TestContext(ctx);
    ctx.doNotTestCopy = true;
    if (tx.getBlock() != null) copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy))); // copy block for testing
    testTxWallet(copy, ctx);
    
    // test merging with copy
    MoneroTxWallet merged = copy.merge(copy.copy());
    assertEquals(merged.toString(), tx.toString());
  }
  
  private static void testTransfer(MoneroTransfer transfer) {
    assertNotNull(transfer);
    TestUtils.testUnsignedBigInteger(transfer.getAmount());
    
    // transfer and tx reference each other
    assertNotNull(transfer.getTx());
    if (!transfer.equals(transfer.getTx().getOutgoingTransfer())) {
      assertNotNull(transfer.getTx().getIncomingTransfers());
      assertTrue("Transaction does not reference given transfer", transfer.getTx().getIncomingTransfers().contains(transfer));
    }
    
    // test destinations sum to outgoing amount
    if (transfer.getIsOutgoing() && ((MoneroOutgoingTransfer) transfer).getDestinations() != null) {
      List<MoneroDestination> destinations = ((MoneroOutgoingTransfer) transfer).getDestinations();
      assertTrue(destinations.size() > 0);
      BigInteger sum = BigInteger.valueOf(0);
      for (MoneroDestination destination : destinations) {
        assertNotNull(destination.getAddress());
        TestUtils.testUnsignedBigInteger(destination.getAmount(), true);
        sum = sum.add(destination.getAmount());
      }
      if (!transfer.getAmount().equals(sum)) System.out.println(transfer.getTx().toString());
      assertEquals(transfer.getAmount(), sum);
    }
    
    // transfer is outgoing xor incoming
    assertTrue((transfer.getIsOutgoing() == true && transfer.getIsIncoming() == false) || (transfer.getIsOutgoing() == false && transfer.getIsIncoming() == true));
  }
  
  private static void testVout(MoneroOutputWallet vout) {
    assertNotNull(vout);
    assertTrue(vout.getAccountIndex() >= 0);
    assertTrue(vout.getSubaddressIndex() >= 0);
    assertTrue(vout.getIndex() >= 0);
    assertNotNull(vout.getIsSpent());
    assertNotNull(vout.getIsUnlocked());
    assertNotNull(vout.getKeyImage());
    assertTrue(vout.getKeyImage().getHex().length() > 0);
    TestUtils.testUnsignedBigInteger(vout.getAmount(), true);
    
    // vout has circular reference to its transaction which has some initialized fields
    MoneroTxWallet tx = vout.getTx();
    assertNotNull(tx);
    assertTrue(tx.getVouts().contains(vout));
    assertNotNull(tx.getId());
    assertEquals(true, tx.getIsConfirmed());  // TODO monero-wallet-rpc: possible to get unconfirmed vouts?
    assertEquals(true, tx.getIsRelayed());
    assertEquals(false, tx.getIsFailed());
    
    // test copying
    MoneroOutputWallet copy = vout.copy();
    assertTrue(copy != vout);
    assertEquals(vout.toString(), copy.toString());
    assertNull(copy.getTx());  // TODO: should vout copy do deep copy of tx so models are graph instead of tree?  Would need to work out circular references
  }
  
  /**
   * Gets random transactions.
   * 
   * @param wallet is the wallet to query for transactions
   * @param filter filters the transactions to retrieve
   * @param minTxs specifies the minimum number of transactions (null for no minimum)
   * @param maxTxs specifies the maximum number of transactions (null for all filtered transactions)
   * @return List<MoneroTxWallet> are the random transactions
   */
  private static List<MoneroTxWallet> getRandomTransactions(MoneroWallet wallet, MoneroTxFilter filter, Integer minTxs, Integer maxTxs) {
    List<MoneroTxWallet> txs = wallet.getTxs(filter);
    if (minTxs != null) assertTrue(txs.size() + "/" + minTxs + " transactions found with filter", txs.size() >= minTxs);
    Collections.shuffle(txs);
    if (maxTxs == null) return txs;
    else return txs.subList(0, Math.min(maxTxs, txs.size()));
  }
  
  private static void testCheckTx(MoneroTxWallet tx, MoneroCheckTx check) {
    assertNotNull(check.getIsGood());
    if (check.getIsGood()) {
      assert(check.getNumConfirmations() >= 0);
      assertNotNull(check.getInTxPool());
      TestUtils.testUnsignedBigInteger(check.getReceivedAmount());
      if (check.getInTxPool()) assertEquals(0, (int) check.getNumConfirmations());
      else assertTrue(check.getNumConfirmations() > 0); // TODO (monero-wall-rpc) this fails (confirmations is 0) for (at least one) transaction that has 1 confirmation on testCheckTxKey()
    } else {
      assertNull(check.getNumConfirmations());
      assertNull(check.getInTxPool());
      assertNull(check.getNumConfirmations());
    }
  }

  private static void testCheckReserve(MoneroCheckReserve check) {
    assertNotNull(check.getIsGood());
    if (check.getIsGood()) {
      TestUtils.testUnsignedBigInteger(check.getSpentAmount());
      assertEquals(BigInteger.valueOf(0), check.getSpentAmount());  // TODO sometimes see non-zero, seg fault after sweep and send tests
      TestUtils.testUnsignedBigInteger(check.getTotalAmount());
      assert(check.getTotalAmount().compareTo(BigInteger.valueOf(0)) >= 0);
    } else {
      assertNull(check.getSpentAmount());
      assertNull(check.getTotalAmount());
    }
  }
}
