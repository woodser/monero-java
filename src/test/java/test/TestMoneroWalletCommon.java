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
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
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
import monero.wallet.request.MoneroOutputRequest;
import monero.wallet.request.MoneroSendRequest;
import monero.wallet.request.MoneroTransferRequest;
import monero.wallet.request.MoneroTxRequest;
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
  protected static final boolean TEST_NOTIFICATIONS = true;
  protected static final boolean TEST_RESETS = false;
  private static final int MAX_TX_PROOFS = 25;   // maximum number of transactions to check for each proof, undefined to check all
  private static final int SEND_MAX_DIFF = 60;
  private static final int SEND_DIVISOR = 2;
  
  // instance variables
  protected MoneroWallet wallet;        // wallet instance to test
  protected MoneroDaemonRpc daemon;     // daemon instance to test
  private List<MoneroTxWallet> txCache; // local tx cache
  
  public TestMoneroWalletCommon() {
    wallet = getTestWallet();
    daemon = getTestDaemon();
  }

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
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
  public void testGetLanguages() {
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
  
  // Can get the private spend key
  @Test
  public void testGetPrivateSpendKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String privateSpendKey = wallet.getPrivateSpendKey();
    MoneroUtils.validatePrivateSpendKey(privateSpendKey);
  }
  
  // Can get the primary address
  @Test
  public void testGetPrimaryAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String primaryAddress = wallet.getPrimaryAddress();
    MoneroUtils.validateAddress(primaryAddress);
    assertEquals((wallet.getSubaddress(0, 0)).getAddress(), primaryAddress);
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
  }
  
  // Can get addresses out of range of used accounts and subaddresses
  @Test
  public void testGetSubaddressAddressOutOfRange() {
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size();
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNotNull(address);
    assertTrue(address.length() > 0);
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
      assertEquals("Address doesn't belong to the wallet", e.getMessage());
    }
    
    // test invalid address
    try {
      subaddress = wallet.getAddressIndex("this is definitely not an address");
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      assertEquals("Invalid address", e.getMessage());
    }
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
    String invalidPaymentId = "invalid_payment_id_123456";
    try {
      integratedAddress = wallet.getIntegratedAddress(invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown a RPC exception");
    } catch (MoneroException e) {
      //assertEquals(-5, (int) e.getCode());  // TODO: error codes specific to rpc?
      assertEquals("Invalid payment ID: " + invalidPaymentId, e.getDescription());
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
    long numBlocks = 100;
    long chainHeight = daemon.getHeight();
    assertTrue(chainHeight >= numBlocks);
    MoneroSyncResult result = wallet.sync(chainHeight - numBlocks);  // sync end of chain
    assertTrue(result.getNumBlocksFetched() >= 0);
    assertNotNull(result.getReceivedMoney());
  }
  
  // Can get the current height that the wallet is synchronized to
  @Test
  public void testGetHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    long height = wallet.getHeight();
    assertTrue(height >= 0);
  }
  
  // Can get the locked and unlocked balances of the wallet, accounts, and subaddresses
  @Test
  public void testGetAllBalances() {
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
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 1);
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
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 1);
    
    // fetch and test account
    createdAccount = wallet.getAccount(createdAccount.getIndex());
    testAccount(createdAccount);
    assertEquals(label, createdAccount.getLabel());

    // create account with same label
    createdAccount = wallet.createAccount(label);
    testAccount(createdAccount);
    assertEquals(label, createdAccount.getLabel());
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 2);
    
    // fetch and test account
    createdAccount = wallet.getAccount(createdAccount.getIndex());
    testAccount(createdAccount);
    assertEquals(label, createdAccount.getLabel());
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
    Map<Long, MoneroBlock> blockPerHeight = new HashMap<Long, MoneroBlock>();
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
  
  // Can get transactions by id
  @Test
  public void testGetTxsById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    int maxNumTxs = 10;  // max number of txs to test
    
    // fetch all txs for testing
    List<MoneroTxWallet> txs = wallet.getTxs();
    assertTrue("Test requires at least 2 txs to fetch by id", txs.size() > 1);
    
    // randomly pick a few for fetching by id
    Collections.shuffle(txs);
    txs = txs.subList(0, Math.min(txs.size(), maxNumTxs));
    
    // test fetching by id
    MoneroTxWallet fetchedTx = wallet.getTx(txs.get(0).getId());
    assertEquals(txs.get(0).getId(), fetchedTx.getId());
    testTxWallet(fetchedTx);
    
    // test fetching by ids
    String txId1 = txs.get(0).getId();
    String txId2 = txs.get(1).getId();
    List<MoneroTxWallet> fetchedTxs = wallet.getTxs(txId1, txId2);
    
    // test fetching by ids as collection
    List<String> txIds = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) txIds.add(tx.getId());
    fetchedTxs = wallet.getTxs(txIds);
    assertEquals(txs.size(), fetchedTxs.size());
    for (int i = 0; i < txs.size(); i++) {
      assertEquals(txs.get(i).getId(), fetchedTxs.get(i).getId());
      testTxWallet(fetchedTxs.get(i));
    }
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
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxRequest().setTxId(randomTx.getId()), null, true);
      assertEquals(txs.size(), 1);
      MoneroTxWallet merged = txs.get(0).merge(randomTx.copy()); // txs change with chain so check mergeability
      testTxWallet(merged, null);
    }
    
    // get transactions by ids
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxRequest().setTxIds(txIds), null, null);
    assertEquals(txs.size(), randomTxs.size());
    for (MoneroTxWallet tx : txs) assertTrue(txIds.contains(tx.getId()));
    
    // get transactions with an outgoing transfer
    TestContext ctx = new TestContext();
    ctx.hasOutgoingTransfer = true;
    txs = getAndTestTxs(wallet, new MoneroTxRequest().setIsOutgoing(true), ctx, true);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIsOutgoing());
      assertNotNull(tx.getOutgoingTransfer());
      testTransfer(tx.getOutgoingTransfer(), null);
    }
    
    // get transactions without an outgoing transfer
    ctx.hasOutgoingTransfer = false;
    txs = getAndTestTxs(wallet, new MoneroTxRequest().setIsOutgoing(false), ctx, true);
    for (MoneroTxWallet tx : txs) assertNull(tx.getOutgoingTransfer());
    
    // get transactions with incoming transfers
    ctx = new TestContext();
    ctx.hasIncomingTransfers = true;
    txs = getAndTestTxs(wallet, new MoneroTxRequest().setIsIncoming(true), ctx, true);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIsIncoming());
      assertTrue(tx.getIncomingTransfers().size() > 0);
      for (MoneroIncomingTransfer transfer : tx.getIncomingTransfers()) {
        testTransfer(transfer, null);
      }
    }
    
    // get transactions without incoming transfers
    ctx.hasIncomingTransfers = false;
    txs = getAndTestTxs(wallet, new MoneroTxRequest().setIsIncoming(false), ctx, true);
    for (MoneroTxWallet tx : txs)  {
      assertFalse(tx.getIsIncoming());
      assertNull(tx.getIncomingTransfers());
    }
    
    // get transactions associated with an account
    int accountIdx = 1;
    txs = wallet.getTxs(new MoneroTxRequest().setTransferRequest(new MoneroTransferRequest().setAccountIndex(accountIdx)));
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
    txs = wallet.getTxs(new MoneroTxRequest().setTransferRequest(new MoneroTransferRequest().setIsIncoming(true).setAccountIndex(accountIdx)));
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
    
    // get txs with manually built request that are confirmed and have an outgoing transfer from account 0
    ctx = new TestContext();
    ctx.hasOutgoingTransfer = true;
    MoneroTxRequest txRequest = new MoneroTxRequest();
    txRequest.setIsConfirmed(true);
    txRequest.setTransferRequest(new MoneroTransferRequest().setAccountIndex(0).setIsOutgoing(true));
    txs = getAndTestTxs(wallet, txRequest, ctx, true);
    for (MoneroTxWallet tx : txs) {
      if (!tx.getIsConfirmed()) System.out.println(tx);
      assertEquals(true, tx.getIsConfirmed());
      assertTrue(tx.getIsOutgoing());
      assertEquals(0, (int) tx.getOutgoingTransfer().getAccountIndex());
    }
    
    // get txs with outgoing transfers that have destinations to account 1
    txs = getAndTestTxs(wallet, new MoneroTxRequest().setTransferRequest(new MoneroTransferRequest().setHasDestinations(true).setAccountIndex(0)), null, null);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIsOutgoing());
      assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
    }
    
    // test block height filtering
    {
      txs = wallet.getTxs(new MoneroTxRequest().setIsConfirmed(true));
      assertTrue("No transactions; run send to multiple test", txs.size() > 0);
        
      // get and sort block heights in ascending order
      List<Long> heights = new ArrayList<Long>();
      for (MoneroTxWallet tx : txs) {
        heights.add(tx.getBlock().getHeight());
      }
      Collections.sort(heights);
      
      // pick minimum and maximum heights for filtering
      long minHeight = -1;
      long maxHeight = -1;
      if (heights.size() == 1) {
        minHeight = 0;
        maxHeight = heights.get(0) - 1;
      } else {
        minHeight = heights.get(0) + 1;
        maxHeight = heights.get(heights.size() - 1) - 1;
      }
      
      // assert some transactions filtered
      int unfilteredCount = txs.size();
      txs = getAndTestTxs(wallet, new MoneroTxRequest().setMinHeight(minHeight).setMaxHeight(maxHeight), null, true);
      assertTrue(txs.size() < unfilteredCount);
      for (MoneroTx tx : txs) {
        long height = tx.getBlock().getHeight();
        assertTrue(height >= minHeight && height <= maxHeight);
      }
    }
    
    // include outputs with transactions
    ctx = new TestContext();
    ctx.includeOutputs = true;
    txs = getAndTestTxs(wallet, new MoneroTxRequest().setIncludeOutputs(true), ctx, true);
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
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxRequest().setHasPaymentId(true), 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) {
      assertNotNull(randomTx.getPaymentId());
    }
    
    // get transactions by payment id
    List<String> paymentIds = new ArrayList<String>();
    for (MoneroTxWallet tx : randomTxs) paymentIds.add(tx.getPaymentId());
    assertTrue(paymentIds.size() > 1);
    for (String paymentId : paymentIds) {
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxRequest().setPaymentId(paymentId), null, null);
      assertEquals(1, txs.size());
      assertNotNull(txs.get(0).getPaymentId());
      MoneroUtils.validatePaymentId(txs.get(0).getPaymentId());
    }
    
    // get transactions by payment ids
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxRequest().setPaymentIds(paymentIds), null, null);
    for (MoneroTxWallet tx : txs) {
      assertTrue(paymentIds.contains(tx.getPaymentId()));
    }
  }
  
  // Returns all known fields of txs regardless of filtering
  @Test
  public void testGetTxsFieldsWithFiltering() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch wallet txs
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxRequest().setIsConfirmed(true));
    for (MoneroTxWallet tx : txs) {
      
      // find tx sent to same wallet with incoming transfer in different account than src account
      if (tx.getOutgoingTransfer() == null || tx.getIncomingTransfers() == null) continue;
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() == tx.getOutgoingTransfer().getAccountIndex()) continue;
        
        // fetch tx with filtering
        List<MoneroTxWallet> filteredTxs = wallet.getTxs(new MoneroTxRequest().setTransferRequest(new MoneroTransferRequest().setIsIncoming(true).setAccountIndex(transfer.getAccountIndex())));
        MoneroTxWallet filteredTx = Filter.apply(new MoneroTxRequest().setTxIds(tx.getId()), filteredTxs).get(0);
        
        // txs should be the same (mergeable)
        assertEquals(tx.getId(), filteredTx.getId());
        tx.merge(filteredTx);
        
        // test is done
        return;
      }
    }
    
    // test did not fully execute
    throw new RuntimeException("Test requires tx sent from/to different accounts of same wallet but none found; run send tests");
  }
  
  // Validates inputs when getting transactions
  @Test
  public void testGetTxsValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // fetch random txs for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    
    // valid, invalid, and unknown tx ids for tests
    String txId = randomTxs.get(0).getId();
    String invalidId = "invalid_id";
    String unknownId1 = "6c4982f2499ece80e10b627083c4f9b992a00155e98bcba72a9588ccb91d0a61";
    String unknownId2 = "ff397104dd875882f5e7c66e4f852ee134f8cf45e21f0c40777c9188bc92e943";
    
    // fetch unknown tx id
    try {
      wallet.getTx(unknownId1);
      fail("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + unknownId1, e.getDescription());
    }
    
    // fetch unknown tx id using request
    try {
      wallet.getTxs(new MoneroTxRequest().setTxId(unknownId1));
      throw new Error("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + unknownId1, e.getDescription());
    }
    
    // fetch unknown tx id in collection
    try {
      wallet.getTxs(txId, unknownId1);
      fail("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + unknownId1, e.getDescription());
    }
    
    // fetch unknown tx ids in collection
    try {
      wallet.getTxs(txId, unknownId1, unknownId2);
      fail("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + unknownId1, e.getDescription()); // TODO: list all invalid ids in error description?
    }
    
    // fetch invalid id
    try {
      wallet.getTx(invalidId);
      fail("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + invalidId, e.getDescription());
    }
    
    // fetch invalid id collection
    try {
      wallet.getTxs(txId, invalidId);
      fail("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + invalidId, e.getDescription());
    }
    
    // fetch invalid ids in collection
    try {
      wallet.getTxs(txId, invalidId, "invalid_id_2");
      fail("Should have thrown error getting tx id unknown to wallet");
    } catch (MoneroException e) {
      assertEquals("Tx not found in wallet: " + invalidId, e.getDescription());
    }
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
      List<MoneroTransfer> accountTransfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setAccountIndex(account.getIndex()), null, null);
      for (MoneroTransfer transfer : accountTransfers) assertEquals(transfer.getAccountIndex(), account.getIndex());
      
      // get transfers by subaddress index
      List<MoneroTransfer> subaddressTransfers = new ArrayList<MoneroTransfer>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setAccountIndex(subaddress.getAccountIndex()).setSubaddressIndex(subaddress.getIndex()), null, null);
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
      
      // collect unique subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroTransfer transfer : subaddressTransfers) {
        if (transfer.getIsIncoming()) subaddressIndices.add(((MoneroIncomingTransfer) transfer).getSubaddressIndex());
        else subaddressIndices.addAll(((MoneroOutgoingTransfer) transfer).getSubaddressIndices());
      }
      
      // get and test transfers by subaddress indices
      List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), null, null);
      //if (transfers.size() != subaddressTransfers.size()) System.out.println("WARNING: outgoing transfers always from subaddress 0 (monero-wallet-rpc #5171)");
      assertEquals(subaddressTransfers.size(), transfers.size()); // TODO monero-wallet-rpc: these may not be equal because outgoing transfers are always from subaddress 0 (#5171) and/or incoming transfers from/to same account are occluded (#4500)
      for (MoneroTransfer transfer : transfers) {
        assertEquals(transfer.getAccountIndex(), account.getIndex());
        if (transfer.getIsIncoming()) assertTrue(subaddressIndices.contains(((MoneroIncomingTransfer) transfer).getSubaddressIndex()));
        else {
          Set<Integer> intersections = new HashSet<Integer>(subaddressIndices);
          intersections.retainAll(((MoneroOutgoingTransfer) transfer).getSubaddressIndices());
          assertTrue("Subaddresses must overlap", intersections.size() > 0);
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
    List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setIsIncoming(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.getIsIncoming());
    
    // get outgoing transfers
    transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setIsOutgoing(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.getIsOutgoing());
    
    // get confirmed transfers to account 0
    transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setAccountIndex(0).setTxRequest(new MoneroTxRequest().setIsConfirmed(true)), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(0, (int) transfer.getAccountIndex());
      assertTrue(transfer.getTx().getIsConfirmed());
    }
    
    // get confirmed transfers to [1, 2]
    transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setAccountIndex(1).setSubaddressIndex(2).setTxRequest(new MoneroTxRequest().setIsConfirmed(true)), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(1, (int) transfer.getAccountIndex());
      if (transfer.getIsIncoming()) assertEquals(2, (int) ((MoneroIncomingTransfer) transfer).getSubaddressIndex());
      else assertTrue(((MoneroOutgoingTransfer) transfer).getSubaddressIndices().contains(2));
      assertTrue(transfer.getTx().getIsConfirmed());
    }
    
    // get transfers in the tx pool
    transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setInTxPool(true)), null, null);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.getTx().getInTxPool());
    }
    
    // get random transactions
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, null, 3, 5);
    
    // get transfers with a tx id
    List<String> txIds = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) {
      txIds.add(tx.getId());
      transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setId(tx.getId())), null, true);
      for (MoneroTransfer transfer : transfers) assertEquals(tx.getId(), transfer.getTx().getId());
    }
    
    // get transfers with tx ids
    transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setTxIds(txIds)), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(txIds.contains(transfer.getTx().getId()));
    
    // TODO: test that transfers with the same txId have the same tx reference
    
    // TODO: test transfers destinations
    
    // get transfers with pre-built request that are confirmed and have outgoing destinations
    MoneroTransferRequest transferRequest = new MoneroTransferRequest();
    transferRequest.setIsOutgoing(true);
    transferRequest.setHasDestinations(true);
    transferRequest.setTxRequest(new MoneroTxRequest().setIsConfirmed(true));
    transfers = getAndTestTransfers(wallet, transferRequest, null, null);
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
    List<MoneroTransfer> transfers = wallet.getTransfers(new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setId("invalid_id")));
    assertEquals(0, transfers.size());
    
    // test invalid id in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    transfers = wallet.getTransfers(new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setTxIds(randomTxs.get(0).getId(), "invalid_id")));
    assertTrue(transfers.size() > 0);
    MoneroTxWallet tx = transfers.get(0).getTx();
    for (MoneroTransfer transfer : transfers) assertTrue(tx == transfer.getTx());
    
    // test unused subaddress indices
    transfers = wallet.getTransfers(new MoneroTransferRequest().setAccountIndex(0).setSubaddressIndices(1234907));
    assertTrue(transfers.size() == 0);
    
    // test invalid subaddress index
    try {
      transfers = wallet.getTransfers(new MoneroTransferRequest().setAccountIndex(0).setSubaddressIndex(-1));
      throw new RuntimeException("Should have failed");
    } catch (MoneroException e) {
      assertNotEquals("Should have failed", e.getMessage());
    }
  }
  
  // Can get outputs in the wallet, accounts, and subaddresses
  @Test
  public void testGetOutputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

    // get all outputs
    getAndTestOutputs(wallet, null, true);
    
    // get outputs for each account
    boolean nonDefaultIncoming = false;
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    for (MoneroAccount account : accounts) {
      
      // determine if account is used
      boolean isUsed = false;
      for (MoneroSubaddress subaddress : account.getSubaddresses()) if (subaddress.getIsUsed()) isUsed = true;
      
      // get outputs by account index
      List<MoneroOutputWallet> accountOutputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setAccountIndex(account.getIndex()), isUsed);
      for (MoneroOutputWallet output : accountOutputs) assertEquals(account.getIndex(), output.getAccountIndex());
      
      // get outputs by subaddress index
      List<MoneroOutputWallet> subaddressOutputs = new ArrayList<MoneroOutputWallet>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroOutputWallet> outputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()), subaddress.getIsUsed());
        for (MoneroOutputWallet output : outputs) {
          assertEquals(subaddress.getAccountIndex(), output.getAccountIndex());
          assertEquals(subaddress.getIndex(), output.getSubaddressIndex());
          if (output.getAccountIndex() != 0 && output.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          subaddressOutputs.add(output);
        }
      }
      assertEquals(subaddressOutputs.size(), accountOutputs.size());
      
      // get outputs by subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroOutputWallet output : subaddressOutputs) subaddressIndices.add(output.getSubaddressIndex());
      List<MoneroOutputWallet> outputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), isUsed);
      assertEquals(outputs.size(), subaddressOutputs.size());
      for (MoneroOutputWallet output : outputs) {
        assertEquals(account.getIndex(), output.getAccountIndex());
        assertTrue(subaddressIndices.contains(output.getSubaddressIndex()));
      }
    }
    
    // ensure output found with non-zero account and subaddress indices
    assertTrue("No outputs found in non-default account and subaddress; run send-to-multiple tests", nonDefaultIncoming);
  }

  // Can get outputs with additional configuration
  @Test
  public void testGetOutputsWithConfiguration() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get unspent outputs to account 0
    List<MoneroOutputWallet> outputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setAccountIndex(0).setIsSpent(false), null);
    for (MoneroOutputWallet output : outputs) {
      assertEquals(0, output.getAccountIndex(), 0);
      assertEquals(false, output.getIsSpent());
    }
    
    // get spent vouts to account 1
    outputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setAccountIndex(1).setIsSpent(true), true);
    for (MoneroOutputWallet output : outputs) {
      assertEquals(1, (int) output.getAccountIndex());
      assertEquals(true, output.getIsSpent());
    }
    
    // get random transactions
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, new MoneroTxRequest().setIsConfirmed(true), 3, 5);
    
    // get outputs with a tx id
    List<String> txIds = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) {
      txIds.add(tx.getId());
      outputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setTxRequest(new MoneroTxRequest().setId(tx.getId())), true);
      for (MoneroOutputWallet vout : outputs) assertEquals(vout.getTx().getId(), tx.getId());
    }
    
    // get outputs with tx ids
    outputs = getAndTestOutputs(wallet, new MoneroOutputRequest().setTxRequest(new MoneroTxRequest().setTxIds(txIds)), true);
    for (MoneroOutputWallet vout : outputs) assertTrue(txIds.contains(vout.getTx().getId()));
    
    // get confirmed outputs to specific subaddress with pre-built request
    int accountIdx = 0;
    int subaddressIdx = 1;
    MoneroOutputRequest outputRequest = new MoneroOutputRequest();
    outputRequest.setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx);
    outputRequest.setTxRequest(new MoneroTxRequest().setIsConfirmed(true));
    outputs = getAndTestOutputs(wallet, outputRequest, true);
    for (MoneroOutputWallet output : outputs) {
      assertEquals(accountIdx, (int) output.getAccountIndex());
      assertEquals(subaddressIdx, (int) output.getSubaddressIndex());
      assertEquals(true, output.getTx().getIsConfirmed());
    }
    
    // get output by key image
    String keyImage = outputs.get(0).getKeyImage().getHex();
    outputs = wallet.getOutputs(new MoneroOutputRequest().setKeyImage(new MoneroKeyImage(keyImage)));
    assertEquals(1, outputs.size());
    assertEquals(keyImage, outputs.get(0).getKeyImage().getHex());
  }
  
  // Validates inputs when getting wallet outputs
  @Test
  public void testGetOutputsValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // test with invalid id
    List<MoneroOutputWallet> outputs = wallet.getOutputs(new MoneroOutputRequest().setTxRequest(new MoneroTxRequest().setId("invalid_id")));
    assertEquals(0, outputs.size());
    
    // test invalid id in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxRequest().setIsConfirmed(true).setIncludeOutputs(true), 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) assertFalse(randomTx.getVouts().isEmpty());
    outputs = wallet.getOutputs(new MoneroOutputRequest().setTxRequest(new MoneroTxRequest().setTxIds(randomTxs.get(0).getId(), "invalid_id")));
    assertFalse(outputs.isEmpty());
    assertEquals(outputs.size(), randomTxs.get(0).getVouts().size());
    MoneroTxWallet tx = outputs.get(0).getTx();
    for (MoneroOutputWallet output : outputs) assertTrue(tx == output.getTx());
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
  
  // Has correct accounting across accounts, subaddresses, txs, transfers, and outputs
  @Test
  public void testAccounting() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    BigInteger walletBalance = wallet.getBalance();
    BigInteger walletUnlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);  // includes subaddresses
    List<MoneroTxWallet> txs = wallet.getTxs();
    
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
    
    // balance may not equal sum of unspent outputs if if unconfirmed txs
    // TODO monero-wallet-rpc: reason not to return unspent outputs on unconfirmed txs? then this isn't necessary
    boolean hasUnconfirmedTx = false;
    for (MoneroTxWallet tx : txs) if (tx.getInTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent outputs
    BigInteger walletSum = BigInteger.valueOf(0);
    for (MoneroOutputWallet output : wallet.getOutputs(new MoneroOutputRequest().setIsSpent(false))) walletSum = walletSum.add(output.getAmount());
    if (!walletBalance.equals(walletSum)) assertTrue("Wallet balance must equal sum of unspent outputs if no unconfirmed txs", hasUnconfirmedTx);
    
    // account balances are sum of their unspent outputs
    for (MoneroAccount account : accounts) {
      BigInteger accountSum = BigInteger.valueOf(0);
      List<MoneroOutputWallet> accountOutputs = wallet.getOutputs(new MoneroOutputRequest().setAccountIndex(account.getIndex()).setIsSpent(false));
      for (MoneroOutputWallet output : accountOutputs) accountSum = accountSum.add(output.getAmount());
      if (!account.getBalance().equals(accountSum)) assertTrue("Account balance must equal sum of its unspent outputs if no unconfirmed txs", hasUnconfirmedTx);
      
      // subaddress balances are sum of their unspent outputs
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        BigInteger subaddressSum = BigInteger.valueOf(0);
        List<MoneroOutputWallet> subaddressOutputs = wallet.getOutputs(new MoneroOutputRequest().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()).setIsSpent(false));
        for (MoneroOutputWallet output : subaddressOutputs) subaddressSum = subaddressSum.add(output.getAmount());
        if (!subaddress.getBalance().equals(subaddressSum)) assertTrue("Subaddress balance must equal sum of its unspent outputs if no unconfirmed txs", hasUnconfirmedTx);
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
      txs = getRandomTransactions(wallet, new MoneroTxRequest().setIsConfirmed(true).setTransferRequest(new MoneroTransferRequest().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (AssertionError e) {
      if (e.getMessage().contains("found with")) fail("No txs with outgoing destinations found; run send tests");
      throw e;
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
      testInvalidTxIdException(e);
    }
    
    // test check with invalid tx id
    MoneroTxWallet tx = txs.get(0);
    String key = wallet.getTxKey(tx.getId());
    MoneroDestination destination = tx.getOutgoingTransfer().getDestinations().get(0);
    try {
      wallet.checkTxKey("invalid_tx_id", key, destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      testInvalidTxIdException(e);
    }
    
    // test check with invalid key
    try {
      wallet.checkTxKey(tx.getId(), "invalid_tx_key", destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      testInvalidTxKeyException(e);
    }
    
    // test check with invalid address
    try {
      wallet.checkTxKey(tx.getId(), key, "invalid_tx_address");
      throw new RuntimeException("Should have thrown exception");
    } catch (MoneroException e) {
      testInvalidAddressException(e);
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
    assertNotNull("Could not get a different outgoing address to test; run send tests", differentAddress);
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
      txs = getRandomTransactions(wallet, new MoneroTxRequest().setIsConfirmed(true).setTransferRequest(new MoneroTransferRequest().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (AssertionError e) {
      if (e.getMessage().contains("found with")) fail("No txs with outgoing destinations found; run send tests");
      throw e;
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
      throw new RuntimeException("Should throw exception for invalid key");
    } catch (MoneroException e) {
      testInvalidTxIdException(e);
    }
    
    // test check with invalid tx id
    try {
      wallet.checkTxProof("invalid_tx_id", destination.getAddress(), null, signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      testInvalidTxIdException(e);
    }
    
    // test check with invalid address
    try {
      wallet.checkTxProof(tx.getId(), "invalid_tx_address", null, signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      testInvalidAddressException(e);
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
      testInvalidSignatureException(e);
    }
  }
  
  // Can prove a spend using a generated signature and no destination public address
  @Test
  public void testCheckSpendProof() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get random confirmed outgoing txs
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, new MoneroTxRequest().setIsIncoming(false).setInTxPool(false).setIsFailed(false), 2, MAX_TX_PROOFS);
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
      throw new RuntimeException("Should throw exception for invalid key");
    } catch (MoneroException e) {
      testInvalidTxIdException(e);
    }
    
    // test check with invalid tx id
    try {
      wallet.checkSpendProof("invalid_tx_id", null, signature);
      throw new RuntimeException("Should have thrown exception");
    } catch (MoneroException e) {
      testInvalidTxIdException(e);
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
      List<MoneroTxWallet> unconfirmedTxs = wallet.getTxs(new MoneroTxRequest().setInTxPool(true));
      assertTrue("Reserve amount must equal balance unless wallet has unconfirmed txs", unconfirmedTxs.size() > 0);
    }
    
    // test different wallet address
    String differentAddress = TestUtils.getRandomWalletAddress();
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      testNoSubaddressException(e);
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof((wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      testNoSubaddressException(e);
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
      testSignatureHeaderCheckException(e);
    }
  }
  
  // Can prove reserves in an account
  @Test
  @Ignore // TODO: investigate and re-enable
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
          throw new RuntimeException("Should have thrown exception");
        } catch (MoneroException e) {
          assertEquals(-1, (int) e.getCode());
          try {
            wallet.getReserveProofAccount(account.getIndex(), TestUtils.MAX_FEE, msg);
            throw new RuntimeException("Should have thrown exception");
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
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxRequest().setIsOutgoing(true).setIsConfirmed(true));
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
  
  // Can convert between a tx send request and payment URI
  @Test
  public void testCreatePaymentUri() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test with address and amount
    MoneroSendRequest request1 = new MoneroSendRequest(wallet.getAddress(0, 0), BigInteger.valueOf(0));
    String uri = wallet.createPaymentUri(request1);
    MoneroSendRequest request2 = wallet.parsePaymentUri(uri);
    assertEquals(request1, request2);
    
    // test with all fields3
    request1.getDestinations().get(0).setAmount(new BigInteger("425000000000"));
    request1.setPaymentId("03284e41c342f03603284e41c342f03603284e41c342f03603284e41c342f036");
    request1.setRecipientName("John Doe");
    request1.setNote("OMZG XMR FTW");
    uri = wallet.createPaymentUri(request1);
    request2 = wallet.parsePaymentUri(uri);
    assertEquals(request1, request2);
    
    // test with undefined address
    String address = request1.getDestinations().get(0).getAddress();
    request1.getDestinations().get(0).setAddress(null);
    try {
      wallet.createPaymentUri(request1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroException e) {
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
    request1.getDestinations().get(0).setAddress(address);
    
    // test with invalid payment id
    request1.setPaymentId("bizzup");
    try {
      wallet.createPaymentUri(request1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroException e) {
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
  }
  
  // Can start and stop mining
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
    testSendFromMultiple(new MoneroSendRequest().setCanSplit(true));
  }
  
  private void testSendFromMultiple(MoneroSendRequest request) {
    if (request == null) request = new MoneroSendRequest();
    
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
    request.setDestinations(new MoneroDestination(address, sendAmount));
    request.setAccountIndex(srcAccount.getIndex());
    request.setSubaddressIndices(fromSubaddressIndices);
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (Boolean.TRUE.equals(request.getCanSplit())) {
      List<MoneroTxWallet> sendTxs = wallet.sendSplit(request);
      for (MoneroTxWallet tx : sendTxs) txs.add(tx);
    } else {
      txs.add(wallet.send(request));
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
    ctx.sendRequest = request;
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
      throw new RuntimeException("Tx amounts are too different: " + sendAmount + " - " + outgoingSum + " = " + sendAmount.subtract(outgoingSum));
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
    testSendToSingle(new MoneroSendRequest().setPaymentId(paymentId + paymentId + paymentId + paymentId));  // 64 character payment id
  }
  
  // Can send to an address in a single transaction with a ring size
  @Test
  public void testSendWithRingSize() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendRequest().setRingSize(8));
  }
  
  // Can send to an address with split transactions
  @Test
  public void testSendSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendRequest().setCanSplit(true));
  }
  
  // Can create then relay a transaction to send to a single address
  @Test
  public void testCreateThenRelay() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendRequest().setDoNotRelay(true));
  }
  
  // Can create then relay split transactions to send to a single address
  @Test
  public void testCreateThenRelaySplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroSendRequest().setCanSplit(true).setDoNotRelay(true));
  }
  
  private void testSendToSingle(MoneroSendRequest request) {
    if (request == null) request = new MoneroSendRequest();
    
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
    
    // init send request
    BigInteger sendAmount = unlockedBalanceBefore.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    request.setDestinations(new MoneroDestination(address, sendAmount));
    request.setAccountIndex(fromAccount.getIndex());
    request.setSubaddressIndices(fromSubaddress.getIndex());
    
    // send to self
    if (Boolean.TRUE.equals(request.getCanSplit())) {
      List<MoneroTxWallet> sendTxs = wallet.sendSplit(request);
      for (MoneroTxWallet tx : sendTxs) txs.add(tx);
    } else {
      txs.add(wallet.send(request));
    }
    
    // handle non-relayed transaction
    if (Boolean.TRUE.equals(request.getDoNotRelay())) {
      
      // build test context
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendRequest = request;
      ctx.isSendResponse = true;
      
      // test transactions
      for (MoneroTxWallet tx : txs) {
        testTxWallet(tx, ctx);
      }
      
      // relay txs
      List<String> txIds = null;
      if (!Boolean.TRUE.equals(request.getCanSplit())) txIds = Arrays.asList(wallet.relayTx(txs.get(0).getMetadata())); // test relayTx() with single transaction
      else {
        List<String> txMetadatas = new ArrayList<String>();
        for (MoneroTxWallet tx : txs) txMetadatas.add(tx.getMetadata());
        txIds = wallet.relayTxs(txMetadatas); // test relayTxs() with potentially multiple transactions
      }
      for (String txId : txIds) assertEquals(64, txId.length());
      
      // fetch txs for testing
      txs = wallet.getTxs(new MoneroTxRequest().setTxIds(txIds));
    }
    
    // test that balance and unlocked balance decreased
    // TODO: test that other balances did not decrease
    MoneroSubaddress subaddress = wallet.getSubaddress(fromAccount.getIndex(), fromSubaddress.getIndex());
    assertTrue(subaddress.getBalance().compareTo(balanceBefore) < 0);
    assertTrue(subaddress.getUnlockedBalance().compareTo(unlockedBalanceBefore) < 0);
    
    // build test context
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendRequest = request;
    ctx.isSendResponse = Boolean.TRUE.equals(request.getDoNotRelay()) ? false : true;
    
    // test transactions
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      assertEquals(fromAccount.getIndex(), tx.getOutgoingTransfer().getAccountIndex());
      assertEquals(1, tx.getOutgoingTransfer().getSubaddressIndices().size());
      assertEquals(fromSubaddress.getIndex(), tx.getOutgoingTransfer().getSubaddressIndices().get(0));
      assertTrue(sendAmount.equals(tx.getOutgoingAmount()));
      if (request.getPaymentId() != null) assertEquals(request.getPaymentId(), tx.getPaymentId());
      
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
  
  // Can send dust to multiple addresses in split transactions
  @Test
  public void testSendDustToMultipleSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    BigInteger dustAmt = daemon.getFeeEstimate().divide(BigInteger.valueOf(2));
    testSendToMultiple(5, 3, true, dustAmt);
  }
  
  /**
   * Sends funds from the first unlocked account to multiple accounts and subaddresses.
   * 
   * @param numAccounts is the number of accounts to receive funds
   * @param numSubaddressesPerAccount is the number of subaddresses per account to receive funds
   * @param canSplit specifies if the operation can be split into multiple transactions
   * @param sendAmountPerSubaddress is the amount to send to each subaddress (optional, computed if not given)
   */
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit) { testSendToMultiple(numAccounts, numSubaddressesPerAccount, canSplit, null); }
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit, BigInteger sendAmountPerSubaddress) {
    
    // compute the minimum account unlocked balance needed in order to fulfill the request
    BigInteger minAccountAmount = null;
    int totalSubaddresses = numAccounts * numSubaddressesPerAccount;
    if (sendAmountPerSubaddress != null) minAccountAmount = BigInteger.valueOf(totalSubaddresses).multiply(sendAmountPerSubaddress.add(TestUtils.MAX_FEE)); // min account amount must cover the total amount being sent plus the tx fee = numAddresses * (amtPerSubaddress + fee)
    else minAccountAmount = TestUtils.MAX_FEE.multiply(BigInteger.valueOf(totalSubaddresses)).multiply(BigInteger.valueOf(SEND_DIVISOR)).add(TestUtils.MAX_FEE); // account balance must be more than fee * numAddresses * divisor + fee so each destination amount is at least a fee's worth (so dust is not sent)
    
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
    BigInteger balance = srcAccount.getBalance();
    BigInteger unlockedBalance = srcAccount.getUnlockedBalance();
    
    // get amount to send total and per subaddress
    BigInteger sendAmount = null;
    if (sendAmountPerSubaddress == null) {
      sendAmount = unlockedBalance.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
      sendAmountPerSubaddress = sendAmount.divide(BigInteger.valueOf(totalSubaddresses));
    } else {
      sendAmount = sendAmountPerSubaddress.multiply(BigInteger.valueOf(totalSubaddresses));
    }
    
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
    
    // build send request using MoneoSendRequest
    MoneroSendRequest request = new MoneroSendRequest();
    request.setMixin(TestUtils.MIXIN);
    request.setAccountIndex(srcAccount.getIndex());
    request.setDestinations(new ArrayList<MoneroDestination>());
    request.setCanSplit(canSplit);
    for (int i = 0; i < destinationAddresses.size(); i++) {
      request.getDestinations().add(new MoneroDestination(destinationAddresses.get(i), sendAmountPerSubaddress));
    }
    
    // send tx(s) with request
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (canSplit) {
      List<MoneroTxWallet> sendTxs = wallet.sendSplit(request);
      for (MoneroTxWallet tx : sendTxs) txs.add(tx);
    } else {
      txs.add(wallet.send(request));
    }
    
    // test that wallet balance decreased
    MoneroAccount account = wallet.getAccount(srcAccount.getIndex());
    assertTrue(account.getBalance().compareTo(balance) < 0);
    assertTrue(account.getUnlockedBalance().compareTo(unlockedBalance) < 0);
    
    // build test context
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendRequest = request;
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
      fail("Actual send amount is too different from requested send amount: " + sendAmount + " - " + outgoingSum + " = " + sendAmount.subtract(outgoingSum));
    }
  }
  
  // Can sweep individual outputs identified by their key images
  @Test
  public void testSweepOutputs() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    
    // test config
    int numOutputs = 3;
    
    // get outputs to sweep (not spent, unlocked, and amount >= fee)
    List<MoneroOutputWallet> spendableUnlockedOutputs = wallet.getOutputs(new MoneroOutputRequest().setIsSpent(false).setIsUnlocked(true));
    List<MoneroOutputWallet> outputsToSweep = new ArrayList<MoneroOutputWallet>();
    for (int i = 0; i < spendableUnlockedOutputs.size() && outputsToSweep.size() < numOutputs; i++) {
      if (spendableUnlockedOutputs.get(i).getAmount().compareTo(TestUtils.MAX_FEE) > 0) outputsToSweep.add(spendableUnlockedOutputs.get(i));  // output cannot be swept if amount does not cover fee
    }
    assertTrue("Wallet does not have enough sweepable outputs; run send tests", outputsToSweep.size() >= numOutputs);
    
    // sweep each output by key image
    boolean useParams = true; // for loop flips in order to alternate test
    for (MoneroOutputWallet output : outputsToSweep) {
      testOutputWallet(output);
      assertFalse(output.getIsSpent());
      assertTrue(output.getIsUnlocked());
      if (output.getAmount().compareTo(TestUtils.MAX_FEE) <= 0) continue;
      
      // sweep output to address
      String address = wallet.getAddress(output.getAccountIndex(), output.getSubaddressIndex());
      MoneroSendRequest request = new MoneroSendRequest(address).setKeyImage(output.getKeyImage().getHex());
      MoneroTxWallet tx;
      if (useParams) tx = wallet.sweepOutput(address, output.getKeyImage().getHex(), null); // test params
      else tx = wallet.sweepOutput(request);  // test config
      
      // test resulting tx
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendRequest = request;
      ctx.isSendResponse = true;
      ctx.isSweepResponse = true;
      ctx.isSweepOutputResponse = true;
      testTxWallet(tx, ctx);
      useParams = !useParams;
    }
    
    // get outputs after sweeping
    List<MoneroOutputWallet> afterOutputs = wallet.getOutputs();
    
    // swept outputs are now spent
    for (MoneroOutputWallet afterOutput : afterOutputs) {
      for (MoneroOutputWallet output : outputsToSweep) {
        if (output.getKeyImage().getHex().equals(afterOutput.getKeyImage().getHex())) {
          assertTrue("Output should be spent", afterOutput.getIsSpent());
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
    ctx.sendRequest = new MoneroSendRequest().setDoNotRelay(true);
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
    txs = wallet.getTxs(new MoneroTxRequest().setTxIds(txIds));
    ctx.sendRequest.setDoNotRelay(false);
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
    ctx.sendRequest = null;
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
  @Ignore
  public void testUpdateLockedSameAccount() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    MoneroSendRequest request = new MoneroSendRequest(wallet.getPrimaryAddress(), TestUtils.MAX_FEE);
    request.setAccountIndex(0);
    request.setUnlockTime(3);
    request.setCanSplit(false);
    testSendAndUpdateTxs(request);
  }
  
  // Can update split locked txs sent from/to the same account as blocks are added to the chain
  @Test
  @Ignore // TODO: remove these ignores
  public void testUpdateLockedSameAccountSplit() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroSendRequest request = new MoneroSendRequest(0, wallet.getPrimaryAddress(), TestUtils.MAX_FEE);
    request.setAccountIndex(0);
    request.setUnlockTime(3);
    request.setCanSplit(true);
    testSendAndUpdateTxs(request);
  }
  
  // Can update a locked tx sent from/to different accounts as blocks are added to the chain
  @Test
  @Ignore
  public void testUpdateLockedDifferentAccounts() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroSendRequest request = new MoneroSendRequest(0, wallet.getSubaddress(1, 0).getAddress(), TestUtils.MAX_FEE);
    request.setUnlockTime(3);
    request.setCanSplit(false);
    testSendAndUpdateTxs(request);
  }
  
  // Can update locked, split txs sent from/to different accounts as blocks are added to the chain
  @Test
  @Ignore
  public void testUpdateLockedDifferentAccountsSplit() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroSendRequest request = new MoneroSendRequest(0, wallet.getSubaddress(1, 0).getAddress(), TestUtils.MAX_FEE);
    request.setAccountIndex(0);
    request.setUnlockTime(3);
    request.setCanSplit(true);
    testSendAndUpdateTxs(request);
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
   * @param request is the send configuration to send and test
   * @throws InterruptedException 
   */
  private void testSendAndUpdateTxs(MoneroSendRequest request) {
    
    // unlike js version, this test starts and stops its own mining, so it's wrapped in order to stop mining if anything fails
    try {
      
      // attempt to start mining to push the network along
      boolean startedMining = false;
      MoneroMiningStatus miningStatus = daemon.getMiningStatus();
      if (!miningStatus.getIsActive()) {
        try {
          wallet.startMining(8, false, true);
          startedMining = true;
        } catch (Exception e) {
          // no problem
        }
      }
      
      // send transactions
      List<MoneroTxWallet> sentTxs;
      if (request.getCanSplit()) sentTxs = wallet.sendSplit(request);
      else sentTxs = Arrays.asList(wallet.send(request));
      
      // build test context
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendRequest = request;
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
        MoneroTxRequest txRequest = new MoneroTxRequest().setTxIds(txIds);
        List<MoneroTxWallet> fetchedTxs = getAndTestTxs(wallet, txRequest, null, true);
        assertFalse(fetchedTxs.isEmpty());
        
        // test fetched txs
        testOutInPairs(wallet, fetchedTxs, request, false);

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
        testOutInPairs(wallet, updatedTxs, request, false);
        
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
  
  private void testOutInPairs(MoneroWallet wallet, List<MoneroTxWallet> txs, MoneroSendRequest request, boolean isSendResponse) {
    
    // for each out tx
    for (MoneroTxWallet tx : txs) {
      testUnlockTx(wallet, tx, request, isSendResponse);
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
  
  private void testUnlockTx(MoneroWallet wallet, MoneroTxWallet tx, MoneroSendRequest request, boolean isSendResponse) {
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    ctx.sendRequest = request;
    ctx.isSendResponse = isSendResponse;
    try {
      testTxWallet(tx, ctx);
    } catch (MoneroException e) {
      System.out.println(tx.toString());
      throw e;
    }
  }
  
  // --------------------------------- RESET TESTS --------------------------------
  
  // Can sweep subaddresses
  @Test
  public void testSweepSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    
    final int NUM_SUBADDRESSES_TO_SWEEP = 2;
    
    // collect subaddresses with balance and unlocked balance
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesBalance = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesUnlocked = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        subaddresses.add(subaddress);
        if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0) subaddressesBalance.add(subaddress);
        if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) subaddressesUnlocked.add(subaddress);
      }
    }
    
    // test requires at least one more subaddresses than the number being swept to verify it does not change
    assertTrue("Test requires balance in at least " + (NUM_SUBADDRESSES_TO_SWEEP + 1) + " subaddresses; run send-to-multiple tests", subaddressesBalance.size() >= NUM_SUBADDRESSES_TO_SWEEP + 1);
    assertTrue("Wallet is waiting on unlocked funds", subaddressesUnlocked.size() >= NUM_SUBADDRESSES_TO_SWEEP + 1);
    
    // sweep from first unlocked subaddresses
    for (int i = 0; i < NUM_SUBADDRESSES_TO_SWEEP; i++) {
      
      // sweep unlocked account
      MoneroSubaddress unlockedSubaddress = subaddressesUnlocked.get(i);
      List<MoneroTxWallet> txs = wallet.sweepSubaddress(unlockedSubaddress.getAccountIndex(), unlockedSubaddress.getIndex(), wallet.getPrimaryAddress());
      
      // test transactions
      assertTrue(txs.size() > 0);
      for (MoneroTxWallet tx : txs) {
        MoneroSendRequest request = new MoneroSendRequest(wallet.getPrimaryAddress());
        request.setAccountIndex(unlockedSubaddress.getAccountIndex());
        request.setSubaddressIndices(unlockedSubaddress.getIndex());
        TestContext ctx = new TestContext();
        ctx.wallet = wallet;
        ctx.sendRequest = request;
        ctx.isSendResponse = true;
        ctx.isSweepResponse = true;
        testTxWallet(tx, ctx);
      }
      
      // assert no unlocked funds in subaddress
      MoneroSubaddress subaddress = wallet.getSubaddress(unlockedSubaddress.getAccountIndex(), unlockedSubaddress.getIndex());
      assertTrue(subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) == 0);
    }
    
    // test subaddresses after sweeping
    List<MoneroSubaddress> subaddressesAfter = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        subaddressesAfter.add(subaddress);
      }
    }
    assertEquals(subaddresses.size(), subaddressesAfter.size());
    for (int i = 0; i < subaddresses.size(); i++) {
      MoneroSubaddress subaddressBefore = subaddresses.get(i);
      MoneroSubaddress subaddressAfter = subaddressesAfter.get(i);
      
      // determine if subaddress was swept
      boolean swept = false;
      for (int j = 0; j < NUM_SUBADDRESSES_TO_SWEEP; j++) {
        if (subaddressesUnlocked.get(j).getAccountIndex().equals(subaddressBefore.getAccountIndex()) && subaddressesUnlocked.get(j).getIndex().equals(subaddressBefore.getIndex())) {
          swept = true;
          break;
        }
      }
      
      // test that unlocked balance is 0 if swept, unchanged otherwise
      if (swept) {
        assertTrue(subaddressAfter.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) == 0);
      } else {
        assertTrue(subaddressBefore.getUnlockedBalance().compareTo(subaddressAfter.getUnlockedBalance()) == 0);
      }
    }
  }
  
  // Can sweep accounts
  @Test
  public void testSweepAccounts() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    
    final int NUM_ACCOUNTS_TO_SWEEP = 1;
    
    // collect accounts with sufficient balance and unlocked balance to cover the fee
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    List<MoneroAccount> accountsBalance = new ArrayList<MoneroAccount>();
    List<MoneroAccount> accountsUnlocked = new ArrayList<MoneroAccount>();
    for (MoneroAccount account : accounts) {
      if (account.getBalance().compareTo(TestUtils.MAX_FEE) > 0) accountsBalance.add(account);
      if (account.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) accountsUnlocked.add(account);
    }
    
    // test requires at least one more accounts than the number being swept to verify it does not change
    assertTrue("Test requires balance greater than the fee in at least " + (NUM_ACCOUNTS_TO_SWEEP + 1) + " accounts; run send-to-multiple tests", accountsBalance.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    assertTrue("Wallet is waiting on unlocked funds", accountsUnlocked.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    
    // sweep from first unlocked accounts
    for (int i = 0; i < NUM_ACCOUNTS_TO_SWEEP; i++) {
      
      // sweep unlocked account
      MoneroAccount unlockedAccount = accountsUnlocked.get(i);
      List<MoneroTxWallet> txs = wallet.sweepAccount(unlockedAccount.getIndex(), wallet.getPrimaryAddress());
      
      // test transactions
      assertTrue(txs.size() > 0);
      for (MoneroTxWallet tx : txs) {
        MoneroSendRequest request = new MoneroSendRequest(wallet.getPrimaryAddress());
        request.setAccountIndex(unlockedAccount.getIndex());
        TestContext ctx = new TestContext();
        ctx.wallet = wallet;
        ctx.sendRequest = request;
        ctx.isSendResponse = true;
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
        if (accountsUnlocked.get(j).getIndex().equals(accountBefore.getIndex())) {
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
  
  // Can sweep the whole wallet by accounts
  @Test
  @Ignore // disabled so tests don't sweep the whole wallet
  public void testSweepWalletByAccounts() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    testSweepWallet(null);
  }
  
  // Can sweep the whole wallet by subaddresses
  @Test
  @Ignore // disabled so tests don't sweep the whole wallet
  public void testSweepWalletBySubaddresses() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    testSweepWallet(true);
  }
  
  private void testSweepWallet(Boolean sweepEachSubaddress) {
    
    // verify 2 subaddresses with enough unlocked balance to cover the fee
    List<MoneroSubaddress> subaddressesBalance = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesUnlocked = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesBalance.add(subaddress);
        if (subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesUnlocked.add(subaddress);
      }
    }
    assertTrue("Test requires multiple accounts with a balance greater than the fee; run send to multiple first", subaddressesBalance.size() >= 2);
    assertTrue("Wallet is waiting on unlocked funds", subaddressesUnlocked.size() >= 2);
    
    // sweep
    String destination = wallet.getPrimaryAddress();
    MoneroSendRequest req = new MoneroSendRequest(destination).setSweepEachSubaddress(sweepEachSubaddress);
    List<MoneroTxWallet> txs = wallet.sweepAllUnlocked(req);
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      MoneroSendRequest request = new MoneroSendRequest(destination);
      request.setAccountIndex(tx.getOutgoingTransfer().getAccountIndex());
      request.setSweepEachSubaddress(sweepEachSubaddress);
      TestContext ctx = new TestContext();
      ctx.wallet = wallet;
      ctx.sendRequest = request;
      ctx.isSendResponse = true;
      ctx.isSweepResponse = true;
      testTxWallet(tx, ctx);
    }
    
    // all unspent, unlocked outputs must be less than fee
    List<MoneroOutputWallet> spendableOutputs = wallet.getOutputs(new MoneroOutputRequest().setIsSpent(false).setIsUnlocked(true));
    for (MoneroOutputWallet spendableOutput : spendableOutputs) {
      assertTrue("Unspent output should have been swept\n" + spendableOutput.toString(), spendableOutput.getAmount().compareTo(TestUtils.MAX_FEE) < 0);
    }
    
    // all subaddress unlocked balances must be less than fee
    subaddressesBalance.clear();
    subaddressesUnlocked.clear();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        assertTrue("No subaddress should have more unlocked than the fee", subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0);
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
  
  // --------------------------------- HELPERS --------------------------------
  
  private List<MoneroTxWallet> getCachedTxs() {
    if (txCache != null) return txCache;
    txCache = wallet.getTxs();
    return txCache;
  }
  
  /**
   * Fetches and tests transactions according to the given request.
   * 
   * TODO: ensure each tx passes request filter, same with testGetTransfer and getAndTestVouts
   */
  private List<MoneroTxWallet> getAndTestTxs(MoneroWallet wallet, MoneroTxRequest request, TestContext ctx, Boolean isExpected) {
    List<MoneroTxWallet> txs = wallet.getTxs(request);
    assertNotNull(txs);
    if (Boolean.FALSE.equals(isExpected)) assertTrue(txs.isEmpty());
    if (Boolean.TRUE.equals(isExpected)) assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) testTxWallet(tx, ctx);
    return txs;
  }
  
  /**
   * Fetches and tests transfers according to the given request.
   */
  private List<MoneroTransfer> getAndTestTransfers(MoneroWallet wallet, MoneroTransferRequest request, TestContext ctx, Boolean isExpected) {
    List<MoneroTransfer> transfers = wallet.getTransfers(request);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, transfers.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Transfers were expected but not found; run send tests?", transfers.size() > 0);
    if (ctx == null) ctx = new TestContext();
    ctx.wallet = wallet;
    for (MoneroTransfer transfer : transfers) testTxWallet(transfer.getTx(), ctx);
    return transfers;
  }

  /**
   * Fetches and tests wallet outputs (i.e. wallet tx vouts) according to the given request.
   */
  private static List<MoneroOutputWallet> getAndTestOutputs(MoneroWallet wallet, MoneroOutputRequest request, Boolean isExpected) {
    List<MoneroOutputWallet> vouts = wallet.getOutputs(request);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, vouts.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Vouts were expected but not found; run send tests?", vouts.size() > 0);
    for (MoneroOutputWallet vout : vouts) testOutputWallet(vout);
    return vouts;
  }
  
  /**
   * Provides context or configuration for test methods to test a type.
   */
  public static class TestContext {
    MoneroWallet wallet;
    MoneroSendRequest sendRequest;
    Boolean hasOutgoingTransfer;
    Boolean hasIncomingTransfers;
    Boolean hasDestinations;
    Boolean doNotTestCopy;
    Boolean includeOutputs;
    Boolean isSendResponse;
    Boolean isSweepResponse;
    Boolean isSweepOutputResponse;  // TODO monero-wallet-rpc: this only necessary because sweep_output does not return account index
    public TestContext() { }
    public TestContext(TestContext ctx) {
      if (ctx == null) return;
      this.wallet = ctx.wallet;
      this.sendRequest = ctx.sendRequest;
      this.hasOutgoingTransfer = ctx.hasOutgoingTransfer;
      this.hasIncomingTransfers = ctx.hasIncomingTransfers;
      this.hasDestinations = ctx.hasDestinations;
      this.doNotTestCopy = ctx.doNotTestCopy;
      this.includeOutputs = ctx.includeOutputs;
      this.isSendResponse = ctx.isSendResponse;
      this.isSweepResponse = ctx.isSweepResponse;
      this.isSweepOutputResponse = ctx.isSweepOutputResponse;
    }
  }
  
  public static void testWalletsEqual(MoneroWallet w1, MoneroWallet w2) {
    assertEquals(w1.getMnemonic(), w2.getMnemonic());
    assertEquals(w1.getHeight(), w2.getHeight());
    assertEquals(w1.getPrimaryAddress(), w2.getPrimaryAddress());
    assertEquals(w1.getPrivateViewKey(), w2.getPrivateViewKey());
    assertEquals(w1.getPrivateSpendKey(), w2.getPrivateSpendKey());
    testAccountsEqual(w1.getAccounts(true), w2.getAccounts(true));  // test accounts which can include local data not used on chain
   
    // TODO: txs, transfers, outputs, integrated addresses, etc
  }
  
  private static void testAccountsEqual(List<MoneroAccount> accounts1, List<MoneroAccount> accounts2) {
    for (int i = 0; i < Math.max(accounts1.size(), accounts2.size()); i++) {
      if (i < accounts1.size() && i < accounts2.size()) {
        testSubaddressesEqual(accounts1.get(i).getSubaddresses(), accounts2.get(i).getSubaddresses());
      } else if (i >= accounts1.size()) {
        for (int j = i; j < accounts2.size(); j++) {
          assertEquals(BigInteger.valueOf(0), accounts2.get(j).getBalance());
          assertEquals(1, accounts2.get(j).getSubaddresses().size());
        }
        return;
      } else {
        for (int j = i; j < accounts1.size(); j++) {
          assertEquals(BigInteger.valueOf(0), accounts1.get(j).getBalance());
          assertEquals(1, accounts1.get(j).getSubaddresses().size());
        }
        return;
      }
    }
  }
  
  private static void testSubaddressesEqual(List<MoneroSubaddress> subaddresses1, List<MoneroSubaddress> subaddresses2) {
    for (int i = 0; i < Math.max(subaddresses1.size(), subaddresses2.size()); i++) {
      if (i < subaddresses1.size() && i < subaddresses2.size()) {
        assertEquals(subaddresses1.get(i), subaddresses2.get(i));
      } else if (i >= subaddresses1.size()) {
        for (int j = i; j < subaddresses2.size(); j++) {
          assertEquals(BigInteger.valueOf(0), subaddresses2.get(j).getBalance());
          assertFalse(subaddresses2.get(j).getIsUsed());
        }
        return;
      } else {
        for (int j = i; j < subaddresses1.size(); j++) {
          assertEquals(BigInteger.valueOf(0), subaddresses1.get(i).getBalance());
          assertFalse(subaddresses1.get(j).getIsUsed());
        }
        return;
      }
    }
  }
  
  protected void testInvalidAddressException(MoneroException e) {
    assertEquals("Invalid address", e.getDescription());
  }
  
  protected void testInvalidTxIdException(MoneroException e) {
    assertEquals("TX ID has invalid format", e.getDescription());
  }
  
  protected void testInvalidTxKeyException(MoneroException e) {
    assertEquals("Tx key has invalid format", e.getDescription());
  }
  
  protected void testInvalidSignatureException(MoneroException e) {
    assertEquals("Signature size mismatch with additional tx pubkeys", e.getDescription());
  }
  
  protected void testNoSubaddressException(MoneroException e) {
    assertEquals("Address must not be a subaddress", e.getDescription());
  }
  
  protected void testSignatureHeaderCheckException(MoneroException e) {
    assertEquals("Signature header check error", e.getDescription());
  }
  
   private static void testAccount(MoneroAccount account) {
    
    // test account
    assertNotNull(account);
    assertTrue(account.getIndex() >= 0);
    MoneroUtils.validateAddress(account.getPrimaryAddress());
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
      assertTrue("Subaddress balances " + balance + " does not equal account " + account.getIndex() + " balance " + account.getBalance(), account.getBalance().equals(balance));
      assertTrue("Subaddress unlocked balances " + unlockedBalance + " does not equal account " + account.getIndex() + " unlocked balance " + account.getUnlockedBalance(), account.getUnlockedBalance().equals(unlockedBalance));
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
   *        ctx.sendRequest specifies the tx's originating send request
   *        ctx.isSendResponse indicates if the tx is built from a send response, which contains additional fields (e.g. key)
   *        ctx.hasDestinations specifies if the tx has an outgoing transfer with destinations, undefined if doesn't matter
   *        ctx.includeOutputs specifies if outputs were fetched and should therefore be expected with incoming transfers
   */
  protected void testTxWallet(MoneroTxWallet tx) { testTxWallet(tx, null); }
  protected void testTxWallet(MoneroTxWallet tx, TestContext ctx) {
    
    // validate / sanitize inputs
    ctx = new TestContext(ctx);
    ctx.wallet = null;  // TODO: re-enable
    assertNotNull(tx);
    if (ctx.isSendResponse == null || ctx.sendRequest == null) {
      assertNull("if either sendRequest or isSendResponse is defined, they must both be defined", ctx.isSendResponse);
      assertNull("if either sendRequest or isSendResponse is defined, they must both be defined", ctx.sendRequest);
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
      assertEquals(false, tx.getIsDoubleSpend());
      assertTrue(tx.getNumConfirmations() > 0);
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
        //assertTrue(tx.getReceivedTimestamp() > 0);  // TODO: re-enable when received timestamp returned in wallet rpc
      }
    } else {
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
      //assertTrue(tx.getReceivedTimestamp() > 0);  // TODO: re-enable when received timestamp returned in wallet rpc
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
      testTransfer(tx.getOutgoingTransfer(), ctx);
      if (Boolean.TRUE.equals(ctx.isSweepResponse)) assertEquals(1, tx.getOutgoingTransfer().getDestinations().size());
      
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
        testTransfer(transfer, ctx);
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
      MoneroSendRequest request = ctx.sendRequest;
      assertEquals(false, tx.getIsConfirmed());
      testTransfer(tx.getOutgoingTransfer(), ctx);
      assertEquals(request.getMixin(), tx.getMixin());
      assertEquals(request.getUnlockTime() != null ? request.getUnlockTime() : 0, (int) tx.getUnlockTime());
      assertNull(tx.getBlock());
      if (!Boolean.TRUE.equals(request.getCanSplit())) assertTrue(tx.getKey().length() > 0);
      assertNotNull(tx.getFullHex());
      assertTrue(tx.getFullHex().length() > 0);
      assertNotNull(tx.getMetadata());
      assertNull(tx.getReceivedTimestamp());
      
      // test destinations of sent tx
      assertEquals(request.getDestinations().size(), tx.getOutgoingTransfer().getDestinations().size());
      for (int i = 0; i < request.getDestinations().size(); i++) {
        assertEquals(request.getDestinations().get(i).getAddress(), tx.getOutgoingTransfer().getDestinations().get(i).getAddress());
        if (Boolean.TRUE.equals(ctx.isSweepResponse)) {
          assertEquals(1, request.getDestinations().size());
          assertNull(request.getDestinations().get(i).getAmount());
          assertEquals(tx.getOutgoingTransfer().getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        } else {
          assertEquals(request.getDestinations().get(i).getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        }
      }
      
      // test relayed txs
      if (!Boolean.TRUE.equals(request.getDoNotRelay())) {
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
    if (tx.getIsIncoming() && Boolean.TRUE.equals(ctx.includeOutputs)) {
      if (tx.getIsConfirmed()) {
        assertNotNull(tx.getVouts());
        assertTrue(tx.getVouts().size() > 0);
      } else {
        assertNull(tx.getVouts());
      }
    }
    if (tx.getVouts() != null) for (MoneroOutputWallet vout : tx.getVoutsWallet()) testOutputWallet(vout);
    
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
    assertNull(tx.getSize());   // TODO monero-wallet-rpc: add tx_size to get_transfers and get_transfer_by_txid
    assertNull(tx.getWeight());
    assertNull(tx.getReceivedTimestamp());  // TODO monero-wallet-rpc: return received timestamp (asked to file issue if wanted)
  }

  private void testTxWalletCopy(MoneroTxWallet tx, TestContext ctx) {
    
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
  
  private static void testTransfer(MoneroTransfer transfer, TestContext ctx) {
    if (ctx == null) ctx = new TestContext();
    assertNotNull(transfer);
    TestUtils.testUnsignedBigInteger(transfer.getAmount());
    if (!Boolean.TRUE.equals(ctx.isSweepOutputResponse)) assertTrue(transfer.getAccountIndex() >= 0);
    if (!Boolean.TRUE.equals(ctx.isSendResponse)) assertTrue(transfer.getNumSuggestedConfirmations() >= 0); // TODO monero-wallet-rpc: some outgoing transfers have suggested_confirmations_threshold = 0
    if (transfer.getIsIncoming()) testIncomingTransfer((MoneroIncomingTransfer) transfer);
    else testOutgoingTransfer((MoneroOutgoingTransfer) transfer, ctx);
    
    // transfer and tx reference each other
    assertNotNull(transfer.getTx());
    if (!transfer.equals(transfer.getTx().getOutgoingTransfer())) {
      assertNotNull(transfer.getTx().getIncomingTransfers());
      assertTrue("Transaction does not reference given transfer", transfer.getTx().getIncomingTransfers().contains(transfer));
    }
  }
  
  private static void testIncomingTransfer(MoneroIncomingTransfer transfer) {
    assertTrue(transfer.getIsIncoming());
    assertFalse(transfer.getIsOutgoing());
    assertNotNull(transfer.getAddress());
    assertTrue(transfer.getSubaddressIndex() >= 0);
    assertTrue(transfer.getNumSuggestedConfirmations() > 0);
  }
  
  private static void testOutgoingTransfer(MoneroOutgoingTransfer transfer, TestContext ctx) {
    assertFalse(transfer.getIsIncoming());
    assertTrue(transfer.getIsOutgoing());
    if (!Boolean.TRUE.equals(ctx.isSendResponse)) assertNotNull(transfer.getSubaddressIndices());
    if (transfer.getSubaddressIndices() != null) {
      assertTrue(transfer.getSubaddressIndices().size() >= 1);
      for (int subaddressIdx : transfer.getSubaddressIndices()) assertTrue(subaddressIdx >= 0);
    }
    if (transfer.getAddresses() != null) {
      assertEquals(transfer.getSubaddressIndices().size(), transfer.getAddresses().size());
      for (String address : transfer.getAddresses()) assertNotNull(address);
    }
    
    // test destinations sum to outgoing amount
    if (transfer.getDestinations() != null) {
      assertTrue(transfer.getDestinations().size() > 0);
      BigInteger sum = BigInteger.valueOf(0);
      for (MoneroDestination destination : transfer.getDestinations()) {
        assertNotNull(destination.getAddress());
        TestUtils.testUnsignedBigInteger(destination.getAmount(), true);
        sum = sum.add(destination.getAmount());
      }
      if (!transfer.getAmount().equals(sum)) System.out.println(transfer.getTx().toString());
      assertEquals(transfer.getAmount(), sum);
    }
  }
  
  private static void testOutputWallet(MoneroOutputWallet output) {
    assertNotNull(output);
    assertTrue(output.getAccountIndex() >= 0);
    assertTrue(output.getSubaddressIndex() >= 0);
    assertTrue(output.getIndex() >= 0);
    assertNotNull(output.getIsSpent());
    assertNotNull(output.getIsUnlocked());
    assertNotNull(output.getIsFrozen());
    assertNotNull(output.getKeyImage());
    assertTrue(output.getKeyImage().getHex().length() > 0);
    TestUtils.testUnsignedBigInteger(output.getAmount(), true);
    
    // vout has circular reference to its transaction which has some initialized fields
    MoneroTxWallet tx = output.getTx();
    assertNotNull(tx);
    assertTrue(tx.getVouts().contains(output));
    assertNotNull(tx.getId());
    assertEquals(true, tx.getIsConfirmed());  // TODO monero-wallet-rpc: possible to get unconfirmed vouts?
    assertEquals(true, tx.getIsRelayed());
    assertEquals(false, tx.getIsFailed());
    assertTrue(tx.getHeight() > 0);
    
    // test copying
    MoneroOutputWallet copy = output.copy();
    assertTrue(copy != output);
    assertEquals(output.toString(), copy.toString());
    assertNull(copy.getTx());  // TODO: should vout copy do deep copy of tx so models are graph instead of tree?  Would need to work out circular references
  }
  
  /**
   * Gets random transactions.
   * 
   * @param wallet is the wallet to query for transactions
   * @param txRequest specifies the transactions to retrieve
   * @param minTxs specifies the minimum number of transactions (null for no minimum)
   * @param maxTxs specifies the maximum number of transactions (null for all filtered transactions)
   * @return List<MoneroTxWallet> are the random transactions
   */
  private static List<MoneroTxWallet> getRandomTransactions(MoneroWallet wallet, MoneroTxRequest txRequest, Integer minTxs, Integer maxTxs) {
    List<MoneroTxWallet> txs = wallet.getTxs(txRequest);
    if (minTxs != null) assertTrue(txs.size() + "/" + minTxs + " transactions found with request", txs.size() >= minTxs);
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
      assertNull(check.getInTxPool());
      assertNull(check.getNumConfirmations());
      assertNull(check.getReceivedAmount());
    }
  }

  private static void testCheckReserve(MoneroCheckReserve check) {
    assertNotNull(check.getIsGood());
    if (check.getIsGood()) {
      TestUtils.testUnsignedBigInteger(check.getTotalAmount());
      assert(check.getTotalAmount().compareTo(BigInteger.valueOf(0)) >= 0);
      TestUtils.testUnsignedBigInteger(check.getUnconfirmedSpentAmount());
      assertTrue(check.getUnconfirmedSpentAmount().compareTo(BigInteger.valueOf(0)) >= 0);
    } else {
      assertNull(check.getTotalAmount());
      assertNull(check.getUnconfirmedSpentAmount());
    }
  }
}
