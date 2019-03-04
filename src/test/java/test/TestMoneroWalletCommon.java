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

import org.junit.BeforeClass;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroBlock;
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
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroWalletOutput;
import monero.wallet.model.MoneroWalletTx;
import utils.TestUtils;

/**
 * Runs common tests that every Monero wallet implementation should support.
 * 
 * TODO: test filtering with not relayed
 */
public abstract class TestMoneroWalletCommon<T extends MoneroWallet> {
  
  // constants
  private static final int MAX_TX_PROOFS = 25;   // maximum number of transactions to check for each proof, undefined to check all
  
  // instance variables
  private MoneroWallet wallet;    // wallet instance to test
  private MoneroDaemon daemon;    // daemon instance to test
  private List<MoneroWalletTx> txCache; // local tx cache
  
  /**
   * Subclasseses return wallet instance to test and may override default RPC daemon.
   */
  protected abstract MoneroWallet initWallet();
  protected MoneroDaemon initDaemon() {
    return TestUtils.getDaemonRpc();
  }
  
  public TestMoneroWalletCommon() {
    wallet = initWallet();
    daemon = initDaemon();
  }

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }
  
  @Test
  public void testGetHeight() {
    int height = wallet.getHeight();
    assertTrue(height >= 0);
  }
  
  @Test
  public void testGetMnemonic() {
    String mnemonic = wallet.getMnemonic();
    MoneroUtils.validateMnemonic(mnemonic);
    assertEquals(TestUtils.TEST_MNEMONIC, mnemonic);
  }
  
  @Test
  public void testGetSupportedLanguages() {
    List<String> languages = wallet.getLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }
  
  @Test
  public void testGetPrivateViewKey() {
    String privateViewKey = wallet.getPrivateViewKey();
    MoneroUtils.validatePrivateViewKey(privateViewKey);
  }
  
  @Test
  public void testGetPrimaryAddress() {
    String primaryAddress = wallet.getPrimaryAddress();
    MoneroUtils.validateAddress(primaryAddress);
    assertEquals((wallet.getSubaddress(0, 0)).getAddress(), primaryAddress);
  }
  
  @Test
  public void testGetIntegratedAddressFromPaymentId() {
    
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
      assertEquals("Invalid payment ID", e.getMessage());
    }
  }
  
  @Test
  public void testDecodeIntegratedAddress() {
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress("03284e41c342f036");
    MoneroIntegratedAddress decodedAddress = wallet.decodeIntegratedAddress(integratedAddress.toString());
    assertEquals(integratedAddress, decodedAddress);
  }
  
  // TODO: test syncing from start height
  @Test
  public void testSyncWithoutProgress() {
    int numBlocks = 100;
    int chainHeight = daemon.getHeight();
    assertTrue(chainHeight >= numBlocks);
    MoneroSyncResult result = wallet.sync(chainHeight - numBlocks);  // sync end of chain
    assertTrue(result.getNumBlocksFetched() >= 0);
    assertNotNull(result.getReceivedMoney());
  }
  
  @Test
  public void testGetAccountsWithoutSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      assertNull(account.getSubaddresses());
    }
  }
  
  @Test
  public void testGetAccountsWithSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      assertFalse(account.getSubaddresses().isEmpty());
    }
  }
  
  @Test
  public void testGetAccount() {
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
  
  @Test
  public void testCreateAccountWithLabel() {
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount();
    testAccount(createdAccount);
    assertNull(createdAccount.getLabel());
    assertTrue(accountsBefore.size() == (wallet.getAccounts()).size() - 1);
  }
  
  @Test
  public void testCreateAccountWithoutLabel() {
    
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
  
  @Test
  public void testGetSubaddresses() {
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        testSubaddress(subaddress);
        assertEquals(account.getIndex(), subaddress.getIndex());
      }
    }
  }
  
  @Test
  public void testGetSubaddressesByIndices() {
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
  
  @Test
  public void testGetSubaddressByIndex() {
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
  
  @Test
  public void testCreateSubaddress() {
    
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
  
  @Test
  public void testGetSubaddressAddress() {
    assertEquals(wallet.getPrimaryAddress(), (wallet.getSubaddress(0, 0)).getAddress());
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : wallet.getSubaddresses(account.getIndex())) {
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
  
  @Test
  public void testGetAddressIndices() {
    
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
  public void testGetTransactionsWallet() {
    boolean nonDefaultIncoming = false;
    List<MoneroWalletTx> txs1 = getCachedTxs();
    List<MoneroWalletTx> txs2 = getAndTestTxs(wallet, null, null, true);
    assertEquals(txs2.size(), txs1.size());
    
    // build test context
    TestContext ctx = new TestContext();
    ctx.wallet = wallet;
    
    // test each tranasction
    Map<Integer, MoneroBlock> blockPerHeight = new HashMap<Integer, MoneroBlock>();
    for (int i = 0; i < txs1.size(); i++) {
      testWalletTx(txs1.get(i), ctx);
      testWalletTx(txs2.get(i), ctx);
      
      // test merging equivalent txs
      MoneroWalletTx copy1 = txs1.get(i).copy();
      MoneroWalletTx copy2 = txs2.get(i).copy();
      if (copy1.getIsConfirmed()) copy1.setBlock(txs1.get(i).getBlock().copy().setTxs(Arrays.asList(copy1)));
      if (copy2.getIsConfirmed()) copy2.setBlock(txs2.get(i).getBlock().copy().setTxs(Arrays.asList(copy2)));
      MoneroWalletTx merged = copy1.merge(copy2);
      testWalletTx(merged, ctx);
      
      // find non-default incoming
      if (txs1.get(i).getIncomingTransfers() != null) { // TODO: txs1.get(i).isIncoming()
        for (MoneroTransfer transfer : txs1.get(i).getIncomingTransfers()) {
          if (transfer.getAccountIndex() != 0 && transfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
        }
      }
      
      // ensure unique block reference per height
      if (txs2.get(i).getIsConfirmed()) {
        MoneroBlock block = blockPerHeight.get(txs2.get(i).getHeight());
        if (block == null) blockPerHeight.put(txs2.get(i).getHeight(), txs2.get(i).getBlock());
        else assertTrue("Block references for same height must be same", block == txs2.get(i).getBlock());
      }
    }
    
    // ensure non-default account and subaddress tested
    assertTrue("No incoming transfers found to non-default account and subaddress; run send-to-multiple tests first", nonDefaultIncoming);
  }
  
  // Can get transactions with additional configuration
  @Test
  public void testGetTransactionsWithConfiguration() {
    // TODO: litemode
    
    // get random transactions with payment ids for testing
    List<MoneroWalletTx> randomTxs = getRandomTransactions(wallet, new MoneroTxFilter().setHasPaymentId(true), 3, 5);
    for (MoneroWalletTx randomTx : randomTxs) {
      assertNotNull(randomTx.getPaymentId());
    }
    
    // get transactions by id
    List<String> txIds = new ArrayList<String>();
    for (MoneroWalletTx randomTx : randomTxs) {
      txIds.add(randomTx.getId());
      List<MoneroWalletTx> txs = getAndTestTxs(wallet, new MoneroTxFilter().setTxId(randomTx.getId()), null, true);
      assertEquals(txs.size(), 1);
      MoneroWalletTx merged = txs.get(0).merge(randomTx.copy()); // txs change with chain so check mergeability
      testWalletTx(merged, null);
    }
    
    // get transactions by ids
    List<MoneroWalletTx> txs = getAndTestTxs(wallet, new MoneroTxFilter().setTxIds(txIds), null, null);
    assertEquals(txs.size(), randomTxs.size());
    for (MoneroWalletTx tx : txs) assertTrue(txIds.contains(tx.getId()));
    
    // get transactions with an outgoing transfer
    TestContext ctx = new TestContext();
    ctx.hasOutgoingTransfer = true;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsOutgoing(true), ctx, true);
    for (MoneroWalletTx tx : txs) {
      assertTrue(tx.getIsOutgoing());
      assertNotNull(tx.getOutgoingTransfer());
      testTransfer(tx.getOutgoingTransfer());
    }
    
    // get transactions without an outgoing transfer
    ctx.hasOutgoingTransfer = false;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsOutgoing(false), ctx, true);
    for (MoneroWalletTx tx : txs) assertNull(tx.getOutgoingTransfer());
    
    // get transactions with incoming transfers
    ctx = new TestContext();
    ctx.hasIncomingTransfers = true;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIsIncoming(true), ctx, true);
    for (MoneroWalletTx tx : txs) {
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
    for (MoneroWalletTx tx : txs)  {
      assertFalse(tx.getIsIncoming());
      assertNull(tx.getIncomingTransfers());
    }
    
    // get transactions associated with an account
    int accountIdx = 1;
    txs = wallet.getTxs(new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setAccountIndex(accountIdx)));
    for (MoneroWalletTx tx : txs) {
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
    for (MoneroWalletTx tx : txs) {
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
    
    // get txs with manually built filter that are confirmed have an outgoing transfer from account 0
    ctx = new TestContext();
    ctx.hasOutgoingTransfer = true;
    MoneroTxFilter txFilter = new MoneroTxFilter();
    txFilter.setTx(new MoneroWalletTx().setIsConfirmed(true));
    txFilter.setTransferFilter(new MoneroTransferFilter().setTransfer(new MoneroTransfer().setAccountIndex(0)).setIsOutgoing(true));
    txs = getAndTestTxs(wallet, txFilter, ctx, true);
    for (MoneroWalletTx tx : txs) {
      if (!tx.getIsConfirmed()) System.out.println(tx);
      assertEquals(true, tx.getIsConfirmed());
      assertTrue(tx.getIsOutgoing());
      assertEquals(0, (int) tx.getOutgoingTransfer().getAccountIndex());
    }
    
    // get txs with outgoing transfers that have destinations to account 1
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setHasDestinations(true).setAccountIndex(0)), null, null);
    for (MoneroWalletTx tx : txs) {
      assertTrue(tx.getIsOutgoing());
      assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
    }
    
    // get transactions by payment id
    List<String> paymentIds = new ArrayList<String>();
    for (MoneroWalletTx tx : txs) paymentIds.add(tx.getPaymentId());
    assertTrue(paymentIds.size() > 1);
    for (String paymentId : paymentIds) {
      txs = getAndTestTxs(wallet, new MoneroTxFilter().setPaymentId(paymentId), null, null);
      assertEquals(1, txs.size());
      assertNotNull(txs.get(0).getPaymentId());
      MoneroUtils.validatePaymentId(txs.get(0).getPaymentId());
    }
    
    // get transactions by payment ids
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setPaymentIds(paymentIds), null, null);
    for (MoneroWalletTx tx : txs) {
      assertTrue(paymentIds.contains(tx.getPaymentId()));
    }
    
    // test block height filtering
    {
      txs = wallet.getTxs(new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)));
      assertTrue("No transactions; run send to multiple test", txs.size() > 0);
        
      // get and sort block heights in ascending order
      List<Integer> heights = new ArrayList<Integer>();
      for (MoneroWalletTx tx : txs) {
        heights.add(tx.getBlock().getHeader().getHeight());
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
        int height = tx.getBlock().getHeader().getHeight();
        assertTrue(height >= minHeight && height <= maxHeight);
      }
    }
    
    // include vouts with transactions
    ctx = new TestContext();
    ctx.getVouts = true;
    txs = getAndTestTxs(wallet, new MoneroTxFilter().setIncludeVouts(true), ctx, true);
    boolean found = false;
    for (MoneroWalletTx tx : txs) {
      if (tx.getVouts() != null) {
        assertTrue(tx.getVouts().size() > 0);
        found = true;
        break;
      }
    }
    assertTrue("No vouts found in txs", found);
  }
  
  // Returns all known fields of txs regardless of filtering
  @Test
  public void testTransactionFieldsWithFiltering() {
    
    // fetch wallet txs
    List<MoneroWalletTx> txs = wallet.getTxs(new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)));
    for (MoneroWalletTx tx : txs) {
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() == tx.getOutgoingTransfer().getAccountIndex()) continue;
        
        // fetch tx with filtering
        List<MoneroWalletTx> filteredTxs = wallet.getTxs(new MoneroTxFilter().setTransferFilter(new MoneroTransferFilter().setIsIncoming(true).setAccountIndex(transfer.getAccountIndex())));
        MoneroWalletTx filteredTx = ((List<MoneroWalletTx>) new MoneroTxFilter().setTxIds(Arrays.asList(tx.getId())).apply(filteredTxs)).get(0);
        
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
  public void testGetTransactionsValidateInputs() {
    // TODO: liteMode
    
    // test with invalid id
    List<MoneroWalletTx> txs = wallet.getTxs(new MoneroTxFilter().setTxId("invalid_id"));
    assertEquals(txs.size(), 0);
    
    // test invalid id in collection
    List<MoneroWalletTx> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    txs = wallet.getTxs(new MoneroTxFilter().setTxIds(Arrays.asList(randomTxs.get(0).getId(), "invalid_id")));
    assertEquals(1, txs.size());
    assertEquals(randomTxs.get(0).getId(), txs.get(0).getId());
    
    // TODO: test other input validation here
  }

  // Can get transfers in the wallet, accounts, and subaddresses
  @Test
  public void testGetTransfers() {
    
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
          assertEquals(subaddress.getAccountIndex(), transfer.getAccountIndex());
          assertEquals(transfer.getIsOutgoing() ? 0 : subaddress.getIndex(), (int) transfer.getSubaddressIndex());
          if (transfer.getAccountIndex() != 0 && transfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          
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
      for (MoneroTransfer transfer : subaddressTransfers) subaddressIndices.add(transfer.getSubaddressIndex());
      List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), null, null);
      if (transfers.size() != subaddressTransfers.size()) System.out.println("WARNING: outgoing transfers always from subaddress 0 (monero-wallet-rpc #5171)");
      //assertEquals(subaddressTransfers.size(), transfers.size()); // TODO monero-wallet-rpc: these may not be equal because outgoing transfers are always from subaddress 0 (#5171) and/or incoming transfers from/to same account are occluded (#4500)
      for (MoneroTransfer transfer : transfers) {
        assertEquals(transfer.getAccountIndex(), account.getIndex());
        assertTrue(subaddressIndices.contains(transfer.getSubaddressIndex()));
      }
    }
    
    // ensure transfer found with non-zero account and subaddress indices
    assertTrue("No transfers found in non-default account and subaddress; run send-to-multiple tests", nonDefaultIncoming);
  }
  
  // Can get transfers with additional configuration
  @Test
  public void testGetTransfersWithConfiguration() {
    // TODO: liteMode
    
    // get incoming transfers
    List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setIsIncoming(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.getIsIncoming());
    
    // get outgoing transfers
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setIsOutgoing(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.getIsOutgoing());
    
    // get confirmed transfers to account 0
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(0).setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true))), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(0, (int) transfer.getAccountIndex());
      assertTrue(transfer.getTx().getIsConfirmed());
    }
    
    // get confirmed transfers to [1, 2]
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setAccountIndex(1).setSubaddressIndex(2).setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true))), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(1, (int) transfer.getAccountIndex());
      assertEquals(transfer.getIsOutgoing() ? 0 : 2, (int) transfer.getSubaddressIndex());
      assertTrue(transfer.getTx().getIsConfirmed());
    }
    
    // get transfers in the tx pool
    transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setInTxPool(true))), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.getTx().getInTxPool());
    }
    
    // get random transactions
    List<MoneroWalletTx> txs = getRandomTransactions(wallet, null, 3, 5);
    
    // get transfers with a tx id
    List<String> txIds = new ArrayList<String>();
    for (MoneroWalletTx tx : txs) {
      txIds.add(tx.getId());
      transfers = getAndTestTransfers(wallet, new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setId(tx.getId()))), null, true);
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
    transferFilter.setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)));
    transfers = getAndTestTransfers(wallet, transferFilter, null, null);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.getIsOutgoing());
      assertTrue(transfer.getDestinations().size() > 0);
      assertEquals(true, transfer.getTx().getIsConfirmed());
    }
  }
  
  // Validates inputs when getting transfers
  @Test
  public void testGetTransfersValidateInputs() {
    // TODO: liteMode
    
    // test with invalid id
    List<MoneroTransfer> transfers = wallet.getTransfers(new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setId("invalid_id"))));
    assertEquals(0, transfers.size());
    
    // test invalid id in collection
    List<MoneroWalletTx> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    transfers = wallet.getTransfers(new MoneroTransferFilter().setTxFilter(new MoneroTxFilter().setTxIds(Arrays.asList(randomTxs.get(0).getId(), "invalid_id"))));
    assertTrue(transfers.size() > 0);
    MoneroWalletTx tx = transfers.get(0).getTx();
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
      List<MoneroWalletOutput> accountVouts = getAndTestVouts(wallet, new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(account.getIndex())), isUsed);
      for (MoneroWalletOutput vout : accountVouts) assertEquals(account.getIndex(), vout.getAccountIndex());
      
      // get vouts by subaddress index
      List<MoneroWalletOutput> subaddressVouts = new ArrayList<MoneroWalletOutput>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroWalletOutput> vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex())), subaddress.getIsUsed());
        for (MoneroWalletOutput vout : vouts) {
          assertEquals(subaddress.getAccountIndex(), vout.getAccountIndex());
          assertEquals(subaddress.getIndex(), vout.getSubaddressIndex());
          if (vout.getAccountIndex() != 0 && vout.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          subaddressVouts.add(vout);
        }
      }
      assertEquals(subaddressVouts.size(), accountVouts.size());
      
      // get vouts by subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroWalletOutput vout : subaddressVouts) subaddressIndices.add(vout.getSubaddressIndex());
      List<MoneroWalletOutput> vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(account.getIndex())).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), isUsed);
      assertEquals(vouts.size(), subaddressVouts.size());
      for (MoneroWalletOutput vout : vouts) {
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
    // TODO: liteMode
    
    // get unspent vouts to account 0
    List<MoneroWalletOutput> vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(0).setIsSpent(false)), null);
    for (MoneroWalletOutput vout : vouts) {
      assertEquals(0, vout.getAccountIndex(), 0);
      assertEquals(false, vout.getIsSpent());
    }
    
    // get spent vouts to account 1
    vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(1).setIsSpent(true)), true);
    for (MoneroWalletOutput vout : vouts) {
      assertEquals(1, (int) vout.getAccountIndex());
      assertEquals(true, vout.getIsSpent());
    }
    
    // get random transactions
    List<MoneroWalletTx> txs = getRandomTransactions(wallet, new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)), 3, 5);
    
    // get vouts with a tx id
    List<String> txIds = new ArrayList<String>();
    for (MoneroWalletTx tx : txs) {
      txIds.add(tx.getId());
      vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setId(tx.getId()))), true);
      for (MoneroWalletOutput vout : vouts) assertEquals(vout.getTx().getId(), tx.getId());
    }
    
    // get vouts with tx ids
    vouts = getAndTestVouts(wallet, new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setTxIds(txIds)), true);
    for (MoneroWalletOutput vout : vouts) assertTrue(txIds.contains(vout.getTx().getId()));
    
    // get confirmed vouts to specific subaddress with pre-built filter
    int accountIdx = 0;
    int subaddressIdx = 1;
    MoneroVoutFilter voutFilter = new MoneroVoutFilter();
    voutFilter.setVout(new MoneroWalletOutput().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx));
    voutFilter.setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)));
    vouts = getAndTestVouts(wallet, voutFilter, true);
    for (MoneroWalletOutput vout : vouts) {
      assertEquals(accountIdx, (int) vout.getAccountIndex());
      assertEquals(subaddressIdx, (int) vout.getSubaddressIndex());
      assertEquals(true, vout.getTx().getIsConfirmed());
    }
  }
  
  // Validates inputs when getting vouts
  @Test
  public void testGetVoutsValidateInputes() {
    // TODO: liteMode
    
    // test with invalid id
    List<MoneroWalletOutput> vouts = wallet.getVouts(new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setTx(new MoneroWalletTx().setId("invalid_id"))));
    assertEquals(0, vouts.size());
    
    // test invalid id in collection
    List<MoneroWalletTx> randomTxs = getRandomTransactions(wallet, new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)).setIncludeVouts(true), 3, 5);
    vouts = wallet.getVouts(new MoneroVoutFilter().setTxFilter(new MoneroTxFilter().setTxIds(Arrays.asList(randomTxs.get(0).getId(), "invalid_id"))));
    assertEquals(vouts.size(), randomTxs.get(0).getVouts().size());
    MoneroWalletTx tx = vouts.get(0).getTx();
    for (MoneroWalletOutput vout : vouts) assertTrue(tx == vout.getTx());
  }
  
  /**
   * Compares two MoneroTxs by their timestamp.
   */
  public class MoneroTxComparator implements Comparator<MoneroTx> {
    @Override
    public int compare(MoneroTx tx1, MoneroTx tx2) {
      long timestampA = tx1.getIsConfirmed() ? tx1.getBlock().getHeader().getTimestamp() : tx1.getReceivedTimestamp();
      long timestampB = tx2.getIsConfirmed() ? tx2.getBlock().getHeader().getTimestamp() : tx2.getReceivedTimestamp();
      if (timestampA < timestampB) return -1;
      if (timestampA > timestampB) return 1;
      return 0;
    }
  }
  
  // Has correct accounting across accounts, subaddresses, txs, transfers, and vouts
  @Test
  public void testAccounting() {
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    BigInteger walletBalance = wallet.getBalance();
    BigInteger walletUnlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);  // includes subaddresses
    List<MoneroWalletTx> txs = wallet.getTxs();
    
    // sort txs
    Collections.sort(txs, new MoneroTxComparator());
    
    // test wallet balance
    TestUtils.testUnsignedBigInteger(walletBalance);
    TestUtils.testUnsignedBigInteger(walletUnlockedBalance);
    assertTrue(walletBalance.compareTo(walletUnlockedBalance) > 0);
    
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
//    for (MoneroWalletTx tx : txs) {
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
    for (MoneroWalletTx tx : txs) if (tx.getInTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent vouts
    BigInteger walletSum = BigInteger.valueOf(0);
    for (MoneroWalletOutput vout : wallet.getVouts(new MoneroVoutFilter().setVout(new MoneroWalletOutput().setIsSpent(false)))) walletSum = walletSum.add(vout.getAmount());
    if (!walletBalance.equals(walletSum)) assertTrue("Wallet balance must equal sum of unspent vouts if no unconfirmed txs", hasUnconfirmedTx);
    
    // account balances are sum of their unspent vouts
    for (MoneroAccount account : accounts) {
      BigInteger accountSum = BigInteger.valueOf(0);
      List<MoneroWalletOutput> accountVouts = wallet.getVouts(new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(account.getIndex()).setIsSpent(false)));
      for (MoneroWalletOutput vout : accountVouts) accountSum = accountSum.add(vout.getAmount());
      if (!account.getBalance().equals(accountSum)) assertTrue("Account balance must equal sum of its unspent vouts if no unconfirmed txs", hasUnconfirmedTx);
      
      // subaddress balances are sum of their unspent vouts
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        BigInteger subaddressSum = BigInteger.valueOf(0);
        List<MoneroWalletOutput> subaddressVouts = wallet.getVouts(new MoneroVoutFilter().setVout(new MoneroWalletOutput().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()).setIsSpent(false)));
        for (MoneroWalletOutput vout : subaddressVouts) subaddressSum = subaddressSum.add(vout.getAmount());
        if (!subaddress.getBalance().equals(subaddressSum)) assertTrue("Subaddress balance must equal sum of its unspent vouts if no unconfirmed txs", hasUnconfirmedTx);
      }
    }
  }
  
  // Can get and set a transaction note
  @Test
  public void testSetTransactionNote() {
    List<MoneroWalletTx> txs = getRandomTransactions(wallet, null, 1, 5);
    
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
    
    // set tx notes
    String uuid = UUID.randomUUID().toString();
    List<MoneroWalletTx> txs = getCachedTxs();
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
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroWalletTx> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)).setTransferFilter(new MoneroTransferFilter().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (MoneroException e) {
      throw new Error("No txs with outgoing destinations found; run send tests");
    }
    
    // test good checks
    assertTrue("No transactions found with outgoing destinations", txs.size() > 0);
    for (MoneroWalletTx tx : txs) {
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
    MoneroWalletTx tx = txs.get(0);
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
    for (MoneroWalletTx aTx : getCachedTxs()) {
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
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroWalletTx> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxFilter().setTx(new MoneroWalletTx().setIsConfirmed(true)).setTransferFilter(new MoneroTransferFilter().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (MoneroException e) {
      throw new Error("No txs with outgoing destinations found; run send tests");
    }
    
    // test good checks with messages
    for (MoneroWalletTx tx : txs) {
      for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
        String signature = wallet.getTxProof(tx.getId(), destination.getAddress(), "This transaction definitely happened.");
        MoneroCheckTx check = wallet.checkTxProof(tx.getId(), destination.getAddress(), "This transaction definitely happened.", signature);
        testCheckTx(tx, check);
      }
    }
    
    // test good check without message
    MoneroWalletTx tx = txs.get(0);
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
    
    // get random confirmed outgoing txs
    List<MoneroWalletTx> txs = getRandomTransactions(wallet, new MoneroTxFilter().setIsIncoming(false).setTx(new MoneroWalletTx().setInTxPool(false).setIsFailed(false)), 2, MAX_TX_PROOFS);
    for (MoneroWalletTx tx : txs) {
      assertEquals(true, tx.getIsConfirmed());
      assertNull(tx.getIncomingTransfers());
      assertNotNull(tx.getOutgoingTransfer());
    }
    
    // test good checks with messages
    for (MoneroWalletTx tx : txs) {
      String signature = wallet.getSpendProof(tx.getId(), "I am a message.");
      assertTrue(wallet.checkSpendProof(tx.getId(), "I am a message.", signature));
    }
    
    // test good check without message
    MoneroWalletTx tx = txs.get(0);
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
    
    // get proof of entire wallet
    String signature = wallet.getReserveProofWallet("Test message");
    
    // check proof of entire wallet
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", signature);
    assertTrue(check.getIsGood());
    testCheckReserve(check);
    BigInteger balance = wallet.getBalance();
    if (!balance.equals(check.getTotalAmount())) {  // TODO monero-wallet-rpc: this check fails with unconfirmed txs
      List<MoneroWalletTx> unconfirmedTxs = wallet.getTxs(new MoneroTxFilter().setTx(new MoneroWalletTx().setInTxPool(true)));
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
  // TODO: re-enable this after 14.x point release which fixes this
  @Test
  public void getReserveProofAccount() {
    
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
  
//  // Can get outputs in hex format
//  @Test
//  public void testGetOutputsHex() {
//    let outputsHex = wallet.getOutputsHex();
//    assertEquals(typeof outputsHex, "string");  // TODO: this will fail if wallet has no outputs; run these tests on new wallet
//    assertTrue(outputsHex.size() > 0);
//  }
//  
//  // Can import outputs in hex format
//  @Test
//  public void testImportOutputsHex() {
//    
//    // get outputs hex
//    let outputsHex = wallet.getOutputsHex();
//    
//    // import outputs hex
//    if (outputsHex !== undefined) {
//      let numImported = wallet.importOutputsHex(outputsHex);
//      assertTrue(numImported > 0);
//    }
//  }
//  
//  // Can get signed key images
//  @Test
//  public void testGetSignedKeyImages() {
//    let images = wallet.getKeyImages();
//    assertTrue(Array.isArray(images));
//    assertTrue(images.size() > 0, "No signed key images in wallet");
//    for (let image of images) {
//      assertTrue(image instanceof MoneroKeyImage);
//      assertTrue(image.getHex());
//      assertTrue(image.getSignature());
//    }
//  }
//  
//  // Can get new key images from the last import
//  @Test
//  public void testGetNewKeyImagesFromLastImport() {
//    
//    // get outputs hex
//    let outputsHex = wallet.getOutputsHex();
//    
//    // import outputs hex
//    if (outputsHex !== undefined) {
//      let numImported = wallet.importOutputsHex(outputsHex);
//      assertTrue(numImported > 0);
//    }
//    
//    // get and test new key images from last import
//    let images = wallet.getNewKeyImagesFromLastImport();
//    assertTrue(Array.isArray(images));
//    assertTrue(images.size() > 0, "No new key images in last import");  // TODO: these are already known to the wallet, so no new key images will be imported
//    for (let image of images) {
//      assertTrue(image.getHex());
//      assertTrue(image.getSignature());
//    }
//  }
//  
//  // Can import key images
//  @Test
//  public void testImportKeyImages() {
//    let images = wallet.getKeyImages();
//    assertTrue(Array.isArray(images));
//    assertTrue(images.size() > 0, "Wallet does not have any key images; run send tests");
//    let result = wallet.importKeyImages(images);
//    assertTrue(result.getHeight() > 0);
//    
//    // determine if non-zero spent and unspent amounts are expected
//    let txs = wallet.getTxs({isConfirmed: true, transferFilter: {isOutgoing: true}});
//    let balance = wallet.getBalance();
//    let hasSpent = txs.size() > 0;
//    let hasUnspent = balance.toJSValue() > 0;
//    
//    // test amounts
//    TestUtils.testUnsignedBigInteger(result.getSpentAmount(), hasSpent);
//    TestUtils.testUnsignedBigInteger(result.getUnspentAmount(), hasUnspent);
//  }
//  
//  // Can sign and verify messages
//  @Test
//  public void testSignAndVerifyMessages() {
//    let msg = "This is a super important message which needs to be signed and verified.";
//    let signature = wallet.sign(msg);
//    let verified = wallet.verify(msg, wallet.getAddress(0, 0), signature);
//    assertEquals(verified, true);
//    verified = wallet.verify(msg, TestMoneroWalletCommon.SAMPLE_ADDRESS, signature);
//    assertEquals(verified, false);
//  }
//  
//  // Can get and set arbitrary key/value attributes
//  @Test
//  public void testSetKeyValues() {
//    
//    // set attributes
//    let attrs = {};
//    for (int i = 0; i < 5; i++) {
//      let key = "attr" + i;
//      let val = GenUtils.uuidv4();
//      attrs[key] = val;
//      wallet.setAttribute(key, val);
//    }
//    
//    // test attributes
//    for (let key of Object.keys(attrs)) {
//      assertEquals(attrs[key], wallet.getAttribute(key));
//    }
//  }
//  
//  // Can convert between a tx send config and payment URI
//  @Test
//  public void testCreatePaymentUri() {
//    
//    // test with address and amount
//    let config1 = new MoneroSendConfig(wallet.getAddress(0, 0), BigInteger.valueOf(0));
//    let uri = wallet.createPaymentUri(config1);
//    let config2 = wallet.parsePaymentUri(uri);
//    GenUtils.deleteUndefinedKeys(config1);
//    GenUtils.deleteUndefinedKeys(config2);
//    assertEquals(JSON.parse(JSON.stringify(config2)), JSON.parse(JSON.stringify(config1)));
//    
//    // test with all fields3
//    config1.getDestinations()[0].setAmount(new BigInteger("425000000000"));
//    config1.setPaymentId("03284e41c342f036");
//    config1.setRecipientName("John Doe");
//    config1.setNote("OMZG XMR FTW");
//    uri = wallet.createPaymentUri(config1);
//    config2 = wallet.parsePaymentUri(uri);
//    GenUtils.deleteUndefinedKeys(config1);
//    GenUtils.deleteUndefinedKeys(config2);
//    assertEquals(config2, config1);
//    
//    // test with undefined address
//    let address = config1.getDestinations()[0].getAddress();
//    config1.getDestinations()[0].setAddress(undefined);
//    try {
//      wallet.createPaymentUri(config1);
//      fail("Should have thrown RPC exception with invalid parameters");
//    } catch (MoneroException e) {
//      assertEquals(e.getRpcCode(), -11);
//      assertTrue(e.getRpcMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
//    }
//    config1.getDestinations()[0].setAddress(address);
//    
//    // test with invalid payment id
//    config1.setPaymentId("bizzup");
//    try {
//      wallet.createPaymentUri(config1);
//      fail("Should have thrown RPC exception with invalid parameters");
//    } catch (MoneroException e) {
//      assertEquals(e.getRpcCode(), -11);
//      assertTrue(e.getRpcMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
//    }
//  }
//  
//  @Test
//  public void testMining() {
//    wallet.startMining(2, false, true);
//    wallet.stopMining();
//  }
  
  
  // --------------------------------- PRIVATE --------------------------------
  
  private List<MoneroWalletTx> getCachedTxs() {
    if (txCache != null) return txCache;
    txCache = wallet.getTxs();
    return txCache;
  }
  
  /**
   * Fetchs and tests transactions according to the given config.
   * 
   * TODO: convert config to filter and ensure each tx passes filter, same with testGetTransfer and getAndTestVouts
   */
  private List<MoneroWalletTx> getAndTestTxs(MoneroWallet wallet, MoneroTxFilter filter, TestContext ctx, Boolean isExpected) {
    List<MoneroWalletTx> txs = wallet.getTxs(filter);
    assertNotNull(txs);
    if (Boolean.FALSE.equals(isExpected)) assertTrue(txs.isEmpty());
    if (Boolean.TRUE.equals(isExpected)) assertFalse(txs.isEmpty());
    for (MoneroWalletTx tx : txs) testWalletTx(tx, ctx);
    return txs;
  }
  
  /**
   * Fetchs and tests transfers according to the given config.
   */
  private static List<MoneroTransfer> getAndTestTransfers(MoneroWallet wallet, MoneroTransferFilter filter, TestContext ctx, Boolean isExpected) {
    List<MoneroTransfer> transfers = wallet.getTransfers(filter);
    if (isExpected == false) assertEquals(0, transfers.size());
    if (isExpected == true) assertTrue("Transactions were expected but not found; run send tests?", transfers.size() > 0);
    ctx = new TestContext();
    ctx.wallet = wallet;
    for (MoneroTransfer transfer : transfers) testWalletTx(transfer.getTx(), ctx);
    return transfers;
  }

  /**
   * Fetchs and tests vouts according to the given config.
   */
  private static List<MoneroWalletOutput> getAndTestVouts(MoneroWallet wallet, MoneroVoutFilter filter, Boolean isExpected) {
    List<MoneroWalletOutput> vouts = wallet.getVouts(filter);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, vouts.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Vouts were expected but not found; run send tests?", vouts.size() > 0);
    for (MoneroWalletOutput vout : vouts) testVout(vout);
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
    Boolean isSweep;
    Boolean doNotTestCopy;
    Boolean getVouts;
    public TestContext() { }
    public TestContext(TestContext ctx) {
      throw new RuntimeException("Not implemented");
    }
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
    if (subaddress.getBalance().equals(BigInteger.valueOf(0))) assertTrue(subaddress.getIsUsed());
  }
  
  /**
   * Tests a wallet transaction with a test configuration.
   * 
   * @param tx is the wallet transaction to test
   * @param ctx provides test context
   *        ctx.wallet is used to cross reference tx info if available
   *        ctx.sendConfig specifies config of a tx generated with send()
   *        ctx.hasDestinations specifies if the tx has an outgoing transfer with destinations, undefined if doesn't matter
   *        ctx.getVouts specifies if vouts were fetched and should therefore be expected with incoming transfers
   */
  private static void testWalletTx(MoneroWalletTx tx, TestContext ctx) {
    
    // validate / sanitize inputs
    ctx = new TestContext(ctx);
    ctx.wallet = null;  // TODO: re-enable
    assertNotNull(tx);
    
    // test common field types
    testWalletTxTypes(tx);
    
    // test confirmed
    if (tx.getIsConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeader().getHeight() > 0);
      assertTrue(tx.getBlock().getHeader().getTimestamp() > 0);
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
      assertNotNull(tx.getLastFailedHeight());
      assertNotNull(tx.getLastFailedId());
      
      // these should be initialized unless a response from sending
      if (ctx.sendConfig == null) {
        assertTrue(tx.getReceivedTimestamp() > 0);
        assertTrue(tx.getNumEstimatedBlocksUntilConfirmed() > 0);
      }
    } else {
      assertNull(tx.getNumEstimatedBlocksUntilConfirmed());
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
      testTransfer(tx.getOutgoingTransfer());
      if (ctx.isSweep) assertEquals(tx.getOutgoingTransfer().getDestinations().size(), 1);
      
      // TODO: handle special cases
    } else {
      assertTrue(tx.getIncomingTransfers().size() > 0);
      assertNull(tx.getOutgoingAmount());
      assertNull(tx.getOutgoingTransfer());
      assertNull(tx.getMixin());
      assertNull(tx.getHex());
      assertNull(tx.getMetadata());
      assertNull(tx.getKey());
    }
    
    // test incoming transfers
    if (tx.getIncomingTransfers() != null) {
      assertTrue(tx.getIncomingTransfers().size() > 0);
      TestUtils.testUnsignedBigInteger(tx.getIncomingAmount());      
      assertEquals(tx.getIsFailed(), false);
      
      // test each transfer and collect transfer sum
      BigInteger transferSum = BigInteger.valueOf(0);
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
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
    
    // test tx result from send(), sendSplit(), or relayTxs()
    if (ctx.sendConfig != null) {
      
      // test common attributes
      MoneroSendConfig sendConfig = ctx.sendConfig;
      assertEquals(false, tx.getIsConfirmed());
      testTransfer(tx.getOutgoingTransfer());
      assertEquals(sendConfig.getMixin(), tx.getMixin());
      assertEquals(sendConfig.getUnlockTime() != null ? sendConfig.getUnlockTime() : 0, (int) tx.getUnlockTime());
      assertNull(tx.getBlock());
      if (sendConfig.getCanSplit()) assertNull(tx.getKey()); // TODO monero-wallet-rpc: key only known on `transfer` response
      else assertTrue(tx.getKey().length() > 0);
      assertNotNull(tx.getHex());
      assertTrue(tx.getHex().length() > 0);
      assertNotNull(tx.getMetadata());
      assertNull(tx.getReceivedTimestamp());
      
      // test destinations of sent tx
      assertEquals(sendConfig.getDestinations().size(), tx.getOutgoingTransfer().getDestinations().size());
      for (int i = 0; i < sendConfig.getDestinations().size(); i++) {
        assertEquals(sendConfig.getDestinations().get(i).getAddress(), tx.getOutgoingTransfer().getDestinations().get(i).getAddress());
        if (ctx.isSweep) {
          assertEquals(1, sendConfig.getDestinations().size());
          assertNull(sendConfig.getDestinations().get(i).getAmount());
          assertEquals(tx.getOutgoingTransfer().getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        } else {
          assertEquals(sendConfig.getDestinations().get(i).getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        }
      }
      
      // test relayed txs
      if (!sendConfig.getDoNotRelay()) {
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
    } else {
      assertNull(tx.getMixin());
      assertNull(tx.getKey());
      assertNull(tx.getHex());
      assertNull(tx.getMetadata());
      assertNull(tx.getLastRelayedTimestamp());
    }
    
    // test vouts
    if (tx.getIncomingTransfers() != null && Boolean.TRUE.equals(tx.getIsConfirmed()) && ctx.getVouts) assertTrue(tx.getVouts().size() > 0);
    if (tx.getVouts() != null) for (MoneroWalletOutput vout : tx.getVouts()) testVout(vout);
    
    // test deep copy
    if (!Boolean.TRUE.equals(ctx.doNotTestCopy)) testWalletTxCopy(tx, ctx);
  }

  /**
   * Tests that common tx field types are valid regardless of tx state.
   * 
   * @param tx is the tx to test
   */
  private static void testWalletTxTypes(MoneroWalletTx tx) {
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

  private static void testWalletTxCopy(MoneroWalletTx tx, TestContext ctx) {
    
    // copy tx and assert deep equality
    MoneroWalletTx copy = tx.copy();
    assertTrue(copy instanceof MoneroWalletTx);
    assertEquals(copy, tx);
    
    // test different references
    if (tx.getOutgoingTransfer() != null) {
      assertTrue(tx.getOutgoingTransfer() != copy.getOutgoingTransfer());
      assertTrue(tx.getOutgoingTransfer().getTx() != copy.getOutgoingTransfer().getTx());
      //assertTrue(tx.getOutgoingTransfer().getAmount() != copy.getOutgoingTransfer().getAmount());  // TODO: BI 0 == BI 0?, testing this instead:
      if (tx.getOutgoingTransfer().getAmount() == copy.getOutgoingTransfer().getAmount()) assertTrue(tx.getOutgoingTransfer().getAmount().equals(BigInteger.valueOf(0)));
      if (tx.getOutgoingTransfer().getDestinations() != null) {
        assertTrue(tx.getOutgoingTransfer().getDestinations() != copy.getOutgoingTransfer().getDestinations());
        for (int i = 0; i < tx.getOutgoingTransfer().getDestinations().size(); i++) {
          assertEquals(copy.getOutgoingTransfer().getDestinations().get(i), tx.getOutgoingTransfer().getDestinations().get(i));
          assertTrue(tx.getOutgoingTransfer().getDestinations().get(i) != copy.getOutgoingTransfer().getDestinations().get(i));
          if (tx.getOutgoingTransfer().getDestinations().get(i).getAmount() == copy.getOutgoingTransfer().getDestinations().get(i).getAmount()) assertTrue(tx.getOutgoingTransfer().getDestinations().get(i).getAmount().equals(BigInteger.valueOf(0)));
        }
      }
    }
    if (tx.getIncomingTransfers() != null) {
      for (int i = 0; i < tx.getIncomingTransfers().size(); i++) {
        assertEquals(copy.getIncomingTransfers().get(i), tx.getIncomingTransfers().get(i));
        assertTrue(tx.getIncomingTransfers().get(i) != copy.getIncomingTransfers().get(i));
        if (tx.getIncomingTransfers().get(i).getAmount() == copy.getIncomingTransfers().get(i).getAmount()) assertTrue(tx.getIncomingTransfers().get(i).getAmount().equals(BigInteger.valueOf(0)));
      }
    }
    if (tx.getVouts() != null) {
      for (int i = 0; i < tx.getVouts().size(); i++) {
        assertEquals(copy.getVouts().get(i), tx.getVouts().get(i));
        assertTrue(tx.getVouts().get(i) != copy.getVouts().get(i));
        if (tx.getVouts().get(i).getAmount() == copy.getVouts().get(i).getAmount()) assertTrue(tx.getVouts().get(i).getAmount().equals(BigInteger.valueOf(0)));
      }
    }
    
    // test copied tx
    ctx = new TestContext(ctx);
    ctx.doNotTestCopy = true;
    if (tx.getBlock() != null) copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy))); // copy block for testing
    testWalletTx(copy, ctx);
    
    // test merging with copy
    MoneroWalletTx merged = copy.merge(copy.copy());
    assertEquals(merged.toString(), tx.toString());
  }
  
  private static void testTransfer(MoneroTransfer transfer) {
    assertNotNull(transfer);
    TestUtils.testUnsignedBigInteger(transfer.getAmount());
    
    // transfer and tx reference each other
    assertNotNull(transfer.getTx());
    if (!transfer.equals(transfer.getTx().getOutgoingTransfer())) {
      boolean found = false;
      assertNotNull(transfer.getTx().getIncomingTransfers());
      assertTrue("Transaction does not reference given transfer", transfer.getTx().getIncomingTransfers().contains(transfer));
    }
    
    // test destinations sum to outgoing amount
    if (transfer.getDestinations() != null) {
      assertTrue(transfer.getDestinations().size() > 0);
      assertEquals(true, transfer.getIsOutgoing());
      BigInteger sum = BigInteger.valueOf(0);
      for (MoneroDestination destination : transfer.getDestinations()) {
        assertNotNull(destination.getAddress());
        TestUtils.testUnsignedBigInteger(destination.getAmount(), true);
        sum = sum.add(destination.getAmount());
      }
      try {
        assertEquals(transfer.getAmount().toString(), sum.toString());
      } catch (MoneroException e) {
        System.out.println(transfer.getTx().toString());
        throw e;
      }
    }
    
    // transfer is outgoing xor incoming
    assertTrue((transfer.getIsOutgoing() == true && transfer.getIsIncoming() == false) || (transfer.getIsOutgoing() == false && transfer.getIsIncoming() == true));
  }
  
  private static void testVout(MoneroWalletOutput vout) {
    assertNotNull(vout);
    assertTrue(vout.getAccountIndex() >= 0);
    assertTrue(vout.getSubaddressIndex() >= 0);
    assertTrue(vout.getIndex() >= 0);
    assertNotNull(vout.getIsSpent());
    assertNotNull(vout.getKeyImage());
    assertTrue(vout.getKeyImage().getHex().length() > 0);
    TestUtils.testUnsignedBigInteger(vout.getAmount(), true);
    
    // vout has circular reference to its transaction which has some initialized fields
    MoneroWalletTx tx = vout.getTx();
    assertNotNull(tx);
    assertTrue(tx.getVouts().contains(vout));
    assertNotNull(tx.getId());
    assertEquals(true, tx.getIsConfirmed());  // TODO monero-wallet-rpc: possible to get unconfirmed vouts?
    assertEquals(true, tx.getIsRelayed());
    assertEquals(false, tx.getIsFailed());
    
    // test copying
    MoneroWalletOutput copy = vout.copy();
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
   * @return List<MoneroWalletTx> are the random transactions
   */
  private static List<MoneroWalletTx> getRandomTransactions(MoneroWallet wallet, MoneroTxFilter filter, Integer minTxs, Integer maxTxs) {
    List<MoneroWalletTx> txs = wallet.getTxs(filter);
    if (minTxs != null) assertTrue(txs.size() + "/" + minTxs + " transactions found with filter: " + filter, txs.size() >= minTxs);
    Collections.shuffle(txs);
    if (maxTxs == null) return txs;
    else return txs.subList(0, Math.min(maxTxs, txs.size()));
  }
  
  private static void testCheckTx(MoneroWalletTx tx, MoneroCheckTx check) {
    assertNotNull(check.getIsGood());
    if (check.getIsGood()) {
      assert(check.getNumConfirmations() >= 0);
      assertNotNull(check.getInTxPool());
      TestUtils.testUnsignedBigInteger(check.getReceivedAmount());
      if (check.getInTxPool()) assertEquals(0, (int) check.getNumConfirmations());
      else assert(check.getNumConfirmations() > 0); // TODO (monero-wall-rpc) this fails (confirmations is 0) for (at least one) transaction that has 1 confirmation on testCheckTxKey()
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
