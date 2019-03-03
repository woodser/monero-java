package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.BeforeClass;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import utils.TestUtils;

/**
 * Runs common tests that every Monero wallet implementation should support.
 * 
 * TODO: test filtering with not relayed
 */
public abstract class TestMoneroWalletCommon<T extends MoneroWallet> {
  
  // instances to test
  private MoneroWallet wallet;
  private MoneroDaemon daemon;
  
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
      throw new Error("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown a RPC exception");
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
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Address does not belong to the wallet", e.getMessage());
    }
    
    // test invalid address
    try {
      subaddress = wallet.getAddressIndex("this is definitely not an address");
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Address does not belong to the wallet", e.getMessage());
    }
  }
  
  it("Can get the locked and unlocked balances of the wallet, accounts, and subaddresses", async function() {
    
    // fetch accounts with all info as reference
    let accounts = await wallet.getAccounts(true);
    
    // test that balances add up between accounts and wallet
    let accountsBalance = new BigInteger(0);
    let accountsUnlockedBalance = new BigInteger(0);
    for (let account of accounts) {
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
      
      // test that balances add up between subaddresses and accounts
      let subaddressesBalance = new BigInteger(0);
      let subaddressesUnlockedBalance = new BigInteger(0);
      for (let subaddress of account.getSubaddresses()) {
        subaddressesBalance = subaddressesBalance.add(subaddress.getBalance());
        subaddressesUnlockedBalance = subaddressesUnlockedBalance.add(subaddress.getUnlockedBalance());
        
        // test that balances are consistent with getAccounts() call
        assert.equal((await wallet.getBalance(subaddress.getAccountIndex(), subaddress.getIndex())).toString(), subaddress.getBalance().toString());
        assert.equal((await wallet.getUnlockedBalance(subaddress.getAccountIndex(), subaddress.getIndex())).toString(), subaddress.getUnlockedBalance().toString());
      }
      assert.equal((await wallet.getBalance(account.getIndex())).toString(), subaddressesBalance.toString());
      assert.equal((await wallet.getUnlockedBalance(account.getIndex())).toString(), subaddressesUnlockedBalance.toString());
    }
    TestUtils.testUnsignedBigInteger(accountsBalance);
    TestUtils.testUnsignedBigInteger(accountsUnlockedBalance);
    assert.equal((await wallet.getBalance()).toString(), accountsBalance.toString());
    assert.equal((await wallet.getUnlockedBalance()).toString(), accountsUnlockedBalance.toString());
    
    // test invalid input
    try {
      await wallet.getBalance(undefined, 0);
      throw new Error("Should have failed");
    } catch(e) {
      assert.notEqual("Should have failed", e.message);
    }
  });
  
  it("Can get transactions in the wallet", async function() {
    let nonDefaultIncoming = false;
    let txs1 = await getCachedTxs();
    let txs2 = await testGetTxs(wallet, undefined, true);
    assert.equal(txs2.length, txs1.length);
    
    // test each tranasction
    let blocksPerHeight = {};
    for (let i = 0; i < txs1.length; i++) {
      await testWalletTx(txs1[i], {wallet: wallet});
      await testWalletTx(txs2[i], {wallet: wallet});
      
      // test merging equivalent txs
      let copy1 = txs1[i].copy();
      let copy2 = txs2[i].copy();
      if (copy1.getIsConfirmed()) copy1.setBlock(txs1[i].getBlock().copy().setTxs([copy1]));
      if (copy2.getIsConfirmed()) copy2.setBlock(txs2[i].getBlock().copy().setTxs([copy2]));
      let merged = copy1.merge(copy2);
      await testWalletTx(merged, {wallet: wallet});
      
      // find non-default incoming
      if (txs1[i].getIncomingTransfers()) {
        for (let transfer of txs1[i].getIncomingTransfers()) {
          if (transfer.getAccountIndex() !== 0 && transfer.getSubaddressIndex() !== 0) nonDefaultIncoming = true;
        }
      }
      
      // ensure unique block reference per height
      if (txs2[i].getIsConfirmed()) {
        let block = blocksPerHeight[txs2[i].getHeight()];
        if (block === undefined) blocksPerHeight[txs2[i].getHeight()] = txs2[i].getBlock();
        else assert(block === txs2[i].getBlock(), "Block references for same height must be same");
      }
    }
    
    // ensure non-default account and subaddress tested
    assert(nonDefaultIncoming, "No incoming transfers found to non-default account and subaddress; run send-to-multiple tests first");
  });
  
  if (!liteMode)
  it("Can get transactions with additional configuration", async function() {
    
    // get random transactions with payment ids for testing
    let randomTxs = await getRandomTransactions(wallet, {hasPaymentId: true}, 3, 5);
    for (let randomTx of randomTxs) {
      assert(randomTx.getPaymentId());
    }
    
    // get transactions by id
    let txIds = [];
    for (let randomTx of randomTxs) {
      txIds.push(randomTx.getId());
      let txs = await testGetTxs(wallet, {txId: randomTx.getId()}, true);
      assert.equal(txs.length, 1);
      let merged = txs[0].merge(randomTx.copy()); // txs change with chain so check mergeability
      await testWalletTx(merged);
    }
    
    // get transactions by ids
    let txs = await testGetTxs(wallet, {txIds: txIds});
    assert.equal(txs.length, randomTxs.length);
    for (let tx of txs) assert(txIds.includes(tx.getId()));
    
    // get transactions with an outgoing transfer
    txs = await testGetTxs(wallet, {hasOutgoingTransfer: true}, true);
    for (let tx of txs) assert(tx.getOutgoingTransfer() instanceof MoneroTransfer);
    
    // get transactions without an outgoing transfer
    txs = await testGetTxs(wallet, {hasOutgoingTransfer: false}, true);
    for (let tx of txs) assert.equal(tx.getOutgoingTransfer(), undefined);
    
    // get transactions with incoming transfers
    txs = await testGetTxs(wallet, {hasIncomingTransfers: true}, true);
    for (let tx of txs) {
      assert(tx.getIncomingTransfers().length > 0);
      for (let transfer of tx.getIncomingTransfers()) assert(transfer instanceof MoneroTransfer);
    }
    
    // get transactions without incoming transfers
    txs = await testGetTxs(wallet, {hasIncomingTransfers: false}, true);
    for (let tx of txs) assert.equal(tx.getIncomingTransfers(), undefined);
    
    // get transactions associated with an account
    let accountIdx = 1;
    txs = await wallet.getTxs({transferFilter: {accountIndex: accountIdx}});
    for (let tx of txs) {
      let found = false;
      if (tx.getOutgoingTransfer() && tx.getOutgoingTransfer().getAccountIndex() === accountIdx) found = true;
      else if (tx.getIncomingTransfers()) {
        for (let transfer of tx.getIncomingTransfers()) {
          if (transfer.getAccountIndex() === accountIdx) {
            found = true;
            break;
          }
        }
      }
      assert(found, ("Transaction is not associated with account " + accountIdx + ":\n" + tx.toString()));
    }
    
    // get transactions with incoming transfers to an account
    txs = await wallet.getTxs({transferFilter: {isIncoming: true, accountIndex: accountIdx}});
    for (let tx of txs) {
      assert(tx.getIncomingTransfers().length > 0);
      let found = false;
      for (let transfer of tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() === accountIdx) {
          found = true;
          break;
        }
      }
      assert(found, "No incoming transfers to account " + accountIdx + " found:\n" + tx.toString());
    }
    
    // get txs with manually built filter that are confirmed have an outgoing transfer from account 0
    let txFilter = new MoneroTxFilter();
    txFilter.setTx(new MoneroTx().setIsConfirmed(true));
    txFilter.setTransferFilter(new MoneroTransferFilter().setTransfer(new MoneroTransfer().setAccountIndex(0)).setIsOutgoing(true));
    txs = await testGetTxs(wallet, txFilter, true);
    for (let tx of txs) {
      if (!tx.getIsConfirmed()) console.log(tx.toString());
      assert.equal(tx.getIsConfirmed(), true);
      assert(tx.getOutgoingTransfer());
      assert.equal(tx.getOutgoingTransfer().getAccountIndex(), 0);
    }
    
    // get txs with outgoing transfers that have destinations to account 1
    txs = await testGetTxs(wallet, {transferFilter: {hasDestinations: true, accountIndex: 0}});
    for (let tx of txs) {
      assert(tx.getOutgoingTransfer());
      assert(tx.getOutgoingTransfer().getDestinations().length > 0);
    }
    
    // get transactions by payment id
    let paymentIds = randomTxs.map(tx => tx.getPaymentId());
    assert(paymentIds.length > 1);
    for (let paymentId of paymentIds) {
      txs = await testGetTxs(wallet, {paymentId: paymentId});
      assert.equal(txs.length, 1);
      assert(txs[0].getPaymentId());
      MoneroUtils.validatePaymentId(txs[0].getPaymentId());
    }
    
    // get transactions by payment ids
    txs = await testGetTxs(wallet, {paymentIds: paymentIds});
    for (let tx of txs) {
      assert(paymentIds.includes(tx.getPaymentId()));
    }
    
    // test block height filtering
    {
      txs = await wallet.getTxs({accountIndex: 0, isConfirmed: true});
      assert(txs.length > 0, "No transactions; run send to multiple test");
        
      // get and sort block heights in ascending order
      let heights = [];
      for (let tx of txs) {
        heights.push(tx.getBlock().getHeader().getHeight());
      }
      GenUtils.sort(heights);
      
      // pick minimum and maximum heights for filtering
      let minHeight = -1;
      let maxHeight = -1;
      if (heights.length == 1) {
        minHeight = 0;
        maxHeight = heights[0] - 1;
      } else {
        minHeight = heights[0] + 1;
        maxHeight = heights[heights.length - 1] - 1;
      }
      
      // assert some transactions filtered
      let unfilteredCount = txs.length;
      txs = await testGetTxs(wallet, {accountIndex: 0, minHeight: minHeight, maxHeight: maxHeight}, true);
      assert(txs.length < unfilteredCount);
      for (let tx of txs) {
        let height = tx.getBlock().getHeader().getHeight();
        assert(height >= minHeight && height <= maxHeight);
      }
    }
    
    // include vouts with transactions
    txs = await testGetTxs(wallet, {getVouts: true}, true);
    let found = false;
    for (let tx of txs) {
      if (tx.getVouts()) {
        assert(tx.getVouts().length > 0);
        found = true;
        break;
      }
    }
    assert(found, "No vouts found in txs");
  });
  
  it("Returns all known fields of txs regardless of filtering", async function() {
    
    // fetch wallet txs
    let txs = await wallet.getTxs({isConfirmed: true});
    
    for (let tx of txs) {
      
      // find tx sent to same wallet with incoming transfer in different account than src account
      if (!tx.getOutgoingTransfer() || !tx.getIncomingTransfers()) continue;
      for (let transfer of tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() === tx.getOutgoingTransfer().getAccountIndex()) continue;
        
        // fetch tx with filtering
        let filteredTxs = await wallet.getTxs({transferFilter: {isIncoming: true, accountIndex: transfer.getAccountIndex()}});
        let filteredTx = new MoneroTxFilter().setTxIds([tx.getId()]).apply(filteredTxs)[0];
        
        // txs should be the same (mergeable)
        assert.equal(filteredTx.getId(), tx.getId());
        tx.merge(filteredTx);
        
        // test is done
        return;
      }
    }
    
    // test did not fully execute
    throw new Error("Test requires tx sent from/to different accounts of same wallet but none found; run send tests");
  });
  
  if (!liteMode)
  it("Validates inputs when getting transactions", async function() {
    
    // test with invalid id
    let txs = await wallet.getTxs({txId: "invalid_id"});
    assert.equal(txs.length, 0);
    
    // test invalid id in collection
    let randomTxs = await getRandomTransactions(wallet, undefined, 3, 5);
    txs = await wallet.getTxs({txIds: [randomTxs[0].getId(), "invalid_id"]});
    assert.equal(txs.length, 1);
    assert.equal(txs[0].getId(), randomTxs[0].getId());
    
    // TODO: test other input validation here
  });

  it("Can get transfers in the wallet, accounts, and subaddresses", async function() {
    
    // get all transfers
    await testGetTransfers(wallet, undefined, true);
    
    // get transfers by account index
    let nonDefaultIncoming = false;
    for (let account of await wallet.getAccounts(true)) {
      let accountTransfers = await testGetTransfers(wallet, {accountIndex: account.getIndex()});
      for (let transfer of accountTransfers) assert.equal(transfer.getAccountIndex(), account.getIndex());
      
      // get transfers by subaddress index
      let subaddressTransfers = [];
      for (let subaddress of account.getSubaddresses()) {
        let transfers = await testGetTransfers(wallet, {accountIndex: subaddress.getAccountIndex(), subaddressIndex: subaddress.getIndex()});
        for (let transfer of transfers) {
          assert.equal(transfer.getAccountIndex(), subaddress.getAccountIndex());
          assert.equal(transfer.getSubaddressIndex(), transfer.getIsOutgoing() ? 0 : subaddress.getIndex());
          if (transfer.getAccountIndex() !== 0 && transfer.getSubaddressIndex() !== 0) nonDefaultIncoming = true;
          
          // don't add duplicates TODO monero-wallet-rpc: duplicate outgoing transfers returned for different subaddress indices, way to return outgoing subaddress indices?
          let found = false;
          for (let subaddressTransfer of subaddressTransfers) {
            if (transfer.toString() === subaddressTransfer.toString() && transfer.getTx().getId() === subaddressTransfer.getTx().getId()) {
              found = true;
              break;
            }
          }
          if (!found) subaddressTransfers.push(transfer);
        }
      }
      assert.equal(subaddressTransfers.length, accountTransfers.length);
      
      // get transfers by subaddress indices
      let subaddressIndices = Array.from(new Set(subaddressTransfers.map(transfer => transfer.getSubaddressIndex())));
      let transfers = await testGetTransfers(wallet, {accountIndex: account.getIndex(), subaddressIndices: subaddressIndices});
      if (transfers.length !== subaddressTransfers.length) console.log("WARNING: outgoing transfers always from subaddress 0 (monero-wallet-rpc #5171)");
      //assert.equal(transfers.length, subaddressTransfers.length); // TODO monero-wallet-rpc: these may not be equal because outgoing transfers are always from subaddress 0 (#5171) and/or incoming transfers from/to same account are occluded (#4500)
      for (let transfer of transfers) {
        assert.equal(transfer.getAccountIndex(), account.getIndex());
        assert(subaddressIndices.includes(transfer.getSubaddressIndex()));
      }
    }
    
    // ensure transfer found with non-zero account and subaddress indices
    assert(nonDefaultIncoming, "No transfers found in non-default account and subaddress; run send-to-multiple tests");
  });
  
  if (!liteMode)
  it("Can get transfers with additional configuration", async function() {
    
    // get incoming transfers
    let transfers = await testGetTransfers(wallet, {isIncoming: true}, true);
    for (let transfer of transfers) assert(transfer.getIsIncoming());
    
    // get outgoing transfers
    transfers = await testGetTransfers(wallet, {isOutgoing: true}, true);
    for (let transfer of transfers) assert(transfer.getIsOutgoing());
    
    // get confirmed transfers to account 0
    transfers = await testGetTransfers(wallet, {accountIndex: 0, isConfirmed: true}, true);
    for (let transfer of transfers) {
      assert.equal(transfer.getAccountIndex(), 0);
      assert(transfer.getTx().getIsConfirmed());
    }
    
    // get confirmed transfers to [1, 2]
    transfers = await testGetTransfers(wallet, {accountIndex: 1, subaddressIndex: 2, isConfirmed: true}, true);
    for (let transfer of transfers) {
      assert.equal(transfer.getAccountIndex(), 1);
      assert.equal(transfer.getSubaddressIndex(), transfer.getIsOutgoing() ? 0 : 2);
      assert(transfer.getTx().getIsConfirmed());
    }
    
    // get transfers in the tx pool
    transfers = await testGetTransfers(wallet, {inTxPool: true});
    for (let transfer of transfers) {
      assert.equal(transfer.getTx().getInTxPool(), true);
    }
    
    // get random transactions
    let txs = await getRandomTransactions(wallet, undefined, 3, 5);
    
    // get transfers with a tx id
    let txIds = [];
    for (let tx of txs) {
      txIds.push(tx.getId());
      transfers = await testGetTransfers(wallet, {txId: tx.getId()}, true);
      for (let transfer of transfers) assert.equal(transfer.getTx().getId(), tx.getId());
    }
    
    // get transfers with tx ids
    transfers = await testGetTransfers(wallet, {txIds: txIds}, true);
    for (let transfer of transfers) assert(txIds.includes(transfer.getTx().getId()));
    
    // TODO: test that transfers with the same txId have the same tx reference
    
    // TODO: test transfers destinations
    
    // get transfers with pre-built filter that are confirmed and have outgoing destinations
    let transferFilter = new MoneroTransferFilter();
    transferFilter.setIsOutgoing(true);
    transferFilter.setHasDestinations(true);
    transferFilter.setTxFilter(new MoneroTxFilter().setTx(new MoneroTx().setIsConfirmed(true)));
    transfers = await testGetTransfers(wallet, transferFilter);
    for (let transfer of transfers) {
      assert.equal(transfer.getIsOutgoing(), true);
      assert(transfer.getDestinations().length > 0);
      assert.equal(transfer.getTx().getIsConfirmed(), true);
    }
  });
  
  if (!liteMode)
  it("Validates inputs when getting transfers", async function() {
    
    // test with invalid id
    let transfers = await wallet.getTransfers({txId: "invalid_id"});
    assert.equal(transfers.length, 0);
    
    // test invalid id in collection
    let randomTxs = await getRandomTransactions(wallet, undefined, 3, 5);
    transfers = await wallet.getTransfers({txIds: [randomTxs[0].getId(), "invalid_id"]});
    assert(transfers.length > 0);
    let tx = transfers[0].getTx();
    for (let transfer of transfers) assert(tx === transfer.getTx());
    
    // test unused subaddress indices
    transfers = await wallet.getTransfers({accountIndex: 0, subaddressIndices: [1234907]});
    assert(transfers.length === 0);
    
    // test invalid subaddress index
    try {
      let transfers = await wallet.getTransfers({accountIndex: 0, subaddressIndex: -10});
      throw new Error("Should have failed");
    } catch (e) {
      assert.notEqual(e.message, "Should have failed");
    }
  });
  
  it("Can get vouts in the wallet, accounts, and subaddresses", async function() {

    // get all vouts
    await testGetVouts(wallet, undefined, true);
    
    // get vouts for each account
    let nonDefaultIncoming = false;
    let accounts = await wallet.getAccounts(true);
    for (let account of accounts) {
      
      // determine if account is used
      let isUsed = false;
      for (let subaddress of account.getSubaddresses()) if (subaddress.getIsUsed()) isUsed = true;
      
      // get vouts by account index
      let accountVouts = await testGetVouts(wallet, {accountIndex: account.getIndex()}, isUsed);
      for (let vout of accountVouts) assert.equal(vout.getAccountIndex(), account.getIndex());
      
      // get vouts by subaddress index
      let subaddressVouts = [];
      for (let subaddress of account.getSubaddresses()) {
        let vouts = await testGetVouts(wallet, {accountIndex: account.getIndex(), subaddressIndex: subaddress.getIndex()}, subaddress.getIsUsed());
        for (let vout of vouts) {
          assert.equal(vout.getAccountIndex(), subaddress.getAccountIndex());
          assert.equal(vout.getSubaddressIndex(), subaddress.getIndex());
          if (vout.getAccountIndex() !== 0 && vout.getSubaddressIndex() !== 0) nonDefaultIncoming = true;
          subaddressVouts.push(vout);
        }
      }
      assert.equal(subaddressVouts.length, accountVouts.length);
      
      // get vouts by subaddress indices
      let subaddressIndices = Array.from(new Set(subaddressVouts.map(vout => vout.getSubaddressIndex())));
      let vouts = await testGetVouts(wallet, {accountIndex: account.getIndex(), subaddressIndices: subaddressIndices}, isUsed);
      assert.equal(vouts.length, subaddressVouts.length);
      for (let vout of vouts) {
        assert.equal(vout.getAccountIndex(), account.getIndex());
        assert(subaddressIndices.includes(vout.getSubaddressIndex()));
      }
    }
    
    // ensure vout found with non-zero account and subaddress indices
    assert(nonDefaultIncoming, "No vouts found in non-default account and subaddress; run send-to-multiple tests");
  });
  
  if (!liteMode)
  it("Can get vouts with additional configuration", async function() {
    
    // get unspent vouts to account 0
    let vouts = await testGetVouts(wallet, {accountIndex: 0, isSpent: false});
    for (let vout of vouts) {
      assert.equal(vout.getAccountIndex(), 0);
      assert.equal(vout.getIsSpent(), false);
    }
    
    // get spent vouts to account 1
    vouts = await testGetVouts(wallet, {accountIndex: 1, isSpent: true}, true);
    for (let vout of vouts) {
      assert.equal(vout.getAccountIndex(), 1);
      assert.equal(vout.getIsSpent(), true);
    }
    
    // get random transactions
    let txs = await getRandomTransactions(wallet, {isConfirmed: true}, 3, 5);
    
    // get vouts with a tx id
    let txIds = [];
    for (let tx of txs) {
      txIds.push(tx.getId());
      vouts = await testGetVouts(wallet, {txId: tx.getId()}, true);
      for (let vout of vouts) assert.equal(vout.getTx().getId(), tx.getId());
    }
    
    // get vouts with tx ids
    vouts = await testGetVouts(wallet, {txIds: txIds}, true);
    for (let vout of vouts) assert(txIds.includes(vout.getTx().getId()));
    
    // get confirmed vouts to specific subaddress with pre-built filter
    let accountIdx = 0;
    let subaddressIdx = 1;
    let voutFilter = new MoneroVoutFilter();
    voutFilter.setVout(new MoneroWalletOutput().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx));
    voutFilter.setTxFilter(new MoneroTxFilter().setTx(new MoneroTx().setIsConfirmed(true)));
    vouts = await testGetVouts(wallet, voutFilter, true);
    for (let vout of vouts) {
      assert.equal(vout.getAccountIndex(), accountIdx);
      assert.equal(vout.getSubaddressIndex(), subaddressIdx);
      assert.equal(vout.getTx().getIsConfirmed(), true);
    }
  });
  
  if (!liteMode)
  it("Validates inputs when getting vouts", async function() {
    
    // test with invalid id
    let vouts = await wallet.getVouts({txId: "invalid_id"});
    assert.equal(vouts.length, 0);
    
    // test invalid id in collection
    let randomTxs = await getRandomTransactions(wallet, {isConfirmed: true, getVouts: true}, 3, 5);
    vouts = await wallet.getVouts({txIds: [randomTxs[0].getId(), "invalid_id"]});
    assert.equal(randomTxs[0].getVouts().length, vouts.length);
    let tx = vouts[0].getTx();
    for (let vout of vouts) assert(tx === vout.getTx());
  });
  
  it("Has correct accounting across accounts, subaddresses, txs, transfers, and vouts", async function() {
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    let walletBalance = await wallet.getBalance();
    let walletUnlockedBalance = await wallet.getUnlockedBalance();
    let accounts = await wallet.getAccounts(true);  // includes subaddresses
    let txs = await wallet.getTxs();
    
    // sort txs
    txs.sort((a, b) => {
      let timestampA = a.getIsConfirmed() ? a.getBlock().getHeader().getTimestamp() : a.getReceivedTimestamp();
      let timestampB = b.getIsConfirmed() ? b.getBlock().getHeader().getTimestamp() : b.getReceivedTimestamp();
      if (timestampA < timestampB) return -1;
      if (timestampA > timestampB) return 1;
      return 0;
    })
    
    // test wallet balance
    TestUtils.testUnsignedBigInteger(walletBalance);
    TestUtils.testUnsignedBigInteger(walletUnlockedBalance);
    assert(walletBalance >= walletUnlockedBalance);
    
    // test that wallet balance equals sum of account balances
    let accountsBalance = new BigInteger(0);
    let accountsUnlockedBalance = new BigInteger(0);
    for (let account of accounts) {
      testAccount(account); // test that account balance equals sum of subaddress balances
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
    }
    assert.equal(walletBalance.compare(accountsBalance), 0);
    assert.equal(walletUnlockedBalance.compare(accountsUnlockedBalance), 0);
    
//    // test that wallet balance equals net of wallet's incoming and outgoing tx amounts
//    // TODO monero-wallet-rpc: these tests are disabled because incoming transfers are not returned when sent from the same account, so doesn't balance #4500
//    // TODO: test unlocked balance based on txs, requires e.g. tx.isLocked()
//    let outgoingSum = new BigInteger(0);
//    let incomingSum = new BigInteger(0);
//    for (let tx of txs) {
//      if (tx.getOutgoingAmount()) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//      if (tx.getIncomingAmount()) incomingSum = incomingSum.add(tx.getIncomingAmount());
//    }
//    assert.equal(incomingSum.subtract(outgoingSum).toString(), walletBalance.toString());
//    
//    // test that each account's balance equals net of account's incoming and outgoing tx amounts
//    for (let account of accounts) {
//      if (account.getIndex() !== 1) continue; // find 1
//      outgoingSum = new BigInteger(0);
//      incomingSum = new BigInteger(0);
//      let filter = new MoneroTxFilter();
//      filter.setAccountIndex(account.getIndex());
//      for (let tx of txs.filter(tx => filter.meetsCriteria(tx))) { // normally we'd call wallet.getTxs(filter) but we're using pre-fetched txs
//        if (tx.getId() === "8d3919d98dd5a734da8c52eddc558db3fbf059ad55d432f0052ecd59ef122ecb") console.log(tx.toString(0));
//        
//        //console.log((tx.getOutgoingAmount() ? tx.getOutgoingAmount().toString() : "") + ", " + (tx.getIncomingAmount() ? tx.getIncomingAmount().toString() : ""));
//        if (tx.getOutgoingAmount()) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//        if (tx.getIncomingAmount()) incomingSum = incomingSum.add(tx.getIncomingAmount());
//      }
//      assert.equal(incomingSum.subtract(outgoingSum).toString(), account.getBalance().toString());
//    }
    
    // balance may not equal sum of unspent vouts if if unconfirmed txs
    // TODO monero-wallet-rpc: reason not to return unspent vouts on unconfirmed txs? then this isn't necessary
    let hasUnconfirmedTx = false;
    for (let tx of txs) if (tx.getInTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent vouts
    let walletSum = new BigInteger(0);
    for (let vout of await wallet.getVouts({isSpent: false})) walletSum = walletSum.add(vout.getAmount());
    if (walletBalance.toString() !== walletSum.toString()) assert(hasUnconfirmedTx, "Wallet balance must equal sum of unspent vouts if no unconfirmed txs");
    
    // account balances are sum of their unspent vouts
    for (let account of accounts) {
      let accountSum = new BigInteger(0);
      let accountVouts = await wallet.getVouts({accountIndex: account.getIndex(), isSpent: false});
      for (let vout of accountVouts) accountSum = accountSum.add(vout.getAmount());
      if (account.getBalance().toString() !== accountSum.toString()) assert(hasUnconfirmedTx, "Account balance must equal sum of its unspent vouts if no unconfirmed txs");
      
      // subaddress balances are sum of their unspent vouts
      for (let subaddress of account.getSubaddresses()) {
        let subaddressSum = new BigInteger(0);
        let subaddressVouts = await wallet.getVouts({accountIndex: account.getIndex(), subaddressIndex: subaddress.getIndex(), isSpent: false});
        for (let vout of subaddressVouts) subaddressSum = subaddressSum.add(vout.getAmount());
        if (subaddress.getBalance().toString() !== subaddressSum.toString()) assert(hasUnconfirmedTx, "Subaddress balance must equal sum of its unspent vouts if no unconfirmed txs");
      }
    }
  });
  
  it("Can get and set a transaction note", async function() {
    let txs = await getRandomTransactions(wallet, undefined, 1, 5);
    
    // set notes
    let uuid = GenUtils.uuidv4();
    for (let i = 0; i < txs.length; i++) {
      await wallet.setTxNote(txs[i].getId(), uuid + i); // TODO: can we not iterate over awaits?
    }
    
    // get notes
    for (let i = 0; i < txs.length; i++) {
      assert.equal(await wallet.getTxNote(txs[i].getId()), uuid + i);
    }
  });
  
  // TODO: why does getting cached txs take 2 seconds when should already be cached?
  it("Can get and set multiple transaction notes", async function() {
    
    // set tx notes
    let uuid = GenUtils.uuidv4();
    let txs = await getCachedTxs();
    assert(txs.length >= 3, "Test requires 3 or more wallet transactions; run send tests");
    let txIds = [];
    let txNotes = [];
    for (let i = 0; i < txIds.length; i++) {
      txIds.push(txs[i].getId());
      txNotes.push(uuid + i);
    }
    await wallet.setTxNotes(txIds, txNotes);
    
    // get tx notes
    txNotes = await wallet.getTxNotes(txIds);
    for (let i = 0; i < txIds.length; i++) {
      assert.equal(uuid + i, txNotes[i]);
    }
    
    // TODO: test that get transaction has note
  });
  
  it("Can check a transfer using the transaction's secret key and the destination", async function() {
    
    // get random txs that are confirmed and have outgoing destinations
    let txs;
    try {
      txs = await getRandomTransactions(wallet, {isConfirmed: true, hasOutgoingTransfer: true, transferFilter: {hasDestinations: true}}, 1, MAX_TX_PROOFS);
    } catch (e) {
      throw new Error("No txs with outgoing destinations found; run send tests")
    }
    
    // test good checks
    assert(txs.length > 0, "No transactions found with outgoing destinations");
    for (let tx of txs) {
      let key = await wallet.getTxKey(tx.getId());
      for (let destination of tx.getOutgoingTransfer().getDestinations()) {
        let check = await wallet.checkTxKey(tx.getId(), key, destination.getAddress());
        if (destination.getAmount().compare(new BigInteger()) > 0) {
          // TODO monero-wallet-rpc: indicates amount received amount is 0 despite transaction with transfer to this address
          // TODO monero-wallet-rpc: returns 0-4 errors, not consistent
//        assert(check.getReceivedAmount().compare(new BigInteger(0)) > 0);
          if (check.getReceivedAmount().compare(new BigInteger(0)) === 0) {
            console.log("WARNING: key proof indicates no funds received despite transfer (txid=" + tx.getId() + ", key=" + key + ", address=" + destination.getAddress() + ", amount=" + destination.getAmount() + ")");
          }
        }
        else assert(check.getReceivedAmount().compare(new BigInteger(0)) === 0);
        testCheckTx(tx, check);
      }
    }
    
    // test get tx key with invalid id
    try {
      await wallet.getTxKey("invalid_tx_id");
      throw new Error("Should throw exception for invalid key");
    } catch (e) {
      assert.equal(e.getRpcCode(), -8);
    }
    
    // test check with invalid tx id
    let tx = txs[0];
    let key = await wallet.getTxKey(tx.getId());
    let destination = tx.getOutgoingTransfer().getDestinations()[0];
    try {
      await wallet.checkTxKey("invalid_tx_id", key, destination.getAddress());
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -8);
    }
    
    // test check with invalid key
    try {
      await wallet.checkTxKey(tx.getId(), "invalid_tx_key", destination.getAddress());
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -25);
    }
    
    // test check with invalid address
    try {
      await wallet.checkTxKey(tx.getId(), key, "invalid_tx_address");
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -2);
    }
    
    // test check with different address
    let differentAddress;
    for (let aTx of await getCachedTxs()) {
      if (!aTx.getOutgoingTransfer() || !aTx.getOutgoingTransfer().getDestinations()) continue;
      for (let aDestination of aTx.getOutgoingTransfer().getDestinations()) {
        if (aDestination.getAddress() !== destination.getAddress()) {
          differentAddress = aDestination.getAddress();
          break;
        }
      }
    }
    assert(differentAddress, "Could not get a different address to test");
    let check = await wallet.checkTxKey(tx.getId(), key, differentAddress);
    assert(check.getIsGood());
    assert(check.getReceivedAmount().compare(new BigInteger(0)) >= 0);
    testCheckTx(tx, check);
  });
  
  it("Can prove a transaction by getting its signature", async function() {
    
    // get random txs that are confirmed and have outgoing destinations
    let txs;
    try {
      txs = await getRandomTransactions(wallet, {isConfirmed: true, hasOutgoingTransfer: true, transferFilter: {hasDestinations: true}}, 1, MAX_TX_PROOFS);
    } catch (e) {
      throw new Error("No txs with outgoing destinations found; run send tests")
    }
    
    // test good checks with messages
    for (let tx of txs) {
      for (let destination of tx.getOutgoingTransfer().getDestinations()) {
        let signature = await wallet.getTxProof(tx.getId(), destination.getAddress(), "This transaction definitely happened.");
        let check = await wallet.checkTxProof(tx.getId(), destination.getAddress(), "This transaction definitely happened.", signature);
        testCheckTx(tx, check);
      }
    }
    
    // test good check without message
    let tx = txs[0];
    let destination = tx.getOutgoingTransfer().getDestinations()[0];
    let signature = await wallet.getTxProof(tx.getId(), destination.getAddress());
    let check = await wallet.checkTxProof(tx.getId(), destination.getAddress(), undefined, signature);
    testCheckTx(tx, check);
    
    // test get proof with invalid id
    try {
      await wallet.getTxProof("invalid_tx_id", destination.getAddress());
      throw new Error("Should throw exception for invalid key");
    } catch (e) {
      assert.equal(e.getRpcCode(), -8);
    }
    
    // test check with invalid tx id
    try {
      await wallet.checkTxProof("invalid_tx_id", destination.getAddress(), undefined, signature);
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -8);
    }
    
    // test check with invalid address
    try {
      await wallet.checkTxProof(tx.getId(), "invalid_tx_address", undefined, signature);
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -2);
    }
    
    // test check with wrong message
    signature = await wallet.getTxProof(tx.getId(), destination.getAddress(), "This is the right message");
    check = await wallet.checkTxProof(tx.getId(), destination.getAddress(), "This is the wrong message", signature);
    assert.equal(check.getIsGood(), false);
    testCheckTx(tx, check);
    
    // test check with wrong signature
    let wrongSignature = await wallet.getTxProof(txs[1].getId(), txs[1].getOutgoingTransfer().getDestinations()[0].getAddress(), "This is the right message");
    try {
      check = await wallet.checkTxProof(tx.getId(), destination.getAddress(), "This is the right message", wrongSignature);  
      assert.equal(check.getIsGood(), false);
    } catch (e) {
      assert.equal(e.getRpcCode(), -1); // TODO: sometimes comes back bad, sometimes throws exception.  ensure txs come from different addresses?
    }
  });
  
  it("Can prove a spend using a generated signature and no destination public address", async function() {
    
    // get random confirmed outgoing txs
    let filter = new MoneroTxFilter();
    let txs = await getRandomTransactions(wallet, {hasIncomingTransfers: false, inTxPool: false, isFailed: false}, 2, MAX_TX_PROOFS);
    for (let tx of txs) {
      assert.equal(tx.getIsConfirmed(), true);
      assert.equal(tx.getIncomingTransfers(), undefined);
      assert(tx.getOutgoingTransfer());
    }
    
    // test good checks with messages
    for (let tx of txs) {
      let signature = await wallet.getSpendProof(tx.getId(), "I am a message.");
      assert(await wallet.checkSpendProof(tx.getId(), "I am a message.", signature));
    }
    
    // test good check without message
    let tx = txs[0];
    let signature = await wallet.getSpendProof(tx.getId());
    assert(await wallet.checkSpendProof(tx.getId(), undefined, signature));
    
    // test get proof with invalid id
    try {
      await wallet.getSpendProof("invalid_tx_id");
      throw new Error("Should throw exception for invalid key");
    } catch (e) {
      assert.equal(e.getRpcCode(), -8);
    }
    
    // test check with invalid tx id
    try {
      await wallet.checkSpendProof("invalid_tx_id", undefined, signature);
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -8);
    }
    
    // test check with invalid message
    signature = await wallet.getSpendProof(tx.getId(), "This is the right message");
    assert.equal(await wallet.checkSpendProof(tx.getId(), "This is the wrong message", signature), false);
    
    // test check with wrong signature
    signature = await wallet.getSpendProof(txs[1].getId(), "This is the right message");
    assert.equal(await wallet.checkSpendProof(tx.getId(), "This is the right message", signature), false);
  });
  
  it("Can prove reserves in the wallet", async function() {
    
    // get proof of entire wallet
    let signature = await wallet.getReserveProofWallet("Test message");
    
    // check proof of entire wallet
    let check = await wallet.checkReserveProof(await wallet.getPrimaryAddress(), "Test message", signature);
    assert(check.getIsGood());
    testCheckReserve(check);
    let balance = await wallet.getBalance();
    if (balance.compare(check.getTotalAmount()) !== 0) {  // TODO monero-wallet-rpc: this check fails with unconfirmed txs
      let unconfirmedTxs = await wallet.getTxs({inTxPool: true});
      assert(unconfirmedTxs.length > 0, "Reserve amount must equal balance unless wallet has unconfirmed txs");
    }
    
    // test different wallet address
    // TODO: openWallet is not common so this won't work for other wallet impls
    await wallet.openWallet(TestUtils.WALLET_RPC_NAME_2, TestUtils.WALLET_RPC_PW_2);
    let differentAddress = await wallet.getPrimaryAddress();
    await wallet.openWallet(TestUtils.WALLET_RPC_NAME_1, TestUtils.WALLET_RPC_PW_1);
    try {
      await wallet.checkReserveProof(differentAddress, "Test message", signature);
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -1);
    }
    
    // test subaddress
    try {
      
      await wallet.checkReserveProof((await wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -1);
    }
    
    // test wrong message
    check = await wallet.checkReserveProof(await wallet.getPrimaryAddress(), "Wrong message", signature);
    assert.equal(check.getIsGood(), false);  // TODO: specifically test reserve checks, probably separate objects
    testCheckReserve(check);
    
    // test wrong signature
    try {
      await wallet.checkReserveProof(await wallet.getPrimaryAddress(), "Test message", "wrong signature");
      throw new Error("Should have thrown exception");
    } catch (e) {
      assert.equal(e.getRpcCode(), -1);
    }
  });
  
  // TODO: re-enable this after 14.x point release which fixes this
//  it("Can prove reserves in an account", async function() {
//    
//    // test proofs of accounts
//    let numNonZeroTests = 0;
//    let msg = "Test message";
//    let accounts = await wallet.getAccounts();
//    let signature;
//    for (let account of accounts) {
//      if (account.getBalance().compare(new BigInteger(0)) > 0) {
//        let checkAmount = (await account.getBalance()).divide(new BigInteger(2));
//        signature = await wallet.getReserveProofAccount(account.getIndex(), checkAmount, msg);
//        let check = await wallet.checkReserveProof(await wallet.getPrimaryAddress(), msg, signature);
//        assert(check.getIsGood());
//        testCheckReserve(check);
//        assert(check.getTotalAmount().compare(checkAmount) >= 0);
//        numNonZeroTests++;
//      } else {
//        try {
//          await wallet.getReserveProofAccount(account.getIndex(), account.getBalance(), msg);
//          throw new Error("Should have thrown exception");
//        } catch (e) {
//          assert.equal(e.getRpcCode(), -1);
//          try {
//            await wallet.getReserveProofAccount(account.getIndex(), TestUtils.MAX_FEE, msg);
//            throw new Error("Should have thrown exception");
//          } catch (e2) {
//            assert.equal(e2.getRpcCode(), -1);
//          }
//        }
//      }
//    }
//    assert(numNonZeroTests > 1, "Must have more than one account with non-zero balance; run send-to-multiple tests");
//    
//    // test error when not enough balance for requested minimum reserve amount
//    try {
//      await wallet.getReserveProofAccount(0, accounts[0].getBalance().add(TestUtils.MAX_FEE), "Test message");
//      throw new Error("Should have thrown exception");
//    } catch (e) {
//      assert.equal(e.getRpcCode(), -1);
//    }
//    
//    // test different wallet address
//    // TODO: openWallet is not common so this won't work for other wallet impls
//    await wallet.openWallet(TestUtils.WALLET_RPC_NAME_2, TestUtils.WALLET_RPC_PW_2);
//    let differentAddress = await wallet.getPrimaryAddress();
//    await wallet.openWallet(TestUtils.WALLET_RPC_NAME_1, TestUtils.WALLET_RPC_PW_1);
//    try {
//      await wallet.checkReserveProof(differentAddress, "Test message", signature);
//      throw new Error("Should have thrown exception");
//    } catch (e) {
//      assert.equal(e.getRpcCode(), -1);
//    }
//    
//    // test subaddress
//    try {
//      await wallet.checkReserveProof((await wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
//      throw new Error("Should have thrown exception");
//    } catch (e) {
//      assert.equal(e.getRpcCode(), -1);
//    }
//    
//    // test wrong message
//    let check = await wallet.checkReserveProof(await wallet.getPrimaryAddress(), "Wrong message", signature);
//    assert.equal(check.getIsGood(), false); // TODO: specifically test reserve checks, probably separate objects
//    testCheckReserve(check);
//    
//    // test wrong signature
//    try {
//      await wallet.checkReserveProof(await wallet.getPrimaryAddress(), "Test message", "wrong signature");
//      throw new Error("Should have thrown exception");
//    } catch (e) {
//      assert.equal(e.getRpcCode(), -1);
//    }
//  });
  
  it("Can get outputs in hex format", async function() {
    let outputsHex = await wallet.getOutputsHex();
    assert.equal(typeof outputsHex, "string");  // TODO: this will fail if wallet has no outputs; run these tests on new wallet
    assert(outputsHex.length > 0);
  });
  
  it("Can import outputs in hex format", async function() {
    
    // get outputs hex
    let outputsHex = await wallet.getOutputsHex();
    
    // import outputs hex
    if (outputsHex !== undefined) {
      let numImported = await wallet.importOutputsHex(outputsHex);
      assert(numImported > 0);
    }
  });
  
  it("Can get signed key images", async function() {
    let images = await wallet.getKeyImages();
    assert(Array.isArray(images));
    assert(images.length > 0, "No signed key images in wallet");
    for (let image of images) {
      assert(image instanceof MoneroKeyImage);
      assert(image.getHex());
      assert(image.getSignature());
    }
  });
  
  it("Can get new key images from the last import", async function() {
    
    // get outputs hex
    let outputsHex = await wallet.getOutputsHex();
    
    // import outputs hex
    if (outputsHex !== undefined) {
      let numImported = await wallet.importOutputsHex(outputsHex);
      assert(numImported > 0);
    }
    
    // get and test new key images from last import
    let images = await wallet.getNewKeyImagesFromLastImport();
    assert(Array.isArray(images));
    assert(images.length > 0, "No new key images in last import");  // TODO: these are already known to the wallet, so no new key images will be imported
    for (let image of images) {
      assert(image.getHex());
      assert(image.getSignature());
    }
  });
  
  it("Can import key images", async function() {
    let images = await wallet.getKeyImages();
    assert(Array.isArray(images));
    assert(images.length > 0, "Wallet does not have any key images; run send tests");
    let result = await wallet.importKeyImages(images);
    assert(result.getHeight() > 0);
    
    // determine if non-zero spent and unspent amounts are expected
    let txs = await wallet.getTxs({isConfirmed: true, transferFilter: {isOutgoing: true}});
    let balance = await wallet.getBalance();
    let hasSpent = txs.length > 0;
    let hasUnspent = balance.toJSValue() > 0;
    
    // test amounts
    TestUtils.testUnsignedBigInteger(result.getSpentAmount(), hasSpent);
    TestUtils.testUnsignedBigInteger(result.getUnspentAmount(), hasUnspent);
  });
  
  it("Can sign and verify messages", async function() {
    let msg = "This is a super important message which needs to be signed and verified.";
    let signature = await wallet.sign(msg);
    let verified = await wallet.verify(msg, await wallet.getAddress(0, 0), signature);
    assert.equal(verified, true);
    verified = await wallet.verify(msg, TestMoneroWalletCommon.SAMPLE_ADDRESS, signature);
    assert.equal(verified, false);
  });
  
  it("Can get and set arbitrary key/value attributes", async function() {
    
    // set attributes
    let attrs = {};
    for (let i = 0; i < 5; i++) {
      let key = "attr" + i;
      let val = GenUtils.uuidv4();
      attrs[key] = val;
      await wallet.setAttribute(key, val);
    }
    
    // test attributes
    for (let key of Object.keys(attrs)) {
      assert.equal(attrs[key], await wallet.getAttribute(key));
    }
  });
  
  it("Can convert between a tx send config and payment URI", async function() {
    
    // test with address and amount
    let config1 = new MoneroSendConfig(await wallet.getAddress(0, 0), new BigInteger(0));
    let uri = await wallet.createPaymentUri(config1);
    let config2 = await wallet.parsePaymentUri(uri);
    GenUtils.deleteUndefinedKeys(config1);
    GenUtils.deleteUndefinedKeys(config2);
    assert.deepEqual(JSON.parse(JSON.stringify(config2)), JSON.parse(JSON.stringify(config1)));
    
    // test with all fields3
    config1.getDestinations()[0].setAmount(new BigInteger("425000000000"));
    config1.setPaymentId("03284e41c342f036");
    config1.setRecipientName("John Doe");
    config1.setNote("OMZG XMR FTW");
    uri = await wallet.createPaymentUri(config1);
    config2 = await wallet.parsePaymentUri(uri);
    GenUtils.deleteUndefinedKeys(config1);
    GenUtils.deleteUndefinedKeys(config2);
    assert.deepEqual(config2, config1);
    
    // test with undefined address
    let address = config1.getDestinations()[0].getAddress();
    config1.getDestinations()[0].setAddress(undefined);
    try {
      await wallet.createPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (e) {
      assert.equal(e.getRpcCode(), -11);
      assert(e.getRpcMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
    config1.getDestinations()[0].setAddress(address);
    
    // test with invalid payment id
    config1.setPaymentId("bizzup");
    try {
      await wallet.createPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (e) {
      assert.equal(e.getRpcCode(), -11);
      assert(e.getRpcMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
  });
  
  it("Can start and stop mining", async function() {
    await wallet.startMining(2, false, true);
    await wallet.stopMining();
  });
  
  // --------------------------------- PRIVATE --------------------------------
  
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
}
