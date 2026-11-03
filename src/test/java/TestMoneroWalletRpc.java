

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;
import monero.common.MoneroError;
import monero.common.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import utils.TestUtils;

/**
 * Tests monero-wallet-rpc non-relaying calls.
 */
@TestInstance(Lifecycle.PER_CLASS) // so @BeforeAll and @AfterAll can be used on non-static functions
public class TestMoneroWalletRpc extends TestMoneroWalletCommon {
  
  public TestMoneroWalletRpc() {
    super();
  }
  
  @Override
  @BeforeAll
  public void beforeAll() {
    super.beforeAll();
    
    // if full tests ran, wait for full wallet's pool txs to confirm
    if (TestMoneroWalletFull.FULL_TESTS_RUN) {
      MoneroWallet walletFull = TestUtils.getWalletFull();
      TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(walletFull);
      walletFull.close(true);
    }
  }
  
  @Override
  @AfterAll
  public void afterAll() {
    super.afterAll();
    for (MoneroWalletRpc walletRpc : new HashSet<MoneroWalletRpc>(TestUtils.WALLET_PORT_OFFSETS.keySet())) {
      System.err.println("WARNING: Wallet RPC process was not stopped after all tests, stopping");
      try { TestUtils.stopWalletRpcProcess(walletRpc); }
      catch (Exception e) { throw new RuntimeException(e); }
    }
  }
  
  @Override
  @AfterEach
  public void afterEach(TestInfo testInfo) {
    super.afterEach(testInfo);
    for (MoneroWalletRpc walletRpc : new HashSet<MoneroWalletRpc>(TestUtils.WALLET_PORT_OFFSETS.keySet())) {
      System.err.println("WARNING: Wallet RPC process was not stopped after test " + testInfo.getDisplayName() + ", stopping");
      try { TestUtils.stopWalletRpcProcess(walletRpc); }
      catch (Exception e) { throw new RuntimeException(e); }
    }
  }

  @Override
  protected MoneroWallet getTestWallet() {
    return TestUtils.getWalletRpc();
  }
  
  @Override
  protected MoneroWalletRpc openWallet(MoneroWalletConfig config) {
    return TestUtils.openWalletRpc(config);
  }
  
  @Override
  protected MoneroWalletRpc createWallet(MoneroWalletConfig config) {
    return TestUtils.createWalletRpc(config);
  }
  
  @Override
  public void closeWallet(MoneroWallet wallet, boolean save) {
    MoneroWalletRpc walletRpc = (MoneroWalletRpc) wallet;
    walletRpc.close(save);
    try {
      TestUtils.stopWalletRpcProcess(walletRpc);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
  
  @Override
  protected List<String> getSeedLanguages() {
    return ((MoneroWalletRpc) wallet).getSeedLanguages();
  }
  
  // ---------------------------- BEGIN TESTS ---------------------------------
  
  // Can create a wallet with a randomly generated seed
  @Test
  @Tag("CI")
  public void testCreateWalletRandomRpc() {
    assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = UUID.randomUUID().toString();
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPath(path));
    String seed = wallet.getSeed();
    MoneroUtils.validateMnemonic(seed);
    assertNotEquals(TestUtils.SEED, seed);
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    wallet.sync();  // very quick because restore height is chain height
    closeWallet(wallet);

    // create random wallet with non defaults
    path = UUID.randomUUID().toString();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setLanguage("Spanish"));
    MoneroUtils.validateMnemonic(wallet.getSeed());
    assertNotEquals(seed, wallet.getSeed());
    seed = wallet.getSeed();
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    
    // attempt to create wallet which already exists
    try {
      createWallet(new MoneroWalletConfig().setPath(path).setLanguage("Spanish"));
    } catch (MoneroError e) {
      assertEquals(e.getMessage(), "Wallet already exists: " + path);
      assertEquals(-21, (int) e.getCode());
      assertEquals(seed, wallet.getSeed());
    }
    closeWallet(wallet);
  }
  
  // Can create a RPC wallet from a seed
  @Test
  @Tag("CI")
  public void testCreateWalletFromSeedRpc() {
    assumeTrue(TEST_NON_RELAYS);
      
    // create wallet with seed and defaults
    String path = UUID.randomUUID().toString();
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
    List<MoneroTxWallet> txs = wallet.getTxs();
    assertFalse(txs.isEmpty()); // wallet is used
    assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, (long) txs.get(0).getHeight());
    closeWallet(wallet); // TODO: monero-wallet-rpc: if wallet is not closed, primary address will not change
    
    // create wallet with non-defaults
    path = UUID.randomUUID().toString();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setLanguage("German").setSeedOffset("my offset!").setSaveCurrent(false));
    MoneroUtils.validateMnemonic(wallet.getSeed());
    assertNotEquals(TestUtils.SEED, wallet.getSeed()); // seed is different because of offset
    assertNotEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
    assertTrue(wallet.getTxs().isEmpty());  // wallet is not used
    closeWallet(wallet);
  }
  
  // Can open wallets
  @Test
  @Tag("CI")
  public void testOpenWallet() {
    assumeTrue(TEST_NON_RELAYS);

    // create names of test wallets
    int numTestWallets = 3;
    List<String> names = new ArrayList<String>();
    for (int i = 0; i < numTestWallets; i++) names.add(UUID.randomUUID().toString());
    
    // create test wallets
    List<String> seeds = new ArrayList<String>();
    for (String name : names) {
      MoneroWalletRpc wallet = createWallet(new MoneroWalletConfig().setPath(name));
      seeds.add(wallet.getSeed());
      closeWallet(wallet, true);
    }
    
    // open test wallets
    List<MoneroWallet> wallets = new ArrayList<MoneroWallet>();
    for (int i = 0; i < numTestWallets; i++) {
      MoneroWallet wallet = openWallet(new MoneroWalletConfig().setPath(names.get(i)).setPassword(TestUtils.WALLET_PASSWORD));
      assertEquals(seeds.get(i), wallet.getSeed());
      wallets.add(wallet);
    }
    
    // attempt to re-open already opened wallet
    try {
      openWallet(new MoneroWalletConfig().setPath(names.get(numTestWallets - 1)).setPassword(TestUtils.WALLET_PASSWORD));
      fail("Cannot open wallet which is already open");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode()); // -1 indicates wallet does not exist (or is open by another app)
    }
    
    // attempt to open non-existent
    try {
      openWallet(new MoneroWalletConfig().setPath("btc_integrity").setPassword(TestUtils.WALLET_PASSWORD));
      fail("Cannot open non-existent wallet");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // close wallets
    for (MoneroWallet wallet : wallets) closeWallet(wallet);
  }
  
  // Can indicate if multisig import is needed for correct balance information
  @Test
  @Tag("CI")
  public void testIsMultisigNeeded() {
    assumeTrue(TEST_NON_RELAYS);
    assertEquals(false, wallet.isMultisigImportNeeded()); // TODO: test with multisig wallet
  }
  
  // Can tag accounts and query accounts by tag
  @Test
  @Tag("CI")
  public void testAccountTags() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue(accounts.size() >= 3, "Not enough accounts to test; run create account test");
    
    // tag some of the accounts
    MoneroAccountTag tag = new MoneroAccountTag("my_tag_" + UUID.randomUUID(), "my tag label", Arrays.asList(0, 1));
    wallet.tagAccounts(tag.getTag(), tag.getAccountIndices());
    
    // query accounts by tag
    List<MoneroAccount> taggedAccounts = wallet.getAccounts(false, tag.getTag());
    assertEquals(2, taggedAccounts.size());
    assertEquals(0, (int) taggedAccounts.get(0).getIndex());
    assertEquals(tag.getTag(), taggedAccounts.get(0).getTag());
    assertEquals(1, (int) taggedAccounts.get(1).getIndex());
    assertEquals(tag.getTag(), taggedAccounts.get(1).getTag());

    // set tag label
    wallet.setAccountTagLabel(tag.getTag(), tag.getLabel());
    
    // fetch tags and ensure new tag is contained
    List<MoneroAccountTag> tags = wallet.getAccountTags();
    assertTrue(tags.contains(tag));
    
    // re-tag an account
    MoneroAccountTag tag2 = new MoneroAccountTag("my_tag_" + UUID.randomUUID(), "my tag label 2", Arrays.asList(1));
    wallet.tagAccounts(tag2.getTag(), tag2.getAccountIndices());
    List<MoneroAccount> taggedAccounts2 = wallet.getAccounts(false, tag2.getTag());
    assertEquals(1, taggedAccounts2.size(), 1);
    assertEquals(1, taggedAccounts2.get(0).getIndex(), 1);
    assertEquals(tag2.getTag(), taggedAccounts2.get(0).getTag());
    
    // re-query original tag which only applies to one account now
    taggedAccounts = wallet.getAccounts(false, tag.getTag());
    assertEquals(1, taggedAccounts.size());
    assertEquals(0, (int) taggedAccounts.get(0).getIndex());
    assertEquals(tag.getTag(), taggedAccounts.get(0).getTag());
    
    // untag and query accounts
    wallet.untagAccounts(Arrays.asList(0, 1));
    assertEquals(0, wallet.getAccountTags().size());
    try {
      wallet.getAccounts(false, tag.getTag());
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test that non-existing tag returns no accounts
    try {
      wallet.getAccounts(false, "non_existing_tag");
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
  }
  
  // Can get addresses out of range of used accounts and subaddresses
  @Override
  @Test
  @Tag("CI")
  public void testGetSubaddressAddressOutOfRange() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size();
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNull(address);
  }
  
  // Can save the wallet
  @Test
  @Tag("CI")
  public void testSave() {
    assumeTrue(TEST_NON_RELAYS);
    wallet.save();
  }
  
  // Can close a wallet
  @Test
  @Tag("CI")
  public void testClose() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create a test wallet
    String path = UUID.randomUUID().toString();
    MoneroWalletRpc wallet = createWallet(new MoneroWalletConfig().setPath(path));
    wallet.sync();
    
    // close the wallet
    wallet.close();
    
    // attempt to interact with the wallet
    try {
      wallet.getHeight();
    } catch (MoneroError e) {
      assertEquals(-13, (int) e.getCode());
      assertEquals("No wallet file", e.getMessage());
    }
    try {
      wallet.getSeed();
    } catch (MoneroError e) {
      assertEquals(-13, (int) e.getCode());
      assertEquals("No wallet file", e.getMessage());
    }
    try {
      wallet.sync();
    } catch (MoneroError e) {
      assertEquals(-13, (int) e.getCode());
      assertEquals("No wallet file", e.getMessage());
    }
    
    // re-open the wallet
    wallet.openWallet(path, TestUtils.WALLET_PASSWORD);
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
    
    // close the wallet
    closeWallet(wallet, true);
  }
  
  // Can stop the RPC server
  @Test
  @Disabled   // disabled so server not actually stopped
  public void testStop() {
    ((MoneroWalletRpc) wallet).stop();
  }
  
  // ---------------------------------- PRIVATE -------------------------------
  
  // rpc-specific tx tests
  @Override
  protected void testTxWallet(MoneroTxWallet tx, TxContext ctx) {
    ctx = new TxContext(ctx);
    
    // run common tests
    super.testTxWallet(tx, ctx);
  }
  
  @Override
  protected void testInvalidTxHashError(MoneroError e) {
    super.testInvalidTxHashError(e);
    assertEquals(-8, (int) e.getCode());
  }
  
  @Override
  protected void testInvalidTxKeyError(MoneroError e) {
    super.testInvalidTxKeyError(e);
    assertEquals(-25, (int) e.getCode());
  }
  
  @Override
  protected void testInvalidAddressError(MoneroError e) {
    super.testInvalidAddressError(e);
    assertEquals(-2, (int) e.getCode());
  }
  
  @Override
  protected void testInvalidSignatureError(MoneroError e) {
    super.testInvalidSignatureError(e);
    assertEquals(-1, (int) e.getCode()); // TODO: sometimes comes back bad, sometimes throws exception.  ensure txs come from different addresses?
  }
  
  @Override
  protected void testNoSubaddressError(MoneroError e) {
    super.testNoSubaddressError(e);
    assertEquals(-1, (int) e.getCode());
  }
  
  @Override
  protected void testSignatureHeaderCheckError(MoneroError e) {
    super.testSignatureHeaderCheckError(e);
    assertEquals(-1, (int) e.getCode());
  }
  
  // -------------------- OVERRIDES TO BE DIRECTLY RUNNABLE -------------------
  
  @Override
  @Test
  @Tag("CI")
  public void testCreateWalletRandom() {
    super.testCreateWalletRandom();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testCreateWalletFromSeed() {
    super.testCreateWalletFromSeed();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testCreateWalletFromSeedWithOffset() {
    super.testCreateWalletFromSeedWithOffset();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testCreateWalletFromKeys() {
    super.testCreateWalletFromKeys();
  }
  
  @Override
  @Test
  public void testSubaddressLookahead() {
    super.testSubaddressLookahead();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetVersion() {
    super.testGetVersion();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetPath() {
    super.testGetPath();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSetDaemonConnection() {
    super.testSetDaemonConnection();
  }

  @Test
  @Tag("CI")
  public void testConnectionManager() {
    super.testConnectionManager();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetHeight() {
    super.testGetHeight();
  }
  
  @Override
  @Test
  public void testGetHeightByDate() {
    super.testGetHeightByDate();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSeed() {
    super.testGetSeed();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSeedLanguages() {
    super.testGetSeedLanguages();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetPrivateViewKey() {
    super.testGetPrivateViewKey();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetPrivateSpendKey() {
    super.testGetPrivateSpendKey();
  }
  
  @Override
  @Test
  public void testGetPublicViewKey() {
    super.testGetPublicViewKey();
  }
  
  @Override
  @Test
  public void testGetPublicSpendKey() {
    super.testGetPublicSpendKey();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetPrimaryAddress() {
    super.testGetPrimaryAddress();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetIntegratedAddress() {
    super.testGetIntegratedAddress();
  }

  @Override
  @Test
  @Tag("CI")
  public void testDecodeIntegratedAddress() {
    super.testDecodeIntegratedAddress();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSyncWithoutProgress() {
    super.testSyncWithoutProgress();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testWalletEqualityGroundTruth() {
    super.testWalletEqualityGroundTruth();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetAccountsWithoutSubaddresses() {
    super.testGetAccountsWithoutSubaddresses();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetAccountsWithSubaddresses() {
    super.testGetAccountsWithSubaddresses();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetAccount() {
    super.testGetAccount();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCreateAccountWithoutLabel() {
    super.testCreateAccountWithoutLabel();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCreateAccountWithLabel() {
    super.testCreateAccountWithLabel();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetAccountLabel() {
    super.testSetAccountLabel();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSubaddresses() {
    super.testGetSubaddresses();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSubaddressesByIndices() {
    super.testGetSubaddressesByIndices();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSubaddressByIndex() {
    super.testGetSubaddressByIndex();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCreateSubaddress() {
    super.testCreateSubaddress();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetSubaddressLabel() {
    super.testSetSubaddressLabel();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetSubaddressAddress() {
    super.testGetSubaddressAddress();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetAddressIndices() {
    super.testGetAddressIndices();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetAllBalances() {
    super.testGetAllBalances();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetTxsWallet() {
    super.testGetTxsWallet();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetTxsByHash() {
    super.testGetTxsByHash();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetTxsWithQuery() {
    super.testGetTxsWithQuery();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetTxsByHeight() {
    super.testGetTxsByHeight();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetTxsWithPaymentIds() {
    super.testGetTxsWithPaymentIds();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetTxsFieldsWithFiltering() {
    super.testGetTxsFieldsWithFiltering();
  }

  @Override
  @Test
  @Tag("CI")
  public void testValidateInputsGetTxs() {
    super.testValidateInputsGetTxs();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetTransfers() {
    super.testGetTransfers();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetTransfersWithQuery() {
    super.testGetTransfersWithQuery();
  }

  @Override
  @Test
  @Tag("CI")
  public void testValidateInputsGetTransfers() {
    super.testValidateInputsGetTransfers();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetIncomingOutgoingTransfers() {
    super.testGetIncomingOutgoingTransfers();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetOutputs() {
    super.testGetOutputs();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetOutputsWithQuery() {
    super.testGetOutputsWithQuery();
  }

  @Override
  @Test
  @Tag("CI")
  public void testValidateInputsGetOutputs() {
    super.testValidateInputsGetOutputs();
  }

  @Override
  @Test
  @Tag("CI")
  public void testAccounting() {
    super.testAccounting();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetTxNote() {
    super.testSetTxNote();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetTxNotes() {
    super.testSetTxNotes();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testCheckTxKey() {
    super.testCheckTxKey();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCheckTxProof() {
    super.testCheckTxProof();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCheckSpendProof() {
    super.testCheckSpendProof();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetReserveProofWallet() {
    super.testGetReserveProofWallet();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetReserveProofAccount() {
    super.testGetReserveProofAccount();
  }

  @Override
  @Test
  @Tag("CI")
  public void testExportOutputs() {
    super.testExportOutputs();
  }

  @Override
  @Test
  @Tag("CI")
  public void testImportOutputs() {
    super.testImportOutputs();
  }

  @Override
  @Test
  @Tag("CI")
  public void testExportKeyImages() {
    super.testExportKeyImages();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetNewKeyImagesFromLastImport() {
    super.testGetNewKeyImagesFromLastImport();
  }

  @Override
  @Test
  @Disabled // TODO (monero-project): disabled because importing key images deletes corresponding incoming transfers: https://github.com/monero-project/monero/issues/5812
  public void testImportKeyImages() {
    super.testImportKeyImages();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testViewOnlyAndOfflineWallets() {
    super.testViewOnlyAndOfflineWallets();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSignAndVerifyMessages() {
    super.testSignAndVerifyMessages();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testAddressBook() {
    super.testAddressBook();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetAttributes() {
    super.testSetAttributes();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetPaymentUri() {
    super.testGetPaymentUri();
  }

  @Override
  @Test
  @Tag("CI")
  public void testMining() {
    super.testMining();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testValidateInputsSendingFunds() {
    super.testValidateInputsSendingFunds();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSyncWithPoolSameAccounts() {
    super.testSyncWithPoolSameAccounts();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSyncWithPoolSubmitAndFlush() {
    super.testSyncWithPoolSubmitAndFlush();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSyncWithPoolSubmitAndRelay() {
    super.testSyncWithPoolSubmitAndRelay();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSyncWithPoolRelay() {
    super.testSyncWithPoolRelay();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSendToSelf() {
    super.testSendToSelf();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSendToExternal() {
    super.testSendToExternal();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSendFromSubaddresses() {
    super.testSendFromSubaddresses();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSendFromSubaddressesSplit() {
    super.testSendFromSubaddressesSplit();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSend() {
    super.testSend();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSendWithPaymentId() {
    super.testSendWithPaymentId();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSendSplit() {
    super.testSendSplit();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCreateThenRelay() {
    super.testCreateThenRelay();
  }

  @Override
  @Test
  @Tag("CI")
  public void testCreateThenRelaySplit() {
    super.testCreateThenRelaySplit();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSendToMultiple() {
    super.testSendToMultiple();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSendToMultipleSplit() {
    super.testSendToMultipleSplit();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSendDustToMultipleSplit() {
    super.testSendDustToMultipleSplit();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSubtractFeeFrom() {
    super.testSubtractFeeFrom();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSubtractFeeFromSplit() {
    super.testSubtractFeeFromSplit();
  }

  @Override
  @Test
  @Tag("CI")
  public void testUpdateLockedSameAccount() {
    super.testUpdateLockedSameAccount();
  }

  @Override
  @Test
  @Tag("CI")
  public void testUpdateLockedSameAccountSplit() {
    super.testUpdateLockedSameAccountSplit();
  }

  @Override
  @Test
  @Tag("CI")
  public void testUpdateLockedDifferentAccounts() {
    super.testUpdateLockedDifferentAccounts();
  }

  @Override
  @Test
  @Tag("CI")
  public void testUpdateLockedDifferentAccountsSplit() {
    super.testUpdateLockedDifferentAccountsSplit();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSweepOutputs() {
    super.testSweepOutputs();
  }

  @Override
  @Test
  public void testSweepSubaddresses() {
    super.testSweepSubaddresses();
  }

  @Override
  @Test
  public void testSweepAccounts() {
    super.testSweepAccounts();
  }

  @Override
  @Test
  public void testSweepWalletByAccounts() {
    super.testSweepWalletByAccounts();
  }
  
  @Override
  @Test
  public void testSweepWalletBySubaddresses() {
    super.testSweepWalletBySubaddresses();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSweepDustNoRelay() {
    super.testSweepDustNoRelay();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSweepDust() {
    super.testSweepDust();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testScanTxs() {
    super.testScanTxs();
  }
  
  // Can rescan spent // TODO: run this in full wallet too
  @Test
  @Tag("CI")
  public void testRescanSpent() {
    assumeTrue(TEST_NON_RELAYS);
    wallet.rescanSpent();
  }
  
  @Override
  @Test
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testMultisig() {
    super.testMultisig();
  }

  @Override
  @Test
  @Disabled
  public void testMultisigStress() {
    super.testMultisigStress();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testChangePassword() {
    super.testChangePassword();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSaveAndClose() {
    super.testSaveAndClose();
  }
  
  @Override
  @Test
  @Tag("CI")
  @Tag("NotificationTest")
  public void testNotificationsDifferentWallet() {
    super.testNotificationsDifferentWallet();
  }
  
  @Override
  @Test
  @Tag("CI")
  @Tag("NotificationTest")
  public void testNotificationsDifferentWalletWhenRelayed() {
    super.testNotificationsDifferentWalletWhenRelayed();
  }
  
  @Override
  @Test
  @Tag("CI")
  @Tag("NotificationTest")
  public void testNotificationsDifferentAccounts() {
    super.testNotificationsDifferentAccounts();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  public void testNotificationsSameAccount() {
    super.testNotificationsSameAccount();
  }
  
  @Override
  @Test
  @Tag("CI")
  @Tag("NotificationTest")
  public void testNotificationsDifferentAccountSweepOutput() {
    super.testNotificationsDifferentAccountSweepOutput();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  public void testNotificationsSameAccountSweepOutputWhenRelayed() {
    super.testNotificationsSameAccountSweepOutputWhenRelayed();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testStopListening() {
    super.testStopListening();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testCreateAndReceive() {
    super.testCreateAndReceive();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testFreezeOutputs() {
    super.testFreezeOutputs();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testInputKeyImages() {
    super.testInputKeyImages();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testProveUnrelayedTxs() {
    super.testProveUnrelayedTxs();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testGetDefaultFeePriority() {
    super.testGetDefaultFeePriority();
  }
}
