package test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
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
      TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(walletFull);
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
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    
    // create client connected to internal monero-wallet-rpc process
    MoneroWalletRpc wallet;
    try {
      wallet = TestUtils.startWalletRpcProcess();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    
    // open wallet
    try {
      wallet.openWallet(config.getPath(), config.getPassword());
      if ("".equals(config.getServerUri())) wallet.setDaemonConnection(""); // serverUri "" denotes offline wallet for tests
      else wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      return wallet;
    } catch (MoneroError e) {
      try { TestUtils.stopWalletRpcProcess(wallet); } catch (Exception e2) { throw new RuntimeException(e2); }
      throw e;
    }
  }
  
  @Override
  protected MoneroWalletRpc createWallet(MoneroWalletConfig config) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getMnemonic() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
    // create client connected to internal monero-wallet-rpc process
    MoneroWalletRpc wallet;
    try {
      wallet = TestUtils.startWalletRpcProcess();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    
    // create wallet
    try {
      wallet.createWallet(config);
      if ("".equals(config.getServerUri())) wallet.setDaemonConnection(""); // serverUri "" denotes offline wallet for tests
      else wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      return wallet;
    } catch (MoneroError e) {
      try { TestUtils.stopWalletRpcProcess(wallet); } catch (Exception e2) { throw new RuntimeException(e2); }
      throw e;
    }
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
  protected List<String> getMnemonicLanguages() {
    return ((MoneroWalletRpc) wallet).getMnemonicLanguages();
  }
  
  // ---------------------------- BEGIN TESTS ---------------------------------
  
  // Can create a wallet with a randomly generated mnemonic
  @Test
  public void testCreateWalletRandomRpc() {
    assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = UUID.randomUUID().toString();
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPath(path));
    String mnemonic = wallet.getMnemonic();
    MoneroUtils.validateMnemonic(mnemonic);
    assertNotEquals(TestUtils.MNEMONIC, mnemonic);
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    wallet.sync();  // very quick because restore height is chain height
    closeWallet(wallet);

    // create random wallet with non defaults
    path = UUID.randomUUID().toString();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setLanguage("Spanish"));
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    assertNotEquals(mnemonic, wallet.getMnemonic());
    mnemonic = wallet.getMnemonic();
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    
    // attempt to create wallet which already exists
    try {
      createWallet(new MoneroWalletConfig().setPath(path).setLanguage("Spanish"));
    } catch (MoneroError e) {
      assertEquals(e.getMessage(), "Wallet already exists: " + path);
      assertEquals(-21, (int) e.getCode());
      assertEquals(mnemonic, wallet.getMnemonic());
    }
    closeWallet(wallet);
  }
  
  // Can create a RPC wallet from a mnemonic phrase
  @Test
  public void testCreateWalletFromMnemonicRpc() {
    assumeTrue(TEST_NON_RELAYS);
      
    // create wallet with mnemonic and defaults
    String path = UUID.randomUUID().toString();
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
    List<MoneroTxWallet> txs = wallet.getTxs();
    assertFalse(txs.isEmpty()); // wallet is used
    assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, (long) txs.get(0).getHeight());
    closeWallet(wallet); // TODO: monero-wallet-rpc: if wallet is not closed, primary address will not change
    
    // create wallet with non-defaults
    path = UUID.randomUUID().toString();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setLanguage("German").setSeedOffset("my offset!").setSaveCurrent(false));
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    assertNotEquals(TestUtils.MNEMONIC, wallet.getMnemonic());  // mnemonic is different because of offset
    assertNotEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
    assertTrue(wallet.getTxs().isEmpty());  // wallet is not used
    closeWallet(wallet);
  }
  
  // Can open wallets
  @Test
  public void testOpenWallet() {
    assumeTrue(TEST_NON_RELAYS);

    // create names of test wallets
    int numTestWallets = 3;
    List<String> names = new ArrayList<String>();
    for (int i = 0; i < numTestWallets; i++) names.add(UUID.randomUUID().toString());
    
    // create test wallets
    List<String> mnemonics = new ArrayList<String>();
    for (String name : names) {
      MoneroWalletRpc wallet = createWallet(new MoneroWalletConfig().setPath(name));
      mnemonics.add(wallet.getMnemonic());
      closeWallet(wallet, true);
    }
    
    // open test wallets
    List<MoneroWallet> wallets = new ArrayList<MoneroWallet>();
    for (int i = 0; i < numTestWallets; i++) {
      MoneroWallet wallet = openWallet(new MoneroWalletConfig().setPath(names.get(i)).setPassword(TestUtils.WALLET_PASSWORD));
      assertEquals(mnemonics.get(i), wallet.getMnemonic());
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
  public void testIsMultisigNeeded() {
    assumeTrue(TEST_NON_RELAYS);
    assertEquals(false, wallet.isMultisigImportNeeded()); // TODO: test with multisig wallet
  }
  
  // Can tag accounts and query accounts by tag
  @Test
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
  public void testGetSubaddressAddressOutOfRange() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size();
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNull(address);
  }
  
  // Can rescan spent
  @Test
  public void testRescanSpent() {
    assumeTrue(TEST_NON_RELAYS);
    wallet.rescanSpent();
  }
  
  // Can save the wallet
  @Test
  public void testSave() {
    assumeTrue(TEST_NON_RELAYS);
    wallet.save();
  }
  
  // Can close a wallet
  @Test
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
      wallet.getMnemonic();
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
  public void testCreateWalletRandom() {
    super.testCreateWalletRandom();
  }
  
  @Override
  @Test
  public void testCreateWalletFromMnemonic() {
    super.testCreateWalletFromMnemonic();
  }
  
  @Override
  @Test
  public void testCreateWalletFromMnemonicWithOffset() {
    super.testCreateWalletFromMnemonicWithOffset();
  }
  
  @Override
  @Test
  public void testCreateWalletFromKeys() {
    super.testCreateWalletFromKeys();
  }
  
  @Override
  @Test
  public void testGetVersion() {
    super.testGetVersion();
  }
  
  @Override
  @Test
  public void testGetPath() {
    super.testGetPath();
  }

  @Override
  @Test
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
  public void testGetMnemonic() {
    super.testGetMnemonic();
  }

  @Override
  @Test
  public void testGetMnemonicLanguages() {
    super.testGetMnemonicLanguages();
  }

  @Override
  @Test
  public void testGetPrivateViewKey() {
    super.testGetPrivateViewKey();
  }
  
  @Override
  @Test
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
  public void testGetPrimaryAddress() {
    super.testGetPrimaryAddress();
  }

  @Override
  @Test
  public void testGetIntegratedAddressFromPaymentId() {
    super.testGetIntegratedAddressFromPaymentId();
  }

  @Override
  @Test
  public void testDecodeIntegratedAddress() {
    super.testDecodeIntegratedAddress();
  }

  @Override
  @Test
  public void testSyncWithoutProgress() {
    super.testSyncWithoutProgress();
  }
  
  @Override
  @Test
  public void testWalletEqualityGroundTruth() {
    super.testWalletEqualityGroundTruth();
  }

  @Override
  @Test
  public void testGetAccountsWithoutSubaddresses() {
    super.testGetAccountsWithoutSubaddresses();
  }

  @Override
  @Test
  public void testGetAccountsWithSubaddresses() {
    super.testGetAccountsWithSubaddresses();
  }

  @Override
  @Test
  public void testGetAccount() {
    super.testGetAccount();
  }

  @Override
  @Test
  public void testCreateAccountWithoutLabel() {
    super.testCreateAccountWithoutLabel();
  }

  @Override
  @Test
  public void testCreateAccountWithLabel() {
    super.testCreateAccountWithLabel();
  }

  @Override
  @Test
  public void testGetSubaddresses() {
    super.testGetSubaddresses();
  }

  @Override
  @Test
  public void testGetSubaddressesByIndices() {
    super.testGetSubaddressesByIndices();
  }

  @Override
  @Test
  public void testGetSubaddressByIndex() {
    super.testGetSubaddressByIndex();
  }

  @Override
  @Test
  public void testCreateSubaddress() {
    super.testCreateSubaddress();
  }

  @Override
  @Test
  public void testGetSubaddressAddress() {
    super.testGetSubaddressAddress();
  }

  @Override
  @Test
  public void testGetAddressIndices() {
    super.testGetAddressIndices();
  }

  @Override
  @Test
  public void testGetAllBalances() {
    super.testGetAllBalances();
  }

  @Override
  @Test
  public void testGetTxsWallet() {
    super.testGetTxsWallet();
  }
  
  @Override
  @Test
  public void testGetTxsByHash() {
    super.testGetTxsByHash();
  }

  @Override
  @Test
  public void testGetTxsWithQuery() {
    super.testGetTxsWithQuery();
  }
  
  @Override
  @Test
  public void testGetTxsByHeight() {
    super.testGetTxsByHeight();
  }

  @Override
  @Test
  public void testGetTxsWithPaymentIds() {
    super.testGetTxsWithPaymentIds();
  }

  @Override
  @Test
  public void testGetTxsFieldsWithFiltering() {
    super.testGetTxsFieldsWithFiltering();
  }

  @Override
  @Test
  public void testValidateInputsGetTxs() {
    super.testValidateInputsGetTxs();
  }

  @Override
  @Test
  public void testGetTransfers() {
    super.testGetTransfers();
  }

  @Override
  @Test
  public void testGetTransfersWithQuery() {
    super.testGetTransfersWithQuery();
  }

  @Override
  @Test
  public void testValidateInputsGetTransfers() {
    super.testValidateInputsGetTransfers();
  }
  
  @Override
  @Test
  public void testGetIncomingOutgoingTransfers() {
    super.testGetIncomingOutgoingTransfers();
  }

  @Override
  @Test
  public void testGetOutputs() {
    super.testGetOutputs();
  }

  @Override
  @Test
  public void testGetOutputsWithQuery() {
    super.testGetOutputsWithQuery();
  }

  @Override
  @Test
  public void testValidateInputsGetOutputs() {
    super.testValidateInputsGetOutputs();
  }

  @Override
  @Test
  public void testAccounting() {
    super.testAccounting();
  }

  @Override
  @Test
  public void testSetTxNote() {
    super.testSetTxNote();
  }

  @Override
  @Test
  public void testSetTxNotes() {
    super.testSetTxNotes();
  }
  
  @Override
  @Test
  public void testCheckTxKey() {
    super.testCheckTxKey();
  }

  @Override
  @Test
  public void testCheckTxProof() {
    super.testCheckTxProof();
  }

  @Override
  @Test
  public void testCheckSpendProof() {
    super.testCheckSpendProof();
  }

  @Override
  @Test
  public void testGetReserveProofWallet() {
    super.testGetReserveProofWallet();
  }

  @Override
  @Test
  public void testGetReserveProofAccount() {
    super.testGetReserveProofAccount();
  }

  @Override
  @Test
  public void testExportOutputs() {
    super.testExportOutputs();
  }

  @Override
  @Test
  public void testImportOutputs() {
    super.testImportOutputs();
  }

  @Override
  @Test
  public void testExportKeyImages() {
    super.testExportKeyImages();
  }

  @Override
  @Test
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
  public void testViewOnlyAndOfflineWallets() {
    super.testViewOnlyAndOfflineWallets();
  }

  @Override
  @Test
  public void testSignAndVerifyMessages() {
    super.testSignAndVerifyMessages();
  }
  
  @Override
  @Test
  public void testAddressBook() {
    super.testAddressBook();
  }

  @Override
  @Test
  public void testSetAttributes() {
    super.testSetAttributes();
  }
  
  @Override
  @Test
  public void testCreatePaymentUri() {
    super.testCreatePaymentUri();
  }

  @Override
  @Test
  public void testMining() {
    super.testMining();
  }
  
  @Override
  @Test
  public void testValidateInputsSendingFunds() {
    super.testValidateInputsSendingFunds();
  }
  
  @Override
  @Test
  public void testSyncWithPoolSameAccounts() {
    super.testSyncWithPoolSameAccounts();
  }
  
  @Override
  @Test
  public void testSyncWithPoolSubmitAndDiscard() {
    super.testSyncWithPoolSubmitAndDiscard();
  }
  
  @Override
  @Test
  public void testSyncWithPoolSubmitAndRelay() {
    super.testSyncWithPoolSubmitAndRelay();
  }
  
  @Override
  @Test
  public void testSyncWithPoolRelay() {
    super.testSyncWithPoolRelay();
  }
  
  @Override
  @Test
  public void testSendToExternal() {
    super.testSendToExternal();
  }

  @Override
  @Test
  public void testSendFromSubaddresses() {
    super.testSendFromSubaddresses();
  }

  @Override
  @Test
  public void testSendFromSubaddressesSplit() {
    super.testSendFromSubaddressesSplit();
  }

  @Override
  @Test
  public void testSend() {
    super.testSend();
  }

  @Override
  @Test
  public void testSendWithPaymentId() {
    super.testSendWithPaymentId();
  }

  @Override
  @Test
  public void testSendSplit() {
    super.testSendSplit();
  }

  @Override
  @Test
  public void testCreateThenRelay() {
    super.testCreateThenRelay();
  }

  @Override
  @Test
  public void testCreateThenRelaySplit() {
    super.testCreateThenRelaySplit();
  }

  @Override
  @Test
  public void testSendToMultiple() {
    super.testSendToMultiple();
  }

  @Override
  @Test
  public void testSendToMultipleSplit() {
    super.testSendToMultipleSplit();
  }
  
  @Override
  @Test
  public void testSendDustToMultipleSplit() {
    super.testSendDustToMultipleSplit();
  }

  @Override
  @Test
  public void testUpdateLockedSameAccount() {
    super.testUpdateLockedSameAccount();
  }

  @Override
  @Test
  public void testUpdateLockedSameAccountSplit() {
    super.testUpdateLockedSameAccountSplit();
  }

  @Override
  @Test
  public void testUpdateLockedDifferentAccounts() {
    super.testUpdateLockedDifferentAccounts();
  }

  @Override
  @Test
  public void testUpdateLockedDifferentAccountsSplit() {
    super.testUpdateLockedDifferentAccountsSplit();
  }
  
  @Override
  @Test
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
  public void testSweepDustNoRelay() {
    super.testSweepDustNoRelay();
  }

  @Override
  @Test
  public void testSweepDust() {
    super.testSweepDust();
  }
  
  @Override
  @Test
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }
  
  @Override
  @Test
  public void testMultisig() {
    super.testMultisig();
  }
  
  @Override
  @Test
  public void testSaveAndClose() {
    super.testSaveAndClose();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  public void testNotificationsDifferentWallet() {
    super.testNotificationsDifferentWallet();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  public void testNotificationsDifferentWalletWhenRelayed() {
    super.testNotificationsDifferentWalletWhenRelayed();
  }
  
  @Override
  @Test
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
  public void testStopListening() {
    super.testStopListening();
  }
  
  @Override
  @Test
  public void testCreateAndReceive() {
    super.testCreateAndReceive();
  }
  
  @Override
  @Test
  public void testFreezeOutputs() {
    super.testFreezeOutputs();
  }
  
  @Override
  @Test
  public void testInputKeyImages() {
    super.testInputKeyImages();
  }
}
