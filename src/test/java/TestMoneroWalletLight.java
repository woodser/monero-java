import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletLight;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.junit.jupiter.api.TestInstance.Lifecycle;

import common.utils.GenUtils;

import org.junit.jupiter.api.TestInstance;

import utils.StartMining;
import utils.TestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.util.List;
import java.util.UUID;


@TestInstance(Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions
public class TestMoneroWalletLight extends TestMoneroWalletCommon {

  public TestMoneroWalletLight() {
      super();
  }

  @Override
  @BeforeAll
  public void beforeAll() {
    System.out.println("Starting Light Wallet Tests");
    super.beforeAll();
    wallet = getTestWallet();
  }
  
  @Override
  @AfterAll
  public void afterAll() {
    System.out.println("End Light Wallet Tests");
    // try to stop mining
    try { StartMining.stopMining(); }
    catch (MoneroError e) { }
    
    // close wallet
    if (wallet != null) wallet.close(false);
  }

  @Override
  protected MoneroWalletLight getTestWallet() {
      return TestUtils.getWalletLight();
  }

  @Override
  @BeforeEach
  public void beforeEach(TestInfo testInfo) {
    System.out.println("Before test " + testInfo.getDisplayName());
    super.beforeEach(testInfo);
  }

  private MoneroRpcConnection getRpcConnection() {
    return new MoneroRpcConnection(TestUtils.WALLET_LWS_URI);
  }

  @Override
  protected MoneroWalletLight openWallet(MoneroWalletConfig config) {
    return openWallet(config, true);
  }

  @Override
  protected MoneroWalletLight openWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(getRpcConnection());
    
    // open wallet
    MoneroWalletLight wallet = MoneroWalletLight.openWallet(config);
    if (startSyncing != false && wallet.isConnectedToDaemon()) {
      wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      syncWithDaemon(wallet);
    }
    return wallet;
  }

  @Override
  protected MoneroWalletLight createWallet(MoneroWalletConfig config) {
    return createWallet(config, true);
  }

  @Override
  protected MoneroWalletLight createWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    //boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServerUri(TestUtils.WALLET_LWS_URI);
    
    // create wallet
    MoneroWalletLight wallet = MoneroWalletLight.createWallet(config);
    if (startSyncing != false && wallet.isConnectedToDaemon()) {
      wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      syncWithDaemon(wallet);
    }
    return wallet;
  }
  
  private MoneroWalletFull createWalletFull(MoneroWalletConfig config) {
    return createWalletFull(config, true);
  }
  
  private MoneroWalletFull createWalletFull(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServerUri(TestUtils.DAEMON_RPC_URI);
    
    // create wallet
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(config);
    if (!random) assertEquals(config.getRestoreHeight() == null ? 0l : config.getRestoreHeight(), wallet.getRestoreHeight());
    if (startSyncing != false && wallet.isConnectedToDaemon()) wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    return wallet;
  }

  @Override
  public void closeWallet(MoneroWallet wallet, boolean save) {
      wallet.close(save);
  }

  /**
   * Get the wallet's supported languages for the seed.  This is an
   * instance method for wallet rpc and a static utility for other wallets.
   *
   * @return List<String> are the wallet's supported languages
   */
  @Override
  protected List<String> getSeedLanguages() {
      return MoneroWalletLight.getSeedLanguages();
  }

  // a light wallet does not support what monero-lws does not keep, such as labels, tx notes, attributes and the address book
  private static void testNotSupported(Runnable test) {
    try {
      test.run();
    } catch (Exception e) { // the native library throws a plain exception
      assertTrue(e.getMessage().contains("not supported") || e.getMessage().contains("doesn't support"), "Unexpected error: " + e.getMessage());
      return;
    }
    fail("Should have thrown exception");
  }

  // ------------------------------- BEGIN TESTS ------------------------------
  
  // Can create a full wallet from keys
  @Test
  @Tag("CI")
  public void testCreateWalletFromKeysJni() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    MoneroWalletLight walletKeys = openWallet(new MoneroWalletConfig().setServerUri(TestUtils.WALLET_LWS_URI).setSeed(TestUtils.SEED).setNetworkType(TestUtils.NETWORK_TYPE), false);
    testCreateWalletFromKeysJni(walletKeys);
  }
  
  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
  @Tag("CI")
  public void testViewOnlyAndOfflineWalletCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only wallet in wallet rpc process
    MoneroWalletLight viewOnlyWallet = openWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setServerUri(TestUtils.WALLET_LWS_URI));
    syncWithDaemon(viewOnlyWallet);
    
    // create offline full wallet
    MoneroWalletFull offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      closeWallet(offlineWallet);
    }
  };

  // Does not interfere with other wallet notifications
  @Test
  @Tag("CI")
  public void testWalletsDoNotInterfere() {    
    MoneroWalletLight wallet1 = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    MoneroWalletLight wallet2 = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    
    // the wallets of the same account are up to date until a block is scanned, which they notify when they sync
    StartMining.generateBlocks(1);
    TestUtils.waitForLwsScan(wallet1);
    long height = daemon.getHeight();
    
    testWalletsDoNotInterfere(wallet1, wallet2, height, height);
  }
    
  // Can be closed
  @Test
  @Tag("CI")
  public void testClose() {
    MoneroWalletConfig config = new MoneroWalletConfig().setPrimaryAddress(TestUtils.ADDRESS).setPrivateViewKey(TestUtils.PRIVATE_VIEW_KEY).setServerUri(TestUtils.WALLET_LWS_URI);
    testClose(new MoneroWalletConfig(), config);
  }
  
  // ---------------------------------- HELPERS -------------------------------
  
  // jni-specific tx tests
  @Override
  protected void testTxWallet(MoneroTxWallet tx, TxContext ctx) {
    if (ctx == null) ctx = new TxContext();
    
    // run common tests
    super.testTxWallet(tx, ctx);
  }

  // -------------------- OVERRIDES TO BE DIRECTLY RUNNABLE -------------------
  
  @Override
  @Test
  @Tag("CI")
  public void testSyncRandom() {

    // wait for monero-lws to catch up with the daemon before the wallet is created, as it lags behind the blocks that the previous test mined:
    // the start height of a new account is the one that lws scanned, and the height of the daemon of the wallet is the one that lws has of it
    TestUtils.waitForLwsScan((MoneroWalletLight) wallet);
    TestUtils.waitForLwsDaemonHeight(daemon.getHeight());
    super.testSyncRandom();
  }

  @Override
  @Test
  @Tag("CI")
  public void testResyncExisting() {
    testNotSupported(() -> super.testResyncExisting());
  }

  @Override
  @Test
  @Tag("CI")
  public void testSyncWalletFromKeys() {
    super.testSyncWalletFromKeys();
  }

  @Override
  @Test
  @Tag("CI")
  public void testStartStopSyncing() {
    super.testStartStopSyncing();
  }

  @Override
  @Test
  @Disabled // infinite loop, to monitor the memory of the process by hand
  public void testMemoryLeak() {
    super.testMemoryLeak();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetDaemonMaxPeerHeight() {
    super.testGetDaemonMaxPeerHeight();
  }

  @Override
  @Test
  @Tag("CI")
  public void testDaemon() {
    super.testDaemon();
  }

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
  @Disabled // bug in monero_wallet_keys?
  public void testCreateWalletFromSeedWithOffset() {
    testCreateWalletFromSeedWithOffset(GenUtils.getUUID());
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testCreateWalletFromKeys() {
    super.testCreateWalletFromKeys();
  }

  @Override
  @Test
  @Disabled // TODO too much high height for light wallet
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
  @Disabled // not supported by light wallet
  @Test
  public void testGetPath() {
    super.testGetPath();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testSetDaemonConnection() {
    try {
      super.testSetDaemonConnection();
      fail("Should have thrown exception"); // a light wallet cannot change its light wallet server
    } catch (MoneroError e) {
      assertEquals("set_daemon_connection() not supported", e.getMessage());
    }
  }

  @Test
  @Disabled // set_daemon_connection() not supported by light wallet
  @Override
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
  @Disabled // not supported by light wallet
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
  @Tag("CI")
  public void testGetPublicViewKey() {
    super.testGetPublicViewKey();
  }
  
  @Override
  @Test
  @Tag("CI")
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
    testNotSupported(() -> super.testCreateAccountWithLabel());
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetAccountLabel() {
    testNotSupported(() -> super.testSetAccountLabel());
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
    testNotSupported(() -> super.testCreateSubaddress());
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetSubaddressLabel() {
    testNotSupported(() -> super.testSetSubaddressLabel());
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
  @Disabled // TODO implement payment for light wallet
  public void testGetTxsWithPaymentIds() {
    /* LITE_MODE enabled */
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
  @Disabled // TODO implemenent for light wallet
  public void testCheckTxKey() {
    super.testCheckTxKey();
  }

  @Override
  @Test
  @Disabled // TODO implemenent for light wallet
  public void testCheckTxProof() {
    super.testCheckTxProof();
  }

  @Override
  @Test
  @Disabled // TODO implemenent for light wallet
  public void testCheckSpendProof() {
    super.testCheckSpendProof();
  }

  @Override
  @Test
  @Disabled // TODO still no way to get rpc_version from lws
  public void testGetReserveProofWallet() {
    super.testGetReserveProofWallet();
  }

  @Override
  @Test
  @Disabled // TODO still no way to get rpc_version from lws
  public void testGetReserveProofAccount() {
    super.testGetReserveProofAccount();
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetTxNote() {
    testNotSupported(() -> super.testSetTxNote());
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetTxNotes() {
    testNotSupported(() -> super.testSetTxNotes());
  }

  @Override
  @Test
  @Tag("CI")
  public void testExportOutputs() {
    super.testExportOutputs();
  }

  @Override
  @Disabled // import_outputs() not supported by light wallet
  @Test
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
  @Disabled // disabled in the common tests too (importing key images can erase incoming transfers per wallet2.cpp:11957), and the light wallet exports no key images
  public void testImportKeyImages() {
    super.testImportKeyImages();
  }

  @Override
  @Test
  @Disabled // the test creates a wallet with an offline server, a light wallet needs monero-lws to be created
  public void testGetNewKeyImagesFromLastImport() {
    super.testGetNewKeyImagesFromLastImport();
  }
  
  @SuppressWarnings("unused")
  @Test
  @Tag("CI")
  @Override
  public void testViewOnlyAndOfflineWallets() {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only and offline wallets
    MoneroWallet viewOnlyWallet = openWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setServer(wallet.getDaemonConnection()));
    MoneroWallet offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    assertFalse(offlineWallet.isConnectedToDaemon());
    syncWithDaemon(viewOnlyWallet);
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      closeWallet(viewOnlyWallet);
      closeWallet(offlineWallet);
    }
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
    testNotSupported(() -> super.testAddressBook());
  }

  @Override
  @Test
  @Tag("CI")
  public void testSetAttributes() {
    testNotSupported(() -> super.testSetAttributes());
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetPaymentUri() {
    super.testGetPaymentUri();
  }

  @Override
  @Test
  @Disabled // not supported by light wallet
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
  @Disabled // unconfirmed txs not supported by lws
  public void testSyncWithPoolSameAccounts() {
    super.testSyncWithPoolSameAccounts();
  }
  
  @Override
  @Test
  @Disabled // unconfirmed txs not supported by lws
  public void testSyncWithPoolSubmitAndRelay() {
    super.testSyncWithPoolSubmitAndRelay();
  }
  
  @Override
  @Test
  @Disabled // unconfirmed txs not supported by lws
  public void testSyncWithPoolRelay() {
    super.testSyncWithPoolRelay();
  }

  @Override
  @Test
  @Disabled // unconfirmed txs not supported by lws
  public void testSyncWithPoolSubmitAndFlush() {
    super.testSyncWithPoolSubmitAndFlush();
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement tx splitting for light wallet
  public void testSendToMultipleSplit() {
    super.testSendToMultipleSplit();
  }

  @Override
  @Test
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement
  @Test
  public void testSweepOutputs() {
    super.testSweepOutputs();
  }

  @Override
  @Disabled // TODO implement
  @Test
  public void testSweepSubaddresses() {
    super.testSweepSubaddresses();
  }

  @Override
  @Disabled // TODO implement
  @Test
  public void testSweepAccounts() {
    super.testSweepAccounts();
  }

  @Override
  @Disabled // TODO implement
  @Test
  public void testSweepWalletByAccounts() {
    super.testSweepWalletByAccounts();
  }

  @Override
  @Disabled // TODO implement
  @Test
  public void testSweepWalletBySubaddresses() {
    super.testSweepWalletBySubaddresses();
  }

  @Override
  @Disabled // TODO implement
  @Test
  public void testSweepDustNoRelay() {
    super.testSweepDustNoRelay();
  }

  @Override
  @Disabled // TODO implement
  @Test
  public void testSweepDust() {
    super.testSweepDust();
  }
  
  @Override
  @Test
  @Tag("CI")
  public void testScanTxs() {
    // cannot recreate wallet on same lws server, so open it
    MoneroWallet scanWallet = openWallet(new MoneroWalletConfig().setSeed(wallet.getSeed()));
    testScanTxs(scanWallet);
  }

  @Override
  @Test
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }

  @Override
  @Test
  @Disabled // not supported by light wallet
  public void testMultisig() {
    super.testMultisig();
  }

  @Override
  @Test
  @Disabled // not supported by light wallet
  public void testMultisigStress() {
    super.testMultisigStress();
  }
  
  @Override
  @Disabled // not supported by light wallet
  @Test
  public void testChangePassword() {
    super.testChangePassword();
  }
  
  @Override
  @Disabled // not supported by light wallet
  @Test
  public void testSaveAndClose() {
    super.testSaveAndClose();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled // the created tx has no destinations in its outgoing transfer, and the test then reads the can split of a config that does not set it (NullPointerException)
  public void testNotificationsDifferentWallet() {
    super.testNotificationsDifferentWallet();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled // the created tx has no destinations in its outgoing transfer, and the test then reads the can split of a config that does not set it (NullPointerException)
  public void testNotificationsDifferentWalletWhenRelayed() {
    super.testNotificationsDifferentWalletWhenRelayed();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled // light wallet does not announce the received and spent outputs to its listeners, lws has no unconfirmed txs
  public void testNotificationsDifferentAccounts() {
    super.testNotificationsDifferentAccounts();
  }
  
  @Override
  @Test
  @Disabled // light wallet does not announce the received and spent outputs to its listeners, lws has no unconfirmed txs
  @Tag("NotificationTest")
  public void testNotificationsSameAccount() {
    super.testNotificationsSameAccount();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled // light wallet does not announce the received and spent outputs to its listeners, lws has no unconfirmed txs
  public void testNotificationsDifferentAccountSweepOutput() {
    super.testNotificationsDifferentAccountSweepOutput();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled // light wallet does not announce the received and spent outputs to its listeners, lws has no unconfirmed txs
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
  @Disabled // sweep_dust() not supported by light wallet
  public void testInputKeyImages() {
    super.testInputKeyImages();
  }

  @Override
  @Test
  @Disabled // light wallet cannot prove unrelayed txs
  public void testProveUnrelayedTxs() {
    super.testProveUnrelayedTxs();
  } 
  
  @Override
  @Test
  @Tag("CI")
  public void testGetDefaultFeePriority() {
    super.testGetDefaultFeePriority();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSeedLanguage() {
    super.testGetSeedLanguage();
  }

  @Override
  @Test
  @Tag("CI")
  public void testGetSubaddressAddressOutOfRange() {
    super.testGetSubaddressAddressOutOfRange();
  }

}
