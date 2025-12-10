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

import utils.TestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
    if (daemon != null) {
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
    }
    
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

  // ------------------------------- BEGIN TESTS ------------------------------
  
  // Can create a full wallet from keys
  @Test
  public void testCreateWalletFromKeysJni() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    MoneroWalletLight walletKeys = openWallet(new MoneroWalletConfig().setServerUri(TestUtils.WALLET_LWS_URI).setSeed(TestUtils.SEED).setNetworkType(TestUtils.NETWORK_TYPE), false);
    testCreateWalletFromKeysJni(walletKeys);
  }
  
  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
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
  public void testWalletsDoNotInterfere() {    
    MoneroWalletLight wallet1 = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    MoneroWalletLight wallet2 = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    
    testWalletsDoNotInterfere(wallet1, wallet2, TestUtils.FIRST_RECEIVE_HEIGHT + 5, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
    
  // Can be closed
  @Test
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
  public void testSyncRandom() {
    super.testSyncRandom();
  }

  @Override
  @Test
  public void testResyncExisting() {
    super.testResyncExisting();
  }

  @Override
  @Test
  public void testSyncWalletFromKeys() {
    super.testSyncWalletFromKeys();
  }

  @Override
  @Test
  public void testStartStopSyncing() {
    super.testStartStopSyncing();
  }

  @Override
  @Test
  @Disabled
  public void testMemoryLeak() {
    super.testMemoryLeak();
  }

  @Override
  @Test
  public void testGetDaemonMaxPeerHeight() {
    super.testGetDaemonMaxPeerHeight();
  }

  @Override
  @Test
  public void testDaemon() {
    super.testDaemon();
  }

  @Override
  @Test
  public void testCreateWalletRandom() {
    super.testCreateWalletRandom();
  }
  
  @Override
  @Test
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
  public void testSetDaemonConnection() {
    super.testSetDaemonConnection();
  }

  @Test
  @Disabled
  @Override
  public void testConnectionManager() {
    super.testConnectionManager();
  }

  @Override
  @Test
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
  public void testGetSeed() {
    super.testGetSeed();
  }

  @Override
  @Test
  public void testGetSeedLanguages() {
    super.testGetSeedLanguages();
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
  public void testGetIntegratedAddress() {
    super.testGetIntegratedAddress();
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
  public void testSetAccountLabel() {
    super.testSetAccountLabel();
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
  public void testSetSubaddressLabel() {
    super.testSetSubaddressLabel();
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
  @Disabled // TODO implement payment for light wallet
  public void testGetTxsWithPaymentIds() {
    /* LITE_MODE enabled */
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
  public void testExportOutputs() {
    super.testExportOutputs();
  }

  @Override
  @Disabled
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
  @Disabled
  public void testImportKeyImages() {
    super.testImportKeyImages();
  }

  @Override
  @Test
  @Disabled
  public void testGetNewKeyImagesFromLastImport() {
    super.testGetNewKeyImagesFromLastImport();
  }
  
  @SuppressWarnings("unused")
  @Test
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
  public void testSendToSelf() {
    super.testSendToSelf();
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled // TODO implement tx splitting for light wallet
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
  @Disabled
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
  @Disabled
  public void testUpdateLockedSameAccount() {
    super.testUpdateLockedSameAccount();
  }

  @Override
  @Test
  @Disabled
  public void testUpdateLockedSameAccountSplit() {
    super.testUpdateLockedSameAccountSplit();
  }

  @Override
  @Test
  @Disabled
  public void testUpdateLockedDifferentAccounts() {
    super.testUpdateLockedDifferentAccounts();
  }

  @Override
  @Test
  @Disabled
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
  @Disabled
  public void testNotificationsDifferentWallet() {
    super.testNotificationsDifferentWallet();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled
  public void testNotificationsDifferentWalletWhenRelayed() {
    super.testNotificationsDifferentWalletWhenRelayed();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled
  public void testNotificationsDifferentAccounts() {
    super.testNotificationsDifferentAccounts();
  }
  
  @Override
  @Test
  @Disabled
  @Tag("NotificationTest")
  public void testNotificationsSameAccount() {
    super.testNotificationsSameAccount();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled
  public void testNotificationsDifferentAccountSweepOutput() {
    super.testNotificationsDifferentAccountSweepOutput();
  }
  
  @Override
  @Test
  @Tag("NotificationTest")
  @Disabled
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
  @Disabled
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
  public void testGetDefaultFeePriority() {
    super.testGetDefaultFeePriority();
  }

  @Override
  @Test
  public void testGetSeedLanguage() {
    super.testGetSeedLanguage();
  }

  @Override
  @Test
  public void testGetSubaddressAddressOutOfRange() {
    super.testGetSubaddressAddressOutOfRange();
  }

}
