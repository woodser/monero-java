package test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import common.utils.GenUtils;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroMultisigInfo;
import monero.wallet.model.MoneroMultisigInitResult;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import utils.StartMining;
import utils.TestUtils;
import utils.WalletEqualityUtils;
import utils.WalletSyncPrinter;

/**
 * Tests specific to the fully client-side wallet.
 */
@TestInstance(Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions
public class TestMoneroWalletFull extends TestMoneroWalletCommon {
  
  public static boolean FULL_TESTS_RUN = false;

  public TestMoneroWalletFull() {
    super();
  }
  
  @Override
  @BeforeAll
  public void beforeAll() {
    super.beforeAll();
  }
  
  @Override
  @AfterAll
  public void afterAll() {
    super.afterAll();
    FULL_TESTS_RUN = true;
  }

  @Override
  protected MoneroWallet getTestWallet() {
    return TestUtils.getWalletFull();
  }
  
  @Override
  protected MoneroWalletFull openWallet(MoneroWalletConfig config) {
    return openWallet(config, true);
  }
  
  protected MoneroWalletFull openWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getServerUri() == null) config.setServer(daemon.getRpcConnection());
    
    // open wallet
    MoneroWalletFull wallet = MoneroWalletFull.openWallet(config);
    if (startSyncing != false && wallet.isConnected()) wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    return wallet;
  }
  
  @Override
  protected MoneroWalletFull createWallet(MoneroWalletConfig config) {
    return createWallet(config, true);
  }
  
  protected MoneroWalletFull createWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getMnemonic() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getServerUri() == null) config.setServer(daemon.getRpcConnection());
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
    // create wallet
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(config);
    if (!random) assertEquals(config.getRestoreHeight() == null ? 0l : config.getRestoreHeight(), wallet.getSyncHeight());
    if (startSyncing != false && wallet.isConnected()) wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    return wallet;
  }
  
  @Override
  public void closeWallet(MoneroWallet wallet, boolean save) {
    wallet.close(save);
  }
  
  @Override
  protected List<String> getMnemonicLanguages() {
    return MoneroWalletFull.getMnemonicLanguages();
  }

  // --------------- DEMONSTRATION OF MONERO PROJECT ISSUES ----------------------
  
  /**
   * This test demonstrates that importing key images erases incoming transfers.
   */
  //@Disabled // TODO monero-project: fix this https://github.com/monero-project/monero/issues/5812
  @Test
  public void testImportKeyImagesAndTransfers() {
    MoneroWalletFull wallet = null; // create a wallet for this test since it becomes corrupt TODO: use common wallet and move to common tests when fixed
    try {
      
      // create and sync a new wallet
      String path = getRandomWalletPath();
      wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      wallet.sync();
      
      // repeatedly export and re-import key images and test transfer size
      for (int i = 0; i < 3; i++) {
        
        // get incoming transfers before importing
        List<MoneroTransfer> inTransfers1 = wallet.getTransfers(new MoneroTransferQuery().setIsIncoming(true));
        
        // export and re-import key images
        List<MoneroKeyImage> keyImages = wallet.exportKeyImages();
        wallet.importKeyImages(keyImages);
        
        // get incoming transfers after importing
        List<MoneroTransfer> inTransfers2 = wallet.getTransfers(new MoneroTransferQuery().setIsIncoming(true));
        
        // incoming transfers should be equal
        assertEquals(inTransfers1.size(), inTransfers2.size());
        assertEquals(inTransfers1, inTransfers2);
      }
    } finally {
      wallet.close();
    }
  }
  
  /**
   * Test the daemon's ability to not hang from wallets which are continuously
   * syncing, have registered listeners, and which are not closed.
   */
  @Test
  //@Disabled // TODO monero-project: disabled because observing memory leak behavior when all tests run together
  public void testCreateWalletsWithoutClose() {
    
    // lets make some wallets and then go away
    for (int i = 0; i < 20; i++) {
      String path = getRandomWalletPath();
      MoneroWalletFull willLeaveYouHanging = createWallet(new MoneroWalletConfig().setPath(path));
      willLeaveYouHanging.startSyncing();
      willLeaveYouHanging.addListener(new MoneroWalletListener());  // listen for wallet events which could aggrevate hanging
    }
    
    // check in on the daemon
    daemon.getHeight();
    
    // start mining
    try { StartMining.startMining(); }
    catch (MoneroError e) { }
    
    // wait for a block
    daemon.waitForNextBlockHeader();
    
    // stop mining
    try { daemon.stopMining(); }
    catch (MoneroError e) { }
    
    // check in on the daemon
    daemon.getHeight();
    
    // wallet's intentionally not closed (daemon da man)
  }
  
  // ------------------------------- BEGIN TESTS ------------------------------
  
  // Can get the daemon's height
  @Test
  public void testDaemon() {
    assumeTrue(TEST_NON_RELAYS);
    assertTrue(wallet.isConnected());
    long daemonHeight = wallet.getDaemonHeight();
    assertTrue(daemonHeight > 0);
  }
  
  // Can get the daemon's max peer height
  @Test
  public void testGetDaemonMaxPeerHeight() {
    assumeTrue(TEST_NON_RELAYS);
    long height = ((MoneroWalletFull) wallet).getDaemonMaxPeerHeight();
    assertTrue(height > 0);
  }
  
//  @Test
//  public void getApproximateChainHeight() {
//    long height = wallet.getApproximateChainHeight();
//    assertTrue(height > 0);
//  }
  
  // Can set the daemon connection
  @Test
  public void testSetDaemonConnection() {
    
    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setServerUri(""));
    assertEquals(null, wallet.getDaemonConnection());
    
    // set daemon uri
    wallet.setDaemonConnection(TestUtils.DAEMON_RPC_URI);
    assertEquals(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI), wallet.getDaemonConnection());
    wallet.setDaemonConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD);
    assertTrue(wallet.isConnected());
    
    // nullify daemon connection
    wallet.setDaemonConnection((String) null);
    assertEquals(null, wallet.getDaemonConnection());
    wallet.setDaemonConnection(TestUtils.DAEMON_RPC_URI);
    assertEquals(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI), wallet.getDaemonConnection());
    wallet.setDaemonConnection((MoneroRpcConnection) null);
    assertEquals(null, wallet.getDaemonConnection());
    
    // set daemon uri to non-daemon
    wallet.setDaemonConnection("www.getmonero.org");
    assertEquals(new MoneroRpcConnection("www.getmonero.org"), wallet.getDaemonConnection());
    assertFalse(wallet.isConnected());
    
    // set daemon to invalid uri
    wallet.setDaemonConnection("abc123");
    assertFalse(wallet.isConnected());
    
    // attempt to sync
    try {
      wallet.sync();
      fail("Exception expected");
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    } finally {
      wallet.close();
    }
  }
  
  // Can create a random full wallet
  @Test
  public void testCreateWalletRandomFull() {
    assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setNetworkType(MoneroNetworkType.MAINNET).setServerUri(""));
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), MoneroNetworkType.MAINNET);
    assertEquals(MoneroNetworkType.MAINNET, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.isConnected());
    assertEquals("English", wallet.getMnemonicLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why does height of new unsynced wallet start at 1?
    assertTrue(wallet.getSyncHeight() >= 0);
    
    // cannot get daemon chain height
    try {
      wallet.getDaemonHeight();
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    }
    
    // set daemon connection and check chain height
    wallet.setDaemonConnection(daemon.getRpcConnection());
    assertEquals(daemon.getHeight(), wallet.getDaemonHeight());
    
    // close wallet which releases resources
    wallet.close();

    // create random wallet with non defaults
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setNetworkType(MoneroNetworkType.TESTNET).setLanguage("Spanish"), false);
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), MoneroNetworkType.TESTNET);
    assertEquals(MoneroNetworkType.TESTNET, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnected());
    assertEquals("Spanish", wallet.getMnemonicLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why is height of unsynced wallet 1?
    if (daemon.isConnected()) assertEquals(daemon.getHeight(), wallet.getSyncHeight());
    else assertTrue(wallet.getSyncHeight() >= 0);
    wallet.close();
  }
  
  // Can create a full wallet from mnemonic
  @Test
  public void testCreateWalletFromMnemonicFull() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create unconnected wallet with mnemonic
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setServerUri(""));
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.isConnected());
    assertEquals("English", wallet.getMnemonicLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getSyncHeight());
    try { wallet.startSyncing(); } catch (MoneroError e) { assertEquals("Wallet is not connected to daemon", e.getMessage()); }
    wallet.close();
    
    // create wallet without restore height
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC), false);
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnected());
    assertEquals("English", wallet.getMnemonicLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getSyncHeight()); // TODO: restore height is lost after closing only in JNI
    wallet.close();
    
    // create wallet with mnemonic, no connection, and restore height
    long restoreHeight = 10000;
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight).setServerUri(""));
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNull(wallet.getDaemonConnection());
    assertFalse(wallet.isConnected());
    assertEquals("English", wallet.getMnemonicLanguage());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getSyncHeight());
    assertEquals(path, wallet.getPath());
    wallet.close(true);
    wallet = openWallet(new MoneroWalletConfig().setPath(path).setServerUri(""));
    assertFalse(wallet.isConnected());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getSyncHeight()); // restore height is lost after closing
    wallet.close();

    // create wallet with mnemonic, connection, and restore height
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnected());
    assertEquals("English", wallet.getMnemonicLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getSyncHeight());
    wallet.close();
  }
  
  // Can create a full wallet from keys
  @Test
  public void testCreateWalletFromKeysJni() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletFull walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    try {
      assertEquals(wallet.getMnemonic(), walletKeys.getMnemonic());
      assertEquals(wallet.getPrimaryAddress(), walletKeys.getPrimaryAddress());
      assertEquals(wallet.getPrivateViewKey(), walletKeys.getPrivateViewKey());
      assertEquals(wallet.getPublicViewKey(), walletKeys.getPublicViewKey());
      assertEquals(wallet.getPrivateSpendKey(), walletKeys.getPrivateSpendKey());
      assertEquals(wallet.getPublicSpendKey(), walletKeys.getPublicSpendKey());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletKeys.getSyncHeight());
      assertTrue(walletKeys.isConnected());
      assertFalse(walletKeys.isSynced());
    } finally {
      walletKeys.close();
    }
  }
  
  // Is compatible with monero-wallet-rpc wallet files
  @Test
  public void testWalletFileCompatibility() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create wallet using monero-wallet-rpc
    String walletName = GenUtils.getUUID();
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    walletRpc.createWallet(new MoneroWalletConfig().setPath(walletName).setPassword(TestUtils.WALLET_PASSWORD).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    walletRpc.sync();
    BigInteger balance = walletRpc.getBalance();
    String outputsHex = walletRpc.exportOutputs();
    walletRpc.close(true);
    
    // open as full wallet
    MoneroWalletFull walletFull = MoneroWalletFull.openWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/" + walletName).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.sync();
    assertEquals(TestUtils.MNEMONIC, walletFull.getMnemonic());
    assertEquals(TestUtils.ADDRESS, walletFull.getPrimaryAddress());
    assertEquals(balance, walletFull.getBalance());
    assertEquals(outputsHex.length(), walletFull.exportOutputs().length());
    walletFull.close(true);
    
    // create full wallet
    walletName = GenUtils.getUUID();
    walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/" + walletName).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.sync();
    balance = walletFull.getBalance();
    outputsHex = walletFull.exportOutputs();
    walletFull.close(true);
    
    // open wallet using monero-wallet-rpc
    walletRpc.openWallet(new MoneroWalletConfig().setPath(walletName).setPassword(TestUtils.WALLET_PASSWORD));
    walletRpc.sync();
    assertEquals(TestUtils.MNEMONIC, walletRpc.getMnemonic());
    assertEquals(TestUtils.ADDRESS, walletRpc.getPrimaryAddress());
    assertEquals(balance, walletRpc.getBalance());
    assertEquals(outputsHex.length(), walletRpc.exportOutputs().length());
    walletRpc.close(true);
  }
  
  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
  public void testViewOnlyAndOfflineWalletCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only wallet in wallet rpc process
    MoneroWalletRpc viewOnlyWallet = TestUtils.startWalletRpcProcess();
    viewOnlyWallet.createWallet(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    
    // create offline full wallet
    MoneroWalletFull offlineWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri("").setRestoreHeight(0l));
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      TestUtils.stopWalletRpcProcess(viewOnlyWallet);
      closeWallet(offlineWallet);
    }
  };
  
  // Is compatible with monero-wallet-rpc multisig wallets
  @Test
  public void testMultisigCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE);
    
    // create participants with full wallet and monero-wallet-rpc
    List<MoneroWallet> participants = new ArrayList<MoneroWallet>();
    participants.add(TestUtils.startWalletRpcProcess().createWallet(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD)));
    participants.add(TestUtils.startWalletRpcProcess().createWallet(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD)));
    participants.add(createWallet(new MoneroWalletConfig()));
    
    // test multisig
    try {
      testMultisigParticipants(participants, 3, 3, true);
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
      
      // save and close participants
      TestUtils.stopWalletRpcProcess((MoneroWalletRpc) participants.get(0));
      TestUtils.stopWalletRpcProcess((MoneroWalletRpc) participants.get(1));
      closeWallet(participants.get(2), true);
    }
  };
  
  // Can re-sync an existing wallet from scratch
  @Test
  @Disabled // TODO monero-project: cannot re-sync from lower block height after wallet saved
  public void testResyncExisting() {
    assertTrue(MoneroWalletFull.walletExists(TestUtils.WALLET_FULL_PATH));
    MoneroWalletFull wallet = openWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_FULL_PATH).setServerUri(""), false);
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    //long startHeight = TestUtils.TEST_RESTORE_HEIGHT;
    long startHeight = 0;
    SyncProgressTester progressTester = new SyncProgressTester(wallet, startHeight, wallet.getDaemonHeight());
    wallet.setSyncHeight(1);
    MoneroSyncResult result = wallet.sync(1l, progressTester);
    progressTester.onDone(wallet.getDaemonHeight());
    
    // test result after syncing
    assertTrue(wallet.isConnected());
    assertTrue(wallet.isSynced());
    assertEquals(wallet.getDaemonHeight() - startHeight, (long) result.getNumBlocksFetched());
    assertTrue(result.getReceivedMoney());
    assertEquals(daemon.getHeight(), wallet.getHeight());
    wallet.close(true);
  }

  // Can sync a wallet with a randomly generated seed
  @Test
  public void testSyncRandom() {
    assumeTrue(TEST_NON_RELAYS);
    assertTrue(daemon.isConnected(), "Not connected to daemon");

    // create test wallet
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig(), false);
    long restoreHeight = daemon.getHeight();

    // test wallet's height before syncing
    assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
    assertEquals(restoreHeight, wallet.getDaemonHeight());
    assertTrue(wallet.isConnected());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(restoreHeight, wallet.getSyncHeight());
    assertEquals(daemon.getHeight(), wallet.getDaemonHeight());

    // sync the wallet
    SyncProgressTester progressTester = new SyncProgressTester(wallet, wallet.getSyncHeight(), wallet.getDaemonHeight());
    MoneroSyncResult result = wallet.sync(null, progressTester);
    progressTester.onDone(wallet.getDaemonHeight());
    
    // test result after syncing
    MoneroWalletFull walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getMnemonic(), restoreHeight);
    walletGt.sync();
    try {
      assertTrue(wallet.isConnected());
      assertTrue(wallet.isSynced());
      assertEquals(0, (long) result.getNumBlocksFetched());
      assertFalse(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), wallet.getHeight());

      // sync the wallet with default params
      wallet.sync();
      assertTrue(wallet.isSynced());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      
      // compare wallet to ground truth
      testWalletEqualityOnChain(walletGt, wallet);
    } finally {
      if (walletGt != null) walletGt.close(true);
      wallet.close();
    }
    
    // attempt to sync unconnected wallet
    wallet = createWallet(new MoneroWalletConfig().setServerUri(""));
    try {
      wallet.sync();
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    } finally {
      wallet.close();
    }
  }
  
  // Can sync a wallet created from mnemonic from the genesis
  @Test
  public void testSyncMnemonicFromGenesis() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(null, null, true, false);
  }
  
  // Can sync a wallet created from mnemonic from a restore height
  @Test
  public void testSyncMnemonicFromRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(null, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  // Can sync a wallet created from mnemonic from a start height
  @Test
  public void testSyncMnemonicFromStartHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(TestUtils.FIRST_RECEIVE_HEIGHT, null, false, true);
  }
  
  // Can sync a wallet created from mnemonic from a start height less than the restore height
  @Test
  public void testSyncMnemonicStartHeightLTRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(TestUtils.FIRST_RECEIVE_HEIGHT, TestUtils.FIRST_RECEIVE_HEIGHT + 3l);
  }
  
  // Can sync a wallet created from mnemonic from a start height greater than the restore height
  @Test
  public void testSyncMnemonicStartHeightGTRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(TestUtils.FIRST_RECEIVE_HEIGHT + 3l, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  private void testSyncMnemonic(Long startHeight, Long restoreHeight) { testSyncMnemonic(startHeight, restoreHeight, false, false); }
  private void testSyncMnemonic(Long startHeight, Long restoreHeight, boolean skipGtComparison, boolean testPostSyncNotifications) {
    assertTrue(daemon.isConnected(), "Not connected to daemon");
    if (startHeight != null && restoreHeight != null) assertTrue(startHeight <= TestUtils.FIRST_RECEIVE_HEIGHT || restoreHeight <= TestUtils.FIRST_RECEIVE_HEIGHT);
    
    // create wallet from mnemonic
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    
    // sanitize expected sync bounds
    if (restoreHeight == null) restoreHeight = 0l;
    long startHeightExpected = startHeight == null ? restoreHeight : startHeight;
    if (startHeightExpected == 0) startHeightExpected = 1;
    long endHeightExpected = wallet.getDaemonMaxPeerHeight();
    
    // test wallet and close as final step
    MoneroWalletFull walletGt = null;
    try {
      
      // test wallet's height before syncing
      assertTrue(wallet.isConnected());
      assertFalse(wallet.isSynced());
      assertEquals(1, wallet.getHeight());
      assertEquals((long) restoreHeight, wallet.getSyncHeight());
      
      // register a wallet listener which tests notifications throughout the sync
      WalletSyncTester walletSyncTester = new WalletSyncTester(wallet, startHeightExpected, endHeightExpected);
      wallet.addListener(walletSyncTester);
      
      // sync the wallet with a listener which tests sync notifications
      SyncProgressTester progressTester = new SyncProgressTester(wallet, startHeightExpected, endHeightExpected);
      MoneroSyncResult result = wallet.sync(startHeight, progressTester);
      
      // test completion of the wallet and sync listeners
      progressTester.onDone(wallet.getDaemonHeight());
      walletSyncTester.onDone(wallet.getDaemonHeight());
      
      // test result after syncing
      assertTrue(wallet.isSynced());
      assertEquals(wallet.getDaemonHeight() - startHeightExpected, (long) result.getNumBlocksFetched());
      assertTrue(result.getReceivedMoney());
      if (wallet.getHeight() != daemon.getHeight()) System.out.println("WARNING: wallet height " + wallet.getHeight() + " is not synced with daemon height " + daemon.getHeight());  // TODO: height may not be same after long sync
      assertEquals(daemon.getHeight(), wallet.getDaemonHeight(), "Daemon heights are not equal: " + wallet.getDaemonHeight() + " vs " + daemon.getHeight());
      if (startHeightExpected > TestUtils.FIRST_RECEIVE_HEIGHT) assertTrue(wallet.getTxs().get(0).getHeight() > TestUtils.FIRST_RECEIVE_HEIGHT);  // wallet is partially synced so first tx happens after true restore height
      else assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, (long) wallet.getTxs().get(0).getHeight());  // wallet should be fully synced so first tx happens on true restore height
      
      // sync the wallet with default params
      result = wallet.sync();
      assertTrue(wallet.isSynced());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      assertTrue(result.getNumBlocksFetched() == 0 || result.getNumBlocksFetched() == 1);  // block might be added to chain
      assertFalse(result.getReceivedMoney());
      
      // compare with ground truth
      if (!skipGtComparison) {
        walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getMnemonic(), startHeightExpected);
        testWalletEqualityOnChain(walletGt, wallet);
      }
      
      // if testing post-sync notifications, wait for a block to be added to the chain
      // then test that sync arg listener was not invoked and registered wallet listener was invoked
      if (testPostSyncNotifications) {
        
        // start automatic syncing
        wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
        
        // attempt to start mining to push the network along  // TODO: TestUtils.tryStartMining() : reqId, TestUtils.tryStopMining(reqId)
        boolean startedMining = false;
        try {
          StartMining.startMining();
          startedMining = true;
        } catch (Exception e) {
          // no problem
        }
        
        try {
          
          // wait for block
          System.out.println("Waiting for next block to test post sync notifications");
          daemon.waitForNextBlockHeader();
          
          // ensure wallet has time to detect new block
          try {
            TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS); // sleep until sync
          } catch (InterruptedException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
          }
          
          // test that wallet listener's onSyncProgress() and onNewBlock() were invoked after previous completion
          assertTrue(walletSyncTester.getOnSyncProgressAfterDone());
          assertTrue(walletSyncTester.getOnNewBlockAfterDone());
        } finally {
          if (startedMining) wallet.stopMining();
        }
      }
    } finally {
      if (walletGt != null) walletGt.close(true);
      wallet.close();
    }
  }
  
  // Can sync a wallet created from keys
  @Test
  public void testSyncWalletFromKeys() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletFull walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT), false);
    
    // create ground truth wallet for comparison
    MoneroWalletFull walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.MNEMONIC, TestUtils.FIRST_RECEIVE_HEIGHT);
    
    // test wallet and close as final step
    try {
      assertEquals(walletKeys.getMnemonic(), walletGt.getMnemonic());
      assertEquals(walletKeys.getPrimaryAddress(), walletGt.getPrimaryAddress());
      assertEquals(walletKeys.getPrivateViewKey(), walletGt.getPrivateViewKey());
      assertEquals(walletKeys.getPublicViewKey(), walletGt.getPublicViewKey());
      assertEquals(walletKeys.getPrivateSpendKey(), walletGt.getPrivateSpendKey());
      assertEquals(walletKeys.getPublicSpendKey(), walletGt.getPublicSpendKey());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletGt.getSyncHeight());
      assertTrue(walletKeys.isConnected());
      assertFalse(walletKeys.isSynced());
      
      // sync the wallet
      SyncProgressTester progressTester = new SyncProgressTester(walletKeys, TestUtils.FIRST_RECEIVE_HEIGHT, walletKeys.getDaemonMaxPeerHeight());
      MoneroSyncResult result = walletKeys.sync(progressTester);
      progressTester.onDone(walletKeys.getDaemonHeight());
      
      // test result after syncing
      assertTrue(walletKeys.isSynced());
      assertEquals(walletKeys.getDaemonHeight() - TestUtils.FIRST_RECEIVE_HEIGHT, (long) result.getNumBlocksFetched());
      assertTrue(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), walletKeys.getHeight());
      assertEquals(daemon.getHeight(), walletKeys.getDaemonHeight());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, (long) walletKeys.getTxs().get(0).getHeight());  // wallet should be fully synced so first tx happens on true restore height
      
      // compare with ground truth
      testWalletEqualityOnChain(walletGt, walletKeys);
    } finally {
      walletGt.close(true);
      walletKeys.close();
    }
    
    // TODO monero-project: importing key images can cause erasure of incoming transfers per wallet2.cpp:11957 which causes this test to fail
//  // sync the wallets until same height
//  while (wallet.getHeight() != walletKeys.getHeight()) {
//    wallet.sync();
//    walletKeys.sync(new WalletSyncPrinter());
//  }
//
//  List<MoneroKeyImage> keyImages = walletKeys.exportKeyImages();
//  walletKeys.importKeyImages(keyImages);
  }
  
  // TODO: test start syncing, notification of syncs happening, stop syncing, no notifications, etc
  // Can start and stop syncing
  @Test
  public void testStartStopSyncing() {
    assumeTrue(TEST_NON_RELAYS);
    
    // test unconnected wallet
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setServerUri(""));
    try {
      assertNotNull(wallet.getMnemonic());
      assertEquals(1, wallet.getHeight());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      wallet.startSyncing();
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    } finally {
      wallet.close();
    }
    
    // test connecting wallet
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setServerUri(""));
    try {
      assertNotNull(wallet.getMnemonic());
      wallet.setDaemonConnection(daemon.getRpcConnection());
      assertEquals(1, wallet.getHeight());
      assertFalse(wallet.isSynced());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      long chainHeight = wallet.getDaemonHeight();
      wallet.setSyncHeight(chainHeight - 3);
      wallet.startSyncing();
      assertEquals(chainHeight - 3, wallet.getSyncHeight());
      assertEquals(daemon.getRpcConnection(), wallet.getDaemonConnection());
      wallet.stopSyncing();
      wallet.sync();
      wallet.stopSyncing();
      wallet.stopSyncing();
    } finally {
      wallet.close();
    }
    
    // test that sync starts automatically
    long restoreHeight = daemon.getHeight() - 100;
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    try {
      
      // start syncing
      assertEquals(1, wallet.getHeight());
      assertEquals(restoreHeight, wallet.getSyncHeight());
      assertFalse(wallet.isSynced());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      
      // pause for sync to start
      try {
        System.out.println("Sleeping to test that sync starts automatically...");
        TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS + 1000);
      } catch (InterruptedException e) {
        e.printStackTrace();
        throw new RuntimeException(e.getMessage());
      }
      
      // test that wallet has started syncing
      assertTrue(wallet.getHeight() > 1);
      
      // stop syncing
      wallet.stopSyncing();
      
   // TODO monero-project: wallet.cpp m_synchronized only ever set to true, never false
//      // wait for block to be added to chain
//      daemon.getNextBlockHeader();
//
//      // wallet is no longer synced
//      assertFalse(wallet.isSynced());
    } finally {
      wallet.close();
    }
  }
  
  // Does not interfere with other wallet notifications
  @Test
  public void testWalletsDoNotInterfere() {
    
    // create 2 wallets with a recent restore height
    long height = daemon.getHeight();
    long restoreHeight = height - 5;
    MoneroWalletFull wallet1 = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    MoneroWalletFull wallet2 = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    
    // track notifications of each wallet
    SyncProgressTester tester1 = new SyncProgressTester(wallet1, restoreHeight, height);
    SyncProgressTester tester2 = new SyncProgressTester(wallet1, restoreHeight, height);
    wallet1.addListener(tester1);
    wallet2.addListener(tester2);
    
    // sync first wallet and test that 2nd is not notified
    wallet1.sync();
    assertTrue(tester1.isNotified());
    assertFalse(tester2.isNotified());
    
    // sync 2nd wallet and test that 1st is not notified
    SyncProgressTester tester3 = new SyncProgressTester(wallet1, restoreHeight, height);
    wallet1.addListener(tester3);
    wallet2.sync();
    assertTrue(tester2.isNotified());
    assertFalse(tester3.isNotified());
  }
  
  // Is equal to the RPC wallet.
  @Test
  public void testWalletEqualityRpc() {
    WalletEqualityUtils.testWalletEqualityOnChain(TestUtils.getWalletRpc(), wallet);
  }
  
  // Is equal to the RPC wallet with a seed offset
  @Test
  public void testWalletEqualityRpcWithOffset() {
    
    // use common offset to compare wallet implementations
    String seedOffset = "my super secret offset!";
    
    // create rpc wallet with offset
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    walletRpc.createWallet(new MoneroWalletConfig().setPath(UUID.randomUUID().toString()).setPassword(TestUtils.WALLET_PASSWORD).setMnemonic(walletRpc.getMnemonic()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setSeedOffset(seedOffset));
    
    // create full wallet with offset
    MoneroWalletFull walletFull = createWallet(new MoneroWalletConfig()
            .setPath(getRandomWalletPath())
            .setPassword(TestUtils.WALLET_PASSWORD)
            .setNetworkType(TestUtils.NETWORK_TYPE)
            .setMnemonic(TestUtils.MNEMONIC)
            .setServer(TestUtils.getDaemonRpc().getRpcConnection())
            .setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT)
            .setSeedOffset(seedOffset));
    
    // deep compare
    try {
      WalletEqualityUtils.testWalletEqualityOnChain(walletRpc, walletFull);
    } finally {
      walletFull.close();
    }
  }
  
  // Can be saved
  @Test
  public void testSave() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create unique path for new test wallet
    String path = getRandomWalletPath();
    
    // wallet does not exist
    assertFalse(MoneroWalletFull.walletExists(path));
    
    // cannot open non-existant wallet
    try {
      openWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE));
      fail("Cannot open non-existant wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
    }
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight).setServerUri(""));
    
    // test wallet at newly created state
    try {
      
      assertTrue(MoneroWalletFull.walletExists(path));
      assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertNull(wallet.getDaemonConnection());
      assertEquals(restoreHeight, wallet.getSyncHeight());
      assertEquals("English", wallet.getMnemonicLanguage());
      assertEquals(1, wallet.getHeight());
      assertEquals(restoreHeight, wallet.getSyncHeight());
      
      // set the wallet's connection and sync
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
      wallet.sync();
      assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
      
      // close the wallet without saving
      wallet.close();
      
      // re-open the wallet
      wallet = openWallet(new MoneroWalletConfig().setPath(path).setServerUri(""));
      
      // test wallet is at newly created state
      assertTrue(MoneroWalletFull.walletExists(path));
      assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertNull(wallet.getDaemonConnection());
      assertFalse(wallet.isConnected());
      assertEquals("English", wallet.getMnemonicLanguage());
      assertFalse(wallet.isSynced());
      assertEquals(1, wallet.getHeight());
      assertEquals(0, wallet.getSyncHeight()); // TODO monero-project: restoreHeight is reset to 0 after closing
      
      // set the wallet's connection and sync
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
      assertTrue(wallet.isConnected());
      wallet.setSyncHeight(restoreHeight);
      wallet.sync();
      assertTrue(wallet.isSynced());
      assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
      long prevHeight = wallet.getHeight();
      
      // save and close the wallet
      wallet.save();
      wallet.close();
      
      // re-open the wallet
      wallet = openWallet(new MoneroWalletConfig().setPath(path).setServerUri(""));
      
      // test wallet state is saved
      assertFalse(wallet.isConnected());
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());  // TODO monero-project: daemon connection not stored in wallet files so must be explicitly set each time
      assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
      assertTrue(wallet.isConnected());
      assertEquals(prevHeight, wallet.getHeight());
      assertEquals(0, wallet.getSyncHeight()); // TODO monero-project: restoreHeight is reset to 0 after closing
      assertTrue(MoneroWalletFull.walletExists(path));
      assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertEquals("English", wallet.getMnemonicLanguage());
      
      // sync
      wallet.sync();
    } finally {
      wallet.close();
    }
  }
  
  // Can be moved
  // TODO: update to be consistent with monero-javascript, file issues with wallet2 store_to()
  @Test
  public void testMoveTo() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create unique name for test wallet
    String walletName = "test_wallet_" + System.currentTimeMillis();
    String path = TestUtils.TEST_WALLETS_DIR + "/" + walletName;
    
    // wallet does not exist
    assertFalse(MoneroWalletFull.walletExists(path));
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight).setServerUri(""));
    String subaddressLabel = "Move test wallet subaddress!";
    MoneroAccount account = wallet.createAccount(subaddressLabel);
    wallet.save();
    
    // wallet exists
    assertTrue(MoneroWalletFull.walletExists(path));
    
    // move wallet to a subdirectory
    String movedPath = TestUtils.TEST_WALLETS_DIR + "/moved/" + walletName;
    wallet.moveTo(movedPath, TestUtils.WALLET_PASSWORD);
    assertFalse(MoneroWalletFull.walletExists(path));
    assertTrue(MoneroWalletFull.walletExists(movedPath));
    wallet.save();
    assertFalse(MoneroWalletFull.walletExists(path));
    assertTrue(MoneroWalletFull.walletExists(movedPath));
    wallet.close();
    assertFalse(MoneroWalletFull.walletExists(path));
    assertTrue(MoneroWalletFull.walletExists(movedPath));
    
    // re-open and test wallet
    wallet = openWallet(new MoneroWalletConfig().setPath(movedPath).setServerUri(""));
    assertEquals(subaddressLabel, wallet.getSubaddress(account.getIndex(), 0).getLabel());
    
    // move wallet back
    wallet.moveTo(path, TestUtils.WALLET_PASSWORD);
    assertTrue(MoneroWalletFull.walletExists(path));
    assertFalse(MoneroWalletFull.walletExists(movedPath));
    wallet.save();
    assertTrue(MoneroWalletFull.walletExists(path));
    assertFalse(MoneroWalletFull.walletExists(movedPath));
    wallet.close();
    assertTrue(MoneroWalletFull.walletExists(path));
    assertFalse(MoneroWalletFull.walletExists(movedPath));
  }
  
  // TODO: this version assumes a wallet can be saved after creation which is not currently supported in wallet2
//  // Can save the wallet
//  @Test
//  public void testSave() {
//    assumeTrue(TEST_NON_RELAYS);
//
//    // create unique path for new test wallet
//    String path = TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + UUID.randomUUID().toString();
//
//    // wallet does not exist
//    assertFalse(MoneroWalletFull.walletExists(path));
//
//    // cannot open non-existant wallet
//    try {
//      new MoneroWalletFull(path, TestUtils.WALLET_FULL_PW, TestUtils.NETWORK_TYPE);
//      fail("Cannot open non-existant wallet");
//    } catch (MoneroException e) {
//      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
//    }
//
//    // create in-memory wallet to test (no connection, english)
//    MoneroWalletFull walletMemory = new MoneroWalletFull(TestUtils.NETWORK_TYPE, null, null);
//    assertEquals(TestUtils.NETWORK_TYPE, walletMemory.getNetworkType());
//    assertNull(walletMemory.getDaemonConnection());
//    assertEquals("English", walletMemory.getMnemonicLanguage());
//    assertEquals(1, walletMemory.getHeight());
//    //assertEquals(0, walletMemory.getChainHeight()); // TODO: this causes dylib runtime_error; test default state of unconnected wallet
//    //assertEquals(1, walletMemory.getRestoreHeight()); // TODO; new wallet() without connection but restoreHeight is checkpointed; where is that data cached?
//    // attempt to save wallet without a path which hasn't been saved before
//    try {
//      walletMemory.save();
//      fail("Must specify path to save wallet because wallet has not been previously saved");
//    } catch (MoneroException e) {
//      assertEquals("Must specify path to save wallet because wallet has not been previously saved", e.getMessage());
//    }
//
//    // save wallet to test_wallets directory
//    walletMemory.save(path, TestUtils.WALLET_FULL_PW);
//
//    // read wallet saved to disk
//    System.out.println("Attempting to read at path: " + path);
//    MoneroWalletFull walletDisk1 = new MoneroWalletFull(path, TestUtils.WALLET_FULL_PW, TestUtils.NETWORK_TYPE);
//    testJniWalletEquality(walletMemory, walletDisk1);
//
//    // sync wallet which isn't connected
//    try {
//      walletDisk1.sync();
//    } catch (MoneroException e) {
//      assertEquals("Wallet has no daemon connection", e.getMessage());
//      assertEquals(0, walletMemory.getHeight());
//      assertEquals(1, walletMemory.getRestoreHeight());
//    }
//
//    // set daemon connection
//    walletDisk1.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
//    assertNull(walletMemory.getDaemonConnection());
//
//    // save wallet to default path
//    wallet.save();
//
//    // read wallet saved to disk
//    MoneroWalletFull walletDisk2 = new MoneroWalletFull(path, TestUtils.WALLET_FULL_PW, TestUtils.NETWORK_TYPE);
//    testJniWalletEquality(walletDisk1, walletDisk2);
//
//    // sync wallet
//    long chainHeight = daemon.getHeight();
//    walletDisk2.sync();
//    assertEquals(chainHeight, walletDisk2.getHeight());
//    assertEquals(chainHeight, walletDisk2.getRestoreHeight());
//
//    // save and re-open wallet
//    walletDisk2.save();
//    MoneroWalletFull walletDisk3 = new MoneroWalletFull(path, TestUtils.WALLET_FULL_PW, TestUtils.NETWORK_TYPE);
//    testJniWalletEquality(walletDisk2, walletDisk3);
//
//    // close wallets to release (c++) resources
//    walletMemory.close();
//    walletDisk1.close();
//    walletDisk2.close();
//    walletDisk3.close();
//  }

  // Can be closed
  @Test
  public void testClose() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create a test wallet
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path));
    wallet.sync();
    assertTrue(wallet.getHeight() > 1);
    assertTrue(wallet.isSynced());
    assertFalse(wallet.isClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.isClosed());
    
    // attempt to interact with the wallet
    try { wallet.getHeight(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.getMnemonic(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.sync(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.startSyncing(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.stopSyncing(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    
    // re-open the wallet
    wallet = openWallet(new MoneroWalletConfig().setPath(path));
    wallet.sync();
    assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
    assertFalse(wallet.isClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.isClosed());
  }
  
  // Supports multisig sample code
  @Test
  public void testMultisigSample() {
    testCreateMultisigWallet(2, 2);
    testCreateMultisigWallet(2, 3);
    testCreateMultisigWallet(2, 4);
  }
  
  private void testCreateMultisigWallet(int M, int N) {
    System.out.println("Creating " + M + "/" + N + " multisig wallet");
    
    // create participating wallets
    List<MoneroWallet> wallets = new ArrayList<MoneroWallet>();
    for (int i = 0; i < N; i++) {
      wallets.add(createWallet(new MoneroWalletConfig()));
    }
    
    // prepare and collect multisig hex from each participant
    List<String> preparedMultisigHexes = new ArrayList<String>();
    for (MoneroWallet wallet : wallets) preparedMultisigHexes.add(wallet.prepareMultisig());
    
    // make each wallet multsig and collect results
    List<String> madeMultisigHexes = new ArrayList<String>();
    for (int i = 0; i < wallets.size(); i++) {
      
      // collect prepared multisig hexes from wallet's peers
      List<String> peerMultisigHexes = new ArrayList<String>();
      for (int j = 0; j < wallets.size(); j++) if (j != i) peerMultisigHexes.add(preparedMultisigHexes.get(j));
    
      // make wallet multisig and collect result hex
      MoneroMultisigInitResult result = wallets.get(i).makeMultisig(peerMultisigHexes, M, TestUtils.WALLET_PASSWORD);
      madeMultisigHexes.add(result.getMultisigHex());
    }
    
    // if wallet is not N/N, exchange multisig keys N-M times
    if (M != N) {
      List<String> multisigHexes = madeMultisigHexes;
      for (int i = 0; i < N - M; i++) {
        
        // exchange multisig keys among participants and collect results for next round if applicable
        List<String> resultMultisigHexes = new ArrayList<String>();
        for (MoneroWallet wallet : wallets) {
          
          // import the multisig hex of other participants and collect results
          MoneroMultisigInitResult result = wallet.exchangeMultisigKeys(multisigHexes, TestUtils.WALLET_PASSWORD);
          resultMultisigHexes.add(result.getMultisigHex());
        }
        
        // use resulting multisig hex for next round of exchange if applicable
        multisigHexes = resultMultisigHexes;
      }
    }
    
    // wallets are now multisig
    for (MoneroWallet wallet : wallets) {
      String primaryAddress = wallet.getAddress(0, 0);
      MoneroUtils.validateAddress(primaryAddress, TestUtils.NETWORK_TYPE);  // TODO: replace with MoneroWallet.getNetworkType() when all methods defined in interface
      MoneroMultisigInfo info = wallet.getMultisigInfo();
      assertTrue(info.isMultisig());
      assertTrue(info.isReady());
      assertEquals(M, (int) info.getThreshold());
      assertEquals(N, (int) info.getNumParticipants());
      wallet.close(true);
    }
  }
  
  // ---------------------------------- HELPERS -------------------------------
  
  public static String getRandomWalletPath() {
    return TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + System.currentTimeMillis();
  }
  
  /**
   * Internal class to test progress updates.
   */
  private class SyncProgressTester extends WalletSyncPrinter {
    
    protected MoneroWalletFull wallet;
    private Long prevHeight;
    private long startHeight;
    private long prevEndHeight;
    private Long prevCompleteHeight;
    protected boolean isDone;
    private Boolean onSyncProgressAfterDone;
    
    public SyncProgressTester(MoneroWalletFull wallet, long startHeight, long endHeight) {
      this.wallet = wallet;
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      this.startHeight = startHeight;
      this.prevEndHeight = endHeight;
      this.isDone = false;
    }
    
    @Override
    public synchronized void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
      super.onSyncProgress(height, startHeight, endHeight, percentDone, message);
      
      // registered wallet listeners will continue to get sync notifications after the wallet's initial sync
      if (isDone) {
        assertTrue(wallet.getListeners().contains(this), "Listener has completed and is not registered so should not be called again");
        onSyncProgressAfterDone = true;
      }
      
      // update tester's start height if new sync session
      if (prevCompleteHeight != null && startHeight == prevCompleteHeight) this.startHeight = startHeight;
      
      // if sync is complete, record completion height for subsequent start heights
      if (Double.compare(percentDone, 1) == 0) prevCompleteHeight = endHeight;
      
      // otherwise start height is equal to previous completion height
      else if (prevCompleteHeight != null) assertEquals((long) prevCompleteHeight, startHeight);
      
      assertTrue(endHeight > startHeight, "end height > start height");
      assertEquals(this.startHeight, startHeight);
      assertTrue(endHeight >= prevEndHeight);  // chain can grow while syncing
      prevEndHeight = endHeight;
      assertTrue(height >= startHeight);
      assertTrue(height < endHeight);
      double expectedPercentDone = (double) (height - startHeight + 1) / (double) (endHeight - startHeight);
      assertTrue(Double.compare(expectedPercentDone, percentDone) == 0);
      if (prevHeight == null) assertEquals(startHeight, height);
      else assertEquals(height, prevHeight + 1);
      prevHeight = height;
    }
    
    public void onDone(long chainHeight) {
      assertFalse(isDone);
      this.isDone = true;
      if (prevHeight == null) {
        assertNull(prevCompleteHeight);
        assertEquals(chainHeight, startHeight);
      } else {
        assertEquals(chainHeight - 1, (long) prevHeight);  // otherwise last height is chain height - 1
        assertEquals(chainHeight, (long) prevCompleteHeight);
      }
    }
    
    public Boolean isNotified() {
      return prevHeight != null;
    }
    
    public Boolean getOnSyncProgressAfterDone() {
      return onSyncProgressAfterDone;
    }
  }
  
  /**
   * Internal class to test all wallet notifications on sync.
   */
  private class WalletSyncTester extends SyncProgressTester {
    
    private Long walletTesterPrevHeight;  // renamed from prevHeight to not interfere with super's prevHeight
    private MoneroOutputWallet prevOutputReceived;
    private MoneroOutputWallet prevOutputSpent;
    private BigInteger incomingTotal;
    private BigInteger outgoingTotal;
    private Boolean onNewBlockAfterDone;
    private BigInteger prevBalance;
    private BigInteger prevUnlockedBalance;
    
    public WalletSyncTester(MoneroWalletFull wallet, long startHeight, long endHeight) {
      super(wallet, startHeight, endHeight);
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      incomingTotal = BigInteger.valueOf(0);
      outgoingTotal = BigInteger.valueOf(0);
    }
    
    @Override
    public synchronized void onNewBlock(long height) {
      if (isDone) {
        assertTrue(wallet.getListeners().contains(this), "Listener has completed and is not registered so should not be called again");
        onNewBlockAfterDone = true;
      }
      if (walletTesterPrevHeight != null) assertEquals(walletTesterPrevHeight + 1, height);
      assertTrue(height >= super.startHeight);
      walletTesterPrevHeight = height;
    }
    
    @Override
    public void onBalancesChanged(BigInteger newBalance, BigInteger newUnlockedBalance) {
      if (this.prevBalance != null) assertTrue(!newBalance.equals(this.prevBalance) || !newUnlockedBalance.equals(this.prevUnlockedBalance));
      this.prevBalance = newBalance;
      this.prevUnlockedBalance = newUnlockedBalance;
    }

    @Override
    public void onOutputReceived(MoneroOutputWallet output) {
      assertNotNull(output);
      prevOutputReceived = output;
      
      // test output
      assertNotNull(output.getAmount());
      assertTrue(output.getAccountIndex() >= 0);
      assertTrue(output.getSubaddressIndex() >= 0);
      
      // test output's tx
      assertNotNull(output.getTx());
      assertNotNull(output.getTx().getHash());
      assertEquals(64, output.getTx().getHash().length());
      assertTrue(output.getTx().getVersion() >= 0);
      assertTrue(output.getTx().getUnlockHeight() >= 0);
      assertNull(output.getTx().getInputs());
      assertEquals(1, output.getTx().getOutputs().size());
      assertTrue(output.getTx().getOutputs().get(0) == output);
      
      // extra is not sent over the jni bridge
      assertNull(output.getTx().getExtra());
      
      // add incoming amount to running total
      incomingTotal = incomingTotal.add(output.getAmount());
    }

    @Override
    public void onOutputSpent(MoneroOutputWallet output) {
      assertNotNull(output);
      prevOutputSpent = output;
      
      // test output
      assertNotNull(output.getAmount());
      assertTrue(output.getAccountIndex() >= 0);
      if (output.getSubaddressIndex() != null) assertTrue(output.getSubaddressIndex() >= 0); // TODO (monero-project): can be undefined because inputs not provided so one created from outgoing transfer
      
      // test output's tx
      assertNotNull(output.getTx());
      assertNotNull(output.getTx().getHash());
      assertEquals(64, output.getTx().getHash().length());
      assertTrue(output.getTx().getVersion() >= 0);
      assertTrue(output.getTx().getUnlockHeight() >= 0);
      assertEquals(1, output.getTx().getInputs().size());
      assertTrue(output.getTx().getInputs().get(0) == output);
      assertNull(output.getTx().getOutputs());
      
      // extra is not sent over the jni bridge
      assertNull(output.getTx().getExtra());
      
      // add outgoing amount to running total
      outgoingTotal = outgoingTotal.add(output.getAmount());
    }
    
    @Override
    public void onDone(long chainHeight) {
      super.onDone(chainHeight);
      assertNotNull(walletTesterPrevHeight);
      assertNotNull(prevOutputReceived);
      assertNotNull(prevOutputSpent);
      BigInteger balance = incomingTotal.subtract(outgoingTotal);
      assertEquals(balance, wallet.getBalance());
      assertEquals(prevBalance, wallet.getBalance());
      assertEquals(prevUnlockedBalance, wallet.getUnlockedBalance());
    }
    
    public Boolean getOnNewBlockAfterDone() {
      return onNewBlockAfterDone;
    }
  }
  
  // jni-specific tx tests
  @Override
  protected void testTxWallet(MoneroTxWallet tx, TxContext ctx) {
    if (ctx == null) ctx = new TxContext();
    
    // run common tests
    super.testTxWallet(tx, ctx);
  }
  
  // possible configuration: on chain xor local wallet data ("strict"), txs ordered same way? TBD
  private static void testWalletEqualityOnChain(MoneroWalletFull wallet1, MoneroWalletFull wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());
    //assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight()); // TODO monero-project: restore height is lost after close
    assertEquals(wallet1.getDaemonConnection(), wallet2.getDaemonConnection());
    assertEquals(wallet1.getMnemonicLanguage(), wallet2.getMnemonicLanguage());
    // TODO: more jni-specific extensions
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
