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
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroMultisigInfo;
import monero.wallet.model.MoneroMultisigInitResult;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import utils.Pair;
import utils.StartMining;
import utils.TestUtils;
import utils.WalletEqualityUtils;
import utils.WalletSyncPrinter;

/**
 * Tests specific to the JNI wallet.
 */
public class TestMoneroWalletJni extends TestMoneroWalletCommon {

  protected MoneroWalletJni wallet;

  public TestMoneroWalletJni() {
    this.wallet = (MoneroWalletJni) getTestWallet();
  }

  @BeforeClass
  public static void beforeClass() throws Exception {
    //Assume.assumeTrue(false); // ignore entire class
  }

  @Override
  protected MoneroWallet getTestWallet() {
    return TestUtils.getWalletJni();
  }
  
  @Override
  protected MoneroWalletJni openWallet(MoneroWalletConfig config) {
    return openWallet(config, true);
  }
  
  protected MoneroWalletJni openWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getServerUri() == null) config.setServer(daemon.getRpcConnection());
    
    // open wallet
    MoneroWalletJni wallet = MoneroWalletJni.openWallet(config);
    if (startSyncing != false && wallet.isConnected()) wallet.startSyncing();
    return wallet;
  }
  
  @Override
  protected MoneroWalletJni createWallet(MoneroWalletConfig config) {
    return createWallet(config, true);
  }
  
  protected MoneroWalletJni createWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getMnemonic() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getServerUri() == null) config.setServer(daemon.getRpcConnection());
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
    // create wallet
    MoneroWalletJni wallet = MoneroWalletJni.createWallet(config);
    if (!random) assertEquals(config.getRestoreHeight() == null ? 0l : config.getRestoreHeight(), wallet.getSyncHeight());
    if (startSyncing != false && wallet.isConnected()) wallet.startSyncing();
    return wallet;
  }
  
  @Override
  protected List<String> getMnemonicLanguages() {
    return MoneroWalletJni.getMnemonicLanguages();
  }

  // --------------- DEMONSTRATION OF MONERO CORE ISSUES ----------------------
  
  /**
   * This test demonstrates that importing key images erases incoming transfers.
   */
  @Ignore // TODO monero-core: fix this https://github.com/monero-project/monero/issues/5812
  @Test
  public void testImportKeyImagesAndTransfers() {
    MoneroWalletJni wallet = null; // create a wallet for this test since it becomes corrupt TODO: use common wallet and move to common tests when fixed
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
        List<MoneroKeyImage> keyImages = wallet.getKeyImages();
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
  @Ignore // TODO monero core: disabled because observing memory leak behavior when all tests run together
  public void testCreateWalletsWithoutClose() {
    
    // lets make some wallets and then go away
    for (int i = 0; i < 20; i++) {
      String path = getRandomWalletPath();
      MoneroWalletJni willLeaveYouHanging = createWallet(new MoneroWalletConfig().setPath(path));
      willLeaveYouHanging.startSyncing();
      willLeaveYouHanging.addListener(new MoneroWalletListener());  // listen for wallet events which could aggrevate hanging
    }
    
    // check in on the daemon
    daemon.getHeight();
    
    // start mining
    try { StartMining.startMining(); }
    catch (MoneroError e) { }
    
    // wait for a block
    daemon.getNextBlockHeader();
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertTrue(wallet.isConnected());
    long daemonHeight = wallet.getDaemonHeight();
    assertTrue(daemonHeight > 0);
  }
  
  // Can get the daemon's max peer height
  @Test
  public void testGetDaemonMaxPeerHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    long height = wallet.getDaemonMaxPeerHeight();
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
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setPath(path).setServerUri(""));
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
  
  // Can create a random JNI wallet
  @Test
  public void testCreateWalletRandomJni() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setPath(path).setNetworkType(MoneroNetworkType.MAINNET).setServerUri(""));
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), MoneroNetworkType.MAINNET);
    assertEquals(MoneroNetworkType.MAINNET, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.isConnected());
    assertEquals("English", wallet.getMnemonicLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
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
    assertEquals(1, wallet.getHeight()); // TODO monero core: why is height of unsynced wallet 1?
    if (daemon.isConnected()) assertEquals(daemon.getHeight(), wallet.getSyncHeight());
    else assertTrue(wallet.getSyncHeight() >= 0);
    wallet.close();
  }
  
  // Can create a JNI wallet from mnemonic
  @Test
  public void testCreateWalletFromMnemonicJni() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create unconnected wallet with mnemonic
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setServerUri(""));
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
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
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
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getSyncHeight());
    wallet.close();
  }
  
  // Can create a JNI wallet from keys
  @Test
  public void testCreateWalletFromKeysJni() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletJni walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
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
  
  // Can re-sync an existing wallet from scratch
  @Test
  @Ignore // TODO monero core: cannot re-sync from lower block height after wallet saved
  public void testResyncExisting() {
    assertTrue(MoneroWalletJni.walletExists(TestUtils.WALLET_JNI_PATH));
    MoneroWalletJni wallet = openWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_JNI_PATH).setServerUri(""), false);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertTrue("Not connected to daemon", daemon.isConnected());

    // create test wallet
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig(), false);
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
    MoneroWalletJni walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getMnemonic(), restoreHeight);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(null, null, true, false);
  }
  
  // Can sync a wallet created from mnemonic from a restore height
  @Test
  public void testSyncMnemonicFromRestoreHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(null, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  // Can sync a wallet created from mnemonic from a start height
  @Test
  public void testSyncMnemonicFromStartHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(TestUtils.FIRST_RECEIVE_HEIGHT, null, false, true);
  }
  
  // Can sync a wallet created from mnemonic from a start height less than the restore height
  @Test
  public void testSyncMnemonicStartHeightLTRestoreHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(TestUtils.FIRST_RECEIVE_HEIGHT, TestUtils.FIRST_RECEIVE_HEIGHT + 3l);
  }
  
  // Can sync a wallet created from mnemonic from a start height greater than the restore height
  @Test
  public void testSyncMnemonicStartHeightGTRestoreHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(TestUtils.FIRST_RECEIVE_HEIGHT + 3l, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  private void testSyncMnemonic(Long startHeight, Long restoreHeight) { testSyncMnemonic(startHeight, restoreHeight, false, false); }
  private void testSyncMnemonic(Long startHeight, Long restoreHeight, boolean skipGtComparison, boolean testPostSyncNotifications) {
    assertTrue("Not connected to daemon", daemon.isConnected());
    if (startHeight != null && restoreHeight != null) assertTrue(startHeight <= TestUtils.FIRST_RECEIVE_HEIGHT || restoreHeight <= TestUtils.FIRST_RECEIVE_HEIGHT);
    
    // create wallet from mnemonic
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    
    // sanitize expected sync bounds
    if (restoreHeight == null) restoreHeight = 0l;
    long startHeightExpected = startHeight == null ? restoreHeight : startHeight;
    if (startHeightExpected == 0) startHeightExpected = 1;
    long endHeightExpected = wallet.getDaemonMaxPeerHeight();
    
    // test wallet and close as final step
    MoneroWalletJni walletGt = null;
    try {
      
      // test wallet's height before syncing
      assertTrue(wallet.isConnected());
      assertFalse(wallet.isSynced());
      assertEquals(1, wallet.getHeight());
      assertEquals((long) restoreHeight, (long) wallet.getSyncHeight());
      
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
      assertEquals("Daemon heights are not equal: " + wallet.getDaemonHeight() + " vs " + daemon.getHeight(), daemon.getHeight(), wallet.getDaemonHeight());
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
        wallet.startSyncing();
        
        // attempt to start mining to push the network along  // TODO: TestUtils.tryStartMining() : reqId, TestUtils.tryStopMining(reqId)
        boolean startedMining = false;
        MoneroMiningStatus miningStatus = daemon.getMiningStatus();
        if (!miningStatus.isActive()) {
          try {
            wallet.startMining(7l, false, true);
            startedMining = true;
          } catch (Exception e) {
            // no problem
          }
        }
        
        try {
          
          // wait for block
          System.out.println("Waiting for next block to test post sync notifications");
          daemon.getNextBlockHeader();
          
          // ensure wallet has time to detect new block
          try {
            TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL);  // sleep for the wallet interval
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletJni walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT), false);
    
    // create ground truth wallet for comparison
    MoneroWalletJni walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.MNEMONIC, TestUtils.FIRST_RECEIVE_HEIGHT);
    
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
    
    // TODO monero core: importing key images can cause erasure of incoming transfers per wallet2.cpp:11957 which causes this test to fail
//  // sync the wallets until same height
//  while (wallet.getHeight() != walletKeys.getHeight()) {
//    wallet.sync();
//    walletKeys.sync(new WalletSyncPrinter());
//  }
//  
//  List<MoneroKeyImage> keyImages = walletKeys.getKeyImages();
//  walletKeys.importKeyImages(keyImages);
  }
  
  // TODO: test start syncing, notification of syncs happening, stop syncing, no notifications, etc
  // Can start and stop syncing
  @Test
  public void testStartStopSyncing() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test unconnected wallet
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setServerUri(""));
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
      wallet.startSyncing();
      
      // pause for sync to start
      try {
        System.out.println("Sleeping to test that sync starts automatically...");
        TimeUnit.MILLISECONDS.sleep(15000);
      } catch (InterruptedException e) {
        e.printStackTrace();
        throw new RuntimeException(e.getMessage());
      }
      
      // test that wallet has started syncing
      assertTrue(wallet.getHeight() > 1);
      
      // stop syncing
      wallet.stopSyncing();
      
   // TODO monero core: wallet.cpp m_synchronized only ever set to true, never false
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
    MoneroWalletJni wallet1 = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    MoneroWalletJni wallet2 = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight), false);
    
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
    
    // create jni wallet with offset
    MoneroWalletJni walletJni = createWallet(new MoneroWalletConfig()
            .setPath(getRandomWalletPath())
            .setPassword(TestUtils.WALLET_PASSWORD)
            .setNetworkType(TestUtils.NETWORK_TYPE)
            .setMnemonic(TestUtils.MNEMONIC)
            .setServer(TestUtils.getDaemonRpc().getRpcConnection())
            .setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT)
            .setSeedOffset(seedOffset));
    
    // deep compare
    try {
      WalletEqualityUtils.testWalletEqualityOnChain(walletRpc, walletJni);
    } finally {
      walletJni.close();
    }
  }
  
  // Can be saved
  @Test
  public void testSave() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create unique path for new test wallet
    String path = getRandomWalletPath();
    
    // wallet does not exist
    assertFalse(MoneroWalletJni.walletExists(path));
    
    // cannot open non-existant wallet
    try {
      openWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE));
      fail("Cannot open non-existant wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
    }
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight).setServerUri(""));
    
    // test wallet at newly created state
    try {
      
      assertTrue(MoneroWalletJni.walletExists(path));
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
      assertTrue(MoneroWalletJni.walletExists(path));
      assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertNull(wallet.getDaemonConnection());
      assertFalse(wallet.isConnected());
      assertEquals("English", wallet.getMnemonicLanguage());
      assertFalse(wallet.isSynced());
      assertEquals(1, wallet.getHeight());
      assertEquals(0, wallet.getSyncHeight()); // TODO monero-core: restoreHeight is reset to 0 after closing
      
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
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());  // TODO monero core: daemon connection not stored in wallet files so must be explicitly set each time
      assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
      assertTrue(wallet.isConnected());
      assertEquals(prevHeight, wallet.getHeight());
      assertEquals(0, wallet.getSyncHeight()); // TODO monero core: restoreHeight is reset to 0 after closing
      assertTrue(MoneroWalletJni.walletExists(path));
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
  @Test
  public void testMoveTo() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create unique name for test wallet
    String walletName = "test_wallet_" + System.currentTimeMillis();
    String path = TestUtils.TEST_WALLETS_DIR + "/" + walletName;
    
    // wallet does not exist
    assertFalse(MoneroWalletJni.walletExists(path));
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setPath(path).setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(restoreHeight).setServerUri(""));
    String subaddressLabel = "Move test wallet subaddress!";
    MoneroAccount account = wallet.createAccount(subaddressLabel);
    wallet.save();
    
    // wallet exists
    assertTrue(MoneroWalletJni.walletExists(path));
    
    // move wallet to a subdirectory
    String movedPath = TestUtils.TEST_WALLETS_DIR + "/moved/" + walletName;
    wallet.moveTo(movedPath, TestUtils.WALLET_PASSWORD);
    assertFalse(MoneroWalletJni.walletExists(path));
    assertFalse(MoneroWalletJni.walletExists(movedPath)); // wallet does not exist until saved
    wallet.save();
    assertFalse(MoneroWalletJni.walletExists(path));
    assertTrue(MoneroWalletJni.walletExists(movedPath));
    wallet.close();
    assertFalse(MoneroWalletJni.walletExists(path));
    assertTrue(MoneroWalletJni.walletExists(movedPath));
    
    // re-open and test wallet
    wallet = openWallet(new MoneroWalletConfig().setPath(movedPath).setServerUri(""));
    assertEquals(subaddressLabel, wallet.getSubaddress(account.getIndex(), 0).getLabel());
    
    // move wallet back
    wallet.moveTo(path, TestUtils.WALLET_PASSWORD);
    assertFalse(MoneroWalletJni.walletExists(path));  // wallet does not exist until saved
    assertFalse(MoneroWalletJni.walletExists(movedPath));
    wallet.save();
    assertTrue(MoneroWalletJni.walletExists(path));
    assertFalse(MoneroWalletJni.walletExists(movedPath));
    wallet.close();
    assertTrue(MoneroWalletJni.walletExists(path));
    assertFalse(MoneroWalletJni.walletExists(movedPath));
  }
  
  // TODO: this version assumes a wallet can be saved after creation which is not currently supported in wallet2
//  // Can save the wallet
//  @Test
//  public void testSave() {
//    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
//    
//    // create unique path for new test wallet
//    String path = TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + UUID.randomUUID().toString();
//    
//    // wallet does not exist
//    assertFalse(MoneroWalletJni.walletExists(path));
//    
//    // cannot open non-existant wallet
//    try {
//      new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
//      fail("Cannot open non-existant wallet");
//    } catch (MoneroException e) {
//      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
//    }
//    
//    // create in-memory wallet to test (no connection, english)
//    MoneroWalletJni walletMemory = new MoneroWalletJni(TestUtils.NETWORK_TYPE, null, null);
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
//    walletMemory.save(path, TestUtils.WALLET_JNI_PW);
//    
//    // read wallet saved to disk
//    System.out.println("Attempting to read at path: " + path);
//    MoneroWalletJni walletDisk1 = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
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
//    MoneroWalletJni walletDisk2 = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
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
//    MoneroWalletJni walletDisk3 = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create a test wallet
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig().setPath(path));
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
  
  // ----------------------------- NOTIFICATION TESTS -------------------------
  
  @Test
  public void testStopListening() throws InterruptedException {
    
    // create wallet and start background synchronizing
    MoneroWalletJni wallet = createWallet(new MoneroWalletConfig());
    
    // add listener
    OutputNotificationCollector listener = new OutputNotificationCollector();
    wallet.addListener(listener);
    TimeUnit.SECONDS.sleep(1);
    
    // remove listener and close
    wallet.removeListener(listener);
    wallet.close();
  }
  
  @Test
  public void testReceivesFundsWithin10Seconds() throws InterruptedException {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testReceivesFundsWithin10Seconds(false);
  }
  
  @Test
  public void testReceivesFundsWithin10SecondsSameAccount() throws InterruptedException {
    org.junit.Assume.assumeTrue(TEST_RELAYS && !LITE_MODE);
    testReceivesFundsWithin10Seconds(true);
  }
  
  private void testReceivesFundsWithin10Seconds(boolean sameAccount) throws InterruptedException {
    MoneroWalletJni sender = wallet;
    MoneroWalletJni receiver = null;
    OutputNotificationCollector receiverListener = null;
    OutputNotificationCollector senderListener = null;
    try {
      
      // assign wallet to receive funds
      receiver = sameAccount ? wallet : createWallet(new MoneroWalletConfig());
      
      // listen for sent funds
      senderListener = new OutputNotificationCollector();
      sender.addListener(senderListener);
      
      // listen for received funds
      receiverListener = new OutputNotificationCollector();
      receiver.addListener(receiverListener);
      
      // send funds
      TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(sender);
      MoneroTxWallet tx = sender.createTx(new MoneroTxConfig().setAccountIndex(0).setAddress(receiver.getPrimaryAddress()).setAmount(TestUtils.MAX_FEE).setRelay(true));
      String txHash = tx.getHash();
      sender.getTx(txHash);
      if (senderListener.getOutputsSpent().isEmpty()) System.out.println("WARNING: no notification on send");
      
      // unconfirmed funds received within 10 seconds
      TimeUnit.SECONDS.sleep(10);
      receiver.getTx(txHash);
      assertFalse("No notification of received funds within 10 seconds in " + (sameAccount ? "same account" : "different wallets"), receiverListener.getOutputsReceived().isEmpty());
      for (MoneroOutputWallet output : receiverListener.getOutputsReceived()) assertNotEquals(null, output.getTx().isConfirmed());
    } finally {
      sender.removeListener(senderListener);
      receiver.removeListener(receiverListener);
      if (!sameAccount && receiver != null) receiver.close();
    }
  }
  
  /**
   * 4 output notification tests are considered when transferring within one wallet.  // TODO: multi-wallet tests
   * 
   * 1. with local wallet data, transfer from/to same account
   * 2. with local wallet data, transfer from/to different accounts
   * 3. without local wallet data, transfer from/to same account
   * 4. without local wallet data, transfer from/to different accounts
   * 
   * For example, two wallets may be instantiated with the same mnemonic,
   * so neither is privy to the local wallet data of the other.
   */

  // Notification test #1: notifies listeners of outputs sent from/to the same account using local wallet data
  @Test
  public void testOutputNotificationsSameAccounts() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    List<String> issues = testOutputNotifications(true, false);
    if (issues == null) return;
    String msg = "testOutputNotificationsSameAccounts() generated " + issues.size() + " issues:\n" + issuesToStr(issues);
    System.out.println(msg);
    assertFalse(msg, msg.contains("ERROR:"));
  }
  
  // Notification test #2: notifies listeners of outputs sent from/to different accounts using local wallet data
  @Test
  public void testOutputNotificationsDifferentAccounts() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    List<String> issues = testOutputNotifications(false, false);
    if (issues == null) return;
    String msg = "testOutputNotificationsDifferentAccounts() generated " + issues.size() + " issues:\n" + issuesToStr(issues);
    System.out.println(msg);
    assertFalse(msg, msg.contains("ERROR:"));
  }
  
  // Notification test #3: notifies listeners of swept outputs
  @Test
  public void testOutputNotificationsSweepOutput() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    List<String> issues = testOutputNotifications(false, true);
    if (issues == null) return;
    String msg = "testOutputNotificationsSweepOutput() generated " + issues.size() + " issues:\n" + issuesToStr(issues);
    System.out.println(msg);
    assertFalse(msg, msg.contains("ERROR:"));
  }
  
  private List<String> testOutputNotifications(boolean sameAccount, boolean sweepOutput) {
    
    // collect errors and warnings
    List<String> errors = new ArrayList<String>();
    
    // wait for wallet txs in the pool in case they were sent from another wallet and therefore will not fully sync until confirmed // TODO monero core
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // get balances before for later comparison
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    
    // register a listener to collect notifications
    OutputNotificationCollector listener = new OutputNotificationCollector();
    wallet.addListener(listener);
    
    // start syncing to test automatic notifications
    wallet.startSyncing();
    
    // send tx
    MoneroTxWallet tx = null;
    int[] destinationAccounts = sameAccount ? (sweepOutput ? new int[] {0} : new int[] {0, 1, 2}) : (sweepOutput ? new int[] {1} : new int[] {1, 2, 3});
    if (sweepOutput) {
      List<MoneroOutputWallet> outputs = wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false).setIsLocked(false).setAccountIndex(0).setMinAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5"))));
      if (outputs.isEmpty()) {
        errors.add("ERROR: No outputs available to sweep");
        return errors;
      }
      tx = wallet.sweepOutput(new MoneroTxConfig()
              .setAddress(wallet.getAddress(destinationAccounts[0], 0))
              .setKeyImage(outputs.get(0).getKeyImage().getHex())
              .setRelay(true));
    } else {
      MoneroTxConfig config = new MoneroTxConfig();
      config.setAccountIndex(0);
      for (int destinationAccount : destinationAccounts) {
        config.addDestination(new MoneroDestination(wallet.getAddress(destinationAccount, 0), TestUtils.MAX_FEE));
      }
      config.setRelay(true);
      tx = wallet.createTx(config);
    }
    
    // test wallet's balance
    BigInteger balanceAfter = wallet.getBalance();
    BigInteger unlockedBalanceAfter = wallet.getUnlockedBalance();
    BigInteger balanceAfterExpected = balanceBefore.subtract(tx.getFee());  // txs sent from/to same wallet so only decrease in balance is tx fee
    if (balanceAfterExpected.compareTo(balanceAfter) != 0) errors.add("WARNING: wallet balance immediately after send expected to be " + balanceAfterExpected + " but was " + balanceAfter);
    if (unlockedBalanceBefore.compareTo(unlockedBalanceAfter) <= 0 && unlockedBalanceBefore.compareTo(new BigInteger("0")) != 0) errors.add("WARNING: Wallet unlocked balance immediately after send was expected to decrease but changed from " + unlockedBalanceBefore + " to " + unlockedBalanceAfter);
        
    // wait for wallet to send notifications
    if (listener.getOutputsSpent().isEmpty()) errors.add("WARNING: wallet does not notify listeners of outputs when tx sent directly through wallet or when refreshed from the pool; must wait for confirmation to receive notifications and have correct balance");
    try { StartMining.startMining(); } catch (Exception e) { }
    while (listener.getOutputsSpent().isEmpty()) {
      if (wallet.getTx(tx.getHash()).isFailed()) {
        try { daemon.stopMining(); } catch (Exception e) { }
        errors.add("ERROR: Tx failed in mempool: " + tx.getHash());
        return errors;
      }
      try { TimeUnit.SECONDS.sleep(5); } catch (Exception e) { throw new RuntimeException(e); }
    }
    try { daemon.stopMining(); } catch (Exception e) { }
    
    // test output notifications
    if (listener.getOutputsReceived().isEmpty()) {
      errors.add("ERROR: got " + listener.getOutputsReceived().size() + " output received notifications when at least 1 was expected");
      return errors;
    }
    if (listener.getOutputsSpent().isEmpty()) {
      errors.add("ERROR: got " + listener.getOutputsSpent().size() + " output spent notifications when at least 1 was expected");
      return errors;
    }
    
    // must receive outputs with known subaddresses and amounts
    for (int destinationAccount : destinationAccounts) {
      if (!hasOutput(listener.getOutputsReceived(), destinationAccount, 0, sweepOutput ? null : TestUtils.MAX_FEE)) {
        errors.add("ERROR: missing expected received output to subaddress [" + destinationAccount + ", 0] of amount " + TestUtils.MAX_FEE);
        return errors;
      }
    }
    
    // since sending from/to the same wallet, the net amount spent = tx fee = outputs spent - outputs received
    int numConfirmedOutputs = 0;
    BigInteger netAmount = new BigInteger("0");
    for (MoneroOutputWallet outputSpent : listener.getOutputsSpent()) netAmount = netAmount.add(outputSpent.getAmount());
    for (MoneroOutputWallet outputReceived : listener.getOutputsReceived()) {
      if (outputReceived.getTx().isConfirmed()) {
        numConfirmedOutputs++;
        netAmount = netAmount.subtract(outputReceived.getAmount());
      }
    }
    if (tx.getFee().compareTo(netAmount) != 0) {
      errors.add("WARNING: net output amount must equal tx fee: " + tx.getFee().toString() + " vs " + netAmount.toString() + " (probably received notifications from other tests)");
      return errors;
    }
    
    // receives balance notification per confirmed output
    if (listener.getBalanceNotifications().size() < numConfirmedOutputs) {
      errors.add("ERROR: expected at least one updated balance notification per confirmed output received");
    }
    
    // test wallet's balance
    balanceAfter = wallet.getBalance();
    unlockedBalanceAfter = wallet.getUnlockedBalance();
    if (balanceAfterExpected.compareTo(balanceAfter) != 0) errors.add("WARNING: Wallet balance after confirmation expected to be " + balanceAfterExpected + " but was " + balanceAfter);
    if (unlockedBalanceBefore.compareTo(unlockedBalanceAfter) <= 0 && unlockedBalanceBefore.compareTo(new BigInteger("0")) != 0) errors.add("WARNING: Wallet unlocked balance immediately after send was expected to decrease but changed from " + unlockedBalanceBefore + " to " + unlockedBalanceAfter);
    
    // remove listener
    wallet.removeListener(listener);

    // return all errors and warnings as single string
    return errors;
  }
  
  private static String issuesToStr(List<String> issues) {
    if (issues.isEmpty()) return null;
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < issues.size(); i++) {
      sb.append((i + 1) + ": " + issues.get(i));
      if (i < issues.size() - 1) sb.append('\n');
    }
    return sb.toString();
  }
  
  private static boolean hasOutput(List<MoneroOutputWallet> outputs, int accountIdx, int subaddressIdx, BigInteger amount) { // TODO: use comon filter?
    MoneroOutputQuery query = new MoneroOutputQuery().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx).setAmount(amount);
    for (MoneroOutputWallet output : outputs) {
      if (query.meetsCriteria(output)) return true;
    }
    return false;
  }
  
  // Can receive notifications when outputs are received, confirmed, and unlocked
  @Test
  public void testReceivedOutputNotifications() throws InterruptedException {
    testReceivedOutputNotificationsWithUnlockHeight(0l);
  }
  
  // Can receive notifications when outputs are received, confirmed, and unlocked with an unlock height
  @Test
  public void testReceivedOutputNotificationsWithUnlockHeight() throws InterruptedException {
    testReceivedOutputNotificationsWithUnlockHeight(13l);
  }
  
  private void testReceivedOutputNotificationsWithUnlockHeight(long unlockDelay) throws InterruptedException {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    long expectedUnlockHeight = daemon.getHeight() + unlockDelay;
    
    // create wallet to test received output notifications
    MoneroWalletJni receiver = createWallet(new MoneroWalletConfig());
    
    // create tx to transfer funds to receiver
    MoneroTxWallet tx = wallet.createTx(new MoneroTxConfig()
      .setAccountIndex(0)
      .setAddress(receiver.getPrimaryAddress())
      .setAmount(TestUtils.MAX_FEE.multiply(BigInteger.valueOf(10)))
      .setUnlockHeight(expectedUnlockHeight)
      .setRelay(false)
    );
    
    // register listener to test notifications
    ReceivedOutputNotificationTester listener = new ReceivedOutputNotificationTester(tx.getHash());
    receiver.addListener(listener);
    
    // flush tx to prevent double spends from previous tests
    daemon.flushTxPool();
    
    // relay transaction to pool
    long submitHeight = daemon.getHeight();
    MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex());
    assertTrue("Bad submit tx result: " + JsonUtils.serialize(result), result.isGood());
    
    // test notification of tx in pool within 10 seconds
    TimeUnit.SECONDS.sleep(10);
    assertNotNull(listener.lastNotifiedOutput);
    assertFalse(listener.lastNotifiedOutput.getTx().isConfirmed());
    
    // listen for new blocks to test output notifications
    receiver.addListener(new MoneroWalletListener() {
      @Override
      public void onNewBlock(long height) {
        if (listener.testComplete) return;
        new Thread(new Runnable() {
          @Override
          public void run() {
            try {
              
              // first confirmation expected within 10 seconds of new block
              if (listener.confirmedHeight == null) {
                TimeUnit.SECONDS.sleep(10);
                if (listener.confirmedHeight == null && Boolean.TRUE.equals(listener.lastNotifiedOutput.getTx().isConfirmed())) { // only run by first thread after confirmation
                  listener.confirmedHeight = listener.lastNotifiedOutput.getTx().getHeight();
                  if (listener.confirmedHeight != submitHeight) System.out.println("WARNING: tx submitted on height " + submitHeight + " but confirmed on height " + listener.confirmedHeight);  // TODO monero-core: sometimes pool tx does not confirm for several blocks
                }
              }
              
              // output should be locked until max of expected unlock height and 10 blocks after confirmation
              if ((listener.confirmedHeight == null || height < Math.max(listener.confirmedHeight + 10, expectedUnlockHeight)) && !Boolean.TRUE.equals(listener.lastNotifiedOutput.isLocked())) throw new RuntimeException("Last notified output expected to be locked but isLocked=" + listener.lastNotifiedOutput.isLocked() + " at height " + height);
              
              // test unlock notification
              if (listener.confirmedHeight != null && height == Math.max(listener.confirmedHeight + 10, expectedUnlockHeight)) {
                
                // receives notification of unlocked tx within 1 second of block notification
                System.out.println("Sleeping on height: " + height);
                TimeUnit.SECONDS.sleep(1);
                if (!Boolean.FALSE.equals(listener.lastNotifiedOutput.isLocked())) throw new RuntimeException("Last notified output expected to be unlocked but isLocked=" + listener.lastNotifiedOutput.isLocked());
                listener.unlockedSeen = true;
                listener.testComplete = true;
              }
            } catch (Exception e) {
              System.out.println("Exception!");
              e.printStackTrace();
              listener.testComplete = true;
              listener.testError = e.getMessage();
            }
          }
        }).start();
      }
    });
    
    // mine until complete
    StartMining.startMining();
    
    // run until test completes
    while (!listener.testComplete) TimeUnit.SECONDS.sleep(10);
    if (daemon.getMiningStatus().isActive()) daemon.stopMining();
    receiver.close();
    assertNull(listener.testError, listener.testError);
    assertNotNull("No notification of confirmed output", listener.confirmedHeight);
    assertTrue("No notification of output unlocked", listener.unlockedSeen);
  }
  
  private class ReceivedOutputNotificationTester extends MoneroWalletListener {

    protected String txHash;
    protected MoneroOutputWallet lastNotifiedOutput;
    protected boolean testComplete = false;
    protected String testError = null;
    protected boolean unlockedSeen = false;
    protected Long confirmedHeight = null;

    public ReceivedOutputNotificationTester(String txHash) {
      this.txHash = txHash;
    }
    
    @Override
    public void onOutputReceived(MoneroOutputWallet output) {
      if (output.getTx().getHash().equals(txHash)) lastNotifiedOutput = output;
    }
  }
  
  // Can be created and receive funds
  @Test
  public void testCreateAndReceive() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    MoneroWalletJni myWallet = null;
    try {
      
      // create a random stagenet wallet
      String path = getRandomWalletPath();
      myWallet = createWallet(new MoneroWalletConfig().setPath(path));
      myWallet.startSyncing();
      
      // listen for received outputs
      OutputNotificationCollector myListener = new OutputNotificationCollector();
      myWallet.addListener(myListener);
      
      // send funds to the created wallet
      TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
      MoneroTxWallet sentTx = wallet.createTx(new MoneroTxConfig().setAccountIndex(0).setAddress(myWallet.getPrimaryAddress()).setAmount(TestUtils.MAX_FEE).setRelay(true));
      
      // wait for funds to confirm
      try { StartMining.startMining(); } catch (Exception e) { }
      while (!(wallet.getTx(sentTx.getHash())).isConfirmed()) {
        if (wallet.getTx(sentTx.getHash()).isFailed()) throw new Error("Tx failed in mempool: " + sentTx.getHash());
        daemon.getNextBlockHeader();
      }
      
      // created wallet should have notified listeners of received outputs
      assertFalse(myListener.getOutputsReceived().isEmpty());
    } finally {
      try { daemon.stopMining(); } catch (Exception e) { }
      myWallet.close();
    }
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
  
  /**
   * Wallet listener to collect output notifications.
   */
  private class OutputNotificationCollector extends MoneroWalletListener {
    
    private List<Pair<BigInteger, BigInteger>> balanceNotifications;
    private List<MoneroOutputWallet> outputsReceived;
    private List<MoneroOutputWallet> outputsSpent;
    
    public OutputNotificationCollector() {
      balanceNotifications = new ArrayList<Pair<BigInteger, BigInteger>>();
      outputsReceived = new ArrayList<MoneroOutputWallet>();
      outputsSpent = new ArrayList<MoneroOutputWallet>();
    }
    
    @Override
    public void onBalancesChanged(BigInteger newBalance, BigInteger newUnlockedBalance) {
      if (!balanceNotifications.isEmpty()) {
        Pair<BigInteger, BigInteger> lastNotification = balanceNotifications.get(balanceNotifications.size() - 1);
        assertTrue(!newBalance.equals(lastNotification.getFirst()) || !newUnlockedBalance.equals(lastNotification.getSecond())); // test that balances change
      }
      balanceNotifications.add(new Pair<BigInteger, BigInteger>(newBalance, newUnlockedBalance));
    }
    
    @Override
    public void onOutputReceived(MoneroOutputWallet output) {
      outputsReceived.add(output);
    }
    
    @Override
    public void onOutputSpent(MoneroOutputWallet output) {
      outputsSpent.add(output);
    }
    
    public List<Pair<BigInteger, BigInteger>> getBalanceNotifications() {
      return balanceNotifications;
    }
    
    public List<MoneroOutputWallet> getOutputsReceived() {
      return outputsReceived;
    }
    
    public List<MoneroOutputWallet> getOutputsSpent() {
      return outputsSpent;
    }
  }
  
  public static String getRandomWalletPath() {
    return TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + System.currentTimeMillis();
  }
  
  /**
   * Internal class to test progress updates.
   */
  private class SyncProgressTester extends WalletSyncPrinter {
    
    protected MoneroWalletJni wallet;
    private Long prevHeight;
    private long startHeight;
    private long prevEndHeight;
    private Long prevCompleteHeight;
    protected boolean isDone;
    private Boolean onSyncProgressAfterDone;
    
    public SyncProgressTester(MoneroWalletJni wallet, long startHeight, long endHeight) {
      this.wallet = wallet;
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      this.startHeight = startHeight;
      this.prevEndHeight = endHeight;
      this.isDone = false;
    }
    
    @Override
    public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
      super.onSyncProgress(height, startHeight, endHeight, percentDone, message);
      
      // registered wallet listeners will continue to get sync notifications after the wallet's initial sync
      if (isDone) {
        assertTrue("Listener has completed and is not registered so should not be called again", wallet.getListeners().contains(this));
        onSyncProgressAfterDone = true;
      }
      
      // update tester's start height if new sync session
      if (prevCompleteHeight != null && startHeight == prevCompleteHeight) this.startHeight = startHeight;  
      
      // if sync is complete, record completion height for subsequent start heights
      if (Double.compare(percentDone, 1) == 0) prevCompleteHeight = endHeight;
      
      // otherwise start height is equal to previous completion height
      else if (prevCompleteHeight != null) assertEquals((long) prevCompleteHeight, startHeight);
      
      assertTrue("end height > start height", endHeight > startHeight);
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
      onSyncProgressAfterDone = false;  // test subsequent onSyncProgress() calls
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
    
    public WalletSyncTester(MoneroWalletJni wallet, long startHeight, long endHeight) {
      super(wallet, startHeight, endHeight);
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      incomingTotal = BigInteger.valueOf(0);
      outgoingTotal = BigInteger.valueOf(0);
    }
    
    @Override
    public void onNewBlock(long height) {
      if (isDone) {
        assertTrue("Listener has completed and is not registered so should not be called again", wallet.getListeners().contains(this));
        onNewBlockAfterDone = true;
      }
      if (walletTesterPrevHeight != null) assertEquals(walletTesterPrevHeight + 1, height);
      assertTrue(height >= super.startHeight);
      walletTesterPrevHeight = height;
    }
    
    @Override
    public void onBalancesChanged(BigInteger newBalance, BigInteger newUnlockedBalance) {
      assertEquals(wallet.getBalance(), newBalance);
      assertEquals(wallet.getUnlockedBalance(), newUnlockedBalance);
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
      
      // add incoming amount to running total if confirmed
      if (output.getTx().isConfirmed()) incomingTotal = incomingTotal.add(output.getAmount());
    }

    @Override
    public void onOutputSpent(MoneroOutputWallet output) {
      assertNotNull(output);
      prevOutputSpent = output;
      
      // test output
      assertNotNull(output.getAmount());
      assertTrue(output.getAccountIndex() >= 0);
      assertTrue(output.getSubaddressIndex() >= 0);
      
      // test output's tx
      assertNotNull(output.getTx());
      assertNotNull(output.getTx().getHash());
      assertEquals(64, output.getTx().getHash().length());
      assertTrue(output.getTx().getVersion() >= 0);
      assertNull(output.getTx().getUnlockHeight());
      assertEquals(1, output.getTx().getInputs().size());
      assertTrue(output.getTx().getInputs().get(0) == output);
      assertNull(output.getTx().getOutputs());
      
      // extra is not sent over the jni bridge
      assertNull(output.getTx().getExtra());
      
      // add outgoing amount to running total
      outgoingTotal = outgoingTotal.add(output.getAmount());
    }
    
    public void onDone(long chainHeight) {
      super.onDone(chainHeight);
      assertNotNull(walletTesterPrevHeight);
      assertNotNull(prevOutputReceived);
      assertNotNull(prevOutputSpent);
      BigInteger balance = incomingTotal.subtract(outgoingTotal);
      assertEquals(balance, wallet.getBalance());
      onNewBlockAfterDone = false;  // test subsequent onNewBlock() calls
      assertEquals(wallet.getBalance(), prevBalance);
      assertEquals(wallet.getUnlockedBalance(), prevUnlockedBalance);
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
  private static void testWalletEqualityOnChain(MoneroWalletJni wallet1, MoneroWalletJni wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());
    //assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight()); // TODO monero-core: restore height is lost after close
    assertEquals(wallet1.getDaemonConnection(), wallet2.getDaemonConnection());
    assertEquals(wallet1.getMnemonicLanguage(), wallet2.getMnemonicLanguage());
    // TODO: more jni-specific extensions
  }
  
  // -------------------- OVERRIDES TO BE DIRECTLY RUNNABLE -------------------
  
  @Test
  public void testCreateWalletRandom() {
    super.testCreateWalletRandom();
  }
  
  @Test
  public void testCreateWalletFromMnemonic() {
    super.testCreateWalletFromMnemonic();
  }
  
  @Test
  public void testCreateWalletFromMnemonicWithOffset() {
    super.testCreateWalletFromMnemonicWithOffset();
  }
  
  @Test
  public void testCreateWalletFromKeys() {
    super.testCreateWalletFromKeys();
  }
  
  @Test
  public void testGetVersion() {
    super.testGetVersion();
  }
  
  @Test
  public void testGetPath() {
    super.testGetPath();
  }

  @Override
  public void testGetHeight() {
    super.testGetHeight();
  }

  @Override
  public void testGetHeightByDate() {
    super.testGetHeightByDate();
  }

  @Override
  public void testGetMnemonic() {
    super.testGetMnemonic();
  }

  @Override
  public void testGetMnemonicLanguages() {
    super.testGetMnemonicLanguages();
  }

  @Override
  public void testGetPrivateViewKey() {
    super.testGetPrivateViewKey();
  }
  
  @Override
  public void testGetPrivateSpendKey() {
    super.testGetPrivateSpendKey();
  }
  
  @Override
  public void testGetPublicViewKey() {
    super.testGetPublicViewKey();
  }
  
  @Override
  public void testGetPublicSpendKey() {
    super.testGetPublicSpendKey();
  }

  @Override
  public void testGetPrimaryAddress() {
    super.testGetPrimaryAddress();
  }

  @Override
  public void testGetIntegratedAddressFromPaymentId() {
    super.testGetIntegratedAddressFromPaymentId();
  }

  @Override
  public void testDecodeIntegratedAddress() {
    super.testDecodeIntegratedAddress();
  }

  @Override
  public void testSyncWithoutProgress() {
    super.testSyncWithoutProgress();
  }
  
  @Override
  public void testWalletEqualityGroundTruth() {
    super.testWalletEqualityGroundTruth();
  }

  @Override
  public void testGetAccountsWithoutSubaddresses() {
    super.testGetAccountsWithoutSubaddresses();
  }

  @Override
  public void testGetAccountsWithSubaddresses() {
    super.testGetAccountsWithSubaddresses();
  }

  @Override
  public void testGetAccount() {
    super.testGetAccount();
  }

  @Override
  public void testCreateAccountWithoutLabel() {
    super.testCreateAccountWithoutLabel();
  }

  @Override
  public void testCreateAccountWithLabel() {
    super.testCreateAccountWithLabel();
  }

  @Override
  public void testGetSubaddresses() {
    super.testGetSubaddresses();
  }

  @Override
  public void testGetSubaddressesByIndices() {
    super.testGetSubaddressesByIndices();
  }

  @Override
  public void testGetSubaddressByIndex() {
    super.testGetSubaddressByIndex();
  }

  @Override
  public void testCreateSubaddress() {
    super.testCreateSubaddress();
  }

  @Override
  public void testGetSubaddressAddress() {
    super.testGetSubaddressAddress();
  }

  @Override
  public void testGetAddressIndices() {
    super.testGetAddressIndices();
  }

  @Override
  public void testGetAllBalances() {
    super.testGetAllBalances();
  }

  @Override
  public void testGetTxsWallet() {
    super.testGetTxsWallet();
  }

  @Override
  public void testGetTxsByHash() {
    super.testGetTxsByHash();
  }

  @Override
  public void testGetTxsWithQuery() {
    super.testGetTxsWithQuery();
  }
  
  @Override
  public void testGetTxsByHeight() {
    super.testGetTxsByHeight();
  }

  @Override
  public void testGetTxsWithPaymentIds() {
    super.testGetTxsWithPaymentIds();
  }

  @Override
  public void testGetTxsFieldsWithFiltering() {
    super.testGetTxsFieldsWithFiltering();
  }

  @Override
  public void testGetTxsValidateInputs() {
    super.testGetTxsValidateInputs();
  }

  @Override
  public void testGetTransfers() {
    super.testGetTransfers();
  }

  @Override
  public void testGetTransfersWithQuery() {
    super.testGetTransfersWithQuery();
  }

  @Override
  public void testGetTransfersValidateInputs() {
    super.testGetTransfersValidateInputs();
  }
  
  @Override
  public void testGetIncomingOutgoingTransfers() {
    super.testGetIncomingOutgoingTransfers();
  }

  @Override
  public void testGetOutputs() {
    super.testGetOutputs();
  }

  @Override
  public void testGetOutputsWithQuery() {
    super.testGetOutputsWithQuery();
  }

  @Override
  public void testGetOutputsValidateInputs() {
    super.testGetOutputsValidateInputs();
  }

  @Override
  public void testAccounting() {
    super.testAccounting();
  }

  @Override
  public void testCheckTxKey() {
    super.testCheckTxKey();
  }

  @Override
  public void testCheckTxProof() {
    super.testCheckTxProof();
  }

  @Override
  public void testCheckSpendProof() {
    super.testCheckSpendProof();
  }

  @Override
  public void testGetReserveProofWallet() {
    super.testGetReserveProofWallet();
  }

  @Override
  public void testGetReserveProofAccount() {
    super.testGetReserveProofAccount();
  }

  @Override
  public void testSetTxNote() {
    super.testSetTxNote();
  }

  @Override
  public void testSetTxNotes() {
    super.testSetTxNotes();
  }

  @Override
  public void testGetOutputsHex() {
    super.testGetOutputsHex();
  }

  @Override
  public void testImportOutputsHex() {
    super.testImportOutputsHex();
  }

  @Override
  public void testGetSignedKeyImages() {
    super.testGetSignedKeyImages();
  }

  @Override
  public void testGetNewKeyImagesFromLastImport() {
    super.testGetNewKeyImagesFromLastImport();
  }

  @Override
  public void testImportKeyImages() {
    super.testImportKeyImages();
  }
  
  @Override
  public void testViewOnlyAndOfflineWallets() {
    super.testViewOnlyAndOfflineWallets();
  }

  @Override
  public void testSignAndVerifyMessages() {
    super.testSignAndVerifyMessages();
  }
  
  @Override
  public void testAddressBook() {
    super.testAddressBook();
  }

  @Override
  public void testSetAttributes() {
    super.testSetAttributes();
  }

  @Override
  public void testCreatePaymentUri() {
    super.testCreatePaymentUri();
  }

  @Override
  public void testMining() {
    super.testMining();
  }
  
  @Override
  public void testSyncWithPoolSameAccounts() {
    super.testSyncWithPoolSameAccounts();
  }
  
  @Override
  public void testSyncWithPoolSubmitAndDiscard() {
    super.testSyncWithPoolSubmitAndDiscard();
  }
  
  @Override
  public void testSyncWithPoolSubmitAndRelay() {
    super.testSyncWithPoolSubmitAndRelay();
  }
  
  @Override
  public void testSyncWithPoolRelay() {
    super.testSyncWithPoolRelay();
  }
  
  @Override
  public void testSendToExternal() {
    super.testSendToExternal();
  }

  @Override
  public void testSendFromSubaddresses() {
    super.testSendFromSubaddresses();
  }
  
  @Override
  public void testSendFromSubaddressesSplit() {
    super.testSendFromSubaddressesSplit();
  }

  @Override
  public void testSend() {
    super.testSend();
  }

  @Override
  public void testSendWithPaymentId() {
    super.testSendWithPaymentId();
  }

  @Override
  public void testSendSplit() {
    super.testSendSplit();
  }

  @Override
  public void testCreateThenRelay() {
    super.testCreateThenRelay();
  }

  @Override
  public void testCreateThenRelaySplit() {
    super.testCreateThenRelaySplit();
  }

  @Override
  public void testSendToMultiple() {
    super.testSendToMultiple();
  }

  @Override
  public void testSendToMultipleSplit() {
    super.testSendToMultipleSplit();
  }

  @Override
  public void testSendDustToMultipleSplit() {
    super.testSendDustToMultipleSplit();
  }

  @Override
  public void testUpdateLockedSameAccount() {
    super.testUpdateLockedSameAccount();
  }

  @Override
  public void testUpdateLockedSameAccountSplit() {
    super.testUpdateLockedSameAccountSplit();
  }

  @Override
  public void testUpdateLockedDifferentAccounts() {
    super.testUpdateLockedDifferentAccounts();
  }

  @Override
  public void testUpdateLockedDifferentAccountsSplit() {
    super.testUpdateLockedDifferentAccountsSplit();
  }

  @Override
  public void testSweepOutputs() {
    super.testSweepOutputs();
  }

  @Override
  public void testSweepSubaddresses() {
    super.testSweepSubaddresses();
  }

  @Override
  public void testSweepAccounts() {
    super.testSweepAccounts();
  }

  @Override
  public void testSweepWalletByAccounts() {
    super.testSweepWalletByAccounts();
  }

  @Override
  public void testSweepWalletBySubaddresses() {
    super.testSweepWalletBySubaddresses();
  }

  @Override
  public void testSweepDustNoRelay() {
    super.testSweepDustNoRelay();
  }

  @Override
  public void testSweepDust() {
    super.testSweepDust();
  }

  @Override
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }
  
  @Override
  public void testMultisig() {
    super.testMultisig();
  }
  
  @Override
  public void testSaveAndClose() {
    super.testSaveAndClose();
  }
}
