import monero.common.MoneroConnectionManager;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletLight;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;

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
import utils.WalletEqualityUtils;
import utils.WalletSyncPrinter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@TestInstance(Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions
public class TestMoneroWalletLight extends TestMoneroWalletCommon {
  protected MoneroWalletLight wallet;

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
    
    // stop mining
    //MoneroMiningStatus status = daemon.getMiningStatus();
    //if (status.isActive()) daemon.stopMining();
  }

  public MoneroRpcConnection getRpcConnection()
  {
    return new MoneroRpcConnection(TestUtils.WALLET_LWS_URI);
  }

  @Override
  protected MoneroWalletLight openWallet(MoneroWalletConfig config) {
    return openWallet(config, true);
  }

  protected MoneroWalletLight openWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServerUri(TestUtils.WALLET_LWS_URI);
    
    // open wallet
    MoneroWalletLight wallet = MoneroWalletLight.openWallet(config);
    if (startSyncing != false && wallet.isConnectedToDaemon()) {
      wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      wallet.sync();
    }
    return wallet;
  }

  @Override
  protected MoneroWalletLight createWallet(MoneroWalletConfig config) {
    return createWallet(config, true);
  }

  protected MoneroWalletLight createWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServerUri(TestUtils.WALLET_LWS_URI);
    
    // create wallet
    MoneroWalletLight wallet = MoneroWalletLight.createWallet(config);
    if (!random) assertEquals(config.getRestoreHeight() == null ? 0l : config.getRestoreHeight(), wallet.getRestoreHeight());
    if (startSyncing != false && wallet.isConnectedToDaemon()) {
      wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
      wallet.sync();
    }
    return wallet;
  }

  protected MoneroWalletLight createWallet() {
    return createWallet(TestUtils.getWalletLightConfig());
  }
  
  protected MoneroWalletFull createWalletFull(MoneroWalletConfig config) {
    return createWalletFull(config, true);
  }
  
  protected MoneroWalletFull createWalletFull(MoneroWalletConfig config, boolean startSyncing) {
    
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

  protected void testAccount(MoneroAccount account) {
    
    // test account
    assertNotNull(account);
    assertTrue(account.getIndex() >= 0);
    MoneroUtils.validateAddress(account.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    TestUtils.testUnsignedBigInteger(account.getBalance());
    TestUtils.testUnsignedBigInteger(account.getUnlockedBalance());
    
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
      assertTrue(account.getBalance().equals(balance), "Subaddress balances " + balance + " != account " + account.getIndex() + " balance " + account.getBalance());
      assertTrue(account.getUnlockedBalance().equals(unlockedBalance), "Subaddress unlocked balances " + unlockedBalance + " != account " + account.getIndex() + " unlocked balance " + account.getUnlockedBalance());
    }
    
    // tag must be undefined or non-empty
    String tag = account.getTag();
    assertTrue(tag == null || tag.length() > 0);
  }


  // --------------- DEMONSTRATION OF MONERO PROJECT ISSUES ----------------------
  
  /**
   * Test the daemon's ability to not hang from wallets which are continuously
   * syncing, have registered listeners, and which are not closed.
   */
  @Test
  @Disabled // TODO monero-project: disabled because observing memory leak behavior when all tests run together
  public void testCreateWalletsWithoutClose() {
    
    // lets make some wallets and then go away
    for (int i = 0; i < 20; i++) {
      MoneroWalletLight willLeaveYouHanging = createWallet(new MoneroWalletConfig());
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
  // ------------------------------- BEGIN TESTS ------------------------------
  
  // Can get the daemon's height
  @Test
  public void testDaemon() {
    assumeTrue(TEST_NON_RELAYS);
    assertTrue(wallet.isConnectedToDaemon());
    long daemonHeight = wallet.getDaemonHeight();
    assertTrue(daemonHeight > 0);
  }
  
  // Can get the daemon's max peer height
  @Test
  public void testGetDaemonMaxPeerHeight() {
    assumeTrue(TEST_NON_RELAYS);
    long height = (wallet).getDaemonMaxPeerHeight();
    assertTrue(height > 0);
  }
  
  // Can create a full wallet from keys
  @Test
  public void testCreateWalletFromKeysJni() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    MoneroWalletLight walletKeys = openWallet(new MoneroWalletConfig().setServerUri(TestUtils.WALLET_LWS_URI).setSeed(TestUtils.SEED).setNetworkType(TestUtils.NETWORK_TYPE), false);
    try {
      assertEquals(wallet.getSeed(), walletKeys.getSeed());
      assertEquals(wallet.getPrimaryAddress(), walletKeys.getPrimaryAddress());
      assertEquals(wallet.getPrivateViewKey(), walletKeys.getPrivateViewKey());
      assertEquals(wallet.getPublicViewKey(), walletKeys.getPublicViewKey());
      assertEquals(wallet.getPrivateSpendKey(), walletKeys.getPrivateSpendKey());
      assertEquals(wallet.getPublicSpendKey(), walletKeys.getPublicSpendKey());
      assertEquals(wallet.getRestoreHeight(), walletKeys.getRestoreHeight());
      assertTrue(walletKeys.isConnectedToDaemon());
      assertFalse(walletKeys.isSynced());
    } finally {
      walletKeys.close();
    }
  }
  
  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
  public void testViewOnlyAndOfflineWalletCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only wallet in wallet rpc process
    MoneroWalletLight viewOnlyWallet = openWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setServerUri(TestUtils.WALLET_LWS_URI));
    viewOnlyWallet.sync();
    
    // create offline full wallet
    MoneroWalletFull offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      closeWallet(offlineWallet);
    }
  };
    
  // Can re-sync an existing wallet from scratch
  @Test
  public void testResyncExisting() {
    assertTrue(MoneroWalletLight.walletExists(TestUtils.getWalletLightConfig()));
    MoneroWalletLight wallet = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setServerUri(TestUtils.OFFLINE_SERVER_URI), false);
    wallet.setDaemonConnection(getRpcConnection());
    //long startHeight = TestUtils.TEST_RESTORE_HEIGHT;
    long startHeight = wallet.getRestoreHeight();
    SyncProgressTester progressTester = new SyncProgressTester(wallet, startHeight, wallet.getDaemonHeight());
    
    MoneroSyncResult result = wallet.sync(progressTester);
    progressTester.onDone(wallet.getDaemonHeight());
    
    // test result after syncing
    assertTrue(wallet.isConnectedToDaemon());
    assertTrue(wallet.isSynced());
    assertEquals(wallet.getDaemonHeight() - startHeight - 1, (long) result.getNumBlocksFetched());
    assertTrue(result.getReceivedMoney());
    assertEquals(daemon.getHeight(), wallet.getHeight());
    wallet.close(false);
  }

  // Can sync a wallet with a randomly generated seed
  @Test
  public void testSyncRandom() {
    assumeTrue(TEST_NON_RELAYS);
    assertTrue(daemon.isConnected(), "Not connected to daemon");

    // create test wallet
    MoneroWalletLight wallet = createWallet(new MoneroWalletConfig(), false);
    long restoreHeight = daemon.getHeight();

    // test wallet's height before syncing
    assertEquals(TestUtils.getWalletLight().getDaemonConnection(), wallet.getDaemonConnection());
    // why test current daemon.getHeight() before syncing?
    //assertEquals(restoreHeight, wallet.getDaemonHeight());
    assertTrue(wallet.isConnectedToDaemon());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(wallet.getDaemonHeight(), wallet.getRestoreHeight());
    //assertEquals(daemon.getHeight(), wallet.getDaemonHeight());

    // sync the wallet
    //SyncProgressTester progressTester = new SyncProgressTester(wallet, wallet.getRestoreHeight(), wallet.getDaemonHeight());
    MoneroSyncResult result = wallet.sync();
    //progressTester.onDone(wallet.getDaemonHeight());
    
    // test result after syncing
    MoneroWalletFull walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getSeed(), null, restoreHeight);
    walletGt.sync();
    try {
      assertTrue(wallet.isConnectedToDaemon());
      assertTrue(wallet.isSynced());
      //assertEquals(0, (long) result.getNumBlocksFetched());
      assertFalse(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), wallet.getHeight());

      // sync the wallet with default params
      wallet.sync();
      assertTrue(wallet.isSynced());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      
      // compare wallet to ground truth
      testWalletEqualityOnChain(walletGt, wallet);
    } finally {
      if (walletGt != null) walletGt.close(false);
      wallet.close();
    }
    
    // attempt to sync unconnected wallet
    wallet = createWallet(new MoneroWalletConfig().setServerUri(TestUtils.OFFLINE_SERVER_URI));
    try {
      wallet.sync();
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    } finally {
      wallet.close();
    }
  }

  // Can sync a wallet created from seed from the genesis
  @Test
  public void testSyncSeedFromGenesis() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(null, null, true, false);
  }
  
  // Can sync a wallet created from seed from a restore height
  @Test
  public void testSyncSeedFromRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS);
    testSyncSeed(null, wallet.getRestoreHeight());
  }
  
  // Can sync a wallet created from seed from a start height.
  @Test
  public void testSyncSeedFromStartHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    testSyncSeed(wallet.getRestoreHeight(), null, false, true);
  }
  
  // Can sync a wallet created from keys
  @Test
  public void testSyncWalletFromKeys() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    MoneroWalletLight walletKeys = openWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.WALLET_LWS_URI), false);
    
    // create ground truth wallet for comparison
    MoneroWalletFull walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.SEED, null, TestUtils.FIRST_RECEIVE_HEIGHT);
    
    // test wallet and close as final step
    try {
      assertEquals(walletKeys.getSeed(), walletGt.getSeed());
      assertEquals(walletKeys.getPrimaryAddress(), walletGt.getPrimaryAddress());
      assertEquals(walletKeys.getPrivateViewKey(), walletGt.getPrivateViewKey());
      assertEquals(walletKeys.getPublicViewKey(), walletGt.getPublicViewKey());
      assertEquals(walletKeys.getPrivateSpendKey(), walletGt.getPrivateSpendKey());
      assertEquals(walletKeys.getPublicSpendKey(), walletGt.getPublicSpendKey());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletGt.getRestoreHeight());
      assertTrue(walletKeys.isConnectedToDaemon());
      assertFalse(walletKeys.isSynced());
      
      // sync the wallet
      SyncProgressTester progressTester = new SyncProgressTester(walletKeys, TestUtils.FIRST_RECEIVE_HEIGHT, walletKeys.getDaemonMaxPeerHeight());
      MoneroSyncResult result = walletKeys.sync(progressTester);
      progressTester.onDone(walletKeys.getDaemonHeight());
      
      // test result after syncing
      assertTrue(walletKeys.isSynced());
      assertEquals(walletKeys.getDaemonHeight() - walletKeys.getRestoreHeight() - 1, (long) result.getNumBlocksFetched());
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
    MoneroWalletLight wallet = createWallet(new MoneroWalletConfig().setServerUri(TestUtils.OFFLINE_SERVER_URI));
    try {
      assertNotNull(wallet.getSeed());
      assertEquals(1, wallet.getHeight());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      wallet.startSyncing();
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    } finally {
      wallet.close();
    }
    
    // test connecting wallet
    wallet = createWallet(new MoneroWalletConfig().setServerUri(TestUtils.OFFLINE_SERVER_URI));
    try {
      assertNotNull(wallet.getSeed());
      wallet.setDaemonConnection(TestUtils.getWalletLight().getDaemonConnection());
      assertEquals(1, wallet.getHeight());
      assertFalse(wallet.isSynced());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      //long chainHeight = wallet.getDaemonHeight();
      // wallet light doesn't support restore height if not admin
      //wallet.setRestoreHeight(chainHeight - 3);
      wallet.startSyncing();
      //assertEquals(chainHeight - 3, wallet.getRestoreHeight());
      assertEquals(getRpcConnection(), wallet.getDaemonConnection());
      wallet.stopSyncing();
      wallet.sync();
      wallet.stopSyncing();
      wallet.stopSyncing();
    } finally {
      wallet.close();
    }
    
    // test that sync starts automatically
    long restoreHeight = daemon.getHeight();
    wallet = createWallet(new MoneroWalletConfig(), false);
    try {
      
      // start syncing
      assertEquals(1, wallet.getHeight());
      assertEquals(0, wallet.getRestoreHeight());
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
      assertEquals(restoreHeight - 1, wallet.getRestoreHeight());
      
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
    MoneroWalletLight wallet1 = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    MoneroWalletLight wallet2 = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    
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
  @Disabled
  public void testWalletEqualityRpc() {
    WalletEqualityUtils.testWalletEqualityOnChain(TestUtils.getWalletRpc(), wallet);
  }
  
  // Is equal to the RPC wallet with a seed offset
  @Test
  @Disabled
  public void testWalletEqualityRpcWithOffset() {
  
  }

  // Can be closed
  @Test
  public void testClose() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create a test wallet
    MoneroWalletLight wallet = createWallet(new MoneroWalletConfig());
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
    try { wallet.getSeed(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.sync(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.startSyncing(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.stopSyncing(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    
    // re-open the wallet
    wallet = openWallet(new MoneroWalletConfig().setPrimaryAddress(TestUtils.ADDRESS).setPrivateViewKey(TestUtils.PRIVATE_VIEW_KEY).setServerUri(TestUtils.WALLET_LWS_URI));
    wallet.sync();
    assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
    assertFalse(wallet.isClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.isClosed());
  }
  
  // Does not leak memory
  @Test
  @Disabled
  public void testMemoryLeak() {
    System.out.println("Infinite loop starting, monitor memory in OS process manager...");
    System.gc();
    int i = 0;
    boolean closeWallet = false;
    long time = System.currentTimeMillis();
    if (closeWallet) wallet.close(false);
    while (true) {
      if (closeWallet) wallet = TestUtils.getWalletLight();
      wallet.sync();
      wallet.getTxs();
      wallet.getTransfers();
      wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false));
      if (i % 100 == 0) {
        System.out.println("Garbage collecting on iteration " + i);
        System.gc();
        System.out.println((System.currentTimeMillis() - time) + " ms since last GC");
        time = System.currentTimeMillis();
      }
      if (closeWallet) wallet.close(false);
      i++;
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
    
    protected MoneroWallet wallet;
    private Long prevHeight;
    private long startHeight;
    private long prevEndHeight;
    private Long prevCompleteHeight;
    protected boolean isDone;
    private Boolean onSyncProgressAfterDone;
    
    public SyncProgressTester(MoneroWallet wallet, long startHeight, long endHeight) {
      this.wallet = wallet;
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      this.startHeight = startHeight;
      this.prevEndHeight = endHeight;
      this.isDone = false;
    }
    
    @Override
    public synchronized void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
      try {
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
       // assertEquals(this.startHeight, startHeight);
        assertTrue(endHeight >= prevEndHeight);  // chain can grow while syncing
        prevEndHeight = endHeight;
        assertTrue(height >= startHeight);
        assertTrue(height <= endHeight);
        double expectedPercentDone = (double) (height - startHeight + 1) / (double) (endHeight - startHeight);
        assertTrue(Double.compare(expectedPercentDone, percentDone) == 0);
        if (prevHeight != null) assertTrue(height > prevHeight);

        prevHeight = height;
      }
      catch (Exception e) {
        e.getMessage();
      }

      super.onSyncProgress(height, startHeight, endHeight, percentDone, message);
      
    }
    
    public void onDone(long chainHeight) {
      assertFalse(isDone);
      this.isDone = true;
      if (prevHeight != null) {
        assertTrue(chainHeight >= prevHeight);
        //assertEquals(chainHeight - 1, (long) prevHeight);  // otherwise last height is chain height - 1
      }
      else {
        assertTrue(chainHeight >= (long) prevCompleteHeight - 1);
        //assertEquals(chainHeight, (long) prevCompleteHeight);
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
    
    public WalletSyncTester(MoneroWallet wallet, long startHeight, long endHeight) {
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
      if (walletTesterPrevHeight != null) assertTrue(walletTesterPrevHeight <= height);
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
      assertTrue(output.getTx().getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
      assertNull(output.getTx().getInputs());
      assertEquals(1, output.getTx().getOutputs().size());
      assertTrue(output.getTx().getOutputs().get(0) == output);
      
      // extra is not sent over the jni bridge
      assertNull(output.getTx().getExtra());
      
      // add incoming amount to running total
      if (output.isLocked()) incomingTotal = incomingTotal.add(output.getAmount()); // TODO: only add if not unlocked, test unlocked received
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
      assertTrue(output.getTx().getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
      assertEquals(1, output.getTx().getInputs().size());
      assertTrue(output.getTx().getInputs().get(0) == output);
      assertNull(output.getTx().getOutputs());
      
      // extra is not sent over the jni bridge
      assertNull(output.getTx().getExtra());
      
      // add outgoing amount to running total
      if (output.isLocked()) outgoingTotal = outgoingTotal.add(output.getAmount());
    }
    
    @Override
    public void onDone(long chainHeight) {
      super.onDone(chainHeight);
      //assertNotNull(walletTesterPrevHeight);
      //assertNotNull(prevOutputReceived);
      //assertNotNull(prevOutputSpent);
      //BigInteger balance = incomingTotal.subtract(outgoingTotal);
      //assertEquals(balance, wallet.getBalance());
      //assertEquals(prevBalance, wallet.getBalance());
      //assertEquals(prevUnlockedBalance, wallet.getUnlockedBalance());
    }
    
    public Boolean getOnNewBlockAfterDone() {
      return onNewBlockAfterDone;
    }
  }
  
  // jni-specific tx tests
  
  protected void testTxWallet(MoneroTxWallet tx, TxContext ctx) {
    // validate / sanitize inputs

    if (ctx == null) ctx = new TxContext();
    else ctx = new TxContext(ctx);
    ctx.wallet = null;  // TODO: re-enable
    assertNotNull(tx);
    if (ctx.isSendResponse == null || ctx.config == null) {
      assertNull(ctx.isSendResponse, "if either sendRequest or isSendResponse is defined, they must both be defined");
      assertNull(ctx.config, "if either sendRequest or isSendResponse is defined, they must both be defined");
    }
    
    // test common field types
    assertNotNull(tx.getHash());
    assertNotNull(tx.isConfirmed());
    assertNotNull(tx.isMinerTx());
    assertNotNull(tx.isFailed());
    assertNotNull(tx.isRelayed());
    assertNotNull(tx.inTxPool());
    assertNotNull(tx.isLocked());
    TestUtils.testUnsignedBigInteger(tx.getFee());
    if (tx.getPaymentId() != null) assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId()); // default payment id converted to null
    if (tx.getNote() != null) assertTrue(tx.getNote().length() > 0);  // empty notes converted to undefined
    assertTrue(tx.getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
    assertNull(tx.getSize());   // TODO monero-wallet-rpc: add tx_size to get_transfers and get_transfer_by_txid
    assertNull(tx.getReceivedTimestamp());  // TODO monero-wallet-rpc: return received timestamp (asked to file issue if wanted)
    
    // test send tx
    if (Boolean.TRUE.equals(ctx.isSendResponse)) {
      assertTrue(tx.getWeight() > 0);
      assertNotNull(tx.getInputs());
      assertTrue(tx.getInputs().size() > 0);
      for (MoneroOutput input : tx.getInputs()) assertTrue(input.getTx() == tx);
    } else {
      assertNull(tx.getWeight());
      assertNull(tx.getInputs());
    }
    
    // test confirmed
    if (tx.isConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeight() > 0);
      assertTrue(tx.getBlock().getTimestamp() > 0);
      assertEquals(true, tx.getRelay());
      assertEquals(true, tx.isRelayed());
      assertEquals(false, tx.isFailed());
      assertEquals(false, tx.inTxPool());
      assertEquals(false, tx.isDoubleSpendSeen());
      assertTrue(tx.getNumConfirmations() > 0);
    } else {
      assertNull(tx.getBlock());
      assertEquals(0, (long) tx.getNumConfirmations());
    }
    
    // test in tx pool
    if (tx.inTxPool()) {
      assertEquals(false, tx.isConfirmed());
      assertEquals(true, tx.getRelay());
      assertEquals(true, tx.isRelayed());
      assertEquals(false, tx.isDoubleSpendSeen()); // TODO: test double spend attempt
      assertEquals(true, tx.isLocked());
      
      // these should be initialized unless a response from sending
      if (!Boolean.TRUE.equals(ctx.isSendResponse)) {
        assertTrue(tx.getReceivedTimestamp() > 0);
      }
    } else {
      assertNull(tx.getLastRelayedTimestamp());
    }
    
    // test miner tx
    if (tx.isMinerTx()) {
      assertEquals(BigInteger.valueOf(0), tx.getFee());
      assertTrue(tx.getIncomingTransfers().size() > 0);
    }
    
    // test failed  // TODO: what else to test associated with failed
    if (tx.isFailed()) {
      assertTrue(tx.getOutgoingTransfer() instanceof MoneroTransfer);
      assertTrue(tx.getReceivedTimestamp() > 0);
    } else {
      if (tx.isRelayed()) assertEquals(tx.isDoubleSpendSeen(), false);
      else {
        assertEquals(false, tx.getRelay());
        assertEquals(false, tx.isRelayed());
        assertNull(tx.isDoubleSpendSeen());
      }
    }
    assertNull(tx.getLastFailedHeight());
    assertNull(tx.getLastFailedHash());
    
    // received time only for tx pool or failed txs
    if (tx.getReceivedTimestamp() != null) {
      assertTrue(tx.inTxPool() || tx.isFailed());
    }
    
    // test relayed tx
    if (tx.isRelayed()) assertEquals(tx.getRelay(), true);
    if (!tx.getRelay()) assertTrue(!tx.isRelayed());
    
    // test outgoing transfer per configuration
    if (Boolean.FALSE.equals(ctx.hasOutgoingTransfer)) assertNull(tx.getOutgoingTransfer());
    if (Boolean.TRUE.equals(ctx.hasDestinations)) assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
    
    // test outgoing transfer
    if (tx.getOutgoingTransfer() != null) {
      assertTrue(tx.isOutgoing());
      testTransfer(tx.getOutgoingTransfer(), ctx);
      if (Boolean.TRUE.equals(ctx.isSweepResponse)) assertEquals(1, tx.getOutgoingTransfer().getDestinations().size());
      
      // TODO: handle special cases
    } else {
      assertTrue(tx.getIncomingTransfers().size() > 0);
      assertNull(tx.getOutgoingAmount());
      assertNull(tx.getOutgoingTransfer());
      assertNull(tx.getRingSize());
      assertNull(tx.getFullHex());
      assertNull(tx.getMetadata());
      assertNull(tx.getKey());
    }
    
    // test incoming transfers
    if (tx.getIncomingTransfers() != null) {
      assertTrue(tx.isIncoming());
      assertTrue(tx.getIncomingTransfers().size() > 0);
      TestUtils.testUnsignedBigInteger(tx.getIncomingAmount());
      assertEquals(tx.isFailed(), false);
      
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
      
      // test tx set
      assertNotNull(tx.getTxSet());
      boolean found = false;
      for (MoneroTxWallet aTx : tx.getTxSet().getTxs()) {
        if (aTx == tx) {
          found = true;
          break;
        }
      }
      if (Boolean.TRUE.equals(ctx.isCopy)) assertFalse(found); // copy will not have back reference from tx set
      else assertTrue(found);
      
      // test common attributes
      MoneroTxConfig config = ctx.config;
      assertEquals(false, tx.isConfirmed());
      testTransfer(tx.getOutgoingTransfer(), ctx);
      assertEquals(MoneroUtils.RING_SIZE, (int) tx.getRingSize());
      assertEquals(BigInteger.valueOf(0), tx.getUnlockTime());
      assertNull(tx.getBlock());
      assertTrue(tx.getKey().length() > 0);
      assertNotNull(tx.getFullHex());
      assertTrue(tx.getFullHex().length() > 0);
      // TODO add tx metadata to light wallet 
      // assertNotNull(tx.getMetadata());
      assertNull(tx.getReceivedTimestamp());
      assertTrue(tx.isLocked());
      
      // test locked state
      if (BigInteger.valueOf(0).equals(tx.getUnlockTime())) assertEquals(tx.isConfirmed(), !tx.isLocked());
      else assertEquals(true, tx.isLocked());
      for (MoneroOutputWallet output : tx.getOutputsWallet()) {
        assertEquals(tx.isLocked(), output.isLocked());
      }
      
      // test destinations of sent tx
      if (tx.getOutgoingTransfer().getDestinations() == null) {
        assertTrue(config.getCanSplit());
        System.err.println("Destinations not returned from split transactions"); // TODO: remove this after >18.3.1 when amounts_by_dest_list official
      } else {
        assertNotNull(tx.getOutgoingTransfer().getDestinations());
        assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
        boolean subtractFeeFromDestinations = ctx.config.getSubtractFeeFrom() != null && ctx.config.getSubtractFeeFrom().size() > 0;
        if (Boolean.TRUE.equals(ctx.isSweepResponse)) {
          assertEquals(1, config.getDestinations().size());
          assertNull(config.getDestinations().get(0).getAmount());
          if (!subtractFeeFromDestinations) {
            assertEquals(tx.getOutgoingTransfer().getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(0).getAmount().toString());
          }
        }
      }

      
      // test relayed txs
      if (Boolean.TRUE.equals(config.getRelay())) {
        assertEquals(true, tx.inTxPool());
        assertEquals(true, tx.getRelay());
        assertEquals(true, tx.isRelayed());
        assertTrue(tx.getLastRelayedTimestamp() > 0);
        assertEquals(false, tx.isDoubleSpendSeen());
      }
      
      // test non-relayed txs
      else {
        assertEquals(false, tx.inTxPool());
        assertEquals(false, tx.getRelay());
        assertEquals(false, tx.isRelayed());
        assertNull(tx.getLastRelayedTimestamp());
        assertNull(tx.isDoubleSpendSeen());
      }
    }
    
    // test tx result query
    else {
      assertNull(tx.getTxSet());  // tx set only initialized on send responses
      assertNull(tx.getRingSize());
      assertNull(tx.getKey());
      assertNull(tx.getFullHex());
      assertNull(tx.getMetadata());
      assertNull(tx.getLastRelayedTimestamp());
    }
    
    // test inputs
    if (Boolean.TRUE.equals(tx.isOutgoing()) && Boolean.TRUE.equals(ctx.isSendResponse)) {
      assertNotNull(tx.getInputs());
      assertTrue(tx.getInputs().size() > 0);
    }
    if (tx.getInputs() != null) for (MoneroOutputWallet input : tx.getInputsWallet()) testInputWallet(input);
    
    // test outputs
    if (Boolean.TRUE.equals(tx.isIncoming()) && Boolean.TRUE.equals(ctx.includeOutputs)) {
      if (tx.isConfirmed()) {
        assertNotNull(tx.getOutputs());
        assertTrue(tx.getOutputs().size() > 0);
      } else {
        assertNull(tx.getOutputs());
      }
    }
    if (tx.getOutputs() != null) for (MoneroOutputWallet output : tx.getOutputsWallet()) testOutputWallet(output);
    
    // test deep copy
    if (!Boolean.TRUE.equals(ctx.isCopy)) testTxWalletCopy(tx, ctx);
  }


  // possible configuration: on chain xor local wallet data ("strict"), txs ordered same way? TBD
  protected static void testWalletEqualityOnChain(MoneroWalletFull wallet1, MoneroWalletFull wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());
    // why cacheData modifies MoneroWalletFull.getRestoreHeight() ?    
    assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight());
    assertEquals(wallet1.getDaemonConnection(), wallet2.getDaemonConnection());
    assertEquals(wallet1.getSeedLanguage(), wallet2.getSeedLanguage());
    // TODO: more jni-specific extensions
  }

  // possible configuration: on chain xor local wallet data ("strict"), txs ordered same way? TBD
  protected static void testWalletEqualityOnChain(MoneroWalletFull wallet1, MoneroWalletLight wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());    
    //assertEquals(wallet1.getRestoreHeight() - 1, wallet2.getRestoreHeight());
    assertEquals(wallet1.getSeedLanguage(), wallet2.getSeedLanguage());
    // TODO: more jni-specific extensions
  }

  protected static void testWalletEqualityOnChain(MoneroWalletLight wallet1, MoneroWalletLight wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());    
    assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight());
    assertEquals(wallet1.getSeedLanguage(), wallet2.getSeedLanguage());
    // TODO: more jni-specific extensions
  }

  protected static void testWalletEqualityOnChain(MoneroWalletLight wallet1, MoneroWalletFull wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());    
    //assertEquals(wallet1.getRestoreHeight() + 1, wallet2.getRestoreHeight());
    assertEquals(wallet1.getSeedLanguage(), wallet2.getSeedLanguage());
    // TODO: more jni-specific extensions
  }
  
  // -------------------- OVERRIDES TO BE DIRECTLY RUNNABLE -------------------
  
  @Override
  @Test
  public void testCreateWalletRandom() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {
      
      // create random wallet
      MoneroWallet wallet = createWallet(new MoneroWalletConfig());
      String seed = wallet.getSeed();
      Exception e2 = null;
      try {
        MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
        MoneroUtils.validatePrivateViewKey(wallet.getPrivateViewKey());
        MoneroUtils.validatePrivateSpendKey(wallet.getPrivateSpendKey());
        MoneroUtils.validateMnemonic(wallet.getSeed());
        assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getSeedLanguage());  // TODO monero-wallet-rpc: get seed language
      } catch (Exception e) {
        e2 = e;
      }
      closeWallet(wallet);
      if (e2 != null) throw e2;
      
      // attempt to create wallet at same path
      try {
        createWallet(new MoneroWalletConfig().setSeed(seed).setServer(getRpcConnection()));
        throw new Error("Should have thrown error");
      } catch(Exception e) {
        assertEquals("Wallet already exists", e.getMessage());
      }
      
      // attempt to create wallet with unknown language
      try {
        createWallet(new MoneroWalletConfig().setLanguage("english")); // TODO: support lowercase?
        throw new Error("Should have thrown error");
      } catch (Exception e) {
        assertEquals("Unknown language: english", e.getMessage());
      }
    } catch (Exception e) {
      e1 = e;
    }
    
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  @Override
  // Can create a wallet from a seed.
  @Test
  public void testCreateWalletFromSeed() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {
      
      // save for comparison
      String primaryAddress = wallet.getPrimaryAddress();
      String privateViewKey = wallet.getPrivateViewKey();
      String privateSpendKey = wallet.getPrivateSpendKey();
      
      // open test wallet from seed
      MoneroWallet wallet = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setNetworkType(TestUtils.NETWORK_TYPE));
      Exception e2 = null;
      try {
        assertEquals(primaryAddress, wallet.getPrimaryAddress());
        assertEquals(privateViewKey, wallet.getPrivateViewKey());
        assertEquals(privateSpendKey, wallet.getPrivateSpendKey());
        assertEquals(TestUtils.SEED, wallet.getSeed());
      } catch (Exception e) {
        e2 = e;
      }
      closeWallet(wallet);
      if (e2 != null) throw e2;
      
      // attempt to create wallet with two missing words
      try {
        String invalidMnemonic = "memoir desk algebra inbound innocent unplugs fully okay five inflamed giant factual ritual toyed topic snake unhappy guarded tweezers haunted inundate giant";
        wallet = createWallet(new MoneroWalletConfig().setSeed(invalidMnemonic));
      } catch(Exception e) {
        assertEquals("Invalid mnemonic", e.getMessage());
      }
      
      // attempt to create wallet at same path
      try {
        createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED));
        throw new RuntimeException("Should have thrown error");
      } catch (Exception e) {
        assertEquals("Wallet already exists", e.getMessage());
      }
    } catch (Exception e) {
      e1 = e;
    }
    
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  @Override
  @Test
  public void testCreateWalletFromSeedWithOffset() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {

      // create test wallet with offset

      String offset= UUID.randomUUID().toString();

      MoneroWallet wallet = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setSeedOffset(offset));

      Exception e2 = null;
      try {
        MoneroUtils.validateMnemonic(wallet.getSeed());
        assertNotEquals(TestUtils.SEED, wallet.getSeed());
        MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
        assertNotEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
        if (!(wallet instanceof MoneroWalletRpc)) assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getSeedLanguage());  // TODO monero-wallet-rpc: support
      } catch (Exception e) {
        e2 = e;
      }
      closeWallet(wallet);
      if (e2 != null) throw e2;
    } catch (Exception e) {
      e1 = e;
    }
    
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  @Override
  // Can create a wallet from keys
  @Test
  public void testCreateWalletFromKeys() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null; // emulating Java "finally" but compatible with other languages
    try {

      // save for comparison
      String primaryAddress = wallet.getPrimaryAddress();
      String privateViewKey = wallet.getPrivateViewKey();
      String privateSpendKey = wallet.getPrivateSpendKey();
      
      // recreate test wallet from keys
      MoneroWallet wallet = openWallet(new MoneroWalletConfig().setNetworkType(TestUtils.NETWORK_TYPE).setPrimaryAddress(primaryAddress).setPrivateViewKey(privateViewKey).setPrivateSpendKey(privateSpendKey));
      Exception e2 = null;
      try {
        assertEquals(primaryAddress, wallet.getPrimaryAddress());
        assertEquals(privateViewKey, wallet.getPrivateViewKey());
        assertEquals(privateSpendKey, wallet.getPrivateSpendKey());
        if (!wallet.isConnectedToDaemon()) System.out.println("WARNING: wallet created from keys is not connected to authenticated daemon"); // TODO monero-project: keys wallets not connected
        assertTrue(wallet.isConnectedToDaemon(), "Wallet created from keys is not connected to authenticated daemon");
        if (!(wallet instanceof MoneroWalletRpc)) {
          MoneroUtils.validateMnemonic(wallet.getSeed()); // TODO monero-wallet-rpc: cannot get seed from wallet created from keys?
          assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getSeedLanguage());
        }
      } catch (Exception e) {
        e2 = e;
      }
      closeWallet(wallet);
      if (e2 != null) throw e2;
      
      // recreate test wallet from spend key
      if (!(wallet instanceof MoneroWalletRpc)) { // TODO monero-wallet-rpc: cannot create wallet from spend key?
        wallet = openWallet(new MoneroWalletConfig().setNetworkType(TestUtils.NETWORK_TYPE).setPrivateSpendKey(privateSpendKey));
        e2 = null;
        try {
          assertEquals(primaryAddress, wallet.getPrimaryAddress());
          assertEquals(privateViewKey, wallet.getPrivateViewKey());
          assertEquals(privateSpendKey, wallet.getPrivateSpendKey());
          if (!wallet.isConnectedToDaemon()) System.out.println("WARNING: wallet created from keys is not connected to authenticated daemon"); // TODO monero-project: keys wallets not connected
          assertTrue(wallet.isConnectedToDaemon(), "Wallet created from keys is not connected to authenticated daemon");
          if (!(wallet instanceof MoneroWalletRpc)) {
            MoneroUtils.validateMnemonic(wallet.getSeed()); // TODO monero-wallet-rpc: cannot get seed from wallet created from keys?
            assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getSeedLanguage());
          }
        } catch (Exception e) {
          e2 = e;
        }
        closeWallet(wallet);
        if (e2 != null) throw e2;
      }
      
      // attempt to create wallet at same path
      try {
        createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setNetworkType(TestUtils.NETWORK_TYPE));
        throw new Error("Should have thrown error");
      } catch(Exception e) {
        assertEquals("Wallet already exists", e.getMessage());
      }
    } catch (Exception e) {
      e1 = e;
    }
    
    if (e1 != null) throw new RuntimeException(e1);
  }

  
  // Can create wallets with subaddress lookahead
  @Override
  @Test
  public void testSubaddressLookahead() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    MoneroWallet receiver = null;
    try {
     
      // create wallet with high subaddress lookahead
      receiver = createWallet(new MoneroWalletConfig().setAccountLookahead(1).setSubaddressLookahead(50).setServerUri(TestUtils.WALLET_LWS_URI));
     
      // transfer funds to subaddress with high index // high index may not be supported by lws
      wallet.createTx(new MoneroTxConfig()
              .setAccountIndex(0)
              .addDestination(receiver.getSubaddress(0, 10).getAddress(), TestUtils.MAX_FEE)
              .setRelay(true));
     
      // observe unconfirmed funds
      //GenUtils.waitFor(1000);
      daemon.waitForNextBlockHeader();
      
      receiver.sync();
      assert(receiver.getBalance().compareTo(new BigInteger("0")) > 0);
    } catch (Exception e) {
      e1 = e;
    }
   
    if (receiver != null) closeWallet(receiver);
    if (e1 != null) throw new RuntimeException(e1);
 }
  
  
  @Override
  @Test
  public void testGetVersion() {
    super.testGetVersion();
  }
  
  @Override
  @Disabled
  @Test
  public void testGetPath() {
    super.testGetPath();
  }
  
  @Override
  @Test
  public void testSetDaemonConnection() {
    
    // create random wallet with default daemon connection
    MoneroWallet wallet = createWallet(new MoneroWalletConfig());
    assertEquals(new MoneroRpcConnection(TestUtils.WALLET_LWS_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD), wallet.getDaemonConnection());
    assertTrue(wallet.isConnectedToDaemon()); // uses default localhost connection
    
    // set empty server uri
    wallet.setDaemonConnection("");
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());
    
    // set offline server uri
    wallet.setDaemonConnection(TestUtils.OFFLINE_SERVER_URI);
    assertEquals(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI, "", ""), wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());
    
    // set daemon with wrong credentials
    wallet.setDaemonConnection(TestUtils.WALLET_LWS_URI, "wronguser", "wrongpass");
    assertEquals(new MoneroRpcConnection(TestUtils.WALLET_LWS_URI, "wronguser", "wrongpass"), wallet.getDaemonConnection());
    if ("".equals(TestUtils.DAEMON_RPC_USERNAME) || TestUtils.DAEMON_RPC_USERNAME == null) assertTrue(wallet.isConnectedToDaemon()); // TODO: monerod without authentication works with bad credentials?
    else assertFalse(wallet.isConnectedToDaemon());
    
    // set daemon with authentication
    wallet.setDaemonConnection(TestUtils.WALLET_LWS_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD);
    assertEquals(new MoneroRpcConnection(TestUtils.WALLET_LWS_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD), wallet.getDaemonConnection());
    assertTrue(wallet.isConnectedToDaemon());
    
    // nullify daemon connection
    wallet.setDaemonConnection((String) null);
    assertEquals(null, wallet.getDaemonConnection());
    wallet.setDaemonConnection(TestUtils.WALLET_LWS_URI);
    assertEquals(new MoneroRpcConnection(TestUtils.WALLET_LWS_URI), wallet.getDaemonConnection());
    wallet.setDaemonConnection((MoneroRpcConnection) null);
    assertEquals(null, wallet.getDaemonConnection());
    
    // set daemon uri to non-daemon
    wallet.setDaemonConnection("www.getmonero.org");
    assertEquals(new MoneroRpcConnection("www.getmonero.org"), wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());
    
    // set daemon to invalid uri
    wallet.setDaemonConnection("abc123");
    assertFalse(wallet.isConnectedToDaemon());
    
    // attempt to sync
    try {
      wallet.sync();
      fail("Exception expected");
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    } finally {
      closeWallet(wallet);
    }
  }

  // Can use a connection manager, must setup monero than one lws
  @Test
  @Disabled
  @Override
  public void testConnectionManager() {

    // create connection manager with monerod connections
    MoneroConnectionManager connectionManager = new MoneroConnectionManager();
    MoneroRpcConnection connection1 = new MoneroRpcConnection(wallet.getDaemonConnection()).setPriority(1);
    MoneroRpcConnection connection2 = new MoneroRpcConnection("localhost:8443").setPriority(2);
    connectionManager.setConnection(connection1);
    connectionManager.addConnection(connection2);

    // create wallet with connection manager
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setServerUri("").setConnectionManager(connectionManager));
    assertEquals(getRpcConnection(), wallet.getDaemonConnection());
    assertTrue(wallet.isConnectedToDaemon());

    // set manager's connection
    connectionManager.setConnection(connection2);
    GenUtils.waitFor(TestUtils.AUTO_CONNECT_TIMEOUT_MS);
    assertEquals(connection2, wallet.getDaemonConnection());

    // reopen wallet with connection manager
    closeWallet(wallet);
    wallet = openWallet(new MoneroWalletConfig().setServerUri("").setConnectionManager(connectionManager));
    assertEquals(connection2, wallet.getDaemonConnection());

    // disconnect
    connectionManager.setConnection((String) null);
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());

    // start polling connections
    connectionManager.startPolling(TestUtils.SYNC_PERIOD_IN_MS);

    // test that wallet auto connects
    GenUtils.waitFor(TestUtils.AUTO_CONNECT_TIMEOUT_MS);
    assertEquals(connection1, wallet.getDaemonConnection());
    assertTrue(wallet.isConnectedToDaemon());

    // test override with bad connection
    wallet.addListener(new MoneroWalletListener());
    connectionManager.setAutoSwitch(false);
    connectionManager.setConnection("http://foo.bar.xyz");
    assertEquals("http://foo.bar.xyz", wallet.getDaemonConnection().getUri());
    assertEquals(wallet.isConnectedToDaemon(), false);
    GenUtils.waitFor(5000);
    assertEquals(wallet.isConnectedToDaemon(), false);

    // set to another connection manager
    MoneroConnectionManager connectionManager2 = new MoneroConnectionManager();
    connectionManager2.setConnection(connection2);
    wallet.setConnectionManager(connectionManager2);
    assertEquals(connection2, wallet.getDaemonConnection());

    // unset connection manager
    wallet.setConnectionManager(null);
    assertEquals(null, wallet.getConnectionManager());
    assertEquals(connection2, wallet.getDaemonConnection());

    // stop polling and close
    connectionManager.stopPolling();
    closeWallet(wallet);
  }


  @Override
  @Test
  public void testGetHeight() {
    super.testGetHeight();
  }

  @Override
  @Test
  @Disabled
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

  // Light wallet doesn't support check tx key
  @Override
  @Test
  @Disabled
  public void testCheckTxKey() {
    super.testCheckTxKey();
  }

  // Light wallet doesn't support check tx proof
  @Override
  @Test
  @Disabled
  public void testCheckTxProof() {
    super.testCheckTxProof();
  }

  // Light wallet doesn't support check spend proof
  @Override
  @Test
  @Disabled
  public void testCheckSpendProof() {
    super.testCheckSpendProof();
  }

  // TODO monero-project: still no way to get rpc_version from lws
  @Override
  @Test
  @Disabled
  public void testGetReserveProofWallet() {
    super.testGetReserveProofWallet();
  }

  // TODO monero-project: still no way to get rpc_version from lws
  @Override
  @Test
  @Disabled
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
  public void testGetNewKeyImagesFromLastImport() {
    super.testGetNewKeyImagesFromLastImport();
  }
  
  // Supports view-only and offline wallets to create, sign, and submit transactions
  @SuppressWarnings("unused")
  @Test
  @Override
  public void testViewOnlyAndOfflineWallets() {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only and offline wallets
    MoneroWallet viewOnlyWallet = openWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setServer(wallet.getDaemonConnection()));
    MoneroWallet offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    assertFalse(offlineWallet.isConnectedToDaemon());
    viewOnlyWallet.sync();
    
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

  // Light wallet has no mining support
  @Override
  @Test
  @Disabled
  public void testMining() {
    super.testMining();
  }
  
  @Override
  @Test
  public void testValidateInputsSendingFunds() {
    super.testValidateInputsSendingFunds();
  }
  
  // Light wallet cannot be aware of txs in pool
  @Override
  @Test
  @Disabled
  public void testSyncWithPoolSameAccounts() {
    super.testSyncWithPoolSameAccounts();
  }

  // Light wallet cannot be aware of txs in pool
  @Override
  @Test
  @Disabled
  public void testSyncWithPoolSubmitAndDiscard() {
    super.testSyncWithPoolSubmitAndDiscard();
  }
  
  // Light wallet cannot be aware of txs in pool
  @Override
  @Test
  @Disabled
  public void testSyncWithPoolSubmitAndRelay() {
    super.testSyncWithPoolSubmitAndRelay();
  }
  
  // Light wallet cannot be aware of txs in pool
  @Override
  @Test
  @Disabled
  public void testSyncWithPoolRelay() {
    super.testSyncWithPoolRelay();
  }
  
  @Override
  @Test
  public void testSendToSelf() {
    assumeTrue(TEST_RELAYS);
    
    // wait for txs to confirm and for sufficient unlocked balance
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    BigInteger amount = TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3));
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, 0, null, amount);
    
    // collect sender balances before
    BigInteger balance1 = wallet.getBalance();
    BigInteger unlockedBalance1 = wallet.getUnlockedBalance();
    
    // send funds to self
    MoneroTxWallet tx = wallet.createTx(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getIntegratedAddress().getIntegratedAddress())
            .setAmount(amount)
            .setRelay(true));
    
    // test balances after
    BigInteger balance2 = wallet.getBalance();
    BigInteger unlockedBalance2 = wallet.getUnlockedBalance();
    assertTrue(unlockedBalance2.compareTo(unlockedBalance1) < 0); // unlocked balance should decrease
    BigInteger expectedBalance = balance1.subtract(tx.getFee());
    if (!expectedBalance.equals(balance2)) System.out.println("Expected=" + expectedBalance + " - Actual=" + balance2 + " = " + (expectedBalance.subtract(balance2)));
    assertEquals(expectedBalance, balance2, "Balance after send was not balance before - fee");
  }
  
  
// Can send to an external address
  @Override
  @Test
  public void testSendToExternal() {
    assumeTrue(TEST_RELAYS);
    MoneroWalletLight recipient = null;
    try {
      
      // wait for txs to confirm and for sufficient unlocked balance
      TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
      BigInteger amount = TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3));
      TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, 0, null, amount);
      
      // create recipient wallet
      recipient = createWallet(new MoneroWalletConfig());
      
      // collect sender balances before
      BigInteger balance1 = wallet.getBalance();
      BigInteger unlockedBalance1 = wallet.getUnlockedBalance();
      
      // send funds to recipient
      MoneroTxWallet tx = wallet.createTx(new MoneroTxConfig()
              .setAccountIndex(0)
              .setAddress(wallet.getIntegratedAddress(recipient.getPrimaryAddress(), "54491f3bb3572a37").getIntegratedAddress())
              .setAmount(amount)
              .setRelay(true));
      
      // test sender balances after
      BigInteger balance2 = wallet.getBalance();
      BigInteger unlockedBalance2 = wallet.getUnlockedBalance();
      assertTrue(unlockedBalance2.compareTo(unlockedBalance1) < 0); // unlocked balance should decrease
      BigInteger expectedBalance = balance1.subtract(tx.getOutgoingAmount()).subtract(tx.getFee());
      assertEquals(expectedBalance, balance2, "Balance after send was not balance before - net tx amount - fee (5 - 1 != 4 test)");
      
      try { StartMining.startMining(); } catch (Exception e) { }

      // test recipient balance after
      // lws may not support unconfirmed txs, so it's better to wait for block confirmation
      daemon.waitForNextBlockHeader();

      recipient.sync();

      assertTrue(recipient.isSynced());
      //assertFalse(wallet.getTxs(new MoneroTxQuery().setIsConfirmed(false)).isEmpty());
      assertFalse(wallet.getTxs(new MoneroTxQuery().setIsConfirmed(true)).isEmpty());

      assertEquals(amount, recipient.getBalance());
    } finally {
      if (recipient != null && !recipient.isClosed()) closeWallet(recipient);
      try { daemon.stopMining(); } catch (Exception e) { }
    }
  }

  @Override
  @Test
  public void testSendFromSubaddresses() {
    super.testSendFromSubaddresses();
  }
  
  @Override
  @Disabled
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
  @Disabled
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
  @Disabled
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
  @Disabled
  @Test
  public void testSendToMultipleSplit() {
    super.testSendToMultipleSplit();
  }

  @Override
  @Disabled
  @Test
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
  @Disabled
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
  @Disabled
  @Test
  public void testSweepOutputs() {
    super.testSweepOutputs();
  }

  @Override
  @Disabled
  @Test
  public void testSweepSubaddresses() {
    super.testSweepSubaddresses();
  }

  @Override
  @Disabled
  @Test
  public void testSweepAccounts() {
    super.testSweepAccounts();
  }

  @Override
  @Disabled
  @Test
  public void testSweepWalletByAccounts() {
    super.testSweepWalletByAccounts();
  }

  @Override
  @Disabled
  @Test
  public void testSweepWalletBySubaddresses() {
    super.testSweepWalletBySubaddresses();
  }

  @Override
  @Disabled
  @Test
  public void testSweepDustNoRelay() {
    super.testSweepDustNoRelay();
  }

  @Override
  @Disabled
  @Test
  public void testSweepDust() {
    super.testSweepDust();
  }
  
  @Override
  @Test
  public void testScanTxs() {
    
    // get a few tx hashes
    List<String> txHashes = new ArrayList<String>();
    List<MoneroTxWallet> txs = wallet.getTxs();
    if (txs.size() < 3) fail("Not enough txs to scan");
    for (int i = 0; i < 3; i++) txHashes.add(txs.get(i).getHash());
    
    // start wallet without scanning
    MoneroWallet scanWallet = openWallet(new MoneroWalletConfig().setSeed(wallet.getSeed()).setServer(wallet.getDaemonConnection()).setNetworkType(wallet.getNetworkType()));
    scanWallet.stopSyncing(); // TODO: create wallet without daemon connection (offline does not reconnect, default connects to localhost, offline then online causes confirmed txs to disappear)
    assertTrue(scanWallet.isConnectedToDaemon());
    
    // scan txs
    scanWallet.scanTxs(txHashes);
    
    // TODO: scanning txs causes merge problems reconciling 0 fee, isMinerTx with test txs
    
//    // txs are scanned
//    assertEquals(txHashes.size(), scanWallet.getTxs().size());
//    for (int i = 0; i < txHashes.size(); i++) {
//      assertEquals(wallet.getTx(txHashes.get(i)), scanWallet.getTx(txHashes.get(i)));
//    }
//    List<MoneroTxWallet> scannedTxs = scanWallet.getTxs(txHashes);
//    assertEquals(txHashes.size(), scannedTxs.size());
    
    // close wallet
    closeWallet(scanWallet, false);
  }
  

  @Override
  @Test
  @Disabled
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }

  // Light wallet doesn't support multisign
  @Override
  @Test
  @Disabled
  public void testMultisig() {
    super.testMultisig();
  }

  // Light wallet doesn't support multisign

  @Override
  @Test
  @Disabled
  public void testMultisigStress() {
    super.testMultisigStress();
  }
  
  @Override
  @Disabled
  @Test
  public void testChangePassword() {
    super.testChangePassword();
  }
  
  @Override
  @Disabled
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
  
  // Can be created and receive funds
  @Override
  @Test
  public void testCreateAndReceive() {
    assumeTrue(TEST_NOTIFICATIONS);
    
    // create random wallet
    MoneroWallet receiver = createWallet(new MoneroWalletConfig());
    try {
      
      // listen for received outputs
      WalletNotificationCollector myListener = new WalletNotificationCollector();
      receiver.addListener(myListener);
      
      // wait for txs to confirm and for sufficient unlocked balance
      TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
      TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, 0, null, TestUtils.MAX_FEE);
      
      // send funds to the receiver
      MoneroTxWallet sentTx = wallet.createTx(new MoneroTxConfig().setAccountIndex(0).setAddress(receiver.getPrimaryAddress()).setAmount(TestUtils.MAX_FEE).setRelay(true));
      
      // wait for funds to confirm
      try { StartMining.startMining(); } catch (Exception e) { }
      while (!(wallet.getTx(sentTx.getHash())).isConfirmed()) {
        if (wallet.getTx(sentTx.getHash()).isFailed()) throw new Error("Tx failed in mempool: " + sentTx.getHash());
        daemon.waitForNextBlockHeader();
      }
      
      // receiver should have notified listeners of received outputs
      try { TimeUnit.MILLISECONDS.sleep(1000); } catch (InterruptedException e) {  throw new RuntimeException(e); } // zmq notifications received within 1 second
      // wallet2 doesn't notify outputs received when it is light mode
      //assertFalse(myListener.getOutputsReceived().isEmpty());
    } finally {
      closeWallet(receiver);
      try { daemon.stopMining(); } catch (Exception e) { }
    }
  }
  
  // Can freeze and thaw outputs
  @Override
  @Test
  public void testFreezeOutputs() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get an available output
    List<MoneroOutputWallet> outputs = wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false).setIsFrozen(false).setTxQuery(new MoneroTxQuery().setIsLocked(false)));
    for (MoneroOutputWallet output : outputs) assertFalse(output.isFrozen());
    assertTrue(outputs.size() > 0);
    MoneroOutputWallet output = outputs.get(0);
    assertFalse(output.getTx().isLocked());
    assertFalse(output.isSpent());
    assertFalse(output.isFrozen());
    assertFalse(wallet.isOutputFrozen(output.getKeyImage().getHex()));
    
    // freeze output by key image
    int numFrozenBefore = wallet.getOutputs(new MoneroOutputQuery().setIsFrozen(true)).size();
    wallet.freezeOutput(output.getKeyImage().getHex());
    assertTrue(wallet.isOutputFrozen(output.getKeyImage().getHex()));

    // test querying
    assertEquals(numFrozenBefore + 1, wallet.getOutputs(new MoneroOutputQuery().setIsFrozen(true)).size());
    outputs = wallet.getOutputs(new MoneroOutputQuery().setKeyImage(new MoneroKeyImage().setHex(output.getKeyImage().getHex())).setIsFrozen(true));
    assertEquals(1, outputs.size());
    MoneroOutputWallet outputFrozen = outputs.get(0);
    assertTrue(outputFrozen.isFrozen());
    assertEquals(output.getKeyImage().getHex(), outputFrozen.getKeyImage().getHex());
    
    // try to sweep frozen output
    try {
      //wallet.sweepOutput(new MoneroTxConfig().setAddress(wallet.getPrimaryAddress()).setKeyImage(output.getKeyImage().getHex()));
      //fail("Should have thrown error");
    } catch (MoneroError e) {
      assertEquals("No outputs found", e.getMessage());
    }
    
    // try to freeze null key image
    try {
      wallet.freezeOutput(null);
      fail("Should have thrown error");
    } catch (MoneroError e) {
      assertEquals("Must specify key image to freeze", e.getMessage());
    }
    
    // try to freeze empty key image
    try {
      wallet.freezeOutput("");
      fail("Should have thrown error");
    } catch (MoneroError e) {
        assertEquals("Must specify key image to freeze", e.getMessage());
    }
    
    // try to freeze bad key image
    try {
      wallet.freezeOutput("123");
      fail("Should have thrown error");
    } catch (MoneroError e) {
      //assertEquals("Bad key image", e.getMessage());
    }

    // thaw output by key image
    wallet.thawOutput(output.getKeyImage().getHex());
    assertFalse(wallet.isOutputFrozen(output.getKeyImage().getHex()));

    // test querying
    assertEquals(numFrozenBefore, wallet.getOutputs(new MoneroOutputQuery().setIsFrozen(true)).size());
    outputs = wallet.getOutputs(new MoneroOutputQuery().setKeyImage(new MoneroKeyImage().setHex(output.getKeyImage().getHex())).setIsFrozen(true));
    assertEquals(0, outputs.size());
    outputs = wallet.getOutputs(new MoneroOutputQuery().setKeyImage(new MoneroKeyImage().setHex(output.getKeyImage().getHex())).setIsFrozen(false));
    assertEquals(1, outputs.size());
    MoneroOutputWallet outputThawed = outputs.get(0);
    assertFalse(outputThawed.isFrozen());
    assertEquals(output.getKeyImage().getHex(), outputThawed.getKeyImage().getHex());
  }
  
  @Override
  @Test
  @Disabled
  public void testInputKeyImages() {
    super.testInputKeyImages();
  }

  // Light wallet cannot prove unrelayed txs
  @Override
  @Test
  @Disabled
  public void testProveUnrelayedTxs() {
    super.testProveUnrelayedTxs();
  } 
    
  private void testSyncSeed(Long startHeight, Long restoreHeight) { testSyncSeed(startHeight, restoreHeight, false, false); }
  
  protected void testSyncSeed(Long startHeight, Long restoreHeight, boolean skipGtComparison, boolean testPostSyncNotifications) {
    assertTrue(daemon.isConnected(), "Not connected to daemon");
    if (startHeight != null && restoreHeight != null) assertTrue(startHeight <= TestUtils.FIRST_RECEIVE_HEIGHT || restoreHeight <= TestUtils.FIRST_RECEIVE_HEIGHT);
    
    // create wallet from seed
    MoneroWalletLight wallet = openWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED), false);
    
    // sanitize expected sync bounds
    if (restoreHeight == null) restoreHeight = 0l;
    long startHeightExpected = startHeight == null ? restoreHeight : startHeight;
    if (startHeightExpected == 0) startHeightExpected = 1;
    long endHeightExpected = wallet.getDaemonMaxPeerHeight();
    
    // test wallet and close as final step
    MoneroWalletFull walletGt = null;
    try {
      
      // test wallet's height before syncing
      assertTrue(wallet.isConnectedToDaemon());
      assertFalse(wallet.isSynced());
      assertEquals(1, wallet.getHeight());
      
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
        walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getSeed(), startHeight, restoreHeight);
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
            TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS + 3000); // sleep for wallet interval + time to sync
          } catch (InterruptedException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
          }
          
          // test that wallet listener's onSyncProgress() and onNewBlock() were invoked after previous completion
          assertTrue(walletSyncTester.getOnSyncProgressAfterDone());
          assertTrue(walletSyncTester.getOnNewBlockAfterDone());
        } finally {
          if (startedMining) daemon.stopMining();
        }
      }
    } finally {
      if (walletGt != null) walletGt.close(true);
      wallet.close();
    }
  }

}
