

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import common.utils.GenUtils;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroKeyImageExportResult;
import monero.wallet.model.MoneroMultisigInfo;
import monero.wallet.model.MoneroMultisigInitResult;
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
    if (config.getRegtest() == null && config.getNetworkType() == TestUtils.NETWORK_TYPE) config.setRegtest(true);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(daemon.getRpcConnection());
    
    // open wallet
    MoneroWalletFull wallet = MoneroWalletFull.openWallet(config);
    if (startSyncing != false && wallet.isConnectedToDaemon()) wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    return wallet;
  }
  
  @Override
  protected MoneroWalletFull createWallet(MoneroWalletConfig config) {
    return createWallet(config, true);
  }
  
  protected MoneroWalletFull createWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getRegtest() == null && config.getNetworkType() == TestUtils.NETWORK_TYPE) config.setRegtest(true);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(daemon.getRpcConnection());
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
    // create wallet
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(config);
    if (!random) assertEquals(config.getRestoreHeight() == null ? 0l : config.getRestoreHeight(), wallet.getRestoreHeight());
    if (startSyncing != false && wallet.isConnectedToDaemon()) wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    return wallet;
  }
  
  @Override
  public void closeWallet(MoneroWallet wallet, boolean save) {
    if (wallet instanceof MoneroWalletRpc) TestUtils.stopWalletRpcProcess((MoneroWalletRpc) wallet);
    else wallet.close(save);
  }
  
  @Override
  protected List<String> getSeedLanguages() {
    return MoneroWalletFull.getSeedLanguages();
  }

  // --------------- DEMONSTRATION OF MONERO PROJECT ISSUES ----------------------
  
  /**
   * This test demonstrates that importing key images erases incoming transfers.
   */
  //@Disabled // TODO monero-project: fix this https://github.com/monero-project/monero/issues/5812
  @Test
  @Tag("CI")
  public void testImportKeyImagesAndTransfers() {
    MoneroWalletFull wallet = null; // create a wallet for this test since it becomes corrupt TODO: use common wallet and move to common tests when fixed
    try {
      
      // create and sync a new wallet
      String path = getRandomWalletPath();
      wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      wallet.sync();
      
      // repeatedly export and re-import key images and test transfer size
      for (int i = 0; i < 3; i++) {
        
        // get incoming transfers before importing
        List<MoneroTransfer> inTransfers1 = wallet.getTransfers(new MoneroTransferQuery().setIsIncoming(true));
        
        // export and re-import key images
        MoneroKeyImageExportResult keyImageResult = wallet.exportKeyImages();
        wallet.importKeyImages(keyImageResult.getKeyImages(), keyImageResult.getOffset());
        
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
  @Tag("CI")
  //@Disabled // TODO monero-project: disabled because observing memory leak behavior when all tests run together
  public void testCreateWalletsWithoutClose() {
    
    // lets make some wallets and leave them open
    List<MoneroWalletFull> wallets = new ArrayList<MoneroWalletFull>();
    try {
      for (int i = 0; i < 20; i++) {
        String path = getRandomWalletPath();
        MoneroWalletFull willLeaveYouHanging = createWallet(new MoneroWalletConfig().setPath(path));
        wallets.add(willLeaveYouHanging);
        willLeaveYouHanging.startSyncing();
        willLeaveYouHanging.addListener(new MoneroWalletListener());  // listen for wallet events which could aggrevate hanging
      }
      
      // check in on the daemon
      daemon.getHeight();
      
      // start mining
      StartMining.startMining();
      
      // wait for a block
      daemon.waitForNextBlockHeader();
      
      // stop mining
      StartMining.stopMining();
      
      // check in on the daemon
      daemon.getHeight();
    } finally {
      for (MoneroWalletFull wallet : wallets) wallet.close(); // release daemon connections for later tests
    }
  }
  
  // ------------------------------- BEGIN TESTS ------------------------------
  
//  @Test
//  public void getApproximateChainHeight() {
//    long height = wallet.getApproximateChainHeight();
//    assertTrue(height > 0);
//  }
  
  // Can create a random full wallet
  @Test
  @Tag("CI")
  public void testCreateWalletRandomFull() {
    assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setNetworkType(MoneroNetworkType.MAINNET).setServerUri(TestUtils.OFFLINE_SERVER_URI));
    MoneroUtils.validateMnemonic(wallet.getSeed());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), MoneroNetworkType.MAINNET);
    assertEquals(MoneroNetworkType.MAINNET, wallet.getNetworkType());
    assertEquals(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI), wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());
    assertEquals("English", wallet.getSeedLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why does height of new unsynced wallet start at 1?
    assertTrue(wallet.getRestoreHeight() >= 0);
    
    // cannot get daemon chain height
    try {
      wallet.getDaemonHeight();
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals("daemon error", e.getMessage()); // wallet2 masks errors from untrusted daemons
    }
    
    // set daemon connection and check chain height
    wallet.setDaemonConnection(daemon.getRpcConnection());
    assertEquals(daemon.getHeight(), wallet.getDaemonHeight());
    
    // close wallet which releases resources
    wallet.close();

    // create random wallet with non defaults
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setNetworkType(MoneroNetworkType.TESTNET).setLanguage("Spanish"), false);
    MoneroUtils.validateMnemonic(wallet.getSeed());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress(), MoneroNetworkType.TESTNET);
    assertEquals(MoneroNetworkType.TESTNET, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnectedToDaemon());
    assertEquals("Spanish", wallet.getSeedLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why is height of unsynced wallet 1?
    if (daemon.isConnected()) assertEquals(daemon.getHeight(), wallet.getRestoreHeight());
    else assertTrue(wallet.getRestoreHeight() >= 0);
    wallet.close();

    // test error creating wallet with regtest mode
    try {
      createWallet(new MoneroWalletConfig().setNetworkType(MoneroNetworkType.TESTNET).setRegtest(true));
      throw new RuntimeException("Should have thrown exception");
    } catch (Exception e) {
      assertEquals("Network type must be mainnet when using regtest option", e.getMessage());
    }
  }
  
  // Can create a full wallet from seed
  @Test
  @Tag("CI")
  public void testCreateWalletFromSeedFull() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create unconnected wallet with seed
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setServerUri(TestUtils.OFFLINE_SERVER_URI));
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI), wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());
    assertEquals("English", wallet.getSeedLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight());
    wallet.startSyncing(); // succeeds while offline, syncing when a daemon becomes reachable
    wallet.close();
    
    // create wallet without restore height
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED), false);
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnectedToDaemon());
    assertEquals("English", wallet.getSeedLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight()); // TODO: restore height is lost after closing only in JNI
    wallet.close();
    
    // create wallet with seed, no connection, and restore height
    long restoreHeight = 10000;
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight).setServerUri(TestUtils.OFFLINE_SERVER_URI));
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI), wallet.getDaemonConnection());
    assertFalse(wallet.isConnectedToDaemon());
    assertEquals("English", wallet.getSeedLanguage());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    assertEquals(path, wallet.getPath());
    wallet.close(true);
    wallet = openWallet(new MoneroWalletConfig().setPath(path).setServerUri(TestUtils.OFFLINE_SERVER_URI));
    assertFalse(wallet.isConnectedToDaemon());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    wallet.close();

    // create wallet with seed, connection, and restore height
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnectedToDaemon());
    assertEquals("English", wallet.getSeedLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    wallet.close();
  }
  
  // Can create a full wallet from keys
  @Test
  @Tag("CI")
  public void testCreateWalletFromKeysJni() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletFull walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    testCreateWalletFromKeysJni(walletKeys);
  }
  
  // Is compatible with monero-wallet-rpc wallet files
  @Test
  @Tag("CI")
  public void testWalletFileCompatibility() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create wallet using monero-wallet-rpc
    String walletName = GenUtils.getUUID();
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    walletRpc.createWallet(new MoneroWalletConfig().setPath(walletName).setPassword(TestUtils.WALLET_PASSWORD).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(walletRpc);
    BigInteger balance = walletRpc.getBalance();
    String outputsHex = walletRpc.exportOutputs();
    walletRpc.close(true);
    
    // open as full wallet
    MoneroWalletFull walletFull = MoneroWalletFull.openWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/" + walletName).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setRegtest(true).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(walletFull);
    assertEquals(TestUtils.SEED, walletFull.getSeed());
    assertEquals(TestUtils.ADDRESS, walletFull.getPrimaryAddress());
    assertEquals(balance, walletFull.getBalance());
    assertEquals(outputsHex.length(), walletFull.exportOutputs().length());
    walletFull.close(true);
    
    // create full wallet
    walletName = GenUtils.getUUID();
    String path = TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/" + walletName;
    walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setRegtest(true).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.sync();
    balance = walletFull.getBalance();
    outputsHex = walletFull.exportOutputs();
    walletFull.close(true);
    
    // rebuild wallet cache using full wallet
    new File(path).delete();
    walletFull = MoneroWalletFull.openWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setRegtest(true).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.close(true);
    
    // open wallet using monero-wallet-rpc
    walletRpc.openWallet(new MoneroWalletConfig().setPath(walletName).setPassword(TestUtils.WALLET_PASSWORD));
    walletRpc.sync();
    assertEquals(TestUtils.SEED, walletRpc.getSeed());
    assertEquals(TestUtils.ADDRESS, walletRpc.getPrimaryAddress());
    assertEquals(balance, walletRpc.getBalance());
    assertEquals(outputsHex.length(), walletRpc.exportOutputs().length());
    walletRpc.close(true); // TODO: this will not get called if there was an error above, leaving the wallet open
  }
  
  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
  @Tag("CI")
  public void testViewOnlyAndOfflineWalletCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only wallet in wallet rpc process
    MoneroWalletRpc viewOnlyWallet = TestUtils.createWalletRpc(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    viewOnlyWallet.sync();
    
    // create offline full wallet
    MoneroWalletFull offlineWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    
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
  @Tag("CI")
  public void testMultisigCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE);
    
    // create participants with full wallet and monero-wallet-rpc
    List<MoneroWallet> participants = new ArrayList<MoneroWallet>();
    participants.add(TestUtils.createWalletRpc(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD)));
    participants.add(TestUtils.createWalletRpc(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD)));
    participants.add(createWallet(new MoneroWalletConfig()));
    
    // test multisig
    try {
      testMultisig(participants, 3, 3, true);
    } finally {
      
      // stop mining at end of test
      StartMining.stopMining();
      
      // save and close participants
      for (MoneroWallet participant : participants) closeWallet(participant, true);
    }
  };
  
  // Can sync a wallet created from seed from the genesis
  @Test
  @Tag("CI")
  public void testSyncSeedFromGenesis() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(null, null, true, false);
  }
  
  // Can sync a wallet created from seed from a restore height
  @Test
  @Tag("CI")
  public void testSyncSeedFromRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS);
    testSyncSeed(null, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  // Can sync a wallet created from seed from a start height.
  @Test
  @Tag("CI")
  public void testSyncSeedFromStartHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(TestUtils.FIRST_RECEIVE_HEIGHT, null, false, true);
  }
  
  // Can sync a wallet created from seed from a start height less than the restore height
  @Test
  @Tag("CI")
  public void testSyncSeedStartHeightLTRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(TestUtils.FIRST_RECEIVE_HEIGHT, TestUtils.FIRST_RECEIVE_HEIGHT + 3l);
  }
  
  // Can sync a wallet created from seed from a start height greater than the restore height
  @Test
  @Tag("CI")
  public void testSyncSeedStartHeightGTRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(TestUtils.FIRST_RECEIVE_HEIGHT + 3l, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  private void testSyncSeed(Long startHeight, Long restoreHeight) { testSyncSeed(startHeight, restoreHeight, false, false); }
  private void testSyncSeed(Long startHeight, Long restoreHeight, boolean skipGtComparison, boolean testPostSyncNotifications) {
    assertTrue(daemon.isConnected(), "Not connected to daemon");
    if (startHeight != null && restoreHeight != null) assertTrue(startHeight <= TestUtils.FIRST_RECEIVE_HEIGHT || restoreHeight <= TestUtils.FIRST_RECEIVE_HEIGHT);

    // wait for txs to clear pool
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // create wallet from seed
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    testSyncSeed(wallet, startHeight, restoreHeight, skipGtComparison, testPostSyncNotifications);
  }
  
  // Does not interfere with other wallet notifications
  @Test
  @Tag("CI")
  public void testWalletsDoNotInterfere() {
    
    // create 2 wallets with a recent restore height
    long height = daemon.getHeight();
    long restoreHeight = height - 5;
    MoneroWalletFull wallet1 = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    MoneroWalletFull wallet2 = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    
    testWalletsDoNotInterfere(wallet1, wallet2, height, restoreHeight);
  }
  
  // Is equal to the RPC wallet.
  @Test
  @Tag("CI")
  public void testWalletEqualityRpc() {

    // wait for pending txs to clear from wallets before rescanning spent outputs
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearWallets(TestUtils.getWalletRpc(), wallet);

    // TODO: rescanning spent outputs is necessary for equality test to mark as spent/unspent correctly
    wallet.rescanSpent();
    TestUtils.getWalletRpc().rescanSpent();

    // compare wallets based on on-chain data
    WalletEqualityUtils.testWalletEqualityOnChain(TestUtils.getWalletRpc(), wallet);
  }
  
  // Is equal to the RPC wallet with a seed offset
  @Test
  @Tag("CI")
  public void testWalletEqualityRpcWithOffset() {
    
    // use common offset to compare wallet implementations
    String seedOffset = "my super secret offset!";
    
    // create rpc wallet with offset
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    walletRpc.createWallet(new MoneroWalletConfig().setPath(UUID.randomUUID().toString()).setPassword(TestUtils.WALLET_PASSWORD).setSeed(walletRpc.getSeed()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setSeedOffset(seedOffset));
    
    // create full wallet with offset
    MoneroWalletFull walletFull = createWallet(new MoneroWalletConfig()
            .setPath(getRandomWalletPath())
            .setPassword(TestUtils.WALLET_PASSWORD)
            .setNetworkType(TestUtils.NETWORK_TYPE)
            .setRegtest(true)
            .setSeed(TestUtils.SEED)
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
  @Tag("CI")
  public void testSave() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create unique path for new test wallet
    String path = getRandomWalletPath();
    
    // wallet does not exist
    assertFalse(MoneroWalletFull.walletExists(path));
    
    // cannot open non-existent wallet
    try {
      openWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE));
      fail("Cannot open non-existent wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
    }
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight).setServerUri(TestUtils.OFFLINE_SERVER_URI));
    
    // test wallet at newly created state
    try {
      
      assertTrue(MoneroWalletFull.walletExists(path));
      assertEquals(TestUtils.SEED, wallet.getSeed());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertEquals(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI), wallet.getDaemonConnection());
      assertEquals(restoreHeight, wallet.getRestoreHeight());
      assertEquals("English", wallet.getSeedLanguage());
      assertEquals(1, wallet.getHeight());
      assertEquals(restoreHeight, wallet.getRestoreHeight());
      
      // set the wallet's connection and sync
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
      wallet.sync();
      assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
      
      // close the wallet without saving
      wallet.close();
      
      // re-open the wallet
      wallet = openWallet(new MoneroWalletConfig().setPath(path).setServerUri(TestUtils.OFFLINE_SERVER_URI));
      
      // test wallet is at newly created state
      assertTrue(MoneroWalletFull.walletExists(path));
      assertEquals(TestUtils.SEED, wallet.getSeed());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertEquals(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI), wallet.getDaemonConnection());
      assertFalse(wallet.isConnectedToDaemon());
      assertEquals("English", wallet.getSeedLanguage());
      assertFalse(wallet.isSynced());
      assertEquals(1, wallet.getHeight());
      assertEquals(restoreHeight, wallet.getRestoreHeight());
      
      // set the wallet's connection and sync
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
      assertTrue(wallet.isConnectedToDaemon());
      wallet.setRestoreHeight(restoreHeight);
      wallet.sync();
      assertTrue(wallet.isSynced());
      assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
      long prevHeight = wallet.getHeight();
      
      // save and close the wallet
      wallet.save();
      wallet.close();
      
      // re-open the wallet
      wallet = openWallet(new MoneroWalletConfig().setPath(path).setServerUri(TestUtils.OFFLINE_SERVER_URI));
      
      // test wallet state is saved
      assertFalse(wallet.isConnectedToDaemon());
      wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());  // TODO monero-project: daemon connection not stored in wallet files so must be explicitly set each time
      assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
      assertTrue(wallet.isConnectedToDaemon());
      assertEquals(prevHeight, wallet.getHeight());
      assertEquals(restoreHeight, wallet.getRestoreHeight());
      assertTrue(MoneroWalletFull.walletExists(path));
      assertEquals(TestUtils.SEED, wallet.getSeed());
      assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
      assertEquals("English", wallet.getSeedLanguage());
      
      // sync
      wallet.sync();
    } finally {
      wallet.close();
    }
  }

  // Can export and import wallet files
  @Test
  @Tag("CI")
  public void testGetData() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroWalletFull wallet = null;
    MoneroWalletFull wallet2 = null;
    try {

      // create random wallet
      wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
              .setNetworkType(MoneroNetworkType.MAINNET)
              .setPassword("password123"));

      // export wallet files
      byte[][] walletData = wallet.getData();
      byte[] keysData = walletData[0];
      byte[] cacheData = walletData[1];

      // import keys file without cache
      wallet2 = MoneroWalletFull.openWallet(new MoneroWalletConfig()
          .setNetworkType(MoneroNetworkType.MAINNET)
          .setPassword("password123")
          .setKeysData(keysData));
      
      // import keys and cache files
      wallet2 = MoneroWalletFull.openWallet(new MoneroWalletConfig()
          .setNetworkType(MoneroNetworkType.MAINNET)
          .setPassword("password123")
          .setKeysData(keysData)
          .setCacheData(cacheData));

      // test that wallets are equal
      assertEquals(wallet2.getSeed(), wallet.getSeed());
      testWalletEqualityOnChain(wallet, wallet2);
    } finally {
      if (wallet != null) wallet.close();
      if (wallet2 != null) wallet2.close();
    }
  }

  // Can reopen regtest wallets from disk and keys after the chain advances
  @Test
  @Tag("CI")
  public void testOpenWalletRegtest() {
    String path = getRandomWalletPath();
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT), false);
    byte[] keys;
    try {
      wallet.sync();
      keys = wallet.getData()[0];
      wallet.save();
    } finally {
      wallet.close();
    }
    StartMining.generateBlocks(1);
    for (boolean fromKeys : new boolean[] {false, true}) {
      MoneroWalletConfig config = new MoneroWalletConfig().setNetworkType(TestUtils.NETWORK_TYPE).setRegtest(true).setPassword(TestUtils.WALLET_PASSWORD).setServer(daemon.getRpcConnection());
      if (fromKeys) config.setKeysData(keys);
      else config.setPath(path);
      wallet = MoneroWalletFull.openWallet(config);
      try {
        wallet.sync();
        assertEquals(daemon.getHeight(), wallet.getHeight());
        assertTrue(wallet.getUnlockedBalance().signum() > 0);
      } finally {
        wallet.close();
      }
    }
  }

  // Can be moved
  @Test
  @Tag("CI")
  public void testMoveTo() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroWalletFull wallet = null;
    Exception err = null;
    try {
      
      // create random in-memory wallet with defaults
      wallet = createWallet(new MoneroWalletConfig().setPath(""));
      String seed = wallet.getSeed();
      wallet.setAttribute("mykey", "myval1");

      // change password of in-memory wallet
      String password2 = "abc123";
      wallet.changePassword(TestUtils.WALLET_PASSWORD, password2);

      // move wallet from memory to disk
      String path1 = TestUtils.TEST_WALLETS_DIR + "/" + GenUtils.getUUID();
      assertTrue(!MoneroWalletFull.walletExists(path1));
      wallet.moveTo(path1);
      assertTrue(MoneroWalletFull.walletExists(path1));
      assertEquals(seed, wallet.getSeed());
      assertEquals(wallet.getAttribute("mykey"), "myval1");

      // move to same path which is same as saving
      wallet.setAttribute("mykey", "myval2");
      wallet.moveTo(path1);
      wallet.close();
      assertTrue(MoneroWalletFull.walletExists(path1));
      wallet = openWallet(new MoneroWalletConfig().setPath(path1).setPassword(password2));
      assertEquals(seed, wallet.getSeed());
      assertEquals(wallet.getAttribute("mykey"), "myval2");

      // move wallet to new directory
      String path2 = TestUtils.TEST_WALLETS_DIR + "/moved/" + GenUtils.getUUID();
      wallet.setAttribute("mykey", "myval3");
      wallet.moveTo(path2);
      assertFalse(MoneroWalletFull.walletExists(path1));
      assertTrue(MoneroWalletFull.walletExists(path2));
      assertEquals(seed, wallet.getSeed());
      
      // re-open and test wallet
      wallet.close();
      wallet = openWallet(new MoneroWalletConfig().setPath(path2).setPassword(password2));
      wallet.sync();
      assertEquals(seed, wallet.getSeed());
      assertEquals(wallet.getAttribute("mykey"), "myval3");
    } catch (Exception e) {
      err = e;
    }

    // final cleanup
    if (wallet != null) wallet.close();
    if (err != null) throw new RuntimeException(err);
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
//    // cannot open non-existent wallet
//    try {
//      new MoneroWalletFull(path, TestUtils.WALLET_FULL_PW, TestUtils.NETWORK_TYPE);
//      fail("Cannot open non-existent wallet");
//    } catch (MoneroException e) {
//      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
//    }
//
//    // create in-memory wallet to test (no connection, english)
//    MoneroWalletFull walletMemory = new MoneroWalletFull(TestUtils.NETWORK_TYPE, null, null);
//    assertEquals(TestUtils.NETWORK_TYPE, walletMemory.getNetworkType());
//    assertNull(walletMemory.getDaemonConnection());
//    assertEquals("English", walletMemory.getSeedLanguage());
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
//    // save wallet to the test wallets directory
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
  @Tag("CI")
  public void testClose() {
    String path = getRandomWalletPath();
    MoneroWalletConfig config = new MoneroWalletConfig().setPath(path);
    testClose(config);
  }
  
  // Supports multisig sample code
  @Test
  @Tag("CI")
  public void testMultisigSample() {
    testCreateMultisigWallet(1, 2);
    testCreateMultisigWallet(1, 4);
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
    
    // make each wallet multisig and collect results
    List<String> madeMultisigHexes = new ArrayList<String>();
    for (int i = 0; i < wallets.size(); i++) {
      
      // collect prepared multisig hexes from wallet's peers
      List<String> peerMultisigHexes = new ArrayList<String>();
      for (int j = 0; j < wallets.size(); j++) if (j != i) peerMultisigHexes.add(preparedMultisigHexes.get(j));
    
      // make wallet multisig and collect result hex
      String multisigHex = wallets.get(i).makeMultisig(peerMultisigHexes, M, TestUtils.WALLET_PASSWORD);
      madeMultisigHexes.add(multisigHex);
    }
    
    // exchange multisig keys N - M + 1 times
    List<String> multisigHexes = madeMultisigHexes;
    for (int i = 0; i < N - M + 1; i++) {
      
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
    super.testSyncRandom();
  }

  @Override
  @Test
  @Disabled // TODO monero-project: cannot re-sync from lower block height after wallet saved
  public void testResyncExisting() {
    super.testResyncExisting();
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

  // Can get the daemon's max peer height
  @Override
  @Test
  @Tag("CI")
  public void testGetDaemonMaxPeerHeight() {
    super.testGetDaemonMaxPeerHeight();
  }

  @Override
  @Test
  @Tag("CI")
  public void testDaemon()
  {
    super.testDaemon();
  }

  // Does not leak memory
  @Override
  @Test
  @Disabled
  public void testMemoryLeak() {
    super.testMemoryLeak();
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
  @Tag("CI")
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
  @Tag("CI")
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
  public void testGetSeedLanguage() {
    super.testGetSeedLanguage();
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
  public void testAccountTags() {
    super.testAccountTags();
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
  public void testGetSubaddressAddressOutOfRange() {
    super.testGetSubaddressAddressOutOfRange();
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
