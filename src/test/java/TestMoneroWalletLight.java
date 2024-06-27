import monero.common.MoneroConnectionManager;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletLight;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroCheckTx;
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

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@TestInstance(Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions
public class TestMoneroWalletLight extends TestMoneroWalletFull {

  public TestMoneroWalletLight() {
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
    MoneroMiningStatus status = daemon.getMiningStatus();
    if (status.isActive()) daemon.stopMining();
  }

  public MoneroRpcConnection getRpcConnection()
  {
    return new MoneroRpcConnection(TestUtils.WALLET_LWS_URI);
  }

  @Override
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

  @Override
  protected MoneroWalletLight createWallet(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServerUri(TestUtils.WALLET_LWS_URI);
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
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
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(getRpcConnection());
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
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

  @Override
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
   * This test demonstrates that importing key images erases incoming transfers.
   */
  @Disabled // TODO monero-project: fix this https://github.com/monero-project/monero/issues/5812
  @Test
  @Override
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
  @Disabled // TODO monero-project: disabled because observing memory leak behavior when all tests run together
  @Override
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
  // ------------------------------- BEGIN TESTS ------------------------------
  
  // Can get the daemon's height
  @Test
  @Override
  public void testDaemon() {
    assumeTrue(TEST_NON_RELAYS);
    assertTrue(wallet.isConnectedToDaemon());
    long daemonHeight = wallet.getDaemonHeight();
    assertTrue(daemonHeight > 0);
  }
  
  // Can get the daemon's max peer height
  @Test
  @Override
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
  
  // Can create a random full wallet
  @Test
  @Override
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
    } catch (MoneroError e) {
      assertEquals("Wallet is not connected to daemon", e.getMessage());
    }
    
    // set daemon connection and check chain height
    wallet.setDaemonConnection(getRpcConnection());
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
    assertTrue(getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.isConnectedToDaemon());
    assertEquals("Spanish", wallet.getSeedLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero-project: why is height of unsynced wallet 1?
    if (daemon.isConnected()) assertEquals(daemon.getHeight(), wallet.getRestoreHeight());
    else assertTrue(wallet.getRestoreHeight() >= 0);
    wallet.close();
  }
  
  // Can create a full wallet from seed
  @Test
  @Override
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
    try { wallet.startSyncing(); } catch (MoneroError e) { assertEquals("Wallet is not connected to daemon", e.getMessage()); }
    wallet.close();
    
    // create wallet without restore height
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED), false);
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(getRpcConnection().equals(wallet.getDaemonConnection()));
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
    assertEquals(0, wallet.getRestoreHeight()); // restore height is lost after closing
    wallet.close();

    // create wallet with seed, connection, and restore height
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    assertEquals(TestUtils.SEED, wallet.getSeed());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(getRpcConnection().equals(wallet.getDaemonConnection()));
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
  @Override
  public void testCreateWalletFromKeysJni() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletFull walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    try {
      assertEquals(wallet.getSeed(), walletKeys.getSeed());
      assertEquals(wallet.getPrimaryAddress(), walletKeys.getPrimaryAddress());
      assertEquals(wallet.getPrivateViewKey(), walletKeys.getPrivateViewKey());
      assertEquals(wallet.getPublicViewKey(), walletKeys.getPublicViewKey());
      assertEquals(wallet.getPrivateSpendKey(), walletKeys.getPrivateSpendKey());
      assertEquals(wallet.getPublicSpendKey(), walletKeys.getPublicSpendKey());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletKeys.getRestoreHeight());
      assertTrue(walletKeys.isConnectedToDaemon());
      assertFalse(walletKeys.isSynced());
    } finally {
      walletKeys.close();
    }
  }
  
  // Is compatible with monero-wallet-rpc wallet files
  @Test
  @Override
  public void testWalletFileCompatibility() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create wallet using monero-wallet-rpc
    String walletName = GenUtils.getUUID();
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    walletRpc.createWallet(new MoneroWalletConfig().setPath(walletName).setPassword(TestUtils.WALLET_PASSWORD).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    walletRpc.sync();
    BigInteger balance = walletRpc.getBalance();
    String outputsHex = walletRpc.exportOutputs();
    walletRpc.close(true);
    
    // open as full wallet
    MoneroWalletLight walletFull = MoneroWalletLight.openWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/" + walletName).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.sync();
    assertEquals(TestUtils.SEED, walletFull.getSeed());
    assertEquals(TestUtils.ADDRESS, walletFull.getPrimaryAddress());
    assertEquals(balance, walletFull.getBalance());
    assertEquals(outputsHex.length(), walletFull.exportOutputs().length());
    walletFull.close(true);
    
    // create full wallet
    walletName = GenUtils.getUUID();
    String path = TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/" + walletName;
    walletFull = MoneroWalletLight.createWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.sync();
    balance = walletFull.getBalance();
    outputsHex = walletFull.exportOutputs();
    walletFull.close(true);
    
    // rebuild wallet cache using full wallet
    new File(path).delete();
    walletFull = MoneroWalletLight.openWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setServerUri(TestUtils.DAEMON_RPC_URI).setServerUsername(TestUtils.DAEMON_RPC_USERNAME).setServerPassword(TestUtils.DAEMON_RPC_PASSWORD));
    walletFull.close(true);
    
    // open wallet using monero-wallet-rpc
    walletRpc.openWallet(new MoneroWalletConfig().setPath(walletName).setPassword(TestUtils.WALLET_PASSWORD));
    walletRpc.sync();
    assertEquals(TestUtils.SEED, walletRpc.getSeed());
    assertEquals(TestUtils.ADDRESS, walletRpc.getPrimaryAddress());
    assertEquals(balance, walletRpc.getBalance());
    assertEquals(outputsHex.length(), walletRpc.exportOutputs().length());
    walletRpc.close(true);
  }
  
  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
  @Disabled
  @Override
  public void testViewOnlyAndOfflineWalletCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only wallet in wallet rpc process
    MoneroWalletLight viewOnlyWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setServerUri(TestUtils.WALLET_LWS_URI));
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
  
  // Is compatible with monero-wallet-rpc multisig wallets
  @Test
  @Disabled
  @Override
  public void testMultisigCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE);
    
    // create participants with full wallet and monero-wallet-rpc
    List<MoneroWallet> participants = new ArrayList<MoneroWallet>();
    participants.add(TestUtils.startWalletRpcProcess().createWallet(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD)));
    participants.add(TestUtils.startWalletRpcProcess().createWallet(new MoneroWalletConfig().setPath(GenUtils.getUUID()).setPassword(TestUtils.WALLET_PASSWORD)));
    participants.add(createWallet(new MoneroWalletConfig()));
    
    // test multisig
    try {
      testMultisig(participants, 3, 3, true);
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
      
      // save and close participants
      if (participants.get(0) instanceof MoneroWalletRpc) TestUtils.stopWalletRpcProcess((MoneroWalletRpc) participants.get(0));
      else participants.get(0).close(true); // multisig tests might restore wallet from seed
      TestUtils.stopWalletRpcProcess((MoneroWalletRpc) participants.get(1));
      closeWallet(participants.get(2), true);
    }
  };
  
  // Can re-sync an existing wallet from scratch
  @Test
  public void testResyncExisting() {
    assertTrue(MoneroWalletFull.walletExists(TestUtils.WALLET_FULL_PATH));
    MoneroWalletFull wallet = openWallet(new MoneroWalletConfig().setPath(TestUtils.WALLET_FULL_PATH).setServerUri(TestUtils.OFFLINE_SERVER_URI), false);
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    //long startHeight = TestUtils.TEST_RESTORE_HEIGHT;
    long startHeight = 0;
    SyncProgressTester progressTester = new SyncProgressTester(wallet, startHeight, wallet.getDaemonHeight());
    wallet.setRestoreHeight(1);
    MoneroSyncResult result = wallet.sync(1l, progressTester);
    progressTester.onDone(wallet.getDaemonHeight());
    
    // test result after syncing
    assertTrue(wallet.isConnectedToDaemon());
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
    assertEquals(TestUtils.getWalletLight().getDaemonConnection(), wallet.getDaemonConnection());
    // why test current daemon.getHeight() before syncing?
    //assertEquals(restoreHeight, wallet.getDaemonHeight());
    assertTrue(wallet.isConnectedToDaemon());
    assertFalse(wallet.isSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight());
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
  @Disabled
  @Override
  public void testSyncSeedFromGenesis() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(null, null, true, false);
  }
  
  // Can sync a wallet created from seed from a restore height
  @Test
  @Disabled
  @Override
  public void testSyncSeedFromRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS);
    testSyncSeed(null, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  // Can sync a wallet created from seed from a start height.
  @Test
  @Disabled
  @Override
  public void testSyncSeedFromStartHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(TestUtils.FIRST_RECEIVE_HEIGHT, null, false, true);
  }
  
  // Can sync a wallet created from seed from a start height less than the restore height
  @Test
  @Disabled
  @Override
  public void testSyncSeedStartHeightLTRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(TestUtils.FIRST_RECEIVE_HEIGHT, TestUtils.FIRST_RECEIVE_HEIGHT + 3l);
  }
  
  // Can sync a wallet created from seed from a start height greater than the restore height
  @Test
  @Disabled
  @Override
  public void testSyncSeedStartHeightGTRestoreHeight() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncSeed(TestUtils.FIRST_RECEIVE_HEIGHT + 3l, TestUtils.FIRST_RECEIVE_HEIGHT);
  }
  
  // Can sync a wallet created from keys
  @Test
  @Override
  @Disabled
  public void testSyncWalletFromKeys() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletFull walletKeys = createWallet(new MoneroWalletConfig().setPath(path).setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT), false);
    
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
    MoneroWalletFull wallet = createWallet(new MoneroWalletConfig().setServerUri(TestUtils.OFFLINE_SERVER_URI));
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
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setServerUri(TestUtils.OFFLINE_SERVER_URI));
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
    long restoreHeight = daemon.getHeight() - 100;
    path = getRandomWalletPath();
    wallet = createWallet(new MoneroWalletConfig().setPath(path).setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    try {
      
      // start syncing
      assertEquals(1, wallet.getHeight());
      assertEquals(restoreHeight, wallet.getRestoreHeight());
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
  @Disabled
  public void testWalletsDoNotInterfere() {
    
    // create 2 wallets with a recent restore height
    long height = daemon.getHeight();
    long restoreHeight = height - 5;
    MoneroWalletFull wallet1 = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    MoneroWalletFull wallet2 = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(restoreHeight), false);
    
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
    walletRpc.createWallet(new MoneroWalletConfig().setPath(UUID.randomUUID().toString()).setPassword(TestUtils.WALLET_PASSWORD).setSeed(walletRpc.getSeed()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setSeedOffset(seedOffset));
    
    // create full wallet with offset
    MoneroWalletFull walletFull = createWallet(new MoneroWalletConfig()
            .setPath(getRandomWalletPath())
            .setPassword(TestUtils.WALLET_PASSWORD)
            .setNetworkType(TestUtils.NETWORK_TYPE)
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
  @Override
  public void testSave() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create unique path for new test wallet
    String path = getRandomWalletPath();
    
    // wallet does not exist
    assertFalse(MoneroWalletLight.walletExists(path));
    
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
      wallet.setDaemonConnection(getRpcConnection());
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
      assertEquals(0, wallet.getRestoreHeight()); // TODO monero-project: restoreHeight is reset to 0 after closing
      
      // set the wallet's connection and sync
      wallet.setDaemonConnection(getRpcConnection());
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
      wallet.setDaemonConnection(getRpcConnection());  // TODO monero-project: daemon connection not stored in wallet files so must be explicitly set each time
      assertEquals(getRpcConnection(), wallet.getDaemonConnection());
      assertTrue(wallet.isConnectedToDaemon());
      assertEquals(prevHeight, wallet.getHeight());
      assertEquals(0, wallet.getRestoreHeight()); // TODO monero-project: restoreHeight is reset to 0 after closing
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
  public void testGetData() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroWalletFull wallet = null;
    MoneroWalletFull wallet2 = null;
    try {

      // create random wallet
      wallet = MoneroWalletLight.createWallet(new MoneroWalletConfig()
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

  // Can be moved
  @Test
  @Override
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
    try { wallet.getSeed(); }
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
  @Disabled
  @Override
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

  // Does not leak memory
  @Test
  @Disabled
  public void testMemoryLeak() {
    System.out.println("Infinite loop starting, monitor memory in OS process manager...");
    System.gc();
    int i = 0;
    boolean closeWallet = false;
    long time = System.currentTimeMillis();
    if (closeWallet) wallet.close(true);
    while (true) {
      if (closeWallet) wallet = TestUtils.getWalletFull();
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
      if (closeWallet) wallet.close(true);
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
      else if (wallet instanceof MoneroWalletLight) assertTrue(height > prevHeight);
      else assertEquals(height, prevHeight + 1);
      prevHeight = height;
    }
    
    public void onDone(long chainHeight) {
      assertFalse(isDone);
      this.isDone = true;
      if (prevHeight == null) {
        assertNull(prevCompleteHeight);
        assertEquals(chainHeight, startHeight);
      } 
      else if (wallet instanceof MoneroWalletLight) {
        assertTrue(chainHeight > prevHeight);
      }
      else {
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
    assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight() + 1);
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
    assertEquals(wallet1.getRestoreHeight() + 1, wallet2.getRestoreHeight());
    assertEquals(wallet1.getSeedLanguage(), wallet2.getSeedLanguage());
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
  public void testCreateWalletFromSeed() {
    super.testCreateWalletFromSeed();
  }
  
  @Override
  @Test
  public void testCreateWalletFromSeedWithOffset() {
    super.testCreateWalletFromSeedWithOffset();
  }
  
  @Override
  @Test
  public void testCreateWalletFromKeys() {
    super.testCreateWalletFromKeys();
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
      receiver = createWallet(new MoneroWalletConfig().setAccountLookahead(1).setSubaddressLookahead(50));
     
      // transfer funds to subaddress with high index
      wallet.createTx(new MoneroTxConfig()
              .setAccountIndex(0)
              .addDestination(receiver.getSubaddress(0, 85000).getAddress(), TestUtils.MAX_FEE)
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

  // Can use a connection manager
  @Test
  @Override
  public void testConnectionManager() {

    // create connection manager with monerod connections
    MoneroConnectionManager connectionManager = new MoneroConnectionManager();
    MoneroRpcConnection connection1 = new MoneroRpcConnection(wallet.getDaemonConnection()).setPriority(1);
    MoneroRpcConnection connection2 = new MoneroRpcConnection("localhost:48081").setPriority(2);
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
    String path = wallet.getPath();
    closeWallet(wallet);
    wallet = openWallet(new MoneroWalletConfig().setServerUri("").setConnectionManager(connectionManager).setPath(path));
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
  @Disabled
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
  @Disabled
  public void testCheckTxProof() {
    super.testCheckTxProof();
  }

  @Override
  @Test
  @Disabled
  public void testCheckSpendProof() {
    super.testCheckSpendProof();
  }

  @Override
  @Test
  @Disabled
  public void testGetReserveProofWallet() {
    super.testGetReserveProofWallet();
  }

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

  @Override
  @Test
  @Disabled // TODO (monero-project): disabled because importing key images deletes corresponding incoming transfers: https://github.com/monero-project/monero/issues/5812
  public void testImportKeyImages() {
    super.testImportKeyImages();
  }
  
  // Supports view-only and offline wallets to create, sign, and submit transactions
  @SuppressWarnings("unused")
  @Test
  @Override
  public void testViewOnlyAndOfflineWallets() {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only and offline wallets
    MoneroWallet viewOnlyWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
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
  @Disabled
  public void testSendToSelf() {
    super.testSendToSelf();
  }
  
  @Override
  @Test
  @Disabled
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
  @Test
  @Disabled
  public void testSweepOutputs() {
    super.testSweepOutputs();
  }

  @Override
  @Test
  @Disabled
  public void testSweepSubaddresses() {
    super.testSweepSubaddresses();
  }

  @Override
  @Test
  @Disabled
  public void testSweepAccounts() {
    super.testSweepAccounts();
  }

  @Override
  @Test
  @Disabled
  public void testSweepWalletByAccounts() {
    super.testSweepWalletByAccounts();
  }

  @Override
  @Test
  @Disabled
  public void testSweepWalletBySubaddresses() {
    super.testSweepWalletBySubaddresses();
  }

  @Override
  @Test
  @Disabled
  public void testSweepDustNoRelay() {
    super.testSweepDustNoRelay();
  }

  @Override
  @Test
  @Disabled
  public void testSweepDust() {
    super.testSweepDust();
  }
  
  @Override
  @Test
  @Disabled
  public void testScanTxs() {
    super.testScanTxs();
  }

  @Override
  @Test
  @Disabled
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }
  
  @Override
  @Test
  @Disabled
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
  public void testChangePassword() {
    super.testChangePassword();
  }
  
  @Override
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
  @Tag("NotificationTest")
  @Disabled
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
  @Disabled
  public void testCreateAndReceive() {
    super.testCreateAndReceive();
  }
  
  @Override
  @Test
  @Disabled
  public void testFreezeOutputs() {
    super.testFreezeOutputs();
  }
  
  @Override
  @Test
  @Disabled
  public void testInputKeyImages() {
    super.testInputKeyImages();
  }

  // Can prove unrelayed txs
  @Override
  @Test
  @Disabled
  public void testProveUnrelayedTxs() {
      
      // create unrelayed tx to verify
      String address1 = TestUtils.getExternalWalletAddress();
      String address2 = wallet.getAddress(0, 0);
      String address3 = wallet.getAddress(1, 0);
      MoneroTxWallet tx = wallet.createTx(new MoneroTxConfig()
              .setAccountIndex(0)
              .addDestination(address1, TestUtils.MAX_FEE)
              .addDestination(address2, TestUtils.MAX_FEE.multiply(BigInteger.valueOf(2l)))
              .addDestination(address3, TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3l))));
      
      // submit tx to daemon but do not relay
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      assertTrue(result.isGood());
      
      // create random wallet to verify transfers
      MoneroWallet verifyingWallet = createWalletFull(new MoneroWalletConfig());
      
      // verify transfer 1
      MoneroCheckTx check = verifyingWallet.checkTxKey(tx.getHash(), tx.getKey(), address1);
      assertTrue(check.isGood());
      assertTrue(check.inTxPool());
      assertEquals(0, check.getNumConfirmations());
      assertEquals(TestUtils.MAX_FEE, check.getReceivedAmount());
      
      // verify transfer 2
      check = verifyingWallet.checkTxKey(tx.getHash(), tx.getKey(), address2);
      assertTrue(check.isGood());
      assertTrue(check.inTxPool());
      assertEquals(0, check.getNumConfirmations());
      assertTrue(check.getReceivedAmount().compareTo(TestUtils.MAX_FEE.multiply(BigInteger.valueOf(2l))) >= 0); // + change amount
      
      // verify transfer 3
      check = verifyingWallet.checkTxKey(tx.getHash(), tx.getKey(), address3);
      assertTrue(check.isGood());
      assertTrue(check.inTxPool());
      assertEquals(0, check.getNumConfirmations());
      assertEquals(TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3l)), check.getReceivedAmount());
      
      // cleanup
      daemon.flushTxPool(tx.getHash());
      closeWallet(verifyingWallet);
  }
  
  
}
