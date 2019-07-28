package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.concurrent.TimeUnit;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpcConnection;
import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroSyncListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTxWallet;
import utils.TestUtils;

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
  
  // ------------------------------- BEGIN TESTS ------------------------------
  
  @Test
  public void testDaemon() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertTrue(wallet.getIsConnected());
    long daemonHeight = wallet.getDaemonHeight();
    assertTrue(daemonHeight > 0);
    long daemonTargetHeight = wallet.getDaemonTargetHeight();
    assertTrue("Expected target height >= daemon height but " + daemonTargetHeight + " < "  + daemonHeight, daemonTargetHeight >= daemonHeight);
    assertEquals(wallet.getIsDaemonSynced(), daemonHeight == daemonTargetHeight);
  }
  
  @Test
  public void testSetDaemonConnection() {
    
    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_JNI_PW);
    assertEquals(null, wallet.getDaemonConnection());
    
    // set daemon uri
    wallet.setDaemonConnection(TestUtils.DAEMON_RPC_URI);
    assertEquals(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI), wallet.getDaemonConnection());
    assertTrue(daemon.getIsConnected());
    
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
    assertFalse(wallet.getIsConnected());
    
    // set daemon to invalid uri
    wallet.setDaemonConnection("abc123");
    assertFalse(wallet.getIsConnected());
    
    // attempt to sync
    try {
      wallet.sync();
      fail("Exception expected");
    } catch (MoneroException e) {
      assertEquals("No connection to daemon", e.getMessage());
    } finally {
      wallet.close();
    }
  }
  
  @Test
  public void testCreateWalletRandom() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_JNI_PW);
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress());
    assertEquals(MoneroNetworkType.MAINNET, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.getIsConnected());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertTrue(wallet.getRestoreHeight() >= 0);
    
    // cannot get daemon chain height
    try {
      wallet.getChainHeight();
    } catch (MoneroException e) {
      assertEquals("No connection to daemon", e.getMessage());
    }
    
    // set daemon connection and check chain height
    wallet.setDaemonConnection(daemon.getRpcConnection());
    assertEquals(daemon.getHeight(), wallet.getChainHeight());
    
    // close wallet which releases resources
    wallet.close();

    // create random wallet with non defaults
    path = getRandomWalletPath();
    wallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_JNI_PW, MoneroNetworkType.TESTNET, daemon.getRpcConnection(), "Spanish");
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress());
    assertEquals(MoneroNetworkType.TESTNET, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.getIsConnected());
    assertEquals("Spanish", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why is height of unsynced wallet 1?
    if (daemon.getIsConnected()) assertEquals(daemon.getHeight(), wallet.getRestoreHeight());
    else assertTrue(wallet.getRestoreHeight() >= 0);
  }
  
  @Test
  public void testCreateWalletFromMnemonic() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create wallet with mnemonic and defaults
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, null, null);
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertFalse(wallet.getIsConnected());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight());
    wallet.close();
    
    // create wallet without restore height
    path = getRandomWalletPath();
    wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), null);
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.getIsConnected());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(0, wallet.getRestoreHeight());
    
    // create wallet with mnemonic, no connection, and restore height
    long restoreHeight = 10000;
    path = getRandomWalletPath();
    wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, null, restoreHeight);
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNull(wallet.getDaemonConnection());
    assertFalse(wallet.getIsConnected());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    wallet.save();
    wallet.close();
    wallet = MoneroWalletJni.openWallet(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    assertFalse(wallet.getIsConnected());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight()); // restore height is lost after closing
    wallet.close();

    // create wallet with mnemonic, connection, and restore height
    path = getRandomWalletPath();
    wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), restoreHeight);
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertTrue(wallet.getIsConnected());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
  }

  @Test
  public void testCreateWalletFromKeys() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletJni walletKeys = MoneroWalletJni.createWalletFromKeys(path, TestUtils.WALLET_JNI_PW, wallet.getPrimaryAddress(), wallet.getPrivateViewKey(), wallet.getPrivateSpendKey(), wallet.getNetworkType(), wallet.getDaemonConnection(), TestUtils.RESTORE_HEIGHT, null);
    try {
      assertEquals(wallet.getMnemonic(), walletKeys.getMnemonic());
      assertEquals(wallet.getPrimaryAddress(), walletKeys.getPrimaryAddress());
      assertEquals(wallet.getPrivateViewKey(), walletKeys.getPrivateViewKey());
      assertEquals(wallet.getPublicViewKey(), walletKeys.getPublicViewKey());
      assertEquals(wallet.getPrivateSpendKey(), walletKeys.getPrivateSpendKey());
      assertEquals(wallet.getPublicSpendKey(), walletKeys.getPublicSpendKey());
      assertEquals(TestUtils.RESTORE_HEIGHT, walletKeys.getRestoreHeight());
      assertTrue(walletKeys.getIsConnected());
      assertFalse(walletKeys.getIsSynced());
    } finally {
      walletKeys.close();
    }
  }
  
  // Can get the public view key
  @Test
  public void testGetPublicViewKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String publicViewKey = wallet.getPublicViewKey();
    MoneroUtils.validatePublicViewKey(publicViewKey);
  }
  
  // Can get the public spend key
  @Test
  public void testGetPublicSpendKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String publicSpendKey = wallet.getPublicSpendKey();
    MoneroUtils.validatePublicSpendKey(publicSpendKey);
  }
  
  // Can re-sync an existing wallet from scratch
  @Test
  @Ignore // TODO monero core: cannot re-sync from lower block height after wallet saved
  public void testResyncExisting() {
    assertTrue(MoneroWalletJni.walletExists(TestUtils.WALLET_JNI_PATH_1));
    MoneroWalletJni wallet = MoneroWalletJni.openWallet(TestUtils.WALLET_JNI_PATH_1, TestUtils.WALLET_JNI_PW, MoneroNetworkType.STAGENET);
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    //long startHeight = TestUtils.TEST_RESTORE_HEIGHT;
    long startHeight = 0;
    SyncProgressTester progressTester = new SyncProgressTester(startHeight, wallet.getChainHeight());
    wallet.setRestoreHeight(1);
    MoneroSyncResult result = wallet.sync(1l, progressTester);
    progressTester.onDone(wallet.getChainHeight());
    
    // test result after syncing
    assertTrue(wallet.getIsConnected());
    assertTrue(wallet.getIsSynced());
    assertEquals(wallet.getChainHeight() - startHeight, (long) result.getNumBlocksFetched());
    assertTrue(result.getReceivedMoney());
    assertEquals(daemon.getHeight(), wallet.getHeight());
  }

  // Can sync a wallet with a randomly generated seed
  @Test
  public void testSyncRandom() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertTrue("Not connected to daemon", daemon.getIsConnected());

    // create test wallet
    long restoreHeight = daemon.getHeight();
    MoneroWalletJni wallet = MoneroWalletJni.createWalletRandom(getRandomWalletPath(), TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection(), null);

    // test wallet's height before syncing
    assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
    assertEquals(restoreHeight, wallet.getChainHeight());
    assertTrue(wallet.getIsConnected());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    assertEquals(daemon.getHeight(), wallet.getChainHeight());

    // sync the wallet
    SyncProgressTester progressTester = new SyncProgressTester(wallet.getRestoreHeight(), wallet.getChainHeight());
    MoneroSyncResult result = wallet.sync(null, progressTester);
    progressTester.onDone(wallet.getChainHeight());
    
    // test result after syncing
    MoneroWalletJni walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getMnemonic(), restoreHeight);
    try {
      assertTrue(wallet.getIsConnected());
      assertTrue(wallet.getIsSynced());
      assertEquals(0, (long) result.getNumBlocksFetched());
      assertFalse(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), wallet.getHeight());

      // sync the wallet with default params
      wallet.sync();
      assertTrue(wallet.getIsSynced());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      
      // compare wallet to ground truth
      testWalletsEqualOnChain(walletGt, wallet);
    } finally {
      walletGt.close();
      wallet.close();
    }
    
    // attempt to sync unconnected wallet
    wallet = MoneroWalletJni.createWalletRandom(getRandomWalletPath(), TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, null, null);
    try {
      wallet.sync();
      fail("Should have thrown exception");
    } catch (MoneroException e) {
      // exception expected
    } finally {
      wallet.close();
    }
  }
  
  // Can sync a wallet with a mnemonic
  @Test
  public void testSyncMnemonicFromGenesis() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(null, null, true);
  }
  
  @Test
  public void testSyncMnemonicFromRestoreHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(null, TestUtils.RESTORE_HEIGHT);
  }
  
  @Test
  public void testSyncMnemonicFromStartHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(TestUtils.RESTORE_HEIGHT, null);
  }
  
  @Test
  public void testSyncMnemonicStartHeightLTRestoreHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(TestUtils.RESTORE_HEIGHT, TestUtils.RESTORE_HEIGHT + 3l);
  }
  
  @Test
  public void testSyncMnemonicStartHeightGTRestoreHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(TestUtils.RESTORE_HEIGHT + 3l, TestUtils.RESTORE_HEIGHT);
  }
  
  private void testSyncMnemonic(Long startHeight, Long restoreHeight) { testSyncMnemonic(startHeight, restoreHeight, false); }
  private void testSyncMnemonic(Long startHeight, Long restoreHeight, boolean skipGtComparison) {
    assertTrue("Not connected to daemon", daemon.getIsConnected());
    if (startHeight != null && restoreHeight != null) assertTrue(startHeight <= TestUtils.RESTORE_HEIGHT || restoreHeight <= TestUtils.RESTORE_HEIGHT);
    
    // create wallet from mnemonic
    MoneroWalletJni wallet = MoneroWalletJni.createWalletFromMnemonic(getRandomWalletPath(), TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection(), restoreHeight);
    
    // sanitize expected sync bounds
    if (restoreHeight == null) restoreHeight = 0l;
    long startHeightExpected = startHeight == null ? restoreHeight : startHeight;
    if (startHeightExpected == 0) startHeightExpected = 1;
    long endHeightExpected = wallet.getDaemonTargetHeight();
    
    // create ground truth wallet for comparison
    MoneroWalletJni walletGt = null;
    if (!skipGtComparison) walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, wallet.getMnemonic(), startHeightExpected);
    
    // test wallet and close as final step
    try {
      
      // test wallet's height before syncing
      assertTrue(wallet.getIsConnected());
      assertFalse(wallet.getIsSynced());
      assertEquals(1, wallet.getHeight());
      assertEquals((long) restoreHeight, (long) wallet.getRestoreHeight());
      
      // sync the wallet
      SyncProgressTester progressTester = new SyncProgressTester(startHeightExpected, endHeightExpected);
      MoneroSyncResult result = wallet.sync(startHeight, progressTester);
      progressTester.onDone(wallet.getChainHeight());
      
      // test result after syncing
      assertTrue(wallet.getIsSynced());
      assertEquals(wallet.getChainHeight() - startHeightExpected, (long) result.getNumBlocksFetched());
      assertTrue(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      assertEquals(daemon.getHeight(), wallet.getChainHeight());
      if (startHeightExpected > TestUtils.RESTORE_HEIGHT) assertTrue(wallet.getTxs().get(0).getHeight() > TestUtils.RESTORE_HEIGHT);  // wallet is partially synced so first tx happens after true restore height
      else assertEquals(TestUtils.RESTORE_HEIGHT, (long) wallet.getTxs().get(0).getHeight());  // wallet should be fully synced so first tx happens on true restore height
      
      // sync the wallet with default params
      result = wallet.sync();
      assertTrue(wallet.getIsSynced());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      assertEquals(0, (long) result.getNumBlocksFetched());
      assertFalse(result.getReceivedMoney());
      
      // compare with ground truth
      if (!skipGtComparison) testWalletsEqualOnChain(walletGt, wallet);
    } finally {
      if (!skipGtComparison) walletGt.close();
      wallet.close();
    }
  }
  
  // TODO: factor out common code with other testSync() methods
  @Test
  public void testSyncWalletFromKeys() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletJni walletKeys = MoneroWalletJni.createWalletFromKeys(path, TestUtils.WALLET_JNI_PW, wallet.getPrimaryAddress(), wallet.getPrivateViewKey(), wallet.getPrivateSpendKey(), wallet.getNetworkType(), wallet.getDaemonConnection(), TestUtils.RESTORE_HEIGHT, null);
    
    // create ground truth wallet for comparison
    MoneroWalletJni walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.MNEMONIC, TestUtils.RESTORE_HEIGHT);
    
    // test wallet and close as final step
    try {
      assertEquals(walletKeys.getMnemonic(), walletKeys.getMnemonic());
      assertEquals(walletKeys.getPrimaryAddress(), walletKeys.getPrimaryAddress());
      assertEquals(walletKeys.getPrivateViewKey(), walletKeys.getPrivateViewKey());
      assertEquals(walletKeys.getPublicViewKey(), walletKeys.getPublicViewKey());
      assertEquals(walletKeys.getPrivateSpendKey(), walletKeys.getPrivateSpendKey());
      assertEquals(walletKeys.getPublicSpendKey(), walletKeys.getPublicSpendKey());
      assertEquals(TestUtils.RESTORE_HEIGHT, walletKeys.getRestoreHeight());
      assertTrue(walletKeys.getIsConnected());
      assertFalse(walletKeys.getIsSynced());
      
      // sync the wallet
      SyncProgressTester progressTester = new SyncProgressTester(TestUtils.RESTORE_HEIGHT, walletKeys.getDaemonTargetHeight());
      MoneroSyncResult result = walletKeys.sync(progressTester);
      progressTester.onDone(walletKeys.getChainHeight());
      
      // test result after syncing
      assertTrue(walletKeys.getIsSynced());
      assertEquals(walletKeys.getChainHeight() - TestUtils.RESTORE_HEIGHT, (long) result.getNumBlocksFetched());
      assertTrue(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), walletKeys.getHeight());
      assertEquals(daemon.getHeight(), walletKeys.getChainHeight());
      assertEquals(TestUtils.RESTORE_HEIGHT, (long) walletKeys.getTxs().get(0).getHeight());  // wallet should be fully synced so first tx happens on true restore height
      
      // compare with ground truth
      testWalletsEqualOnChain(walletGt, walletKeys);
    } finally {
      walletGt.close();
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
  @Test
  public void testStartStopSyncing() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test unconnected wallet
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, null, null);
    try {
      assertNotNull(wallet.getMnemonic());
      wallet.startSyncing();
      assertEquals(1, wallet.getHeight());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      wallet.stopSyncing();
    } finally {
      wallet.close();
    }
    
    // test connected wallet
    path = getRandomWalletPath();
    wallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, null, null);
    try {
      assertNotNull(wallet.getMnemonic());
      wallet.setDaemonConnection(daemon.getRpcConnection());
      wallet.startSyncing();
      assertEquals(1, wallet.getHeight());
      long chainHeight = wallet.getChainHeight();
      assertFalse(wallet.getIsSynced());
      assertEquals(BigInteger.valueOf(0), wallet.getBalance());
      wallet.setRestoreHeight(chainHeight - 3);
      assertEquals(chainHeight - 3, wallet.getRestoreHeight());
      assertEquals(daemon.getRpcConnection(), wallet.getDaemonConnection());
      wallet.stopSyncing();
      wallet.sync();
    } finally {
      wallet.close();
    }
    
    // test that sync starts automatically
    long restoreHeight = daemon.getHeight() - 100;
    path = getRandomWalletPath();
    wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), restoreHeight);
    try {
      
      // start syncing
      assertEquals(restoreHeight, wallet.getRestoreHeight());
      wallet.startSyncing();
      
      // pause for sync to complete automatically
      try {
        System.out.println("Sleeping to test that sync starts automatically...");
        TimeUnit.MILLISECONDS.sleep(15000);
      } catch (InterruptedException e) {
        e.printStackTrace();
        throw new RuntimeException(e.getMessage());
      }
      
      // test that wallet is synced
      assertTrue(wallet.getIsSynced());
      assertEquals(daemon.getHeight(), wallet.getHeight());
      
      // stop syncing
      wallet.stopSyncing();
      assertTrue(wallet.getIsSynced()); // wallet is still synced
      
   // TODO monero core: wallet.cpp m_synchronized only ever set to true, never false
//      // wait for block to be added to chain
//      daemon.getNextBlockHeader();
//      
//      // wallet is no longer synced
//      assertFalse(wallet.getIsSynced());  
    } finally {
      wallet.close();
    }
  }
  
  // Is equal to the RPC wallet
  @Test
  public void testCompareRpcWallet() {
    testWalletsEqualOnChain(TestUtils.getWalletRpc(), wallet);
  }
  
  @Test
  public void testSave() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create unique path for new test wallet
    String path = TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + System.currentTimeMillis();
    
    // wallet does not exist
    assertFalse(MoneroWalletJni.walletExists(path));
    
    // cannot open non-existant wallet
    try {
      MoneroWalletJni.openWallet(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
      fail("Cannot open non-existant wallet");
    } catch (MoneroException e) {
      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
    }
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletJni wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, null, restoreHeight);
    
    // test wallet is at newly created state
    assertTrue(MoneroWalletJni.walletExists(path));
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNull(wallet.getDaemonConnection());
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    assertEquals("English", wallet.getLanguage());
    assertEquals(1, wallet.getHeight());
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    
    // set the wallet's connection and sync
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    wallet.sync();
    assertEquals(wallet.getChainHeight(), wallet.getHeight());
    
    // close the wallet without saving
    wallet.close();
    
    // re-open the wallet
    wallet = MoneroWalletJni.openWallet(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    
    // test wallet is at newly created state
    assertTrue(MoneroWalletJni.walletExists(path));
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNull(wallet.getDaemonConnection());
    assertFalse(wallet.getIsConnected());
    assertEquals("English", wallet.getLanguage());
    assertFalse(wallet.getIsSynced());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight()); // TODO monero-core: restoreHeight is reset to 0 after closing
    
    // set the wallet's connection and sync
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    assertTrue(wallet.getIsConnected());
    wallet.setRestoreHeight(restoreHeight);
    wallet.sync();
    assertTrue(wallet.getIsSynced());
    assertEquals(wallet.getChainHeight(), wallet.getHeight());
    long prevHeight = wallet.getHeight();
    
    // save and close the wallet
    wallet.save();
    wallet.close();
    
    // re-open the wallet
    wallet = MoneroWalletJni.openWallet(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    
    // test wallet state is saved
    assertFalse(wallet.getIsConnected());
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());  // TODO monero core: daemon connection not stored in wallet files so must be explicitly set each time
    assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
    assertTrue(wallet.getIsConnected());
    assertEquals(prevHeight, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight()); // TODO monero core: restoreHeight is reset to 0 after closing
    assertTrue(MoneroWalletJni.walletExists(path));
    assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals("English", wallet.getLanguage());
    
    // sync
    wallet.sync();
    
    // save and close the wallet
    wallet.save();
    wallet.close();
  }
  
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
    MoneroWalletJni wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, TestUtils.NETWORK_TYPE, null, restoreHeight);
    String subaddressLabel = "Move test wallet subaddress!";
    MoneroAccount account = wallet.createAccount(subaddressLabel);
    wallet.save();
    
    // wallet exists
    assertTrue(MoneroWalletJni.walletExists(path));
    
    // move wallet to a subdirectory
    String movedPath = TestUtils.TEST_WALLETS_DIR + "/moved/" + walletName;
    wallet.moveTo(movedPath, TestUtils.WALLET_JNI_PW);
    assertFalse(MoneroWalletJni.walletExists(path));
    assertFalse(MoneroWalletJni.walletExists(movedPath)); // wallet does not exist until saved
    wallet.save();
    assertFalse(MoneroWalletJni.walletExists(path));
    assertTrue(MoneroWalletJni.walletExists(movedPath));
    wallet.close();
    assertFalse(MoneroWalletJni.walletExists(path));
    assertTrue(MoneroWalletJni.walletExists(movedPath));
    
    // re-open and test wallet
    wallet = MoneroWalletJni.openWallet(movedPath, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    assertEquals(subaddressLabel, wallet.getSubaddress(account.getIndex(), 0).getLabel());
    
    // move wallet back
    wallet.moveTo(path, TestUtils.WALLET_JNI_PW);
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
//    assertEquals("English", walletMemory.getLanguage());
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

  // Can close a wallet
  @Test
  public void testClose() {
    
    // create a test wallet
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_RPC_PASSWORD, TestUtils.NETWORK_TYPE, TestUtils.DAEMON_RPC_URI);
    wallet.sync();
    assertTrue(wallet.getHeight() > 1);
    assertTrue(wallet.getIsSynced());
    assertFalse(wallet.getIsClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.getIsClosed());
    
    // attempt to interact with the wallet
    try { wallet.getHeight(); }
    catch (MoneroException e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.getMnemonic(); }
    catch (MoneroException e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.sync(); }
    catch (MoneroException e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.startSyncing(); }
    catch (MoneroException e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.stopSyncing(); }
    catch (MoneroException e) { assertEquals("Wallet is closed", e.getMessage()); }
    
    // re-open the wallet
    wallet = MoneroWalletJni.openWallet(path, TestUtils.WALLET_RPC_PASSWORD, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection());
    wallet.sync();
    assertEquals(wallet.getChainHeight(), wallet.getHeight());
    assertFalse(wallet.getIsClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.getIsClosed());
  }
  
  // ---------------------------------- HELPERS -------------------------------
  
  public static String getRandomWalletPath() {
    return TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + System.currentTimeMillis();
  }
  
  /**
   * Internal class to test progress updates.
   */
  private class SyncProgressTester implements MoneroSyncListener {
    
    private long startHeight;
    private long prevEndHeight;
    private Long prevHeight;
    private boolean isDone;
    
    public SyncProgressTester(long startHeight, long endHeight) {
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      this.startHeight = startHeight;
      this.prevEndHeight = endHeight;
      this.isDone = false;
    }

    @Override
    public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
      assertFalse("Sync has completed and progress should not be called again", isDone);
      if ((height - startHeight) % 10000 == 0 || percentDone > .999) System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message + ")");
      assertFalse("Should not call progress if end height <= start height", endHeight <= startHeight);
      assertEquals(this.startHeight, startHeight);
      assertTrue(endHeight >= this.prevEndHeight);  // chain can grow while syncing
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
      this.isDone = true;
      assertEquals(chainHeight, prevEndHeight);
      if (prevEndHeight <= startHeight) assertNull(prevHeight); // progress never called
      else {
        assertNotNull("Progress never called", prevHeight);
        assertEquals(chainHeight - 1, (long) prevHeight);  // otherwise last height is chain height - 1
      }
    }
  }
  
  // jni-specific tx tests
  @Override
  protected void testTxWallet(MoneroTxWallet tx, TestContext ctx) {
    if (ctx == null) ctx = new TestContext();
    
    // run common tests
    super.testTxWallet(tx, ctx);
    
    // test tx results from send or relay
    if (Boolean.TRUE.equals(ctx.isSendResponse)) {
      assertNotNull(tx.getKey());
    }
  }
  
  // possible configuration: on chain xor local wallet data ("strict"), txs ordered same way? TBD
  protected static void testWalletsEqualOnChain(MoneroWalletJni wallet1, MoneroWalletJni wallet2) {
    TestMoneroWalletCommon.testWalletsEqualOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());
    //assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight()); // TODO monero-core: restore height is lost after close
    assertEquals(wallet1.getDaemonConnection(), wallet2.getDaemonConnection());
    assertEquals(wallet1.getLanguage(), wallet2.getLanguage());
    // TODO: more jni-specific extensions
  }
  
  // -------------------- OVERRIDES TO BE DIRECTLY RUNNABLE -------------------

  @Override
  public void testGetHeight() {
    super.testGetHeight();
  }

  @Override
  public void testGetMnemonic() {
    super.testGetMnemonic();
  }

  @Override
  public void testGetLanguages() {
    super.testGetLanguages();
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
  public void testCompareGroundTruth() {
    super.testCompareGroundTruth();
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
  public void testGetTxsById() {
    super.testGetTxsById();
  }

  @Override
  public void testGetTxsWithConfiguration() {
    super.testGetTxsWithConfiguration();
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
  public void testGetTransfersWithConfiguration() {
    super.testGetTransfersWithConfiguration();
  }

  @Override
  public void testGetTransfersValidateInputs() {
    super.testGetTransfersValidateInputs();
  }

  @Override
  public void testGetOutputs() {
    super.testGetOutputs();
  }

  @Override
  public void testGetOutputsWithConfiguration() {
    super.testGetOutputsWithConfiguration();
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
  public void testSetTransactionNote() {
    super.testSetTransactionNote();
  }

  @Override
  public void testSetTransactionNotes() {
    super.testSetTransactionNotes();
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
  public void testSignAndVerifyMessages() {
    super.testSignAndVerifyMessages();
  }

  @Override
  public void testSetKeyValues() {
    super.testSetKeyValues();
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
  public void testSendWithRingSize() {
    super.testSendWithRingSize();
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
}
