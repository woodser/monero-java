package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.UUID;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.daemon.model.MoneroNetworkType;
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

  }

  @Override
  protected MoneroWallet getTestWallet() {
    return TestUtils.getWalletJni();
  }

  @Test
  public void testCreateWalletRandom() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

    // create random wallet with defaults
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW);
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress());
    assertEquals(MoneroNetworkType.MAINNET, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertTrue(wallet.getRestoreHeight() >= 0);
    
    // cannot get daemon chain height
    try {
      wallet.getChainHeight();
    } catch (MoneroException e) {
      assertEquals("No connection to daemon", e.getMessage());
    }
    
    // close wallet which releases resources
    wallet.close();

    // create random wallet with non-defaults
    path = getRandomWalletPath();
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, MoneroNetworkType.TESTNET, daemon.getRpcConnection(), "Spanish");
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress());
    assertEquals(MoneroNetworkType.TESTNET, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertEquals("Spanish", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why is height of unsynced wallet 1?
    if (daemon.getIsConnected()) assertEquals(daemon.getHeight(), wallet.getRestoreHeight());
    else assertTrue(wallet.getRestoreHeight() >= 0);
    wallet.close();
  }

  @Test
  public void testCreateWalletFromMnemonic() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

    // create wallet with mnemonic and defaults
    String path = getRandomWalletPath();
    MoneroWalletJni wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, null, null);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight());
    wallet.close();

    // create wallet without restore height
    path = getRandomWalletPath();
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), null);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(0, wallet.getRestoreHeight());
    wallet.close();
    
    // create wallet with mnemonic, no connection, and restore height
    long restoreHeight = 10000;
    path = getRandomWalletPath();
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, null, restoreHeight);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNull(wallet.getDaemonConnection());
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    wallet.save();
    wallet.close();
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    assertEquals(0, wallet.getRestoreHeight()); // restore height is lost after closing
    wallet.close();

    // create wallet with mnemonic, connection, and restore height
    path = getRandomWalletPath();
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), restoreHeight);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertEquals("English", wallet.getLanguage());
    assertEquals(path, wallet.getPath());
    assertEquals(1, wallet.getHeight()); // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    wallet.close();
  }

  @Test
  public void testCreateWalletFromKeys() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    String path = getRandomWalletPath();
    MoneroWalletJni walletKeys = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, wallet.getPrimaryAddress(), wallet.getPrivateViewKey(), wallet.getPrivateSpendKey(), wallet.getNetworkType(), wallet.getDaemonConnection(), 363492l, null);

    // TODO monero core: importing key images can cause erasure of incoming transfers per wallet2.cpp:11957 which causes this test to fail
//    // sync the wallets until same height
//    while (wallet.getHeight() != walletKeys.getHeight()) {
//      wallet.sync();
//      walletKeys.sync(new WalletSyncPrinter());
//    }
//    
//    List<MoneroKeyImage> keyImages = walletKeys.getKeyImages();
//    walletKeys.importKeyImages(keyImages);
    
    // test equality
    try {
      testWalletsEqualOnChain(wallet, walletKeys);
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

  // Can sync a wallet with a randomly generated seed
  @Test
  public void testSyncRandom() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertTrue("Not connected to daemon", daemon.getIsConnected());

    // create test wallet
    long restoreHeight = daemon.getHeight();
    MoneroWalletJni wallet = new MoneroWalletJni(getRandomWalletPath(), TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection(), null);

    // test wallet's height before syncing
    assertEquals(1, wallet.getHeight());
    assertEquals(restoreHeight, wallet.getRestoreHeight());
    assertEquals(daemon.getHeight(), wallet.getChainHeight());

    // sync the wallet
    SyncProgressTester progressTester = new SyncProgressTester(wallet.getRestoreHeight(), wallet.getChainHeight());
    MoneroSyncResult result = wallet.sync(null, null, progressTester);
    progressTester.onDone(wallet.getChainHeight());

    // test result after syncing
    assertEquals(0, (long) result.getNumBlocksFetched());
    assertFalse(result.getReceivedMoney());
    assertEquals(daemon.getHeight(), wallet.getHeight());

    // sync the wallet with default params
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());

    // TODO: sync wallet with smaller start height, bigger start height, etc
    
    // close wallet to free c++ resources
    wallet.close();
  }

  // Can sync a wallet with a mnemonic
  @Test
  public void testSyncMnemonicFromGenesis() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    testSyncMnemonic(null);
  }

  // Can sync a wallet with a mnemonic and a start height
  @Test
  public void testSyncMnemonicFromHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncMnemonic(wallet.getChainHeight() - 25000);
  }

  private void testSyncMnemonic(Long startHeight) {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertTrue("Not connected to daemon", daemon.getIsConnected());

    // create wallet from mnemonic
    MoneroWalletJni wallet = new MoneroWalletJni(getRandomWalletPath(), TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection(), startHeight);
    
    // test wallet's height before syncing
    assertEquals(1, wallet.getHeight());
    assertEquals(startHeight == null ? 0 : startHeight, wallet.getRestoreHeight());
    
    // sanitize bounds
    long startHeightActual = Math.max(wallet.getHeight(), wallet.getRestoreHeight());
    long chainHeight = wallet.getChainHeight();
    
    // sync the wallet
    SyncProgressTester progressTester = new SyncProgressTester(startHeightActual, chainHeight);
    MoneroSyncResult result = wallet.sync(startHeight, null, progressTester);
    progressTester.onDone(wallet.getChainHeight());
    
    // test result after syncing
    assertEquals(wallet.getChainHeight() - startHeightActual, (long) result.getNumBlocksFetched());
    assertTrue(result.getReceivedMoney());
    assertEquals(daemon.getHeight(), wallet.getHeight());
    assertEquals(daemon.getHeight(), wallet.getChainHeight());
    
    // sync the wallet with default params
    result = wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
    assertEquals(0, (long) result.getNumBlocksFetched());
    assertFalse(result.getReceivedMoney());
    
    // close the wallet to free c++ resources
    wallet.close();
  }
  
  @Test
  public void testSave() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create unique path for new test wallet
    String path = TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + UUID.randomUUID().toString();
    
    // wallet does not exist
    assertFalse(MoneroWalletJni.walletExists(path));
    
    // cannot open non-existant wallet
    try {
      new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
      fail("Cannot open non-existant wallet");
    } catch (MoneroException e) {
      assertEquals("Wallet does not exist at path: " + path, e.getMessage());
    }
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletJni wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, null, restoreHeight);
    
    // test wallet is at newly created state
    assertTrue(MoneroWalletJni.walletExists(path));
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
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
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    
    // test wallet is at newly created state
    assertTrue(MoneroWalletJni.walletExists(path));
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNull(wallet.getDaemonConnection());
    assertEquals("English", wallet.getLanguage());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight()); // TODO monero-core: restoreHeight is reset to 0 after closing
    
    // set the wallet's connection and sync
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    wallet.setRestoreHeight(restoreHeight);
    wallet.sync();
    assertEquals(wallet.getChainHeight(), wallet.getHeight());
    long prevHeight = wallet.getHeight();
    
    // save and close the wallet
    wallet.save();
    wallet.close();
    
    // re-open the wallet
    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    
    // test wallet state is saved
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());  // TODO monero core: daemon connection not stored in wallet files so must be explicitly set each time
    assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
    assertEquals(prevHeight, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight()); // TODO monero core: restoreHeight is reset to 0 after closing
    assertTrue(MoneroWalletJni.walletExists(path));
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
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
    String walletName = "test_wallet_" + UUID.randomUUID().toString();
    String path = TestUtils.TEST_WALLETS_DIR + "/" + walletName;
    
    // wallet does not exist
    assertFalse(MoneroWalletJni.walletExists(path));
    
    // create wallet at the path
    long restoreHeight = daemon.getHeight() - 200;
    MoneroWalletJni wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, null, restoreHeight);
    String accountLabel = "Move test wallet account!";
    MoneroAccount account = wallet.createAccount(accountLabel);
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
    wallet = new MoneroWalletJni(movedPath, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
    assertEquals(accountLabel, wallet.getAccount(account.getIndex()).getLabel());
    
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

  // Can close the currently open wallet
  @Test
  @Ignore // disabled so wallet is not actually closed TODO: just re-open wallet?
  public void testClose() {
    wallet.close();
  }

  // ---------------------------------- HELPERS -------------------------------
  
  private static String getRandomWalletPath() {
    return TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + UUID.randomUUID().toString();
  }
  
  /**
   * Internal class to test progress updates.
   */
  private class SyncProgressTester implements MoneroSyncListener {
    
    private long startHeight;
    private long prevEndHeight;
    private Long prevHeight;
    
    public SyncProgressTester(long startHeight, long endHeight) {
      assertTrue(startHeight >= 0);
      assertTrue(endHeight >= 0);
      this.startHeight = startHeight;
      this.prevEndHeight = endHeight;
    }

    @Override
    public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
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
      assertEquals(chainHeight, prevEndHeight);
      if (prevEndHeight <= startHeight) assertNull(prevHeight); // progress never called
      else assertEquals(chainHeight - 1, (long) prevHeight);  // otherwise last height is chain height - 1
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
  private static void testWalletsEqualOnChain(MoneroWalletJni wallet1, MoneroWalletJni wallet2) {
    new TestMoneroWalletsEqual().setWallet1(wallet1).setWallet2(wallet2).testWalletsEqualOnChain();
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
