

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import common.types.Filter;
import common.utils.GenUtils;
import common.utils.JsonUtils;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import monero.common.MoneroConnectionManager;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroVersion;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroMessageSignatureResult;
import monero.wallet.model.MoneroMessageSignatureType;
import monero.wallet.model.MoneroMultisigInfo;
import monero.wallet.model.MoneroMultisigInitResult;
import monero.wallet.model.MoneroMultisigSignResult;
import monero.wallet.model.MoneroOutgoingTransfer;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxPriority;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.opentest4j.AssertionFailedError;

import utils.Pair;
import utils.StartMining;
import utils.TestUtils;
import utils.WalletEqualityUtils;

/**
 * Runs common tests that every Monero wallet implementation should support.
 */
@TestInstance(Lifecycle.PER_CLASS) // so @BeforeAll and @AfterAll can be used on non-static functions
public abstract class TestMoneroWalletCommon {
  
  // test constants
  protected static final boolean LITE_MODE = false;
  protected static final boolean TEST_NON_RELAYS = true;
  protected static final boolean TEST_RELAYS = true;
  protected static final boolean TEST_NOTIFICATIONS = true;
  protected static final boolean TEST_RESETS = false;
  private static final int MAX_TX_PROOFS = 25; // maximum number of transactions to check for each proof, undefined to check all
  private static final int SEND_MAX_DIFF = 60;
  private static final int SEND_DIVISOR = 10;
  private static final int NUM_BLOCKS_LOCKED = 10;
  
  // instance variables
  protected MoneroWallet wallet;        // wallet instance to test
  protected MoneroDaemonRpc daemon;     // daemon instance to test
  
  public TestMoneroWalletCommon() {

  }
  
  @BeforeAll
  public void beforeAll() {
    wallet = getTestWallet();
    daemon = getTestDaemon();
    TestUtils.WALLET_TX_TRACKER.reset(); // all wallets need to wait for txs to confirm to reliably sync
  }
  
  @BeforeEach
  public void beforeEach(TestInfo testInfo) {
    System.out.println("Before test " + testInfo.getDisplayName());
    
    // stop mining
    MoneroMiningStatus status = daemon.getMiningStatus();
    if (status.isActive()) wallet.stopMining();
  }
  
  @AfterAll
  public void afterAll() {
    
    // try to stop mining
    if (daemon != null) {
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
    }
    
    // close wallet
    if (wallet != null) wallet.close(true);
  }
  
  @AfterEach
  public void afterEach(TestInfo testInfo) {
    System.out.println("After test " + testInfo.getDisplayName());
    
    if (daemon.getMiningStatus().isActive()) {
      System.err.println("WARNING: mining is active after test " + testInfo.getDisplayName() + ", stopping");
      daemon.stopMining();
    }
  }
  
  /**
   * Get the daemon to test.
   * 
   * @return the daemon to test
   */
  protected MoneroDaemonRpc getTestDaemon() {
    return TestUtils.getDaemonRpc();
  }
  
  /**
   * Get the main wallet to test.
   * 
   * @return the wallet to test
   */
  protected abstract MoneroWallet getTestWallet();
  
  /**
   * Open a test wallet with default configuration for each wallet type.
   * 
   * @param config configures the wallet to open
   * @return MoneroWallet is the opened wallet
   */
  protected MoneroWallet openWallet(String path, String password) { return openWallet(new MoneroWalletConfig().setPath(path).setPassword(password)); }
  protected abstract MoneroWallet openWallet(MoneroWalletConfig config);
  
  /**
   * Create a test wallet with default configuration for each wallet type.
   * 
   * @param config configures the wallet to create
   * @return MoneroWallet is the created wallet
   */
  protected abstract MoneroWallet createWallet(MoneroWalletConfig config);
  
  /**
   * Close a test wallet with customization for each wallet type.
   * 
   * @param wallet - the wallet to close
   * @param save - whether or not to save the wallet
   */
  protected void closeWallet(MoneroWallet wallet) { closeWallet(wallet, false); }
  protected abstract void closeWallet(MoneroWallet wallet, boolean save);
  
  /**
   * Get the wallet's supported languages for the seed.  This is an
   * instance method for wallet rpc and a static utility for other wallets.
   * 
   * @return List<String> are the wallet's supported languages
   */
  protected abstract List<String> getSeedLanguages();
  
  // ------------------------------ BEGIN TESTS -------------------------------
  
  // Can create a random wallet
  @Test
  public void testCreateWalletRandom() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {
      
      // create random wallet
      MoneroWallet wallet = createWallet(new MoneroWalletConfig());
      String path = wallet.getPath();
      Exception e2 = null;
      try {
        MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
        MoneroUtils.validatePrivateViewKey(wallet.getPrivateViewKey());
        MoneroUtils.validatePrivateSpendKey(wallet.getPrivateSpendKey());
        MoneroUtils.validateMnemonic(wallet.getSeed());
        if (!(wallet instanceof MoneroWalletRpc)) assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getSeedLanguage());  // TODO monero-wallet-rpc: get seed language
      } catch (Exception e) {
        e2 = e;
      }
      closeWallet(wallet);
      if (e2 != null) throw e2;
      
      // attempt to create wallet at same path
      try {
        createWallet(new MoneroWalletConfig().setPath(path));
        throw new Error("Should have thrown error");
      } catch(Exception e) {
        assertEquals("Wallet already exists: " + path, e.getMessage());
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
      
      // recreate test wallet from seed
      MoneroWallet wallet = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      String path = wallet.getPath();
      Exception e2 = null;
      try {
        assertEquals(primaryAddress, wallet.getPrimaryAddress());
        assertEquals(privateViewKey, wallet.getPrivateViewKey());
        assertEquals(privateSpendKey, wallet.getPrivateSpendKey());
        assertEquals(TestUtils.SEED, wallet.getSeed());
        if (!(wallet instanceof MoneroWalletRpc)) assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getSeedLanguage());
      } catch (Exception e) {
        e2 = e;
      }
      closeWallet(wallet);
      if (e2 != null) throw e2;
      
      // attempt to create wallet with two missing words
      try {
        String invalidMnemonic = "memoir desk algebra inbound innocent unplugs fully okay five inflamed giant factual ritual toyed topic snake unhappy guarded tweezers haunted inundate giant";
        wallet = createWallet(new MoneroWalletConfig().setSeed(invalidMnemonic).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      } catch(Exception e) {
        assertEquals("Invalid mnemonic", e.getMessage());
      }
      
      // attempt to create wallet at same path
      try {
        createWallet(new MoneroWalletConfig().setPath(path));
        throw new RuntimeException("Should have thrown error");
      } catch (Exception e) {
        assertEquals("Wallet already exists: " + path, e.getMessage());
      }
    } catch (Exception e) {
      e1 = e;
    }
    
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  // Can create a wallet from a seed with a seed offset
  @Test
  public void testCreateWalletFromSeedWithOffset() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {

      // create test wallet with offset
      MoneroWallet wallet = createWallet(new MoneroWalletConfig().setSeed(TestUtils.SEED).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setSeedOffset("my secret offset!"));
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
      MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(primaryAddress).setPrivateViewKey(privateViewKey).setPrivateSpendKey(privateSpendKey).setRestoreHeight(daemon.getHeight()));
      String path = wallet.getPath();
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
        wallet = createWallet(new MoneroWalletConfig().setPrivateSpendKey(privateSpendKey).setRestoreHeight(daemon.getHeight()));
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
        createWallet(new MoneroWalletConfig().setPath(path));
        throw new Error("Should have thrown error");
      } catch(Exception e) {
        assertEquals("Wallet already exists: " + path, e.getMessage());
      }
    } catch (Exception e) {
      e1 = e;
    }
    
    if (e1 != null) throw new RuntimeException(e1);
  }

  // Can create wallets with subaddress lookahead
  @Test
  public void testSubaddressLookahead() {
    assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    MoneroWallet receiver = null;
    try {
     
      // create wallet with high subaddress lookahead
      receiver = createWallet(new MoneroWalletConfig().setAccountLookahead(1).setSubaddressLookahead(100000));
     
      // transfer funds to subaddress with high index
      wallet.createTx(new MoneroTxConfig()
              .setAccountIndex(0)
              .addDestination(receiver.getSubaddress(0, 85000).getAddress(), TestUtils.MAX_FEE)
              .setRelay(true));
     
      // observe unconfirmed funds
      GenUtils.waitFor(1000);
      receiver.sync();
      assert(receiver.getBalance().compareTo(new BigInteger("0")) > 0);
    } catch (Exception e) {
      e1 = e;
    }
   
    if (receiver != null) closeWallet(receiver);
    if (e1 != null) throw new RuntimeException(e1);
 }
  
  // Can get the wallet's version
  @Test
  public void testGetVersion() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroVersion version = wallet.getVersion();
    assertNotNull(version.getNumber());
    assertTrue(version.getNumber() > 0);
    assertNotNull(version.getIsRelease());
  }
  
  // Can get the wallet's path
  @Test
  public void testGetPath() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create random wallet
    MoneroWallet wallet = createWallet(new MoneroWalletConfig());
    
    // set a random attribute
    String uuid = UUID.randomUUID().toString();
    wallet.setAttribute("uuid", uuid);
    
    // record the wallet's path then save and close
    String path = wallet.getPath();
    closeWallet(wallet, true);
    
    // re-open the wallet using its path
    wallet = openWallet(path, null);
    
    // test the attribute
    assertEquals(uuid, wallet.getAttribute("uuid"));
    closeWallet(wallet);
  }
  
  // Can set the daemon connection
  @Test
  public void testSetDaemonConnection() {
    
    // create random wallet with default daemon connection
    MoneroWallet wallet = createWallet(new MoneroWalletConfig());
    assertEquals(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD), wallet.getDaemonConnection());
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
    wallet.setDaemonConnection(TestUtils.DAEMON_RPC_URI, "wronguser", "wrongpass");
    assertEquals(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, "wronguser", "wrongpass"), wallet.getDaemonConnection());
    if ("".equals(TestUtils.DAEMON_RPC_USERNAME) || TestUtils.DAEMON_RPC_USERNAME == null) assertTrue(wallet.isConnectedToDaemon()); // TODO: monerod without authentication works with bad credentials?
    else assertFalse(wallet.isConnectedToDaemon());
    
    // set daemon with authentication
    wallet.setDaemonConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD);
    assertEquals(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD), wallet.getDaemonConnection());
    assertTrue(wallet.isConnectedToDaemon());
    
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
  public void testConnectionManager() {

    // create connection manager with monerod connections
    MoneroConnectionManager connectionManager = new MoneroConnectionManager();
    MoneroRpcConnection connection1 = new MoneroRpcConnection(TestUtils.getDaemonRpc().getRpcConnection()).setPriority(1);
    MoneroRpcConnection connection2 = new MoneroRpcConnection("localhost:48081").setPriority(2);
    connectionManager.setConnection(connection1);
    connectionManager.addConnection(connection2);

    // create wallet with connection manager
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setServerUri("").setConnectionManager(connectionManager));
    assertEquals(TestUtils.getDaemonRpc().getRpcConnection(), wallet.getDaemonConnection());
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

  // Can get the seed
  @Test
  public void testGetSeed() {
    assumeTrue(TEST_NON_RELAYS);
    String seed = wallet.getSeed();
    MoneroUtils.validateMnemonic(seed);
    assertEquals(TestUtils.SEED, seed);
  }
  
  // Can get the language of the seed
  @Test
  public void testGetSeedLanguage() {
    assumeTrue(TEST_NON_RELAYS);
    String language = wallet.getSeedLanguage();
    assertEquals(MoneroWallet.DEFAULT_LANGUAGE, language);
  }
  
  // Can get a list of supported languages for the seed
  @Test
  public void testGetSeedLanguages() {
    assumeTrue(TEST_NON_RELAYS);
    List<String> languages = getSeedLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }
  
  // Can get the private view key
  @Test
  public void testGetPrivateViewKey() {
    assumeTrue(TEST_NON_RELAYS);
    String privateViewKey = wallet.getPrivateViewKey();
    MoneroUtils.validatePrivateViewKey(privateViewKey);
  }
  
  // Can get the private spend key
  @Test
  public void testGetPrivateSpendKey() {
    assumeTrue(TEST_NON_RELAYS);
    String privateSpendKey = wallet.getPrivateSpendKey();
    MoneroUtils.validatePrivateSpendKey(privateSpendKey);
  }
  
  // Can get the public view key
  @Test
  public void testGetPublicViewKey() {
    assumeTrue(TEST_NON_RELAYS);
    String publicViewKey = wallet.getPublicViewKey();
    MoneroUtils.validatePrivateSpendKey(publicViewKey);
  }
  
  // Can get the public view key
  @Test
  public void testGetPublicSpendKey() {
    assumeTrue(TEST_NON_RELAYS);
    String publicSpendKey = wallet.getPublicSpendKey();
    MoneroUtils.validatePrivateSpendKey(publicSpendKey);
  }
  
  // Can get the primary address
  @Test
  public void testGetPrimaryAddress() {
    assumeTrue(TEST_NON_RELAYS);
    String primaryAddress = wallet.getPrimaryAddress();
    MoneroUtils.validateAddress(primaryAddress, TestUtils.NETWORK_TYPE);
    assertEquals(wallet.getAddress(0, 0), primaryAddress);
  }
  
  // Can get the address of a subaddress at a specified account and subaddress index
  @Test
  public void testGetSubaddressAddress() {
    assumeTrue(TEST_NON_RELAYS);
    assertEquals(wallet.getPrimaryAddress(), (wallet.getSubaddress(0, 0)).getAddress());
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        assertEquals(subaddress.getAddress(), wallet.getAddress(account.getIndex(), subaddress.getIndex()));
      }
    }
  }
  
  // Can get addresses out of range of used accounts and subaddresses
  @Test
  public void testGetSubaddressAddressOutOfRange() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size();
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNotNull(address);
    assertTrue(address.length() > 0);
  }
  
  // Can get the account and subaddress indices of an address
  @Test
  public void testGetAddressIndices() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get last subaddress to test
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    int accountIdx = accounts.size() - 1;
    int subaddressIdx = accounts.get(accountIdx).getSubaddresses().size() - 1;
    String address = wallet.getAddress(accountIdx, subaddressIdx);
    assertNotNull(address);
    
    // get address index
    MoneroSubaddress subaddress = wallet.getAddressIndex(address);
    assertEquals(accountIdx, (int) subaddress.getAccountIndex());
    assertEquals(subaddressIdx, (int) subaddress.getIndex());

    // test valid but unfound address
    String nonWalletAddress = TestUtils.getExternalWalletAddress();
    try {
      subaddress = wallet.getAddressIndex(nonWalletAddress);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals("Address doesn't belong to the wallet", e.getMessage());
    }
    
    // test invalid address
    try {
      subaddress = wallet.getAddressIndex("this is definitely not an address");
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals("Invalid address", e.getMessage());
    }
  }
  
  // Can get an integrated address given a payment id
  @Test
  public void testGetIntegratedAddress() {
    assumeTrue(TEST_NON_RELAYS);
    
    // save address for later comparison
    String address = wallet.getPrimaryAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(null, paymentId);
    assertEquals(integratedAddress.getStandardAddress(), address);
    assertEquals(integratedAddress.getPaymentId(), paymentId);
    
    // test null payment id which generates a new one
    integratedAddress = wallet.getIntegratedAddress();
    assertEquals(integratedAddress.getStandardAddress(), address);
    assertFalse(integratedAddress.getPaymentId().isEmpty());
    
    // test with primary address
    String primaryAddress = wallet.getPrimaryAddress();
    integratedAddress = wallet.getIntegratedAddress(primaryAddress, paymentId);
    assertEquals(integratedAddress.getStandardAddress(), primaryAddress);
    assertEquals(integratedAddress.getPaymentId(), paymentId);
    
    // test with subaddress
    if (wallet.getSubaddresses(0).size() < 2) wallet.createSubaddress(0);
    String subaddress = wallet.getSubaddress(0, 1).getAddress();
    try {
      integratedAddress = wallet.getIntegratedAddress(subaddress, null);
      fail("Getting integrated address from subaddress should have failed");
    } catch (MoneroError e) {
      assertEquals("Subaddress shouldn't be used", e.getMessage());
    }
    
    // test invalid payment id
    String invalidPaymentId = "invalid_payment_id_123456";
    try {
      integratedAddress = wallet.getIntegratedAddress(null, invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown exception");
    } catch (MoneroError e) {
      assertEquals("Invalid payment ID: " + invalidPaymentId, e.getMessage());
    }
  }
  
  // Can decode an integrated address
  @Test
  public void testDecodeIntegratedAddress() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(null, "03284e41c342f036");
    MoneroIntegratedAddress decodedAddress = wallet.decodeIntegratedAddress(integratedAddress.toString());
    assertEquals(integratedAddress, decodedAddress);
    
    // decode invalid address
    try {
      wallet.decodeIntegratedAddress("bad address");
      throw new Error("Should have failed decoding bad address");
    } catch (MoneroError err) {
      assertEquals("Invalid address", err.getMessage());
    }
  }
  
  // Can sync (without progress)
  // TODO: test syncing from start height
  @Test
  public void testSyncWithoutProgress() {
    assumeTrue(TEST_NON_RELAYS);
    long numBlocks = 100;
    long chainHeight = daemon.getHeight();
    assertTrue(chainHeight >= numBlocks);
    MoneroSyncResult result = wallet.sync(chainHeight - numBlocks);  // sync end of chain
    assertTrue(result.getNumBlocksFetched() >= 0);
    assertNotNull(result.getReceivedMoney());
  }
  
  // Is equal to a ground truth wallet according to on-chain data
  @Test
  public void testWalletEqualityGroundTruth() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    MoneroWallet walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.SEED, null, TestUtils.FIRST_RECEIVE_HEIGHT);
    try {
      WalletEqualityUtils.testWalletEqualityOnChain(walletGt, wallet);
    } finally {
      walletGt.close();
    }
  }
  
  // Can get the current height that the wallet is synchronized to
  @Test
  public void testGetHeight() {
    assumeTrue(TEST_NON_RELAYS);
    long height = wallet.getHeight();
    assertTrue(height >= 0);
  }
  
  // Can get a blockchain height by date
  @SuppressWarnings("deprecation")
  @Test
  public void testGetHeightByDate() {
    assumeTrue(TEST_NON_RELAYS);
    
    // collect dates to test starting 100 days ago
    long DAY_MS = 24 * 60 * 60 * 1000;
    Date yesterday = new Date(new Date().getTime() - DAY_MS); // TODO monero-project: today's date can throw exception as "in future" so we test up to yesterday
    List<Date> dates = new ArrayList<Date>();
    for (long i = 99; i >= 0; i--) {
      dates.add(new Date(yesterday.getTime() - DAY_MS * i)); // subtract i days
    }

    // test heights by date
    Long lastHeight = null;
    for (Date date : dates) {
      long height = wallet.getHeightByDate(date.getYear() + 1900, date.getMonth() + 1, date.getDate());
      assertTrue(height >= 0);
      if (lastHeight != null) assertTrue(height >= lastHeight);
      lastHeight = height;
    }
    assertTrue(lastHeight >= 0);
    long height = wallet.getHeight();
    assertTrue(height >= 0);
    
    // test future date
    try {
      Date tomorrow = new Date(yesterday.getTime() + DAY_MS * 2);
      wallet.getHeightByDate(tomorrow.getYear() + 1900, tomorrow.getMonth() + 1, tomorrow.getDate());
      fail("Expected exception on future date");
    } catch (MoneroError err) {
      assertEquals("specified date is in the future", err.getMessage());
    }
  }
  
  // Can get the locked and unlocked balances of the wallet, accounts, and subaddresses
  @Test
  public void testGetAllBalances() {
    assumeTrue(TEST_NON_RELAYS);
    
    // fetch accounts with all info as reference
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    
    // test that balances add up between accounts and wallet
    BigInteger accountsBalance = BigInteger.valueOf(0);
    BigInteger accountsUnlockedBalance = BigInteger.valueOf(0);
    for (MoneroAccount account : accounts) {
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
      
      // test that balances add up between subaddresses and accounts
      BigInteger subaddressesBalance = BigInteger.valueOf(0);
      BigInteger subaddressesUnlockedBalance = BigInteger.valueOf(0);
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        subaddressesBalance = subaddressesBalance.add(subaddress.getBalance());
        subaddressesUnlockedBalance = subaddressesUnlockedBalance.add(subaddress.getUnlockedBalance());
        
        // test that balances are consistent with getAccounts() call
        assertEquals((wallet.getBalance(subaddress.getAccountIndex(), subaddress.getIndex())).toString(), subaddress.getBalance().toString());
        assertEquals((wallet.getUnlockedBalance(subaddress.getAccountIndex(), subaddress.getIndex())).toString(), subaddress.getUnlockedBalance().toString());
      }
      assertEquals((wallet.getBalance(account.getIndex())).toString(), subaddressesBalance.toString());
      assertEquals((wallet.getUnlockedBalance(account.getIndex())).toString(), subaddressesUnlockedBalance.toString());
    }
    TestUtils.testUnsignedBigInteger(accountsBalance);
    TestUtils.testUnsignedBigInteger(accountsUnlockedBalance);
    assertEquals((wallet.getBalance()).toString(), accountsBalance.toString());
    assertEquals((wallet.getUnlockedBalance()).toString(), accountsUnlockedBalance.toString());
  }
  
  // Can get accounts without subaddresses
  @Test
  public void testGetAccountsWithoutSubaddresses() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      assertNull(account.getSubaddresses());
    }
  }
  
  // Can get accounts with subaddresses
  @Test
  public void testGetAccountsWithSubaddresses() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      assertFalse(account.getSubaddresses().isEmpty());
    }
  }
  
  // Can get an account at a specified index
  @Test
  public void testGetAccount() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      testAccount(account);
      
      // test without subaddresses
      MoneroAccount retrieved = wallet.getAccount(account.getIndex());
      assertNull(retrieved.getSubaddresses());
      
      // test with subaddresses
      retrieved = wallet.getAccount(account.getIndex(), true);
      assertFalse(retrieved.getSubaddresses().isEmpty());
    }
  }
  
  // Can create a new account without a label
  @Test
  public void testCreateAccountWithoutLabel() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount();
    testAccount(createdAccount);
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 1);
  }
  
  // Can create a new account with a label
  @Test
  public void testCreateAccountWithLabel() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create account with label
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    String label = UUID.randomUUID().toString();
    MoneroAccount createdAccount = wallet.createAccount(label);
    testAccount(createdAccount);
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 1);
    assertEquals(label, wallet.getSubaddress(createdAccount.getIndex(), 0).getLabel());
    
    // fetch and test account
    createdAccount = wallet.getAccount(createdAccount.getIndex());
    testAccount(createdAccount);

    // create account with same label
    createdAccount = wallet.createAccount(label);
    testAccount(createdAccount);
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 2);
    assertEquals(label, wallet.getSubaddress(createdAccount.getIndex(), 0).getLabel());
    
    // fetch and test account
    createdAccount = wallet.getAccount(createdAccount.getIndex());
    testAccount(createdAccount);
  }

  // Can set account labels
  @Test
  public void testSetAccountLabel() {

    // create account
    if (wallet.getAccounts().size() < 2) wallet.createAccount();

    // set account label
    String label = GenUtils.getUUID();
    wallet.setAccountLabel(1, label);
    assertEquals(label, wallet.getSubaddress(1, 0).getLabel());
  }
  
  // Can get subaddresses at a specified account index
  @Test
  public void testGetSubaddresses() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertFalse(subaddresses.isEmpty());
      for (MoneroSubaddress subaddress : subaddresses) {
        testSubaddress(subaddress);
        assertEquals(account.getIndex(), subaddress.getAccountIndex());
      }
    }
  }
  
  // Can get subaddresses at specified account and subaddress indices
  @Test
  public void testGetSubaddressesByIndices() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertFalse(accounts.isEmpty());
    for (MoneroAccount account : accounts) {
      
      // get subaddresses
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertTrue(subaddresses.size() > 0);
      
      // remove a subaddress for query if possible
      if (subaddresses.size() > 1) subaddresses.remove(0);
      
      // get subaddress indices
      List<Integer> subaddressIndices = new ArrayList<Integer>();
      for (MoneroSubaddress subaddress : subaddresses) subaddressIndices.add(subaddress.getIndex());
      assertTrue(subaddressIndices.size() > 0);
      
      // fetch subaddresses by indices
      List<MoneroSubaddress> fetchedSubaddresses = wallet.getSubaddresses(account.getIndex(), subaddressIndices);
      
      // original subaddresses (minus one removed if applicable) is equal to fetched subaddresses
      assertEquals(subaddresses, fetchedSubaddresses);
    }
  }
  
  // Can get a subaddress at a specified account and subaddress index
  @Test
  public void testGetSubaddressByIndex() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue(accounts.size() > 0);
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(account.getIndex());
      assertTrue(subaddresses.size() > 0);
      for (MoneroSubaddress subaddress : subaddresses) {
        testSubaddress(subaddress);
        assertEquals(subaddress, wallet.getSubaddress(account.getIndex(), subaddress.getIndex()));
        assertEquals(subaddress, (wallet.getSubaddresses(account.getIndex(), Arrays.asList(subaddress.getIndex()))).get(0)); // test plural call with single subaddr number
      }
    }
  }
  
  // Can create a subaddress with and without a label
  @Test
  public void testCreateSubaddress() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create subaddresses across accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    if (accounts.size() < 2) wallet.createAccount();
    accounts = wallet.getAccounts();
    assertTrue(accounts.size() > 1);
    for (int accountIdx = 0; accountIdx < 2; accountIdx++) {
      
      // create subaddress with no label
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(accountIdx);
      MoneroSubaddress subaddress = wallet.createSubaddress(accountIdx);
      assertNull(subaddress.getLabel());
      testSubaddress(subaddress);
      List<MoneroSubaddress> subaddressesNew = wallet.getSubaddresses(accountIdx);
      assertEquals(subaddressesNew.size() - 1, subaddresses.size());
      assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
      
      // create subaddress with label
      subaddresses = wallet.getSubaddresses(accountIdx);
      String uuid = UUID.randomUUID().toString();
      subaddress = wallet.createSubaddress(accountIdx, uuid);
      assertEquals(uuid, subaddress.getLabel());
      testSubaddress(subaddress);
      subaddressesNew = wallet.getSubaddresses(accountIdx);
      assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
      assertEquals(subaddress, subaddressesNew.get(subaddressesNew.size() - 1));
    }
  }

  // Can set subaddress labels
  @Test
  public void testSetSubaddressLabel() {

    // create subaddresses
    while (wallet.getSubaddresses(0).size() < 3) wallet.createSubaddress(0);

    // set subaddress labels
    for (int subaddressIdx = 0; subaddressIdx < wallet.getSubaddresses(0).size(); subaddressIdx++) {
      String label = GenUtils.getUUID();
      wallet.setSubaddressLabel(0, subaddressIdx, label);
      assertEquals(label, wallet.getSubaddress(0, subaddressIdx).getLabel());
    }
  }
  
  // Can get transactions in the wallet
  @Test
  public void testGetTxsWallet() {
    assumeTrue(TEST_NON_RELAYS);
    boolean nonDefaultIncoming = false;
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, null, null, true);
    assertFalse(txs.isEmpty(), "Wallet has no txs to test");
    assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, (long) txs.get(0).getHeight(), "First tx's restore height must match the restore height in TestUtils");
    
    // build test context
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    
    // test each transaction
    Map<Long, MoneroBlock> blockPerHeight = new HashMap<Long, MoneroBlock>();
    for (int i = 0; i < txs.size(); i++) {
      testTxWallet(txs.get(i), ctx);
      
      // test merging equivalent txs
      MoneroTxWallet copy1 = txs.get(i).copy();
      MoneroTxWallet copy2 = txs.get(i).copy();
      if (copy1.isConfirmed()) copy1.setBlock(txs.get(i).getBlock().copy().setTxs(Arrays.asList(copy1)));
      if (copy2.isConfirmed()) copy2.setBlock(txs.get(i).getBlock().copy().setTxs(Arrays.asList(copy2)));
      MoneroTxWallet merged = copy1.merge(copy2);
      testTxWallet(merged, ctx);
      
      // find non-default incoming
      if (txs.get(i).getIncomingTransfers() != null) { // TODO: txs1.get(i).isIncoming()
        for (MoneroIncomingTransfer transfer : txs.get(i).getIncomingTransfers()) {
          if (transfer.getAccountIndex() != 0 && transfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
        }
      }
      
      // ensure unique block reference per height
      if (txs.get(i).isConfirmed()) {
        MoneroBlock block = blockPerHeight.get(txs.get(i).getHeight());
        if (block == null) blockPerHeight.put(txs.get(i).getHeight(), txs.get(i).getBlock());
        else {
          assertEquals(block, txs.get(i).getBlock());
          assertTrue(block == txs.get(i).getBlock(), "Block references for same height must be same");
        }
      }
    }
    
    // ensure non-default account and subaddress tested
    assertTrue(nonDefaultIncoming, "No incoming transfers found to non-default account and subaddress; run send-to-multiple tests first");
  }
  
  // Can get transactions by hash
  @Test
  public void testGetTxsByHash() {
    assumeTrue(TEST_NON_RELAYS);
    
    int maxNumTxs = 10;  // max number of txs to test
    
    // fetch all txs for testing
    List<MoneroTxWallet> txs = wallet.getTxs();
    assertTrue(txs.size() > 1, "Test requires at least 2 txs to fetch by hash");
    
    // randomly pick a few for fetching by hash
    Collections.shuffle(txs);
    txs = txs.subList(0, Math.min(txs.size(), maxNumTxs));
    
    // test fetching by hash
    MoneroTxWallet fetchedTx = wallet.getTx(txs.get(0).getHash());
    assertEquals(txs.get(0).getHash(), fetchedTx.getHash());
    testTxWallet(fetchedTx);
    
    // test fetching by hashes
    String txId1 = txs.get(0).getHash();
    String txId2 = txs.get(1).getHash();
    List<MoneroTxWallet> fetchedTxs = wallet.getTxs(txId1, txId2);
    assertEquals(2,  fetchedTxs.size());
    
    // test fetching by hashes as collection
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) txHashes.add(tx.getHash());
    fetchedTxs = wallet.getTxs(txHashes);
    assertEquals(txs.size(), fetchedTxs.size());
    for (int i = 0; i < txs.size(); i++) {
      assertEquals(txs.get(i).getHash(), fetchedTxs.get(i).getHash());
      testTxWallet(fetchedTxs.get(i));
    }
    
    // test fetching with missing tx hashes
    String missingTxHash = "d01ede9cde813b2a693069b640c4b99c5adbdb49fbbd8da2c16c8087d0c3e320";
    txHashes.add(missingTxHash);
    fetchedTxs = wallet.getTxs(txHashes);
    assertEquals(txs.size(), fetchedTxs.size());
    for (int i = 0; i < txs.size(); i++) {
      assertEquals(txs.get(i).getHash(), fetchedTxs.get(i).getHash());
      testTxWallet(fetchedTxs.get(i));
    }
  }
  
  // Can get transactions with additional configuration
  @Test
  public void testGetTxsWithQuery() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get random transactions for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) testTxWallet(randomTx, null);
    
    // get transactions by hash
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTxWallet randomTx : randomTxs) {
      txHashes.add(randomTx.getHash());
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxQuery().setHash(randomTx.getHash()), null, true);
      assertEquals(txs.size(), 1);
      MoneroTxWallet merged = txs.get(0).merge(randomTx.copy()); // txs change with chain so check mergeability
      testTxWallet(merged, null);
    }
    
    // get transactions by hashes
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxQuery().setHashes(txHashes), null, null);
    assertEquals(txs.size(), randomTxs.size());
    for (MoneroTxWallet tx : txs) assertTrue(txHashes.contains(tx.getHash()));
    
    // get transactions with an outgoing transfer
    TxContext ctx = new TxContext();
    ctx.hasOutgoingTransfer = true;
    txs = getAndTestTxs(wallet, new MoneroTxQuery().setIsOutgoing(true), ctx, true);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.isOutgoing());
      assertNotNull(tx.getOutgoingTransfer());
      testTransfer(tx.getOutgoingTransfer(), null);
    }
    
    // get transactions without an outgoing transfer
    ctx.hasOutgoingTransfer = false;
    txs = getAndTestTxs(wallet, new MoneroTxQuery().setIsOutgoing(false), ctx, true);
    for (MoneroTxWallet tx : txs) assertNull(tx.getOutgoingTransfer());
    
    // get transactions with incoming transfers
    ctx = new TxContext();
    ctx.hasIncomingTransfers = true;
    txs = getAndTestTxs(wallet, new MoneroTxQuery().setIsIncoming(true), ctx, true);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.isIncoming());
      assertTrue(tx.getIncomingTransfers().size() > 0);
      for (MoneroIncomingTransfer transfer : tx.getIncomingTransfers()) {
        testTransfer(transfer, null);
      }
    }
    
    // get transactions without incoming transfers
    ctx.hasIncomingTransfers = false;
    txs = getAndTestTxs(wallet, new MoneroTxQuery().setIsIncoming(false), ctx, true);
    for (MoneroTxWallet tx : txs)  {
      assertFalse(tx.isIncoming());
      assertNull(tx.getIncomingTransfers());
    }
    
    // get transactions associated with an account
    int accountIdx = 1;
    txs = wallet.getTxs(new MoneroTxQuery().setTransferQuery(new MoneroTransferQuery().setAccountIndex(accountIdx)));
    for (MoneroTxWallet tx : txs) {
      boolean found = false;
      if (tx.isOutgoing() && tx.getOutgoingTransfer().getAccountIndex() == accountIdx) found = true;
      else if (tx.getIncomingTransfers() != null) {
        for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
          if (transfer.getAccountIndex() == accountIdx) {
            found = true;
            break;
          }
        }
      }
      assertTrue(found, ("Transaction is not associated with account " + accountIdx + ":\n" + tx.toString()));
    }
    
    // get transactions with incoming transfers to an account
    txs = wallet.getTxs(new MoneroTxQuery().setTransferQuery(new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(accountIdx)));
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getIncomingTransfers().size() > 0);
      boolean found = false;
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() == accountIdx) {
          found = true;
          break;
        }
      }
      assertTrue(found, "No incoming transfers to account " + accountIdx + " found:\n" + tx.toString());
    }
    
    // get txs with manually built query that are confirmed and have an outgoing transfer from account 0
    ctx = new TxContext();
    ctx.hasOutgoingTransfer = true;
    MoneroTxQuery txQuery = new MoneroTxQuery();
    txQuery.setIsConfirmed(true);
    txQuery.setTransferQuery(new MoneroTransferQuery().setAccountIndex(0).setIsOutgoing(true));
    txs = getAndTestTxs(wallet, txQuery, ctx, true);
    for (MoneroTxWallet tx : txs) {
      if (!tx.isConfirmed()) System.out.println(tx);
      assertEquals(true, tx.isConfirmed());
      assertTrue(tx.isOutgoing());
      assertEquals(0, (int) tx.getOutgoingTransfer().getAccountIndex());
    }
    
    // get txs with outgoing transfers that have destinations to account 1
    txs = getAndTestTxs(wallet, new MoneroTxQuery().setTransferQuery(new MoneroTransferQuery().setHasDestinations(true).setAccountIndex(0)), null, null);
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.isOutgoing());
      assertTrue(tx.getOutgoingTransfer().getDestinations().size() > 0);
    }
    
    // include outputs with transactions
    ctx = new TxContext();
    ctx.includeOutputs = true;
    txs = getAndTestTxs(wallet, new MoneroTxQuery().setIncludeOutputs(true), ctx, true);
    boolean found = false;
    for (MoneroTxWallet tx : txs) {
      if (tx.getOutputs() != null) {
        assertTrue(tx.getOutputs().size() > 0);
        found = true;
      } else {
        assertTrue(tx.isOutgoing() || (tx.isIncoming() && !tx.isConfirmed())); // TODO: monero-wallet-rpc: return outputs for unconfirmed txs
      }
    }
    assertTrue(found, "No outputs found in txs");
    
    // get txs with input query // TODO: no inputs returned to filter
//    MoneroTxWallet outgoingTx = wallet.getTxs(new MoneroTxQuery().setIsOutgoing(true)).get(0);
//    List<MoneroTxWallet> outgoingTxs = wallet.getTxs(new MoneroTxQuery().setInputQuery(new MoneroOutputQuery().setKeyImage(new MoneroKeyImage().setHex(outgoingTx.getInputsWallet().get(0).getKeyImage().getHex()))));
//    assertEquals(1, outgoingTxs.size());
//    assertEquals(outgoingTx.getHash(), outgoingTxs.get(0).getHash());
    
    // get txs with output query
    MoneroOutputQuery outputQuery = new MoneroOutputQuery().setIsSpent(false).setAccountIndex(1).setSubaddressIndex(2);
    txs = wallet.getTxs(new MoneroTxQuery().setOutputQuery(outputQuery));
    assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) {
      assertFalse(tx.getOutputs() == null || tx.getOutputs().isEmpty());
      found = false;
      for (MoneroOutputWallet output : tx.getOutputsWallet()) {
        if (output.isSpent() == false && output.getAccountIndex() == 1 && output.getSubaddressIndex() == 2) {
          found = true;
          break;
        }
      }
      if (!found) fail("Tx does not contain specified output");
    }
    
    // get unlocked txs
    txs = wallet.getTxs(new MoneroTxQuery().setIsLocked(false));
    assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) {
      assertFalse(tx.isLocked());
    }
    
    // get confirmed transactions sent from/to same wallet with a transfer with destinations
    txs = wallet.getTxs(new MoneroTxQuery().setIsIncoming(true).setIsOutgoing(true).setIncludeOutputs(true).setIsConfirmed(true).setTransferQuery(new MoneroTransferQuery().setHasDestinations(true)));
    assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.isIncoming());
      assertTrue(tx.isOutgoing());
      assertTrue(tx.isConfirmed());
      assertFalse(tx.getOutputsWallet().isEmpty());
      assertNotNull(tx.getOutgoingTransfer());
      assertNotNull(tx.getOutgoingTransfer().getDestinations());
      assertFalse(tx.getOutgoingTransfer().getDestinations().isEmpty());
    }
  }
  
  // Can get transactions by height
  @Test
  public void testGetTxsByHeight() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get all confirmed txs for testing
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxQuery().setIsConfirmed(true));
    
    // collect all tx heights
    List<Long> txHeights = new ArrayList<Long>();
    for (MoneroTxWallet tx : txs) txHeights.add(tx.getHeight());
    
    // get height that most txs occur at
    Map<Long, Integer> heightCounts = countNumInstances(txHeights);
    assertFalse(heightCounts.isEmpty(), "Wallet has no confirmed txs; run send tests");
    Set<Long> heightModes = getModes(heightCounts);
    Long modeHeight = heightModes.iterator().next();
    
    // fetch txs at mode height
    List<MoneroTxWallet> modeTxs = wallet.getTxs(new MoneroTxQuery().setHeight(modeHeight));
    assertEquals((int) heightCounts.get(modeHeight), modeTxs.size());
    for (MoneroTxWallet tx : modeTxs) {
      assertEquals(modeHeight, tx.getHeight());
    }
    
    // fetch txs at mode height by range
    List<MoneroTxWallet> modeTxsByRange = wallet.getTxs(new MoneroTxQuery().setMinHeight(modeHeight).setMaxHeight(modeHeight));
    assertEquals(modeTxs.size(), modeTxsByRange.size());
    assertEquals(modeTxs, modeTxsByRange);
    
    // fetch all txs by range
    assertEquals(txs, wallet.getTxs(new MoneroTxQuery().setMinHeight(txs.get(0).getHeight()).setMaxHeight(txs.get(txs.size() - 1).getHeight())));
    
    // test some filtered by range
    {
      txs = wallet.getTxs(new MoneroTxQuery().setIsConfirmed(true));
      assertTrue(txs.size() > 0, "No transactions; run send to multiple test");
        
      // get and sort block heights in ascending order
      List<Long> heights = new ArrayList<Long>();
      for (MoneroTxWallet tx : txs) {
        heights.add(tx.getBlock().getHeight());
      }
      Collections.sort(heights);
      
      // pick minimum and maximum heights for filtering
      long minHeight = -1;
      long maxHeight = -1;
      if (heights.size() == 1) {
        minHeight = 0;
        maxHeight = heights.get(0) - 1;
      } else {
        minHeight = heights.get(0) + 1;
        maxHeight = heights.get(heights.size() - 1) - 1;
      }
      
      // assert some transactions filtered
      int unfilteredCount = txs.size();
      txs = getAndTestTxs(wallet, new MoneroTxQuery().setMinHeight(minHeight).setMaxHeight(maxHeight), null, true);
      assertTrue(txs.size() < unfilteredCount);
      for (MoneroTx tx : txs) {
        long height = tx.getBlock().getHeight();
        assertTrue(height >= minHeight && height <= maxHeight);
      }
    }
  }
  
  @Test
  public void testGetTxsWithPaymentIds() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get random transactions with payment ids for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxQuery().setHasPaymentId(true), 2, 5);
    assertFalse(randomTxs.isEmpty(), "No txs with payment ids to test");
    for (MoneroTxWallet randomTx : randomTxs) {
      assertNotNull(randomTx.getPaymentId());
    }
    
    // get transactions by payment id
    List<String> paymentIds = new ArrayList<String>();
    for (MoneroTxWallet tx : randomTxs) paymentIds.add(tx.getPaymentId());
    assertTrue(paymentIds.size() > 1);
    for (String paymentId : paymentIds) {
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxQuery().setPaymentId(paymentId), null, null);
      assertTrue(txs.size() > 0);
      assertNotNull(txs.get(0).getPaymentId());
      MoneroUtils.validatePaymentId(txs.get(0).getPaymentId());
    }
    
    // get transactions by payment hashes
    List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxQuery().setPaymentIds(paymentIds), null, null);
    for (MoneroTxWallet tx : txs) {
      assertTrue(paymentIds.contains(tx.getPaymentId()));
    }
  }
  
  // Returns all known fields of txs regardless of filtering
  @Test
  public void testGetTxsFieldsWithFiltering() {
    assumeTrue(TEST_NON_RELAYS);
    
    // fetch wallet txs
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxQuery().setIsConfirmed(true));
    for (MoneroTxWallet tx : txs) {
      
      // find tx sent to same wallet with incoming transfer in different account than src account
      if (tx.getOutgoingTransfer() == null || tx.getIncomingTransfers() == null) continue;
      for (MoneroTransfer transfer : tx.getIncomingTransfers()) {
        if (transfer.getAccountIndex() == tx.getOutgoingTransfer().getAccountIndex()) continue;
        
        // fetch tx with filtering
        List<MoneroTxWallet> filteredTxs = wallet.getTxs(new MoneroTxQuery().setTransferQuery(new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(transfer.getAccountIndex())));
        MoneroTxWallet filteredTx = Filter.apply(new MoneroTxQuery().setHashes(tx.getHash()), filteredTxs).get(0);
        
        // txs should be the same (mergeable)
        assertEquals(tx.getHash(), filteredTx.getHash());
        tx.merge(filteredTx);
        
        // test is done
        return;
      }
    }
    
    // test did not fully execute
    throw new RuntimeException("Test requires tx sent from/to different accounts of same wallet but none found; run send tests");
  }
  
  // Validates inputs when getting transactions
  @Test
  public void testValidateInputsGetTxs() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // fetch random txs for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    
    // valid, invalid, and unknown tx hashes for tests
    String txHash = randomTxs.get(0).getHash();
    String invalidHash = "invalid_id";
    String unknownHash1 = "6c4982f2499ece80e10b627083c4f9b992a00155e98bcba72a9588ccb91d0a61";
    String unknownHash2 = "ff397104dd875882f5e7c66e4f852ee134f8cf45e21f0c40777c9188bc92e943";
    
    // fetch unknown tx hash
    assertNull(wallet.getTx(unknownHash1));
    
    // fetch unknown tx hash using query
    assertEquals(0, wallet.getTxs(new MoneroTxQuery().setHash(unknownHash1)).size());
    
    // fetch unknown tx hash in collection
    List<MoneroTxWallet> txs = wallet.getTxs(txHash, unknownHash1);
    assertEquals(1, txs.size());
    assertEquals(txHash, txs.get(0).getHash());

    // fetch unknown tx hashes in collection
    txs = wallet.getTxs(txHash, unknownHash1, unknownHash2);
    assertEquals(1, txs.size());
    assertEquals(txHash, txs.get(0).getHash());

    // fetch invalid hash
    assertNull(wallet.getTx(invalidHash));
    
    // fetch invalid hash collection
    txs = wallet.getTxs(txHash, invalidHash);
    assertEquals(1, txs.size());
    assertEquals(txHash, txs.get(0).getHash());

    // fetch invalid hashes in collection
    txs = wallet.getTxs(txHash, invalidHash, "invalid_hash_2");
    assertEquals(1, txs.size());
    assertEquals(txHash, txs.get(0).getHash());
    
    // test collection of invalid hashes
    txs = wallet.getTxs(new MoneroTxQuery().setHashes(Arrays.asList(txHash, invalidHash, "invalid_hash_2")));
    assertEquals(1, txs.size());
    for (MoneroTxWallet tx : txs) testTxWallet(tx);
  }

  // Can get transfers in the wallet, accounts, and subaddresses
  @Test
  public void testGetTransfers() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get all transfers
    getAndTestTransfers(wallet, null, null, true);
    
    // get transfers by account index
    boolean nonDefaultIncoming = false;
    for (MoneroAccount account : wallet.getAccounts(true)) {
      List<MoneroTransfer> accountTransfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setAccountIndex(account.getIndex()), null, null);
      for (MoneroTransfer transfer : accountTransfers) assertEquals(transfer.getAccountIndex(), account.getIndex());
      
      // get transfers by subaddress index
      List<MoneroTransfer> subaddressTransfers = new ArrayList<MoneroTransfer>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setAccountIndex(subaddress.getAccountIndex()).setSubaddressIndex(subaddress.getIndex()), null, null);
        for (MoneroTransfer transfer : transfers) {
          
          // test account and subaddress indices
          assertEquals(subaddress.getAccountIndex(), transfer.getAccountIndex());
          if (transfer.isIncoming()) {
            MoneroIncomingTransfer inTransfer = (MoneroIncomingTransfer) transfer;
            assertEquals(subaddress.getIndex(), inTransfer.getSubaddressIndex());
            if (transfer.getAccountIndex() != 0 && inTransfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          } else {
            MoneroOutgoingTransfer outTransfer = (MoneroOutgoingTransfer) transfer;
            assertTrue(outTransfer.getSubaddressIndices().contains(subaddress.getIndex()));
            if (transfer.getAccountIndex() != 0) {
              for (int subaddrIdx : outTransfer.getSubaddressIndices()) {
                if (subaddrIdx > 0) {
                  nonDefaultIncoming = true;
                  break;
                }
              }
            }
          }
          
          // don't add duplicates TODO monero-wallet-rpc: duplicate outgoing transfers returned for different subaddress indices, way to return outgoing subaddress indices?
          boolean found = false;
          for (MoneroTransfer subaddressTransfer : subaddressTransfers) {
            if (transfer.toString().equals(subaddressTransfer.toString()) && transfer.getTx().getHash().equals(subaddressTransfer.getTx().getHash())) {
              found = true;
              break;
            }
          }
          if (!found) subaddressTransfers.add(transfer);
        }
      }
      assertEquals(accountTransfers.size(), subaddressTransfers.size());
      
      // collect unique subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroTransfer transfer : subaddressTransfers) {
        if (transfer.isIncoming()) subaddressIndices.add(((MoneroIncomingTransfer) transfer).getSubaddressIndex());
        else subaddressIndices.addAll(((MoneroOutgoingTransfer) transfer).getSubaddressIndices());
      }
      
      // get and test transfers by subaddress indices
      List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), null, null);
      //if (transfers.size() != subaddressTransfers.size()) System.out.println("WARNING: outgoing transfers always from subaddress 0 (monero-wallet-rpc #5171)");
      assertEquals(subaddressTransfers.size(), transfers.size()); // TODO monero-wallet-rpc: these may not be equal because outgoing transfers are always from subaddress 0 (#5171) and/or incoming transfers from/to same account are occluded (#4500)
      for (MoneroTransfer transfer : transfers) {
        assertEquals(transfer.getAccountIndex(), account.getIndex());
        if (transfer.isIncoming()) assertTrue(subaddressIndices.contains(((MoneroIncomingTransfer) transfer).getSubaddressIndex()));
        else {
          Set<Integer> intersections = new HashSet<Integer>(subaddressIndices);
          intersections.retainAll(((MoneroOutgoingTransfer) transfer).getSubaddressIndices());
          assertTrue(intersections.size() > 0, "Subaddresses must overlap");
        }
      }
    }
    
    // ensure transfer found with non-zero account and subaddress indices
    assertTrue(nonDefaultIncoming, "No transfers found in non-default account and subaddress; run send-to-multiple tests");
  }
  
  // Can get transfers with additional configuration
  @Test
  public void testGetTransfersWithQuery() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get incoming transfers
    List<MoneroTransfer> transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setIsIncoming(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.isIncoming());
    
    // get outgoing transfers
    transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setIsOutgoing(true), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(transfer.isOutgoing());
    
    // get confirmed transfers to account 0
    transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setAccountIndex(0).setTxQuery(new MoneroTxQuery().setIsConfirmed(true)), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(0, (int) transfer.getAccountIndex());
      assertTrue(transfer.getTx().isConfirmed());
    }
    
    // get confirmed transfers to [1, 2]
    transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setAccountIndex(1).setSubaddressIndex(2).setTxQuery(new MoneroTxQuery().setIsConfirmed(true)), null, true);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(1, (int) transfer.getAccountIndex());
      if (transfer.isIncoming()) assertEquals(2, (int) ((MoneroIncomingTransfer) transfer).getSubaddressIndex());
      else assertTrue(((MoneroOutgoingTransfer) transfer).getSubaddressIndices().contains(2));
      assertTrue(transfer.getTx().isConfirmed());
    }
    
    // get transfers in the tx pool
    transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setTxQuery(new MoneroTxQuery().setInTxPool(true)), null, null);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.getTx().inTxPool());
    }
    
    // get random transactions
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, null, 3, 5);
    
    // get transfers with a tx hash
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) {
      txHashes.add(tx.getHash());
      transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setTxQuery(new MoneroTxQuery().setHash(tx.getHash())), null, true);
      for (MoneroTransfer transfer : transfers) assertEquals(tx.getHash(), transfer.getTx().getHash());
    }
    
    // get transfers with tx hashes
    transfers = getAndTestTransfers(wallet, new MoneroTransferQuery().setTxQuery(new MoneroTxQuery().setHashes(txHashes)), null, true);
    for (MoneroTransfer transfer : transfers) assertTrue(txHashes.contains(transfer.getTx().getHash()));
    
    // TODO: test that transfers with the same txHash have the same tx reference
    
    // TODO: test transfers destinations
    
    // get transfers with pre-built query that are confirmed and have outgoing destinations
    MoneroTransferQuery transferQuery = new MoneroTransferQuery();
    transferQuery.setIsOutgoing(true);
    transferQuery.setHasDestinations(true);
    transferQuery.setTxQuery(new MoneroTxQuery().setIsConfirmed(true));
    transfers = getAndTestTransfers(wallet, transferQuery, null, null);
    for (MoneroTransfer transfer : transfers) {
      assertEquals(true, transfer.isOutgoing());
      assertTrue(((MoneroOutgoingTransfer) transfer).getDestinations().size() > 0);
      assertEquals(true, transfer.getTx().isConfirmed());
    }
    
    // get incoming transfers to account 0 which has outgoing transfers (i.e. originated from the same wallet)
    transfers = wallet.getTransfers(new MoneroTransferQuery().setAccountIndex(1).setIsIncoming(true).setTxQuery(new MoneroTxQuery().setIsOutgoing(true)));
    assertFalse(transfers.isEmpty());
    for (MoneroTransfer transfer : transfers) {
      assertTrue(transfer.isIncoming());
      assertEquals(1, (int) transfer.getAccountIndex());
      assertTrue(transfer.getTx().isOutgoing());
      assertNull(transfer.getTx().getOutgoingTransfer());
    }
    
    // get incoming transfers to a specific address
    String subaddress = wallet.getAddress(1, 0);
    transfers = wallet.getTransfers(new MoneroTransferQuery().setIsIncoming(true).setAddress(subaddress));
    assertTrue(transfers.size() > 0);
    for (MoneroTransfer transfer : transfers) {
      assertTrue(transfer instanceof MoneroIncomingTransfer);
      assertEquals(1, transfer.getAccountIndex());
      assertEquals(0, ((MoneroIncomingTransfer) transfer).getSubaddressIndex());
      assertEquals(subaddress, ((MoneroIncomingTransfer) transfer).getAddress());
    }
  }
  
  // Validates inputs when getting transfers
  @Test
  public void testValidateInputsGetTransfers() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // test with invalid hash
    List<MoneroTransfer> transfers = wallet.getTransfers(new MoneroTransferQuery().setTxQuery(new MoneroTxQuery().setHash("invalid_id")));
    assertEquals(0, transfers.size());
    
    // test invalid hash in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    transfers = wallet.getTransfers(new MoneroTransferQuery().setTxQuery(new MoneroTxQuery().setHashes(randomTxs.get(0).getHash(), "invalid_id")));
    assertTrue(transfers.size() > 0);
    MoneroTxWallet tx = transfers.get(0).getTx();
    for (MoneroTransfer transfer : transfers) assertTrue(tx == transfer.getTx());
    
    // test unused subaddress indices
    transfers = wallet.getTransfers(new MoneroTransferQuery().setAccountIndex(0).setSubaddressIndices(1234907));
    assertTrue(transfers.size() == 0);
    
    // test invalid subaddress index
    try {
      transfers = wallet.getTransfers(new MoneroTransferQuery().setAccountIndex(0).setSubaddressIndex(-1));
      throw new RuntimeException("Should have failed");
    } catch (MoneroError e) {
      assertNotEquals("Should have failed", e.getMessage());
    }
  }
  
  // Can get incoming and outgoing transfers using convenience methods
  @Test
  public void testGetIncomingOutgoingTransfers() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get incoming transfers
    List<MoneroIncomingTransfer> inTransfers = wallet.getIncomingTransfers();
    assertFalse(inTransfers.isEmpty());
    for (MoneroIncomingTransfer transfer : inTransfers) {
      assertTrue(transfer.isIncoming());
      testTransfer(transfer, null);
    }
    
    // get incoming transfers with query
    BigInteger amount = inTransfers.get(0).getAmount();
    int accountIdx = inTransfers.get(0).getAccountIndex();
    int subaddressIdx = inTransfers.get(0).getSubaddressIndex();
    inTransfers = wallet.getIncomingTransfers(new MoneroTransferQuery().setAmount(amount).setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx).setTxQuery(new MoneroTxQuery().setIsConfirmed(true)));
    assertFalse(inTransfers.isEmpty());
    for (MoneroIncomingTransfer transfer : inTransfers) {
      assertTrue(transfer.isIncoming());
      assertEquals(amount, transfer.getAmount());
      assertEquals(accountIdx, (int) transfer.getAccountIndex());
      assertEquals(subaddressIdx, (int) transfer.getSubaddressIndex());
      testTransfer(transfer, null);
    }
    
    // get incoming transfers with contradictory query
    try {
      inTransfers = wallet.getIncomingTransfers(new MoneroTransferQuery().setIsIncoming(false));
    } catch (MoneroError e) {
      assertEquals("Transfer query contradicts getting incoming transfers", e.getMessage());
    }
    
    // get outgoing transfers
    List<MoneroOutgoingTransfer> outTransfers = wallet.getOutgoingTransfers();
    assertFalse(outTransfers.isEmpty());
    for (MoneroOutgoingTransfer transfer : outTransfers) {
      assertTrue(transfer.isOutgoing());
      testTransfer(transfer, null);
    }
    
    // get outgoing transfers with query
    outTransfers = wallet.getOutgoingTransfers(new MoneroTransferQuery().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx));
    assertFalse(outTransfers.isEmpty());
    for (MoneroOutgoingTransfer transfer : outTransfers) {
      assertTrue(transfer.isOutgoing());
      assertEquals(accountIdx, (int) transfer.getAccountIndex());
      assertTrue(transfer.getSubaddressIndices().contains(subaddressIdx));
      testTransfer(transfer, null);
    }
    
    // get outgoing transfers with contradictory query
    try {
      outTransfers = wallet.getOutgoingTransfers(new MoneroTransferQuery().setIsOutgoing(false));
    } catch (MoneroError e) {
      assertEquals("Transfer query contradicts getting outgoing transfers", e.getMessage());
    }
  }
  
  // Can get outputs in the wallet, accounts, and subaddresses
  @Test
  public void testGetOutputs() {
    assumeTrue(TEST_NON_RELAYS);

    // get all outputs
    getAndTestOutputs(wallet, null, true);
    
    // get outputs for each account
    boolean nonDefaultIncoming = false;
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    for (MoneroAccount account : accounts) {
      
      // determine if account is used
      boolean isUsed = false;
      for (MoneroSubaddress subaddress : account.getSubaddresses()) if (subaddress.isUsed()) isUsed = true;
      
      // get outputs by account index
      List<MoneroOutputWallet> accountOutputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setAccountIndex(account.getIndex()), isUsed);
      for (MoneroOutputWallet output : accountOutputs) assertEquals(account.getIndex(), output.getAccountIndex());
      
      // get outputs by subaddress index
      List<MoneroOutputWallet> subaddressOutputs = new ArrayList<MoneroOutputWallet>();
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        List<MoneroOutputWallet> outputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()), subaddress.isUsed());
        for (MoneroOutputWallet output : outputs) {
          assertEquals(subaddress.getAccountIndex(), output.getAccountIndex());
          assertEquals(subaddress.getIndex(), output.getSubaddressIndex());
          if (output.getAccountIndex() != 0 && output.getSubaddressIndex() != 0) nonDefaultIncoming = true;
          subaddressOutputs.add(output);
        }
      }
      assertEquals(subaddressOutputs.size(), accountOutputs.size());
      
      // get outputs by subaddress indices
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      for (MoneroOutputWallet output : subaddressOutputs) subaddressIndices.add(output.getSubaddressIndex());
      List<MoneroOutputWallet> outputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setAccountIndex(account.getIndex()).setSubaddressIndices(new ArrayList<Integer>(subaddressIndices)), isUsed);
      assertEquals(outputs.size(), subaddressOutputs.size());
      for (MoneroOutputWallet output : outputs) {
        assertEquals(account.getIndex(), output.getAccountIndex());
        assertTrue(subaddressIndices.contains(output.getSubaddressIndex()));
      }
    }
    
    // ensure output found with non-zero account and subaddress indices
    assertTrue(nonDefaultIncoming, "No outputs found in non-default account and subaddress; run send-to-multiple tests");
  }

  // Can get outputs with additional configuration
  @Test
  public void testGetOutputsWithQuery() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get unspent outputs to account 0
    List<MoneroOutputWallet> outputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setAccountIndex(0).setIsSpent(false), null);
    for (MoneroOutputWallet output : outputs) {
      assertEquals(0, output.getAccountIndex(), 0);
      assertEquals(false, output.isSpent());
    }
    
    // get spent outputs to account 1
    outputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setAccountIndex(1).setIsSpent(true), true);
    for (MoneroOutputWallet output : outputs) {
      assertEquals(1, (int) output.getAccountIndex());
      assertEquals(true, output.isSpent());
    }
    
    // get random transactions
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, new MoneroTxQuery().setIsConfirmed(true), 3, 5);
    
    // get outputs with a tx hash
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) {
      txHashes.add(tx.getHash());
      outputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setHash(tx.getHash())), true);
      for (MoneroOutputWallet output : outputs) assertEquals(output.getTx().getHash(), tx.getHash());
    }
    
    // get outputs with tx hashes
    outputs = getAndTestOutputs(wallet, new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setHashes(txHashes)), true);
    for (MoneroOutputWallet output : outputs) assertTrue(txHashes.contains(output.getTx().getHash()));
    
    // get confirmed outputs to specific subaddress with pre-built query
    int accountIdx = 0;
    int subaddressIdx = 1;
    MoneroOutputQuery outputQuery = new MoneroOutputQuery();
    outputQuery.setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx);
    outputQuery.setTxQuery(new MoneroTxQuery().setIsConfirmed(true));
    outputQuery.setMinAmount(TestUtils.MAX_FEE);
    outputs = getAndTestOutputs(wallet, outputQuery, true);
    for (MoneroOutputWallet output : outputs) {
      assertEquals(accountIdx, (int) output.getAccountIndex());
      assertEquals(subaddressIdx, (int) output.getSubaddressIndex());
      assertEquals(true, output.getTx().isConfirmed());
      assertTrue(output.getAmount().compareTo(TestUtils.MAX_FEE) >= 0);
    }
    
    // get output by key image
    String keyImage = outputs.get(0).getKeyImage().getHex();
    outputs = wallet.getOutputs(new MoneroOutputQuery().setKeyImage(new MoneroKeyImage(keyImage)));
    assertEquals(1, outputs.size());
    assertEquals(keyImage, outputs.get(0).getKeyImage().getHex());
    
    // get outputs whose transaction is confirmed and has incoming and outgoing transfers
    outputs = wallet.getOutputs(new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setIsConfirmed(true).setIsIncoming(true).setIsOutgoing(true).setIncludeOutputs(true)));
    assertFalse(outputs.isEmpty());
    for (MoneroOutputWallet output : outputs) {
      assertTrue(output.getTx().isIncoming());
      assertTrue(output.getTx().isOutgoing());
      assertTrue(output.getTx().isConfirmed());
      assertFalse(output.getTx().getOutputsWallet().isEmpty());
      assertTrue(output.getTx().getOutputsWallet().contains(output));
    }
  }
  
  // Validates inputs when getting wallet outputs
  @Test
  public void testValidateInputsGetOutputs() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // test with invalid hash
    List<MoneroOutputWallet> outputs = wallet.getOutputs(new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setHash("invalid_id")));
    assertEquals(0, outputs.size());
    
    // test invalid hash in collection
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxQuery().setIsConfirmed(true).setIncludeOutputs(true), 3, 5);
    for (MoneroTxWallet randomTx : randomTxs) assertFalse(randomTx.getOutputs().isEmpty());
    outputs = wallet.getOutputs(new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setHashes(randomTxs.get(0).getHash(), "invalid_id")));
    assertFalse(outputs.isEmpty());
    assertEquals(outputs.size(), randomTxs.get(0).getOutputs().size());
    MoneroTxWallet tx = outputs.get(0).getTx();
    for (MoneroOutputWallet output : outputs) assertTrue(tx == output.getTx());
  }
  
  // Can export outputs in hex format
  @Test
  public void testExportOutputs() {
    assumeTrue(TEST_NON_RELAYS);
    String outputsHex = wallet.exportOutputs();
    assertNotNull(outputsHex);  // TODO: this will fail if wallet has no outputs; run these tests on new wallet
    assertTrue(outputsHex.length() > 0);
    
    // wallet exports outputs since last export by default
    outputsHex = wallet.exportOutputs();
    String outputsHexAll = wallet.exportOutputs(true);
    assertTrue(outputsHexAll.length() > outputsHex.length());
  }
  
  // Can import outputs in hex format
  @Test
  public void testImportOutputs() {
    assumeTrue(TEST_NON_RELAYS);
    
    // export outputs hex
    String outputsHex = wallet.exportOutputs();
    
    // import outputs hex
    if (outputsHex != null) {
      int numImported = wallet.importOutputs(outputsHex);
      assertTrue(numImported >= 0);
    }
  }
  
  // Has correct accounting across accounts, subaddresses, txs, transfers, and outputs
  @Test
  public void testAccounting() {
    assumeTrue(TEST_NON_RELAYS);
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    BigInteger walletBalance = wallet.getBalance();
    BigInteger walletUnlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);  // includes subaddresses
    
    // test wallet balance
    TestUtils.testUnsignedBigInteger(walletBalance);
    TestUtils.testUnsignedBigInteger(walletUnlockedBalance);
    assertTrue(walletBalance.compareTo(walletUnlockedBalance) >= 0);
    
    // test that wallet balance equals sum of account balances
    BigInteger accountsBalance = BigInteger.valueOf(0);
    BigInteger accountsUnlockedBalance = BigInteger.valueOf(0);
    for (MoneroAccount account : accounts) {
      testAccount(account); // test that account balance equals sum of subaddress balances
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
    }
    assertEquals(0, walletBalance.compareTo(accountsBalance));
    assertEquals(0, walletUnlockedBalance.compareTo(accountsUnlockedBalance));
    
//    // test that wallet balance equals net of wallet's incoming and outgoing tx amounts
//    // TODO monero-wallet-rpc: these tests are disabled because incoming transfers are not returned when sent from the same account, so doesn't balance #4500
//    // TODO: test unlocked balance based on txs, requires e.g. tx.isLocked()
//    // TODO: update this test with new api
//    BigInteger outgoingSum = BigInteger.valueOf(0);
//    BigInteger incomingSum = BigInteger.valueOf(0);
//    for (MoneroTxWallet tx : txs) {
//      if (tx.getOutgoingAmount() != null) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//      if (tx.getIncomingAmount() != null) incomingSum = incomingSum.add(tx.getIncomingAmount());
//    }
//    assertEquals(walletBalance, incomingSum.subtract(outgoingSum));
//
//    // test that each account's balance equals net of account's incoming and outgoing tx amounts
//    for (MoneroAccount account : accounts) {
//      if (account.getIndex() != 1) continue; // find 1
//      outgoingSum = BigInteger.valueOf(0);
//      incomingSum = BigInteger.valueOf(0);
//      let filter = new MoneroTxFilter();
//      filter.setAccountIndex(account.getIndex());
//      for (let tx of txs.filter(tx => filter.meetsCriteria(tx))) { // normally we'd call wallet.getTxs(filter) but we're using pre-fetched txs
//        if (tx.getId() === "8d3919d98dd5a734da8c52eddc558db3fbf059ad55d432f0052ecd59ef122ecb") console.log(tx.toString(0));
//
//        //console.log((tx.getOutgoingAmount() ? tx.getOutgoingAmount().toString() : "") + ", " + (tx.getIncomingAmount() ? tx.getIncomingAmount().toString() : ""));
//        if (tx.getOutgoingAmount()) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//        if (tx.getIncomingAmount()) incomingSum = incomingSum.add(tx.getIncomingAmount());
//      }
//      assertEquals(incomingSum.subtract(outgoingSum).toString(), account.getBalance().toString());
//    }
    
    // balance may not equal sum of unspent outputs if unconfirmed txs
    // TODO monero-wallet-rpc: reason not to return unspent outputs on unconfirmed txs? then this isn't necessary
    List<MoneroTxWallet> txs = wallet.getTxs();
    boolean hasUnconfirmedTx = false;
    for (MoneroTxWallet tx : txs) if (tx.inTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent outputs
    BigInteger walletSum = BigInteger.valueOf(0);
    for (MoneroOutputWallet output : wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false))) walletSum = walletSum.add(output.getAmount());
    if (!walletBalance.equals(walletSum)) {
      
      // txs may have changed in between calls so retry test
      walletSum = BigInteger.valueOf(0);
      for (MoneroOutputWallet output : wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false))) walletSum = walletSum.add(output.getAmount());
      if (!walletBalance.equals(walletSum)) assertTrue(hasUnconfirmedTx, "Wallet balance must equal sum of unspent outputs if no unconfirmed txs");
    }
    
    // account balances are sum of their unspent outputs
    for (MoneroAccount account : accounts) {
      BigInteger accountSum = BigInteger.valueOf(0);
      List<MoneroOutputWallet> accountOutputs = wallet.getOutputs(new MoneroOutputQuery().setAccountIndex(account.getIndex()).setIsSpent(false));
      for (MoneroOutputWallet output : accountOutputs) accountSum = accountSum.add(output.getAmount());
      if (!account.getBalance().equals(accountSum)) assertTrue(hasUnconfirmedTx, "Account balance must equal sum of its unspent outputs if no unconfirmed txs");
      
      // subaddress balances are sum of their unspent outputs
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        BigInteger subaddressSum = BigInteger.valueOf(0);
        List<MoneroOutputWallet> subaddressOutputs = wallet.getOutputs(new MoneroOutputQuery().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()).setIsSpent(false));
        for (MoneroOutputWallet output : subaddressOutputs) subaddressSum = subaddressSum.add(output.getAmount());
        if (!subaddress.getBalance().equals(subaddressSum)) assertTrue(hasUnconfirmedTx, "Subaddress balance must equal sum of its unspent outputs if no unconfirmed txs");
      }
    }
  }
  
  // Can check a transfer using the transaction's secret key and the destination
  @Test
  public void testCheckTxKey() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroTxWallet> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxQuery().setIsConfirmed(true).setTransferQuery(new MoneroTransferQuery().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (AssertionError e) {
      if (e.getMessage().contains("found with")) fail("No txs with outgoing destinations found; run send tests");
      throw e;
    }
    
    // test good checks
    assertTrue(txs.size() > 0, "No transactions found with outgoing destinations");
    for (MoneroTxWallet tx : txs) {
      String key = wallet.getTxKey(tx.getHash());
      assertFalse(tx.getOutgoingTransfer().getDestinations().isEmpty());
      for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
        MoneroCheckTx check = wallet.checkTxKey(tx.getHash(), key, destination.getAddress());
        if (destination.getAmount().compareTo(BigInteger.valueOf(0)) > 0) {
          // TODO monero-wallet-rpc: indicates amount received amount is 0 despite transaction with transfer to this address
          // TODO monero-wallet-rpc: returns 0-4 errors, not consistent
//        assertTrue(check.getReceivedAmount().compareTo(BigInteger.valueOf(0)) > 0);
          if (check.getReceivedAmount().equals(BigInteger.valueOf(0))) {
            System.out.println("WARNING: key proof indicates no funds received despite transfer (txid=" + tx.getHash() + ", key=" + key + ", address=" + destination.getAddress() + ", amount=" + destination.getAmount() + ")");
          }
        }
        else assertTrue(check.getReceivedAmount().equals(BigInteger.valueOf(0)));
        testCheckTx(tx, check);
      }
    }
    
    // test get tx key with invalid hash
    try {
      wallet.getTxKey("invalid_tx_id");
      fail("Should throw exception for invalid key");
    } catch (MoneroError e) {
      testInvalidTxHashError(e);
    }
    
    // test check with invalid tx hash
    MoneroTxWallet tx = txs.get(0);
    String key = wallet.getTxKey(tx.getHash());
    MoneroDestination destination = tx.getOutgoingTransfer().getDestinations().get(0);
    try {
      wallet.checkTxKey("invalid_tx_id", key, destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxHashError(e);
    }
    
    // test check with invalid key
    try {
      wallet.checkTxKey(tx.getHash(), "invalid_tx_key", destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxKeyError(e);
    }
    
    // test check with invalid address
    try {
      wallet.checkTxKey(tx.getHash(), key, "invalid_tx_address");
      throw new RuntimeException("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidAddressError(e);
    }
    
    // test check with different address
    String differentAddress = null;
    for (MoneroTxWallet aTx : wallet.getTxs()) {
      if (aTx.getOutgoingTransfer() == null || aTx.getOutgoingTransfer().getDestinations() == null) continue;
      for (MoneroDestination aDestination : aTx.getOutgoingTransfer().getDestinations()) {
        if (!aDestination.getAddress().equals(destination.getAddress())) {
          differentAddress = aDestination.getAddress();
          break;
        }
      }
    }
    assertNotNull("Could not get a different outgoing address to test; run send tests", differentAddress);
    MoneroCheckTx check = wallet.checkTxKey(tx.getHash(), key, differentAddress);
    assertTrue(check.isGood());
    assertTrue(check.getReceivedAmount().compareTo(BigInteger.valueOf(0)) >= 0);
    testCheckTx(tx, check);
  }
  
  // Can prove a transaction by getting its signature
  @Test
  public void testCheckTxProof() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get random txs with outgoing destinations
    List<MoneroTxWallet> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxQuery().setTransferQuery(new MoneroTransferQuery().setHasDestinations(true)), 2, MAX_TX_PROOFS);
    } catch (AssertionError e) {
      if (e.getMessage().contains("found with")) fail("No txs with outgoing destinations found; run send tests");
      throw e;
    }
    
    // test good checks with messages
    for (MoneroTxWallet tx : txs) {
      for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
        String signature = wallet.getTxProof(tx.getHash(), destination.getAddress(), "This transaction definitely happened.");
        MoneroCheckTx check = wallet.checkTxProof(tx.getHash(), destination.getAddress(), "This transaction definitely happened.", signature);
        testCheckTx(tx, check);
      }
    }
    
    // test good check without message
    MoneroTxWallet tx = txs.get(0);
    MoneroDestination destination = tx.getOutgoingTransfer().getDestinations().get(0);
    String signature = wallet.getTxProof(tx.getHash(), destination.getAddress());
    MoneroCheckTx check = wallet.checkTxProof(tx.getHash(), destination.getAddress(), null, signature);
    testCheckTx(tx, check);
    
    // test get proof with invalid hash
    try {
      wallet.getTxProof("invalid_tx_id", destination.getAddress());
      throw new RuntimeException("Should throw exception for invalid key");
    } catch (MoneroError e) {
      testInvalidTxHashError(e);
    }
    
    // test check with invalid tx hash
    try {
      wallet.checkTxProof("invalid_tx_id", destination.getAddress(), null, signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxHashError(e);
    }
    
    // test check with invalid address
    try {
      wallet.checkTxProof(tx.getHash(), "invalid_tx_address", null, signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidAddressError(e);
    }
    
    // test check with wrong message
    signature = wallet.getTxProof(tx.getHash(), destination.getAddress(), "This is the right message");
    check = wallet.checkTxProof(tx.getHash(), destination.getAddress(), "This is the wrong message", signature);
    assertEquals(check.isGood(), false);
    testCheckTx(tx, check);
    
    // test check with wrong signature
    String wrongSignature = wallet.getTxProof(txs.get(1).getHash(), txs.get(1).getOutgoingTransfer().getDestinations().get(0).getAddress(), "This is the right message");
    try {
      check = wallet.checkTxProof(tx.getHash(), destination.getAddress(), "This is the right message", wrongSignature);
      assertEquals(check.isGood(), false);
    } catch (MoneroError e) {
      testInvalidSignatureError(e);
    }
    
    // test check with empty signature
    try {
      check = wallet.checkTxProof(tx.getHash(), destination.getAddress(), "This is the right message", "");
      assertEquals(check.isGood(), false);
    } catch (MoneroError e) {
      assertEquals("Must provide signature to check tx proof", e.getMessage());
    }
  }
  
  // Can prove a spend using a generated signature and no destination public address
  @Test
  public void testCheckSpendProof() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get random confirmed outgoing txs
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, new MoneroTxQuery().setIsIncoming(false).setInTxPool(false).setIsFailed(false), 2, MAX_TX_PROOFS);
    for (MoneroTxWallet tx : txs) {
      assertEquals(true, tx.isConfirmed());
      assertNull(tx.getIncomingTransfers());
      assertNotNull(tx.getOutgoingTransfer());
    }
    
    // test good checks with messages
    for (MoneroTxWallet tx : txs) {
      String signature = wallet.getSpendProof(tx.getHash(), "I am a message.");
      assertTrue(wallet.checkSpendProof(tx.getHash(), "I am a message.", signature));
    }
    
    // test good check without message
    MoneroTxWallet tx = txs.get(0);
    String signature = wallet.getSpendProof(tx.getHash());
    assertTrue(wallet.checkSpendProof(tx.getHash(), null, signature));
    
    // test get proof with invalid hash
    try {
      wallet.getSpendProof("invalid_tx_id");
      throw new RuntimeException("Should throw exception for invalid key");
    } catch (MoneroError e) {
      testInvalidTxHashError(e);
    }
    
    // test check with invalid tx hash
    try {
      wallet.checkSpendProof("invalid_tx_id", null, signature);
      throw new RuntimeException("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxHashError(e);
    }
    
    // test check with invalid message
    signature = wallet.getSpendProof(tx.getHash(), "This is the right message");
    assertEquals(false, wallet.checkSpendProof(tx.getHash(), "This is the wrong message", signature));
    
    // test check with wrong signature
    signature = wallet.getSpendProof(txs.get(1).getHash(), "This is the right message");
    assertEquals(false, wallet.checkSpendProof(tx.getHash(), "This is the right message", signature));
  }
  
  // Can prove reserves in the wallet
  @Test
  public void testGetReserveProofWallet() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get proof of entire wallet
    String signature = wallet.getReserveProofWallet("Test message");
    
    // check proof of entire wallet
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", signature);
    assertTrue(check.isGood());
    testCheckReserve(check);
    BigInteger balance = wallet.getBalance();
    if (!balance.equals(check.getTotalAmount())) {  // TODO monero-wallet-rpc: this check fails with unconfirmed txs
      List<MoneroTxWallet> unconfirmedTxs = wallet.getTxs(new MoneroTxQuery().setInTxPool(true));
      assertTrue(unconfirmedTxs.size() > 0, "Reserve amount must equal balance unless wallet has unconfirmed txs");
    }
    
    // test different wallet address
    String differentAddress = TestUtils.getExternalWalletAddress();
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testNoSubaddressError(e);
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof((wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testNoSubaddressError(e);
    }
    
    // test wrong message
    check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Wrong message", signature);
    assertEquals(false, check.isGood());  // TODO: specifically test reserve checks, probably separate objects
    testCheckReserve(check);
    
    // test wrong signature
    try {
      wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", "wrong signature");
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testSignatureHeaderCheckError(e);
    }
  }
  
  // Can prove reserves in an account
  @Test
  public void testGetReserveProofAccount() {
    assumeTrue(TEST_NON_RELAYS);
    
    // test proofs of accounts
    int numNonZeroTests = 0;
    String msg = "Test message";
    List<MoneroAccount> accounts = wallet.getAccounts();
    String signature = null;
    for (MoneroAccount account : accounts) {
      if (account.getBalance().compareTo(BigInteger.valueOf(0)) > 0) {
        BigInteger checkAmount = (account.getBalance()).divide(BigInteger.valueOf(2));
        signature = wallet.getReserveProofAccount(account.getIndex(), checkAmount, msg);
        MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), msg, signature);
        assertTrue(check.isGood());
        testCheckReserve(check);
        assertTrue(check.getTotalAmount().compareTo(checkAmount) >= 0);
        numNonZeroTests++;
      } else {
        try {
          wallet.getReserveProofAccount(account.getIndex(), account.getBalance(), msg);
          throw new RuntimeException("Should have thrown exception");
        } catch (MoneroError e) {
          assertEquals(-1, (int) e.getCode());
          try {
            wallet.getReserveProofAccount(account.getIndex(), TestUtils.MAX_FEE, msg);
            throw new RuntimeException("Should have thrown exception");
          } catch (MoneroError e2) {
            assertEquals(-1, (int) e2.getCode());
          }
        }
      }
    }
    assertTrue(numNonZeroTests > 1, "Must have more than one account with non-zero balance; run send-to-multiple tests");
    
    // test error when not enough balance for requested minimum reserve amount
    try {
      String proof = wallet.getReserveProofAccount(0, accounts.get(0).getBalance().add(TestUtils.MAX_FEE), "Test message");
      System.out.println("Account balance: " + wallet.getBalance(0));
      System.out.println("accounts.get(0) balance: " + accounts.get(0).getBalance());
      MoneroCheckReserve reserve = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", proof);
      try {
        wallet.getReserveProofAccount(0, accounts.get(0).getBalance().add(TestUtils.MAX_FEE), "Test message");
        throw new RuntimeException("expecting this to succeed");
      } catch (Exception e) {
        assertEquals("expecting this to succeed", e.getMessage());
      }
      System.out.println("Check reserve proof: " + JsonUtils.serialize(reserve));
      fail("Should have thrown exception but got reserve proof: https://github.com/monero-project/monero/issues/6595");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test different wallet address
    String differentAddress = TestUtils.getExternalWalletAddress();
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof((wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test wrong message
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Wrong message", signature);
    assertEquals(check.isGood(), false); // TODO: specifically test reserve checks, probably separate objects
    testCheckReserve(check);
    
    // test wrong signature
    try {
      wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", "wrong signature");
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertEquals(-1, (int) e.getCode());
    }
  }
  
  // Can get and set a transaction note
  @Test
  public void testSetTxNote() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroTxWallet> txs = getRandomTransactions(wallet, null, 1, 5);
    
    // set notes
    String uuid = UUID.randomUUID().toString();
    for (int i = 0; i < txs.size(); i++) {
      wallet.setTxNote(txs.get(i).getHash(), uuid + i);
    }
    
    // get notes
    for (int i = 0; i < txs.size(); i++) {
      assertEquals(wallet.getTxNote(txs.get(i).getHash()), uuid + i);
    }
  }
  
  // Can get and set multiple transaction notes
  // TODO: why does getting cached txs take 2 seconds when should already be cached?
  @Test
  public void testSetTxNotes() {
    assumeTrue(TEST_NON_RELAYS);
    
    // set tx notes
    String uuid = UUID.randomUUID().toString();
    List<MoneroTxWallet> txs = wallet.getTxs();
    assertTrue(txs.size() >= 3, "Test requires 3 or more wallet transactions; run send tests");
    List<String> txHashes = new ArrayList<String>();
    List<String> txNotes = new ArrayList<String>();
    for (int i = 0; i < txHashes.size(); i++) {
      txHashes.add(txs.get(i).getHash());
      txNotes.add(uuid + i);
    }
    wallet.setTxNotes(txHashes, txNotes);
    
    // get tx notes
    txNotes = wallet.getTxNotes(txHashes);
    for (int i = 0; i < txHashes.size(); i++) {
      assertEquals(uuid + i, txNotes.get(i));
    }
    
    // TODO: test that get transaction has note
  }
  
  // Can export signed key images
  @Test
  public void testExportKeyImages() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroKeyImage> images = wallet.exportKeyImages(true);
    assertTrue(images.size() > 0, "No signed key images in wallet");
    for (MoneroKeyImage image : images) {
      assertTrue(image instanceof MoneroKeyImage);
      assertTrue(image.getHex().length() > 0);
      assertTrue(image.getSignature().length() > 0);
    }
    
    // wallet exports key images since last export by default
    images = wallet.exportKeyImages();
    List<MoneroKeyImage> imagesAll = wallet.exportKeyImages(true);
    assert(imagesAll.size() > images.size());
  }
  
  // Can get new key images from the last import
  @Test
  public void testGetNewKeyImagesFromLastImport() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get outputs hex
    String outputsHex = wallet.exportOutputs();
    
    // import outputs hex
    if (outputsHex != null) {
      int numImported = wallet.importOutputs(outputsHex);
      assertTrue(numImported >= 0);
    }
    
    // get and test new key images from last import
    List<MoneroKeyImage> images = wallet.getNewKeyImagesFromLastImport();
    if (images.isEmpty()) fail("No new key images in last import"); // TODO: these are already known to the wallet, so no new key images will be imported
    for (MoneroKeyImage image : images) {
      assertTrue(image.getHex().length() > 0);
      assertTrue(image.getSignature().length() > 0);
    }
  }
  
  // Can import key images
  // TODO monero-project: importing key images can cause erasure of incoming transfers per wallet2.cpp:11957
  @Disabled
  @Test
  public void testImportKeyImages() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroKeyImage> images = wallet.exportKeyImages();
    assertTrue(images.size() > 0, "Wallet does not have any key images; run send tests");
    MoneroKeyImageImportResult result = wallet.importKeyImages(images);
    assertTrue(result.getHeight() > 0);
    
    // determine if non-zero spent and unspent amounts are expected
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxQuery().setIsOutgoing(true).setIsConfirmed(true));
    BigInteger balance = wallet.getBalance();
    boolean hasSpent = txs.size() > 0;
    boolean hasUnspent = balance.compareTo(BigInteger.valueOf(0)) > 0;
    
    // test amounts
    TestUtils.testUnsignedBigInteger(result.getSpentAmount(), hasSpent);
    TestUtils.testUnsignedBigInteger(result.getUnspentAmount(), hasUnspent);
  }
  
  // Supports view-only and offline wallets to create, sign, and submit transactions
  @SuppressWarnings("unused")
  @Test
  public void testViewOnlyAndOfflineWallets() {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only and offline wallets
    MoneroWallet viewOnlyWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    MoneroWallet offlineWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
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
  
  protected void testViewOnlyAndOfflineWallets(MoneroWallet viewOnlyWallet, MoneroWallet offlineWallet) {
    
    // wait for txs to confirm and for sufficient unlocked balance
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, 0, null, TestUtils.MAX_FEE.multiply(new BigInteger("4")));
    
    // test getting txs, transfers, and outputs from view-only wallet
    assertFalse(viewOnlyWallet.getTxs().isEmpty());
    assertFalse(viewOnlyWallet.getTransfers().isEmpty());
    assertFalse(viewOnlyWallet.getOutputs().isEmpty());
    
    // collect info from main test wallet
    String primaryAddress = wallet.getPrimaryAddress();
    String privateViewKey = wallet.getPrivateViewKey();
    
    // test and sync view-only wallet
    assertEquals(primaryAddress, viewOnlyWallet.getPrimaryAddress());
    assertEquals(privateViewKey, viewOnlyWallet.getPrivateViewKey());
    assertTrue(viewOnlyWallet.isViewOnly());
    String errMsg = "Should have failed";
    try {
      viewOnlyWallet.getSeed();
      throw new RuntimeException(errMsg);
    } catch (Exception e) {
      assertNotEquals(errMsg, e.getMessage());
    }
    try {
      viewOnlyWallet.getSeedLanguage();
      throw new RuntimeException(errMsg);
    } catch (Exception e) {
      assertNotEquals(errMsg, e.getMessage());
    }
    try {
      viewOnlyWallet.getPrivateSpendKey();
      throw new RuntimeException(errMsg);
    } catch (Exception e) {
      assertNotEquals(errMsg, e.getMessage());
    }
    assertTrue(viewOnlyWallet.isConnectedToDaemon());  // TODO: this fails with monero-wallet-rpc and monerod with authentication
    viewOnlyWallet.sync();
    assertTrue(viewOnlyWallet.getTxs().size() > 0);
    
    // export outputs from view-only wallet
    String outputsHex = viewOnlyWallet.exportOutputs();
    
    // test offline wallet
    assertFalse(offlineWallet.isConnectedToDaemon());
    assertFalse(offlineWallet.isViewOnly());
    if (!(offlineWallet instanceof MoneroWalletRpc)) assertEquals(TestUtils.SEED, offlineWallet.getSeed()); // TODO monero-project: cannot get seed from offline wallet rpc
    assertEquals(0, offlineWallet.getTxs(new MoneroTxQuery().setInTxPool(false)).size());
    
    // import outputs to offline wallet
    int numOutputsImported = offlineWallet.importOutputs(outputsHex);
    assertTrue(numOutputsImported > 0, "No outputs imported");
    
    // export key images from offline wallet
    List<MoneroKeyImage> keyImages = offlineWallet.exportKeyImages();
    
    // import key images to view-only wallet
    assertTrue(viewOnlyWallet.isConnectedToDaemon());
    viewOnlyWallet.importKeyImages(keyImages);
    assertEquals(wallet.getBalance(), viewOnlyWallet.getBalance());
    
    // create unsigned tx using view-only wallet
    MoneroTxWallet unsignedTx = viewOnlyWallet.createTx(new MoneroTxConfig().setAccountIndex(0).setAddress(primaryAddress).setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("3"))));
    assertNotNull(unsignedTx.getTxSet().getUnsignedTxHex());
    
    // sign tx using offline wallet
    MoneroTxSet signedTxSet = offlineWallet.signTxs(unsignedTx.getTxSet().getUnsignedTxHex());
    assertFalse(signedTxSet.getSignedTxHex().isEmpty());
    assertEquals(1, signedTxSet.getTxs().size());
    assertFalse(signedTxSet.getTxs().get(0).getHash().isEmpty());
    
    // parse or "describe" unsigned tx set
    MoneroTxSet describedTxSet = offlineWallet.describeUnsignedTxSet(unsignedTx.getTxSet().getUnsignedTxHex());
    testDescribedTxSet(describedTxSet);
    
    // submit signed tx using view-only wallet
    if (TEST_RELAYS) {
      List<String> txHashes = viewOnlyWallet.submitTxs(signedTxSet.getSignedTxHex());
      assertEquals(1, txHashes.size());
      assertEquals(64, txHashes.get(0).length());
      TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(viewOnlyWallet); // wait for confirmation for other tests
    }
  }
  
  // Can sign and verify messages
  // TODO: test with view-only wallet
  @Test
  public void testSignAndVerifyMessages() {
    assumeTrue(TEST_NON_RELAYS);
    
    // message to sign and subaddresses to test
    String msg = "This is a super important message which needs to be signed and verified.";
    List<MoneroSubaddress> subaddresses = Arrays.asList(new MoneroSubaddress(0, 0), new MoneroSubaddress(0, 1), new MoneroSubaddress(1, 0));
    
    // test signing message with subaddresses
    for (MoneroSubaddress subaddress : subaddresses) {
      
      // sign and verify message with spend key
      String signature = wallet.signMessage(msg, MoneroMessageSignatureType.SIGN_WITH_SPEND_KEY, subaddress.getAccountIndex(), subaddress.getIndex());
      MoneroMessageSignatureResult result = wallet.verifyMessage(msg, wallet.getAddress(subaddress.getAccountIndex(), subaddress.getIndex()), signature);
      assertEquals(new MoneroMessageSignatureResult(true, false, MoneroMessageSignatureType.SIGN_WITH_SPEND_KEY, 2), result);
      
      // verify message with incorrect address
      result = wallet.verifyMessage(msg, wallet.getAddress(0, 2), signature);
      assertEquals(new MoneroMessageSignatureResult(false, null, null, null), result);
      
      // verify message with invalid address
      result = wallet.verifyMessage(msg, "invalid address", signature);
      assertEquals(new MoneroMessageSignatureResult(false, null, null, null), result);
      
      // verify message with external address
      result = wallet.verifyMessage(msg, TestUtils.getExternalWalletAddress(), signature);
      assertEquals(new MoneroMessageSignatureResult(false, null, null, null), result);
      
      // sign and verify message with view key
      signature = wallet.signMessage(msg, MoneroMessageSignatureType.SIGN_WITH_VIEW_KEY, subaddress.getAccountIndex(), subaddress.getIndex());
      result = wallet.verifyMessage(msg, wallet.getAddress(subaddress.getAccountIndex(), subaddress.getIndex()), signature);
      assertEquals(new MoneroMessageSignatureResult(true, false, MoneroMessageSignatureType.SIGN_WITH_VIEW_KEY, 2), result);
      
      // verify message with incorrect address
      result = wallet.verifyMessage(msg, wallet.getAddress(0, 2), signature);
      assertEquals(new MoneroMessageSignatureResult(false, null, null, null), result);
      
      // verify message with external address
      result = wallet.verifyMessage(msg, TestUtils.getExternalWalletAddress(), signature);
      assertEquals(new MoneroMessageSignatureResult(false, null, null, null), result);
      
      // verify message with invalid address
      result = wallet.verifyMessage(msg, "invalid address", signature);
      assertEquals(new MoneroMessageSignatureResult(false, null, null, null), result);
    }
  }
  
  // Has an address book
  @Test
  public void testAddressBook() {
    assumeTrue(TEST_NON_RELAYS);
    
    // initial state
    List<MoneroAddressBookEntry> entries = wallet.getAddressBookEntries();
    int numEntriesStart = entries.size();
    for (MoneroAddressBookEntry entry : entries) testAddressBookEntry(entry);
    
    // test adding standard addresses
    int NUM_ENTRIES = 5;
    String address = TestUtils.getExternalWalletAddress();
    List<Integer> indices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_ENTRIES; i++) {
      indices.add(wallet.addAddressBookEntry(address, "hi there!"));
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart + NUM_ENTRIES, entries.size());
    for (int idx : indices) {
      boolean found = false;
      for (MoneroAddressBookEntry entry : entries) {
        if (idx == entry.getIndex()) {
          testAddressBookEntry(entry);
          assertEquals(entry.getAddress(), address);
          assertEquals(entry.getDescription(), "hi there!");
          found = true;
          break;
        }
      }
      assertTrue(found, "Index " + idx + " not found in address book indices");
    }
    
    // edit each address book entry
    for (int idx : indices) {
      wallet.editAddressBookEntry(idx, false, null, true, "hello there!!");
    }
    entries = wallet.getAddressBookEntries(indices);
    for (MoneroAddressBookEntry entry : entries) {
      assertEquals("hello there!!", entry.getDescription());
    }
    
    // delete entries at starting index
    int deleteIdx = indices.get(0);
    for (int i = 0; i < indices.size(); i++) {
      wallet.deleteAddressBookEntry(deleteIdx);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(entries.size(), numEntriesStart);
    
    // test adding integrated addresses
    indices = new ArrayList<Integer>();
    String paymentId = "03284e41c342f03"; // payment id less one character
    Map<Integer, MoneroIntegratedAddress> integratedAddresses = new HashMap<Integer, MoneroIntegratedAddress>();
    Map<Integer, String> integratedDescriptions = new HashMap<Integer, String>();
    for (int i = 0; i < NUM_ENTRIES; i++) {
      MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(null, paymentId + i); // create unique integrated address
      String uuid = UUID.randomUUID().toString();
      int idx = wallet.addAddressBookEntry(integratedAddress.toString(), uuid);
      indices.add(idx);
      integratedAddresses.put(idx, integratedAddress);
      integratedDescriptions.put(idx, uuid);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(entries.size(), numEntriesStart + NUM_ENTRIES);
    for (int idx : indices) {
      boolean found = false;
      for (MoneroAddressBookEntry entry : entries) {
        if (idx == entry.getIndex()) {
          testAddressBookEntry(entry);
          assertEquals(integratedDescriptions.get(idx), entry.getDescription());
          assertEquals(integratedAddresses.get(idx).toString(), entry.getAddress());
          assertEquals(null, entry.getPaymentId());
          found = true;
          break;
        }
      }
      assertTrue(found, "Index " + idx + " not found in address book indices");
    }
    
    // delete entries at starting index
    deleteIdx = indices.get(0);
    for (int i = 0; i < indices.size(); i++) {
      wallet.deleteAddressBookEntry(deleteIdx);
    }
    entries = wallet.getAddressBookEntries();
    assertEquals(numEntriesStart, entries.size());
  }
  
  // Can get and set arbitrary key/value attributes
  @Test
  public void testSetAttributes() {
    assumeTrue(TEST_NON_RELAYS);
    
    // set attributes
    Map<String, String> attrs = new HashMap<String, String>();
    for (int i = 0; i < 5; i++) {
      String key = "attr" + i;
      String val = UUID.randomUUID().toString();
      attrs.put(key, val);
      wallet.setAttribute(key, val);
    }
    
    // test attributes
    for (String key : attrs.keySet()) {
      assertEquals(wallet.getAttribute(key), attrs.get(key));
    }
    
    // get an undefined attribute
    assertEquals(null, wallet.getAttribute("unset_key"));
  }
  
  // Can convert between a tx config and payment URI
  @Test
  public void testGetPaymentUri() {
    assumeTrue(TEST_NON_RELAYS);
    
    // test with address and amount
    MoneroTxConfig config1 = new MoneroTxConfig().setAddress(wallet.getAddress(0, 0)).setAmount(BigInteger.valueOf(0));
    String uri = wallet.getPaymentUri(config1);
    MoneroTxConfig config2 = wallet.parsePaymentUri(uri);
    assertEquals(config1, config2);
    
    // test with subaddress and all fields
    config1.getDestinations().get(0).setAddress(wallet.getSubaddress(0, 1).getAddress());
    config1.getDestinations().get(0).setAmount(new BigInteger("425000000000"));
    config1.setRecipientName("John Doe");
    config1.setNote("OMZG XMR FTW");
    uri = wallet.getPaymentUri(config1);
    config2 = wallet.parsePaymentUri(uri);
    assertEquals(config1, config2);
    
    // test with undefined address
    String address = config1.getDestinations().get(0).getAddress();
    config1.getDestinations().get(0).setAddress(null);
    try {
      wallet.getPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroError e) {
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
    config1.getDestinations().get(0).setAddress(address);
    
    // test with standalone payment id
    config1.setPaymentId("03284e41c342f03603284e41c342f03603284e41c342f03603284e41c342f036");
    try {
      wallet.getPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroError e) {
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
  }
  
  // Can start and stop mining
  @Test
  public void testMining() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroMiningStatus status = daemon.getMiningStatus();
    if (status.isActive()) wallet.stopMining();
    wallet.startMining(2l, false, true);
    wallet.stopMining();
  }
  
  // Can change the wallet password
  @Test
  public void testChangePassword() {
    
    // create random wallet
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPassword(TestUtils.WALLET_PASSWORD));
    String path = wallet.getPath();
    
    // change password
    String newPassword = "";
    wallet.changePassword(TestUtils.WALLET_PASSWORD, newPassword);
    
    // close wallet without saving
    closeWallet(wallet);
    
    // old password does not work (password change is auto saved)
    try {
      openWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD));
      fail("Should have thrown");
    } catch (Exception e) {
      assertTrue(e.getMessage().equals("Failed to open wallet") || e.getMessage().equals("invalid password")); // TODO: different errors from rpc and wallet2
    }
    
    // open wallet with new password
    wallet = openWallet(new MoneroWalletConfig().setPath(path).setPassword(newPassword));
    
    // change password with incorrect password
    try {
      wallet.changePassword("badpassword", newPassword);
      fail("Should have thrown");
    } catch (Exception e) {
      assertEquals("Invalid original password.", e.getMessage());
    }
    
    // save and close
    closeWallet(wallet, true);
    
    // open wallet
    wallet = openWallet(new MoneroWalletConfig().setPath(path).setPassword(newPassword));
    
    // close wallet
    closeWallet(wallet);
  }
  
  // Can save and close the wallet in a single call
  @Test
  public void testSaveAndClose() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create random wallet
    String password = "";
    MoneroWallet wallet = createWallet(new MoneroWalletConfig().setPassword(password));
    String path = wallet.getPath();
    
    // set an attribute
    String uuid = UUID.randomUUID().toString();
    wallet.setAttribute("id", uuid);
    
    // close the wallet without saving
    closeWallet(wallet);
    
    // re-open the wallet and ensure attribute was not saved
    wallet = openWallet(path, password);
    assertEquals(null, wallet.getAttribute("id"));
    
    // set the attribute and close with saving
    wallet.setAttribute("id", uuid);
    closeWallet(wallet, true);
    
    // re-open the wallet and ensure attribute was saved
    wallet = openWallet(new MoneroWalletConfig().setPath(path).setPassword(password));
    assertEquals(uuid, wallet.getAttribute("id"));
    closeWallet(wallet);
  }
  
  // ------------------------------- TEST RELAYS ------------------------------
  
  // Validates inputs when sending funds
  @Test
  public void testValidateInputsSendingFunds() {
    
    // try sending with invalid address
    try {
      wallet.createTx(new MoneroTxConfig().setAddress("my invalid address").setAccountIndex(0).setAmount(TestUtils.MAX_FEE));
      fail("Should have thrown");
    } catch (MoneroError err) {
      assertEquals("Invalid destination address", err.getMessage());
    }
  }
  
  // Can sync with txs in the pool sent from/to the same account
  // TODO: this test fails because wallet does not recognize pool tx sent from/to same account
  @Test
  public void testSyncWithPoolSameAccounts() {
    assumeTrue(TEST_NON_RELAYS);
    testSyncWithPoolSubmit(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")))
            .setRelay(false));
  }
  
  // Can sync with txs submitted and discarded from the pool
  @Test
  public void testSyncWithPoolSubmitAndDiscard() {
    assumeTrue(TEST_NON_RELAYS);
    testSyncWithPoolSubmit(new MoneroTxConfig()
            .setAccountIndex(2)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")))
            .setRelay(false));
  }
  
  // Can sync with txs submitted and relayed from the pool
  @Test
  public void testSyncWithPoolSubmitAndRelay() {
    assumeTrue(TEST_RELAYS && !LITE_MODE);
    testSyncWithPoolSubmit(new MoneroTxConfig()
            .setAccountIndex(2)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")))
            .setRelay(true));
  }
  
  private void testSyncWithPoolSubmit(MoneroTxConfig config) {
    
    // wait for txs to confirm and for sufficient unlocked balance
    // TODO monero-project: update from pool does not prevent creating double spend tx
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    assertNull(config.getSubaddressIndices());
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, config.getAccountIndex(), null, config.getAmount());
    
    // record wallet balances before submitting tx to pool
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    
    // create tx but do not relay
    MoneroTxConfig configNoRelay = config.copy().setRelay(false);
    MoneroTxConfig configNoRelayCopy = configNoRelay.copy();
    MoneroTxWallet tx = wallet.createTx(configNoRelay);
    
    // create tx using same config which is double spend
    MoneroTxWallet txDoubleSpend = wallet.createTx(configNoRelay);
    
    // test that config is unchanged
    assertTrue(configNoRelayCopy != configNoRelay);
    assertEquals(configNoRelayCopy, configNoRelay);
    
    // submit tx directly to the pool but do not relay
    MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
    if (!result.isGood()) throw new RuntimeException("Transaction could not be submitted to the pool: " + JsonUtils.serialize(result));
    assertTrue(result.isGood());
    
    // sync wallet which checks pool
    wallet.sync();
    
    // test result and flush on finally
    try {
      
      // wallet should be aware of tx
      try {
        MoneroTxWallet fetched = wallet.getTx(tx.getHash());
        assertNotNull(fetched);
      } catch (MoneroError e) {
        fail("Wallet should be aware of its tx in pool after syncing");
      }
      
      // submit double spend tx
      MoneroSubmitTxResult resultDoubleSpend = daemon.submitTxHex(txDoubleSpend.getFullHex(), true);
      if (resultDoubleSpend.isGood()) {
        daemon.flushTxPool(txDoubleSpend.getHash());
        throw new RuntimeException("Tx submit result should have been double spend");
      }
      
      // relay if configured
      if (Boolean.TRUE.equals(config.getRelay())) daemon.relayTxByHash(tx.getHash());

      // sync wallet which updates from pool
      wallet.sync();
      
      // TODO monero-project: this code fails which indicates issues // TODO (monero-project): sync txs from pool
      boolean runFailingCoreCode = false;
      if (runFailingCoreCode) {
        
        // wallet balances should change
        assertNotEquals(balanceBefore, wallet.getBalance(), "Wallet balance should revert to original after flushing tx from pool without relaying");
        assertNotEquals(unlockedBalanceBefore, wallet.getUnlockedBalance(), "Wallet unlocked balance should revert to original after flushing tx from pool without relaying");  // TODO: test exact amounts, maybe in ux test
        
        // create tx using same config which is no longer double spend
        MoneroTxWallet tx2 = wallet.createTx(configNoRelay);
        MoneroSubmitTxResult result2 = daemon.submitTxHex(tx2.getFullHex(), true);
        
        // test result and flush on finally
        try {
          if (result2.isDoubleSpend()) throw new RuntimeException("Wallet created double spend transaction after syncing with the pool: " + JsonUtils.serialize(result2));
          assertTrue(result.isGood());
          wallet.sync();
          wallet.getTx(tx2.getHash()); // wallet is aware of tx2
        } finally  {
          daemon.flushTxPool(tx2.getHash());
        }
      }
    } finally  {
      if (!Boolean.TRUE.equals(config.getRelay())) {
        
        // flush the tx from the pool
        daemon.flushTxPool(tx.getHash());
        
        // sync wallet which checks pool
        wallet.sync();
        
        // wallet should no longer be aware of tx
        try {
          wallet.getTx(tx.getHash());
          fail("Wallet should no longer be aware of tx which was not relayed and was manually removed from pool");
        } catch (MoneroError e) {
          // exception expected
        }
        
        // wallet balances should be restored
        assertEquals(balanceBefore, wallet.getBalance(), "Wallet balance should be same as original since tx was flushed and not relayed");
        assertEquals(unlockedBalanceBefore, wallet.getUnlockedBalance(), "Wallet unlocked balance should be same as original since tx was flushed and not relayed");
      } else {
        TestUtils.WALLET_TX_TRACKER.reset(); // all wallets need to wait for tx to confirm in order to reliably sync
      }
    }
  }
  
  // Can sync with txs relayed to the pool
  @Test
  public void testSyncWithPoolRelay() {
  assumeTrue(TEST_RELAYS && !LITE_MODE);
    
    // wait one time for wallet txs in the pool to clear
    // TODO monero-project: update from pool does not prevent creating double spend tx
   TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // record wallet balances before submitting tx to pool
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore = wallet.getUnlockedBalance();
    
    // create config
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(2)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")));
    
    // create tx to relay
    MoneroTxWallet tx = wallet.createTx(config);
    
    // create another tx using same config which would be double spend
    MoneroTxWallet txDoubleSpend = wallet.createTx(config);
    
    // relay first tx directly to daemon
    MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex());
    if (!result.isGood()) throw new RuntimeException("Transaction could not be submitted to the pool" + JsonUtils.serialize(result));
    assertTrue(result.isGood());
    
    // sync wallet which updates from pool
    wallet.sync();
    
    // collect issues to report at end of test
    List<String> issues = new ArrayList<String>();
    
    try {
      
      // wallet should be aware of tx
      try {
        MoneroTxWallet fetched = wallet.getTx(tx.getHash());
        assertNotNull(fetched);
      } catch (MoneroError e) {
        throw new RuntimeException("Wallet should be aware of its tx in pool after syncing");
      }
      
      // test wallet balances
      if (unlockedBalanceBefore.compareTo(wallet.getUnlockedBalance()) != 1) issues.add("WARNING: unlocked balance should have decreased after relaying tx directly to the daemon");
      BigInteger expectedBalance = balanceBefore.subtract(tx.getOutgoingAmount()).subtract(tx.getFee());
      if (!expectedBalance.equals(wallet.getBalance())) issues.add("WARNING: expected balance after relaying tx directly to the daemon to be " + expectedBalance + " but was " + wallet.getBalance());
      
      // submit double spend tx
      MoneroSubmitTxResult resultDoubleSpend = daemon.submitTxHex(txDoubleSpend.getFullHex(), true);
      if (resultDoubleSpend.isGood()) {
        daemon.flushTxPool(txDoubleSpend.getHash());
        throw new RuntimeException("Tx submit result should have been double spend");
      }

      // sync wallet which updates from pool
      wallet.sync();
      
      // create tx using same config which is no longer double spend
      MoneroTxWallet tx2 = null;
      try {
        tx2 = wallet.createTx(config.copy().setRelay(true));
      } catch (Exception e) {
        issues.add("WARNING: creating and sending tx through wallet should succeed after syncing wallet with pool but creates a double spend"); // TODO monero-project: this fails meaning wallet did not recognize tx relayed directly to pool
      }
      
      // submit the transaction to the pool and test
      if (tx2 != null) {
        MoneroSubmitTxResult result2 = daemon.submitTxHex(tx2.getFullHex(), true);
        if (result2.isDoubleSpend()) issues.add("WARNING: creating and submitting tx to daemon should succeed after syncing wallet with pool but was a double spend: " + JsonUtils.serialize(result2));
        else assertTrue(result.isGood());
        wallet.sync();
        wallet.getTx(tx2.getHash()); // wallet is aware of tx2
        daemon.flushTxPool(tx2.getHash());
      }
      
      // should be no issues
      assertEquals(0, issues.size(), "testSyncWithPoolRelay() issues: " + issues);
    } finally {
      TestUtils.WALLET_TX_TRACKER.reset(); // all wallets need to wait for tx to confirm to reliably sync tx
    }
  }
  
  // Can send to self
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
    
    // test error sending funds to self with integrated subaddress
    // TODO (monero-project): sending funds to self with integrated subaddress throws error: https://github.com/monero-project/monero/issues/8380
    try {
      wallet.createTx(new MoneroTxConfig()
              .setAccountIndex(0)
              .setAddress(MoneroUtils.getIntegratedAddress(TestUtils.NETWORK_TYPE, wallet.getSubaddress(0, 1).getAddress(), null).getIntegratedAddress())
              .setAmount(amount)
              .setRelay(true));
      throw new RuntimeException("Should have failed sending to self with integrated subaddress");
    } catch (MoneroError err) {
      if (!err.getMessage().contains("Total received by")) throw err;
    }
    
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
  @Test
  public void testSendToExternal() {
    assumeTrue(TEST_RELAYS);
    MoneroWallet recipient = null;
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
      
      // test recipient balance after
      recipient.sync();
      assertFalse(wallet.getTxs(new MoneroTxQuery().setIsConfirmed(false)).isEmpty());
      assertEquals(amount, recipient.getBalance());
    } finally {
      if (recipient != null && !recipient.isClosed()) closeWallet(recipient);
    }
  }
  
  // Can send from multiple subaddresses in a single transaction
  @Test
  public void testSendFromSubaddresses() {
    assumeTrue(TEST_RELAYS);
    testSendFromMultiple(null);
  }
  
  // Can send from multiple subaddresses in split transactions
  @Test
  public void testSendFromSubaddressesSplit() {
    assumeTrue(TEST_RELAYS);
    testSendFromMultiple(new MoneroTxConfig().setCanSplit(true));
  }
  
  private void testSendFromMultiple(MoneroTxConfig config) {
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    if (config == null) config = new MoneroTxConfig();
    
    int NUM_SUBADDRESSES = 2; // number of subaddresses to send from
    
    // get first account with (NUM_SUBADDRESSES + 1) subaddresses with unlocked balances
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertTrue(accounts.size() >= 2, "This test requires at least 2 accounts; run send-to-multiple tests");
    MoneroAccount srcAccount = null;
    List<MoneroSubaddress> unlockedSubaddresses = new ArrayList<MoneroSubaddress>();
    boolean hasBalance = false;
    for (MoneroAccount account : accounts) {
      unlockedSubaddresses.clear();
      int numSubaddressBalances = 0;
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(TestUtils.MAX_FEE) > 0) numSubaddressBalances++;
        if (subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) unlockedSubaddresses.add(subaddress);
      }
      if (numSubaddressBalances >= NUM_SUBADDRESSES + 1) hasBalance = true;
      if (unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1) {
        srcAccount = account;
        break;
      }
    }
    assertTrue(hasBalance, "Wallet does not have account with " + (NUM_SUBADDRESSES + 1) + " subaddresses with balances; run send-to-multiple tests");
    assertTrue(unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1, "Wallet is waiting on unlocked funds");
    
    // determine the indices of the first two subaddresses with unlocked balances
    List<Integer> fromSubaddressIndices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_SUBADDRESSES; i++) {
      fromSubaddressIndices.add(unlockedSubaddresses.get(i).getIndex());
    }
    
    // determine the amount to send
    BigInteger sendAmount = BigInteger.valueOf(0);
    for (int fromSubaddressIdx : fromSubaddressIndices) {
      sendAmount = sendAmount.add(srcAccount.getSubaddresses().get(fromSubaddressIdx).getUnlockedBalance());
    }
    sendAmount = sendAmount.divide(BigInteger.valueOf(SEND_DIVISOR));
    
    BigInteger fromBalance = BigInteger.valueOf(0);
    BigInteger fromUnlockedBalance = BigInteger.valueOf(0);
    for (int subaddressIdx : fromSubaddressIndices) {
      MoneroSubaddress subaddress = wallet.getSubaddress(srcAccount.getIndex(), subaddressIdx);
      fromBalance = fromBalance.add(subaddress.getBalance());
      fromUnlockedBalance = fromUnlockedBalance.add(subaddress.getUnlockedBalance());
    }
    
    // send from the first subaddresses with unlocked balances
    String address = wallet.getPrimaryAddress();
    config.setDestinations(new MoneroDestination(address, sendAmount));
    config.setAccountIndex(srcAccount.getIndex());
    config.setSubaddressIndices(fromSubaddressIndices);
    config.setRelay(true);
    MoneroTxConfig configCopy = config.copy();
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (!Boolean.FALSE.equals(config.getCanSplit())) txs.addAll(wallet.createTxs(config));
    else txs.add(wallet.createTx(config));
    if (Boolean.FALSE.equals(config.getCanSplit())) assertEquals(1, txs.size());  // must have exactly one tx if no split
    
    // test that config is unchanged
    assertTrue(configCopy != config);
    assertEquals(configCopy, config);
    
    // test that balances of intended subaddresses decreased
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    boolean srcUnlockedBalancedDecreased = false;
    for (int i = 0; i < accounts.size(); i++) {
      assertEquals(accounts.get(i).getSubaddresses().size(), accountsAfter.get(i).getSubaddresses().size());
      for (int j = 0; j < accounts.get(i).getSubaddresses().size(); j++) {
        MoneroSubaddress subaddressBefore = accounts.get(i).getSubaddresses().get(j);
        MoneroSubaddress subaddressAfter = accountsAfter.get(i).getSubaddresses().get(j);
        if (i == srcAccount.getIndex() && fromSubaddressIndices.contains(j)) {
          if (subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) < 0) srcUnlockedBalancedDecreased = true;
        } else {
          assertTrue(subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) == 0, "Subaddress [" + i + "," + j + "] unlocked balance should not have changed");
        }
      }
    }
    assertTrue(srcUnlockedBalancedDecreased, "Subaddress unlocked balances should have decreased");
    
    // test context
    TxContext ctx = new TxContext();
    ctx.config = config;
    ctx.wallet = wallet;
    ctx.isSendResponse = true;
    
    // test each transaction
    assertTrue(txs.size() > 0);
    BigInteger outgoingSum = BigInteger.valueOf(0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        BigInteger destinationSum = BigInteger.valueOf(0);
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          testDestination(destination);
          assertEquals(address, destination.getAddress());
          destinationSum = destinationSum.add(destination.getAmount());
        }
        assertTrue(tx.getOutgoingAmount().equals(destinationSum));  // assert that transfers sum up to tx amount
      }
    }
    
    // assert that tx amounts sum up the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(outgoingSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      throw new RuntimeException("Tx amounts are too different: " + sendAmount + " - " + outgoingSum + " = " + sendAmount.subtract(outgoingSum));
    }
  }
  
  // Can send to an address in a single transaction.
  @Test
  public void testSend() {
    assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(false));
  }
  
  // Can send to an address in a single transaction with a payment id
  // NOTE: this test will be invalid when payment hashes are fully removed
  @Test
  public void testSendWithPaymentId() {
    assumeTrue(TEST_RELAYS);
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress();
    String paymentId = integratedAddress.getPaymentId();
    try {
      testSendToSingle(new MoneroTxConfig().setCanSplit(false).setPaymentId(paymentId + paymentId + paymentId + paymentId));  // 64 character payment id
      fail("Should have thrown");
    } catch (MoneroError e) {
      assertEquals("Standalone payment IDs are obsolete. Use subaddresses or integrated addresses instead", e.getMessage());
    }
  }
  
  // Can send to an address with split transactions
  @Test
  public void testSendSplit() {
    assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(true).setRelay(true));
  }
  
  // Can create then relay a transaction to send to a single address
  @Test
  public void testCreateThenRelay() {
    assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(false));
  }
  
  // Can create then relay split transactions to send to a single address
  @Test
  public void testCreateThenRelaySplit() {
    assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(true));
  }
  
  private void testSendToSingle(MoneroTxConfig config) {
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    if (config == null) config = new MoneroTxConfig();
    
    // find a non-primary subaddress to send from
    boolean sufficientBalance = false;
    MoneroAccount fromAccount = null;
    MoneroSubaddress fromSubaddress = null;
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    for (MoneroAccount account : accounts) {
      List<MoneroSubaddress> subaddresses = account.getSubaddresses();
      for (int i = 1; i < subaddresses.size(); i++) {
        if (subaddresses.get(i).getBalance().compareTo(TestUtils.MAX_FEE) > 0) sufficientBalance = true;
        if (subaddresses.get(i).getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) {
          fromAccount = account;
          fromSubaddress = subaddresses.get(i);
          break;
        }
      }
      if (fromAccount != null) break;
    }
    assertTrue(sufficientBalance, "No non-primary subaddress found with sufficient balance");
    assertTrue(fromSubaddress != null, "Wallet is waiting on unlocked funds");
    
    // get balance before send
    BigInteger balanceBefore = fromSubaddress.getBalance();
    BigInteger unlockedBalanceBefore  = fromSubaddress.getUnlockedBalance();
    
    // init tx config
    BigInteger sendAmount = unlockedBalanceBefore.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    config.setDestinations(new MoneroDestination(address, sendAmount));
    config.setAccountIndex(fromAccount.getIndex());
    config.setSubaddressIndices(fromSubaddress.getIndex());
    MoneroTxConfig configCopy = config.copy();
    
    // test sending to invalid address
    try {
      config.setAddress("my invalid address");
      if (!Boolean.FALSE.equals(config.getCanSplit())) wallet.createTxs(config);
      else wallet.createTx(config);
      fail("Should have thrown error creating tx with invalid address");
    } catch (MoneroError e) {
      assertEquals("Invalid destination address", e.getMessage());
      config.setAddress(address);
    }
    
    // send to self
    List<MoneroTxWallet> txs = wallet.createTxs(config);
    if (Boolean.FALSE.equals(config.getCanSplit())) assertEquals(1, txs.size());  // must have exactly one tx if no split
    
    // test that config is unchanged
    assertTrue(configCopy != config);
    assertEquals(configCopy, config);
    
    // test common tx set among txs
    testCommonTxSets(txs, false, false, false);
    
    // handle non-relayed transaction
    if (!Boolean.TRUE.equals(config.getRelay())) {
      
      // build test context
      TxContext ctx = new TxContext();
      ctx.wallet = wallet;
      ctx.config = config;
      ctx.isSendResponse = true;
      
      // test transactions
      for (MoneroTxWallet tx : txs) {
        testTxWallet(tx, ctx);
      }
      
      // txs are not in the pool
      for (MoneroTxWallet txCreated : txs) {
        for (MoneroTx txPool : daemon.getTxPool()) {
          assertFalse(txPool.getHash().equals(txCreated.getHash()), "Created tx should not be in the pool");
        }
      }
      
      // relay txs
      List<String> txHashes = null;
      if (!Boolean.TRUE.equals(config.getCanSplit())) txHashes = Arrays.asList(wallet.relayTx(txs.get(0))); // test relayTx() with single transaction
      else {
        List<String> txMetadatas = new ArrayList<String>();
        for (MoneroTxWallet tx : txs) txMetadatas.add(tx.getMetadata());
        txHashes = wallet.relayTxs(txMetadatas); // test relayTxs() with potentially multiple transactions
      }
      for (String txHash : txHashes) assertEquals(64, txHash.length());
      
      // fetch txs for testing
      txs = wallet.getTxs(new MoneroTxQuery().setHashes(txHashes));
    }
    
    // test that balance and unlocked balance decreased
    // TODO: test that other balances did not decrease
    MoneroSubaddress subaddress = wallet.getSubaddress(fromAccount.getIndex(), fromSubaddress.getIndex());
    assertTrue(subaddress.getBalance().compareTo(balanceBefore) < 0);
    assertTrue(subaddress.getUnlockedBalance().compareTo(unlockedBalanceBefore) < 0);
    
    // query locked txs
    List<MoneroTxWallet> lockedTxs = getAndTestTxs(wallet, new MoneroTxQuery().setIsLocked(true), null, true);
    for (MoneroTxWallet lockedTx : lockedTxs) assertEquals(true, lockedTx.isLocked());
    
    // build test context
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = config;
    ctx.isSendResponse = Boolean.TRUE.equals(config.getRelay());
    
    // test transactions
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      assertEquals(fromAccount.getIndex(), tx.getOutgoingTransfer().getAccountIndex());
      assertEquals(1, tx.getOutgoingTransfer().getSubaddressIndices().size());
      assertEquals(fromSubaddress.getIndex(), tx.getOutgoingTransfer().getSubaddressIndices().get(0));
      assertTrue(sendAmount.equals(tx.getOutgoingAmount()));
      if (config.getPaymentId() != null) assertEquals(config.getPaymentId(), tx.getPaymentId());
      
      // test outgoing destinations
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        assertEquals(1, tx.getOutgoingTransfer().getDestinations().size());
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          testDestination(destination);
          assertEquals(destination.getAddress(), address);
          assertTrue(sendAmount.equals(destination.getAmount()));
        }
      }
      
      // tx is among locked txs
      boolean found = false;
      for (MoneroTxWallet locked : lockedTxs) {
        if (locked.getHash().equals(tx.getHash())) {
          found = true;
          break;
        }
      }
      assertTrue(found, "Created txs should be among locked txs");
    }
    
    // if tx was relayed in separate step, all wallets will need to wait for tx to confirm in order to reliably sync
    if (!Boolean.TRUE.equals(config.getRelay())) {
      TestUtils.WALLET_TX_TRACKER.reset(); // TODO: resetExcept(wallet), or does this test wallet also need to be waited on?
    }
  }
  
  // Can send to multiple addresses in a single transaction
  @Test
  public void testSendToMultiple() {
    assumeTrue(TEST_RELAYS);
    testSendToMultiple(5, 3, false);
  }
  
  // Can send to multiple addresses in split transactions
  @Test
  public void testSendToMultipleSplit() {
    assumeTrue(TEST_RELAYS);
    testSendToMultiple(3, 15, true);
  }
  
  // Can send dust to multiple addresses in split transactions
  @Test
  public void testSendDustToMultipleSplit() {
    assumeTrue(TEST_RELAYS);
    BigInteger dustAmt = daemon.getFeeEstimate().getFee().divide(BigInteger.valueOf(2));
    testSendToMultiple(5, 3, true, dustAmt);
  }

  // Can subtract fees from destinations
  @Test
  public void testSubtractFeeFrom() {
    assumeTrue(TEST_RELAYS);
    testSendToMultiple(5, 3, false, null, true);
  }

  // Cannot subtract fees from destinations in split transactions
  @Test
  public void testSubtractFeeFromSplit() {
    assumeTrue(TEST_RELAYS);
    testSendToMultiple(3, 15, true, null, true);
  }
  
  /**
   * Sends funds from the first unlocked account to multiple accounts and subaddresses.
   * 
   * @param numAccounts is the number of accounts to receive funds
   * @param numSubaddressesPerAccount is the number of subaddresses per account to receive funds
   * @param canSplit specifies if the operation can be split into multiple transactions
   * @param sendAmountPerSubaddress is the amount to send to each subaddress (optional, computed if not given)
   * @param subtractFeeFromDestinations specifies to subtract the fee from destination addresses
   */
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit) { testSendToMultiple(numAccounts, numSubaddressesPerAccount, canSplit, null); }
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit, BigInteger sendAmountPerSubaddress) { testSendToMultiple(numAccounts, numSubaddressesPerAccount, canSplit, sendAmountPerSubaddress, false); }
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit, BigInteger sendAmountPerSubaddress, boolean subtractFeeFromDestinations) {
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // compute the minimum account unlocked balance needed in order to fulfill the config
    BigInteger minAccountAmount = null;
    int totalSubaddresses = numAccounts * numSubaddressesPerAccount;
    if (sendAmountPerSubaddress != null) minAccountAmount = BigInteger.valueOf(totalSubaddresses).multiply(sendAmountPerSubaddress.add(TestUtils.MAX_FEE)); // min account amount must cover the total amount being sent plus the tx fee = numAddresses * (amtPerSubaddress + fee)
    else minAccountAmount = TestUtils.MAX_FEE.multiply(BigInteger.valueOf(totalSubaddresses)).multiply(BigInteger.valueOf(SEND_DIVISOR)).add(TestUtils.MAX_FEE); // account balance must be more than fee * numAddresses * divisor + fee so each destination amount is at least a fee's worth (so dust is not sent)
    
    // send funds from first account with sufficient unlocked funds
    MoneroAccount srcAccount = null;
    boolean hasBalance = false;
    for (MoneroAccount account : wallet.getAccounts()) {
      if (account.getBalance().compareTo(minAccountAmount) > 0) hasBalance = true;
      if (account.getUnlockedBalance().compareTo(minAccountAmount) > 0) {
        srcAccount = account;
        break;
      }
    }
    assertTrue(hasBalance, "Wallet does not have enough balance; load '" + TestUtils.WALLET_NAME + "' with XMR in order to test sending");
    assertNotNull(srcAccount, "Wallet is waiting on unlocked funds");
    BigInteger balance = srcAccount.getBalance();
    BigInteger unlockedBalance = srcAccount.getUnlockedBalance();
    
    // get amount to send total and per subaddress
    BigInteger sendAmount = null;
    if (sendAmountPerSubaddress == null) {
      sendAmount = TestUtils.MAX_FEE.multiply(new BigInteger("5")).multiply(BigInteger.valueOf(totalSubaddresses));
      sendAmountPerSubaddress = sendAmount.divide(BigInteger.valueOf(totalSubaddresses));
    } else {
      sendAmount = sendAmountPerSubaddress.multiply(BigInteger.valueOf(totalSubaddresses));
    }
    
    // create minimum number of accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    for (int i = 0; i < numAccounts - accounts.size(); i++) {
      wallet.createAccount();
    }
    
    // create minimum number of subaddresses per account and collect destination addresses
    List<String> destinationAddresses = new ArrayList<String>();
    for (int i = 0; i < numAccounts; i++) {
      List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(i);
      for (int j = 0; j < numSubaddressesPerAccount - subaddresses.size(); j++) wallet.createSubaddress(i);
      subaddresses = wallet.getSubaddresses(i);
      assertTrue(subaddresses.size() >= numSubaddressesPerAccount);
      for (int j = 0; j < numSubaddressesPerAccount; j++) destinationAddresses.add(subaddresses.get(j).getAddress());
    }
    
    // build tx config
    MoneroTxConfig config = new MoneroTxConfig();
    config.setAccountIndex(srcAccount.getIndex());
    config.setSubaddressIndices((Integer) null); // test assigning null
    config.setDestinations(new ArrayList<MoneroDestination>());
    config.setRelay(true);
    config.setCanSplit(canSplit);
    config.setPriority(MoneroTxPriority.NORMAL);
    List<Integer> subtractFeeFrom = new ArrayList<Integer>();
    for (int i = 0; i < destinationAddresses.size(); i++) {
      config.getDestinations().add(new MoneroDestination(destinationAddresses.get(i), sendAmountPerSubaddress));
      subtractFeeFrom.add(i);
    }
    if (subtractFeeFromDestinations) config.setSubtractFeeFrom(subtractFeeFrom);

    MoneroTxConfig configCopy = config.copy();
    
    // send tx(s) with config
    List<MoneroTxWallet> txs  = null;
    try {
      txs = wallet.createTxs(config);
    } catch (MoneroError e) {

      // test error applying subtractFromFee with split txs
      if (subtractFeeFromDestinations && txs == null) {
        if (!e.getMessage().equals("subtractfeefrom transfers cannot be split over multiple transactions yet")) throw e;
        return;
      }

      throw e;
    }
    if (!canSplit) assertEquals(1, txs.size());
    
    // test that config is unchanged
    assertTrue(configCopy != config);
    assertEquals(configCopy, config);
    
    // test that wallet balance decreased
    MoneroAccount account = wallet.getAccount(srcAccount.getIndex());
    assertTrue(account.getBalance().compareTo(balance) < 0);
    assertTrue(account.getUnlockedBalance().compareTo(unlockedBalance) < 0);
    
    // build test context
    config.setCanSplit(canSplit);
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = config;
    ctx.isSendResponse = true;
    
    // test each transaction
    assertTrue(txs.size() > 0);
    BigInteger feeSum = BigInteger.valueOf(0);
    BigInteger outgoingSum = BigInteger.valueOf(0);
    testTxsWallet(txs, ctx);
    for (MoneroTxWallet tx : txs) {
      feeSum = feeSum.add(tx.getFee());
      outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        BigInteger destinationSum = BigInteger.valueOf(0);
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          testDestination(destination);
          assertTrue(destinationAddresses.contains(destination.getAddress()));
          destinationSum = destinationSum.add(destination.getAmount());
        }
        assertTrue(tx.getOutgoingAmount().equals(destinationSum));  // assert that transfers sum up to tx amount
      }
    }
    
    // assert that outgoing amounts sum up to the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(subtractFeeFromDestinations ? feeSum : BigInteger.valueOf(0)).subtract(outgoingSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      fail("Actual send amount is too different from requested send amount: " + sendAmount + " - " + (subtractFeeFromDestinations ? feeSum : BigInteger.valueOf(0)) + " - " + outgoingSum + " = " + sendAmount.subtract(subtractFeeFromDestinations ? feeSum : BigInteger.valueOf(0)).subtract(outgoingSum));
    }
  }
  
  // Can sweep individual outputs identified by their key images
  @Test
  public void testSweepOutputs() {
    assumeTrue(TEST_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // test config
    int numOutputs = 3;
    
    // get outputs to sweep (not spent, unlocked, and amount >= fee)
    List<MoneroOutputWallet> spendableUnlockedOutputs = wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false).setTxQuery(new MoneroTxQuery().setIsLocked(false)));
    List<MoneroOutputWallet> outputsToSweep = new ArrayList<MoneroOutputWallet>();
    for (int i = 0; i < spendableUnlockedOutputs.size() && outputsToSweep.size() < numOutputs; i++) {
      if (spendableUnlockedOutputs.get(i).getAmount().compareTo(TestUtils.MAX_FEE) > 0) outputsToSweep.add(spendableUnlockedOutputs.get(i));  // output cannot be swept if amount does not cover fee
    }
    assertTrue(outputsToSweep.size() >= numOutputs, "Wallet does not have enough sweepable outputs; run send tests");
    
    // sweep each output by key image
    for (MoneroOutputWallet output : outputsToSweep) {
      testOutputWallet(output);
      assertFalse(output.isSpent());
      assertFalse(output.isLocked());
      if (output.getAmount().compareTo(TestUtils.MAX_FEE) <= 0) continue;
      
      // sweep output to address
      String address = wallet.getAddress(output.getAccountIndex(), output.getSubaddressIndex());
      MoneroTxConfig config = new MoneroTxConfig().setAddress(address).setKeyImage(output.getKeyImage().getHex()).setRelay(true);
      MoneroTxWallet tx = wallet.sweepOutput(config);
      
      // test resulting tx
      TxContext ctx = new TxContext();
      ctx.wallet = wallet;
      ctx.config = config;
      ctx.config.setCanSplit(false);
      ctx.isSendResponse = true;
      ctx.isSweepResponse = true;
      ctx.isSweepOutputResponse = true;
      testTxWallet(tx, ctx);
    }
    
    // get outputs after sweeping
    List<MoneroOutputWallet> afterOutputs = wallet.getOutputs();
    
    // swept outputs are now spent
    for (MoneroOutputWallet afterOutput : afterOutputs) {
      for (MoneroOutputWallet output : outputsToSweep) {
        if (output.getKeyImage().getHex().equals(afterOutput.getKeyImage().getHex())) {
          assertTrue(afterOutput.isSpent(), "Output should be spent");
        }
      }
    }
  }
  
  // Can sweep dust without relaying
  @Test
  public void testSweepDustNoRelay() {
    assumeTrue(TEST_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // sweep dust which returns empty list if no dust to sweep (dust does not exist after rct)
    List<MoneroTxWallet> txs = wallet.sweepDust(false);
    if (txs.size() == 0) return;
    
    // test txs
    TxContext ctx = new TxContext();
    ctx.isSendResponse = true;
    ctx.config = new MoneroTxConfig().setRelay(false);
    ctx.isSweepResponse = true;
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
    
    // relay txs
    List<String> metadatas = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) metadatas.add(tx.getMetadata());
    List<String> txHashes = wallet.relayTxs(metadatas);
    assertEquals(txHashes.size(), txs.size());
    for (String txHash : txHashes) assertEquals(64, txHash.length());
    
    // fetch and test txs
    txs = wallet.getTxs(new MoneroTxQuery().setHashes(txHashes));
    ctx.config.setRelay(true);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
  }
  
  // Can sweep dust
  @Test
  public void testSweepDust() {
    assumeTrue(TEST_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // sweep dust which returns empty list if no dust to sweep (dust does not exist after rct)
    List<MoneroTxWallet> txs = wallet.sweepDust(true);
    
    // test any txs
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = null;
    ctx.isSendResponse = true;
    ctx.isSweepResponse = true;
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
  }
  
  // Supports multisig wallets
  @Test
  public void testMultisig() {
    testMultisig(2, 2, false); // n/n
    testMultisig(2, 3, false); // (n-1)/n
    testMultisig(2, 4, TEST_RELAYS && !LITE_MODE); // m/n
  }
  
  private void testMultisig(int m, int n, boolean testTx) {
    
    // create n participants
    List<MoneroWallet> participants = new ArrayList<MoneroWallet>();
    for (int i = 0; i < n; i++) participants.add(createWallet(new MoneroWalletConfig()));

    // test multisig
    try {
      testMultisig(participants, m, n, testTx);
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
      
      // save and close participants
      for (MoneroWallet participant : participants) closeWallet(participant, true);
    }
  }
  
  protected void testMultisig(List<MoneroWallet> participants, int M, int N, boolean testTx) {
    System.out.println("testMultisigParticipants(" + M + ", " + N + ")");
    assertEquals(N, participants.size());

    // init and fund multisig wallets
    int accountIdx = 0;
    initMultisigWallets(participants, M, N, accountIdx, 3);
    MoneroWallet participant = participants.get(0);
    
    // create txs to send funds from a subaddress in the multisig wallet
    System.out.println("Sending");
    List<MoneroTxWallet> txs = participant.createTxs(new MoneroTxConfig()
        .setAddress(wallet.getAddress(accountIdx, 0))
        .setAmount(TestUtils.MAX_FEE)
        .setAccountIndex(accountIdx).setSubaddressIndex(0));
    assertFalse(txs.isEmpty());
    MoneroTxSet txSet = txs.get(0).getTxSet();
    assertNotNull(txSet.getMultisigTxHex());
    assertNull(txSet.getSignedTxHex());
    assertNull(txSet.getUnsignedTxHex());
    
    // parse multisig tx hex and test
    testDescribedTxSet(participant.describeMultisigTxSet(txSet.getMultisigTxHex()));
    
    // sign the tx with participants 1 through m - 1 to meet threshold
    String multisigTxHex = txSet.getMultisigTxHex();
    System.out.println("Signing");
    for (int i = 1; i < M; i++) {
      MoneroMultisigSignResult result = participants.get(i).signMultisigTxHex(multisigTxHex);
      multisigTxHex = result.getSignedMultisigTxHex();
    }
    
    //System.out.println("Submitting signed multisig tx hex: " + multisigTxHex);
    
    // submit the signed multisig tx hex to the network
    System.out.println("Submitting");
    List<String> txHashes = participant.submitMultisigTxHex(multisigTxHex);
    assertTrue(txHashes.size() > 0);
    
    // synchronize the multisig participants since spending outputs
    System.out.println("Synchronizing participants");
    synchronizeMultisigParticipants(participants);
    
    // fetch the wallet's multisig txs
    List<MoneroTxWallet> multisigTxs = participant.getTxs(new MoneroTxQuery().setHashes(txHashes));
    assertEquals(multisigTxs.size(), txHashes.size());
    
    // sweep an output from subaddress [accountIdx,1]
    String returnAddress = wallet.getPrimaryAddress();
    List<MoneroOutputWallet> outputs = participant.getOutputs(new MoneroOutputQuery().setAccountIndex(accountIdx).setSubaddressIndex(1));
    assertFalse(outputs.isEmpty());
    assertFalse(outputs.get(0).isSpent());
    txSet = participant.sweepOutput(new MoneroTxConfig().setAddress(returnAddress).setKeyImage(outputs.get(0).getKeyImage().getHex()).setRelay(true)).getTxSet();
    assertNotNull(txSet.getMultisigTxHex());
    assertNull(txSet.getSignedTxHex());
    assertNull(txSet.getUnsignedTxHex());
    assertFalse(txSet.getTxs().isEmpty());
    
    // parse multisig tx hex and test
    testDescribedTxSet(participant.describeMultisigTxSet(txSet.getMultisigTxHex()));
    
    // sign the tx with participants 1 through m - 1 to meet threshold
    multisigTxHex = txSet.getMultisigTxHex();
    System.out.println("Signing sweep output");
    for (int i = 1; i < M; i++) {
      MoneroMultisigSignResult result = participants.get(i).signMultisigTxHex(multisigTxHex);
      multisigTxHex = result.getSignedMultisigTxHex();
    }
    
    // submit the signed multisig tx hex to the network
    System.out.println("Submitting sweep output");
    txHashes = participant.submitMultisigTxHex(multisigTxHex);
    
    // synchronize the multisig participants since spending outputs
    System.out.println("Synchronizing participants");
    synchronizeMultisigParticipants(participants);
    
    // fetch the wallet's multisig txs
    multisigTxs = participant.getTxs(new MoneroTxQuery().setHashes(txHashes));
    assertEquals(multisigTxs.size(), txHashes.size());
    
    // sweep remaining balance
    System.out.println("Sweeping");
    txs = participant.sweepUnlocked(new MoneroTxConfig().setAddress(returnAddress).setAccountIndex(accountIdx).setRelay(true)); // TODO: test multisig with sweepEachSubaddress which will generate multiple tx sets without synchronizing participants
    assertFalse(txs.isEmpty(), "No txs created on sweepUnlocked");
    txSet = txs.get(0).getTxSet();
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getTxSet() == txSet);
      assertTrue(tx.getTxSet().getTxs().contains(tx));
    }
    assertNotNull(txSet.getMultisigTxHex());
    assertNull(txSet.getSignedTxHex());
    assertNull(txSet.getUnsignedTxHex());
    
    // parse multisig tx hex and test
    testDescribedTxSet(participant.describeTxSet(txSet));
    
    // sign the tx with participants 1 through m - 1 to meet threshold
    multisigTxHex = txSet.getMultisigTxHex();
    System.out.println("Signing sweep");
    for (int i = 1; i < M; i++) {
      MoneroMultisigSignResult result = participants.get(i).signMultisigTxHex(multisigTxHex);
      multisigTxHex = result.getSignedMultisigTxHex();
    }
    
    // submit the signed multisig tx hex to the network
    System.out.println("Submitting sweep");
    txHashes = participant.submitMultisigTxHex(multisigTxHex);
    
    // synchronize the multisig participants since spending outputs
    System.out.println("Synchronizing participants");
    synchronizeMultisigParticipants(participants);
    
    // fetch the wallet's multisig txs
    multisigTxs = participant.getTxs(new MoneroTxQuery().setHashes(txHashes));
    assertEquals(multisigTxs.size(), txHashes.size());
  }

  private void initMultisigWallets(List<MoneroWallet> participants, int M, int N, int accountIdx, int numAddressesToFund) {
    System.out.println("initMultisigWallets(" + M + ", " + N + ")");
    assertEquals(N, participants.size());
    
    // prepare multisig hexes
    List<String> preparedMultisigHexes = new ArrayList<String>();
    for (int i = 0; i < N; i++) {
      MoneroWallet participant = participants.get(i);
      preparedMultisigHexes.add(participant.prepareMultisig());
    }

    // make wallets multisig
    List<String> madeMultisigHexes = new ArrayList<String>();
    for (int i = 0; i < participants.size(); i++) {
      MoneroWallet participant = participants.get(i);
      
      // test bad input
      try {
        participant.makeMultisig(Arrays.asList("asd", "dsa"), M, TestUtils.WALLET_PASSWORD);
        throw new RuntimeException("Should have thrown error making wallet multisig with incorrect values");
      } catch (MoneroError e) {
        if (!e.getMessage().equals("Kex message unexpectedly small.")) System.err.println("Unexpected error message: " + e.getMessage());
      }
      
      // collect prepared multisig hexes from wallet's peers
      List<String> peerMultisigHexes = new ArrayList<String>();
      for (int j = 0; j < participants.size(); j++) if (j != i) peerMultisigHexes.add(preparedMultisigHexes.get(j));

      // make the wallet multisig
      String multisigHex = participant.makeMultisig(peerMultisigHexes, M, TestUtils.WALLET_PASSWORD);
      madeMultisigHexes.add(multisigHex);
    }
    
    // try to get seed before wallet initialized
    try {
      participants.get(0).getSeed();
      throw new RuntimeException("Expected error getting seed before multisig wallet initialized");
    } catch (Exception e) {
      assertEquals("This wallet is multisig, but not yet finalized", e.getMessage());
    }
    
    // exchange keys N - M + 1 times
    String address = null;
    assertEquals(N, madeMultisigHexes.size());
    List<String> prevMultisigHexes = madeMultisigHexes;
    for (int i = 0; i < N - M + 1; i++) {
      //System.out.println("Exchanging multisig keys round " + (i + 1) + " / " + (N - M + 1));
      
      // exchange multisig keys with each wallet and collect results
      List<String> exchangeMultisigHexes = new ArrayList<String>();
      for (int j = 0; j < participants.size(); j++) {
        MoneroWallet participant = participants.get(j);
        
        // test bad input
        try {
          participant.exchangeMultisigKeys(Arrays.asList("asd", "dsa"), TestUtils.WALLET_PASSWORD);
          throw new RuntimeException("Should have thrown error exchanging multisig keys with bad input");
        } catch (MoneroError e) {
          assertTrue(e.getMessage().length() > 0);
        }
        
        // collect the multisig hexes of the wallet's peers from last round
        List<String> peerMultisigHexes = new ArrayList<String>();
        for (int k = 0; k < participants.size(); k++) if (k != j) peerMultisigHexes.add(prevMultisigHexes.get(k));
        
        // import the multisig hexes of the wallet's peers
        MoneroMultisigInitResult result = participant.exchangeMultisigKeys(peerMultisigHexes, TestUtils.WALLET_PASSWORD);
        
        // test result
        assertNotNull(result.getMultisigHex());
        assertFalse(result.getMultisigHex().isEmpty());
        if (i == N - M) {  // result on last round has address
          assertNotNull(result.getAddress());
          assertFalse(result.getAddress().isEmpty());
          if (address == null) address = result.getAddress();
          else assertEquals(address, result.getAddress());
        } else {
          assertNull(result.getAddress());
          exchangeMultisigHexes.add(result.getMultisigHex());
        }
      }
      
      // use results for next round of exchange
      prevMultisigHexes = exchangeMultisigHexes;
    }
    
    // validate final multisig
    MoneroWallet participant = participants.get(0);
    MoneroUtils.validateAddress(participant.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    testMultisigInfo(participant.getMultisigInfo(), M, N);
    String seed = participant.getSeed();
    assertFalse(seed.isEmpty());

    // restore participant from multisig seed
    closeWallet(participant);
    participant = createWallet(new MoneroWalletConfig().setRestoreHeight(daemon.getHeight()).setSeed(seed).setIsMultisig(true));
    MoneroUtils.validateAddress(participant.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    assertEquals(address, participant.getPrimaryAddress());
    testMultisigInfo(participant.getMultisigInfo(), M, N);
    assertEquals(seed, participant.getSeed());
    participants.set(0, participant);
    
    // test sending a multisig transaction if configured
    if (numAddressesToFund == 0) return;
      
    // create accounts in the first multisig wallet to receive funds
    for (int i = 0; i < accountIdx; i++) participant.createAccount();
    
    // get destinations to subaddresses within the account of the multisig wallet
    List<MoneroDestination> destinations = new ArrayList<MoneroDestination>();
    for (int i = 0; i < numAddressesToFund; i++) {
      destinations.add(new MoneroDestination(participant.getAddress(accountIdx, i), MoneroUtils.xmrToAtomicUnits(2)));
      if (i + 1 < numAddressesToFund) participant.createSubaddress(accountIdx);
    }
    
    // wait for txs to confirm and for sufficient unlocked balance
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, 0, null, TestUtils.MAX_FEE.multiply(new BigInteger("20")));
    
    // send funds from the main test wallet to destinations in the first multisig wallet
    assertTrue(wallet.getBalance().compareTo(BigInteger.valueOf(0)) > 0);
    System.out.println("Sending funds from main wallet");
    wallet.createTx(new MoneroTxConfig().setAccountIndex(0).setDestinations(destinations).setRelay(true));
    
    System.out.println("Starting mining");
    
    // attempt to start mining
    try { StartMining.startMining(); }
    catch (MoneroError e) { if ("BUSY".equals(e.getMessage())) throw e; }
    
    // wait for the multisig wallet's funds to unlock // TODO: replace with MoneroWalletListener.onOutputReceived() which is called when output unlocked
    Long lastNumConfirmations = null;
    while (true) {
      
      // wait a moment
      try { TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS); }
      catch (InterruptedException e) { throw new RuntimeException(e); }
      
      // fetch and test outputs
      List<MoneroOutputWallet> outputs = participant.getOutputs();
      if (outputs.isEmpty()) System.out.println("No outputs reported yet");
      else {
        
        // print num confirmations
        long height = daemon.getHeight();
        long numConfirmations = height - outputs.get(0).getTx().getHeight();
        if (lastNumConfirmations == null || lastNumConfirmations != numConfirmations) System.out.println("Output has " + (height - outputs.get(0).getTx().getHeight()) + " confirmations");
        lastNumConfirmations = numConfirmations;
        
        // outputs are not spent
        for (MoneroOutputWallet output : outputs) assertFalse(output.isSpent());
        
        // break if output is unlocked
        if (!outputs.get(0).isLocked()) break;
      }
    }
    
    // stop mining
    daemon.stopMining();
    
    // multisig wallet should have unlocked balance in subaddresses 0-3
    for (int i = 0; i < numAddressesToFund; i++) {
      assertTrue(participant.getUnlockedBalance(accountIdx, i).compareTo(BigInteger.valueOf(0)) > 0);
    }
    List<MoneroOutputWallet> outputs = participant.getOutputs(new MoneroOutputQuery().setAccountIndex(accountIdx));
    assertFalse(outputs.isEmpty());
    if (outputs.size() < 3) System.out.println("WARNING: not one output per subaddress?");
    //assertTrue(outputs.size() >= 3);  // TODO
    for (MoneroOutputWallet output : outputs) assertFalse(output.isLocked());
    
    // wallet requires importing multisig to be reliable
    assertTrue(participant.isMultisigImportNeeded());
    
    // attempt creating and relaying transaction without synchronizing with participants
    String returnAddress = wallet.getPrimaryAddress(); // funds will be returned to this address from the multisig wallet
    try {
      participant.createTxs(new MoneroTxConfig().setAccountIndex(accountIdx).setAddress(returnAddress).setAmount(TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3))));
      throw new RuntimeException("Should have failed sending funds without synchronizing with peers");
    } catch (MoneroError e) {
      assertEquals("No transaction created", e.getMessage());
    }
    
    // synchronize the multisig participants since receiving outputs
    System.out.println("Synchronizing participants");
    synchronizeMultisigParticipants(participants);
    
    // expect error exporting key images
    try {
      participant.exportKeyImages(true);
    } catch (Exception e) {
      assertTrue(e.getMessage().contains("key_image generated not matched with cached key image"), "Unexpected error: " + e.getMessage());
    }
    
    // attempt relaying created transactions without co-signing
    try {
      participant.createTx(new MoneroTxConfig().setAddress(returnAddress).setAmount(TestUtils.MAX_FEE).setAccountIndex(accountIdx).setSubaddressIndex(0).setRelay(true));
      throw new RuntimeException("Should have failed");
    } catch (Exception e) {
      assertTrue(e instanceof MoneroError);
      assertEquals("Cannot relay multisig transaction until co-signed", e.getMessage());
    }
  }

  // Supports multisig wallets under stress
  @Test
  public void testMultisigStress() {
    //String walletName = "multisig_stress_d2015ffb-380f-474f-8a3e-60fdbd334dbb"; // set previous wallet name to resume stress test
    String walletName = null;
    testMultisigStress(walletName);
  }

  private void testMultisigStress(String walletName) {

    // test config
    int M = 2;
    int N = 2;
    boolean restartWallets = true;
    int numAddressesToFund = 5;
    int maxTxs = 200;
    if (walletName == null) walletName = "multisig_stress_" + GenUtils.getUUID();
    String walletPath = (wallet instanceof MoneroWalletRpc ? "" : TestUtils.TEST_WALLETS_DIR + "/") + walletName;
    System.out.println("Stress testing multisig wallets: " + walletPath);

    // open or create multisig wallets
    List<MoneroWallet> participants = new ArrayList<MoneroWallet>();
    try {
      for (int i = 0; i < N; i++) participants.add(openWallet(new MoneroWalletConfig().setPath(walletPath + "_" + i))); // TODO: remove hack, using getTestWalletRoot() which subclasses override?
    } catch (Exception e) {
      for (int i = 0; i < N; i++) participants.add(createWallet(new MoneroWalletConfig().setPath(walletPath + "_" + i)));
      initMultisigWallets(participants, M, N, 0, numAddressesToFund);
    }
    MoneroWallet participant = participants.get(0);

    // start mining
    StartMining.startMining();

    // add listener
    participant.addListener(new MoneroWalletListener() {
      @Override
      public void onOutputReceived(MoneroOutputWallet received) {
        System.out.println("Output received!");
      }
    });

    // cycle funds
    System.gc();
    int numTxs = 0;
    while (numTxs < maxTxs) {
      try {
        BigInteger unlockedBalance = participant.getUnlockedBalance(0);
        while (unlockedBalance.compareTo(BigInteger.valueOf(0)) > 0) {

          // restart wallets
          if (restartWallets) {

            // collect paths
            List<String> paths = new ArrayList<String>();
            for (MoneroWallet wallet : participants) paths.add(wallet.getPath());

            // save and close wallets
            List<Runnable> tasks = new ArrayList<Runnable>();
            for (MoneroWallet wallet : participants) tasks.add(() -> closeWallet(wallet, true));
            GenUtils.executeTasks(tasks);

            // restart wallets
            participants.clear();
            for (String path : paths) participants.add(openWallet(new MoneroWalletConfig().setPath(path)));
            participant = participants.get(0);
            System.out.println("Done restarting wallets");
          }

          // synchronize the multisig participants since spending outputs
          System.out.println("Synchronizing participants");
          synchronizeMultisigParticipants(participants);

          // create txs to cycle funds
          MoneroTxConfig config = new MoneroTxConfig().setAccountIndex(0);
          for (int i = 0; i < numAddressesToFund; i++) config.addDestination(participant.getAddress(0, i), unlockedBalance.divide(BigInteger.valueOf(numAddressesToFund * 3)));
          List<MoneroTxWallet> txs = participant.createTxs(config);
          assertFalse(txs.isEmpty());
          MoneroTxSet txSet = txs.get(0).getTxSet();
          assertNotNull(txSet.getMultisigTxHex());
          assertNull(txSet.getSignedTxHex());
          assertNull(txSet.getUnsignedTxHex());

          // parse multisig tx hex and test
          testDescribedTxSet(participant.describeMultisigTxSet(txSet.getMultisigTxHex()));
              
          // sign the tx with participants 1 through m - 1 to meet threshold
          String multisigTxHex = txSet.getMultisigTxHex();
          System.out.println("Signing");
          for (int j = 1; j < M; j++) {
            MoneroMultisigSignResult result = participants.get(j).signMultisigTxHex(multisigTxHex);
            multisigTxHex = result.getSignedMultisigTxHex();
          }
          
          // submit the signed multisig tx hex to the network
          System.out.println("Submitting: " + multisigTxHex.length());
          List<String> txHashes = participant.submitMultisigTxHex(multisigTxHex);
          assertTrue(txHashes.size() > 0);
          System.out.println("Tx submitted successfully!");

          // fetch the wallet's multisig txs
          List<MoneroTxWallet> multisigTxs = participant.getTxs(new MoneroTxQuery().setHashes(txHashes));
          assertEquals(multisigTxs.size(), txHashes.size());
          List<MoneroTxWallet> allTxs = participant.getTxs();
          numTxs = allTxs.size();
          System.out.println("All txs size: " + allTxs.size());
          if (numTxs >= maxTxs) break;

          System.gc();
        }
      } catch (Exception e) {
        System.out.println("There was an error: " + e.getMessage());
      } catch (AssertionFailedError e) {
        System.out.println("There was an assertion error: " + e.getMessage());
      }
      
      // save wallets
      List<Runnable> tasks = new ArrayList<Runnable>();
      for (MoneroWallet wallet : participants) tasks.add(() -> wallet.save());
      GenUtils.executeTasks(tasks);
      
      // loop
      System.out.println("Starting another loop!");
      GenUtils.waitFor(5000);
    }
    
    // save and close wallets
    List<Runnable> tasks = new ArrayList<Runnable>();
    for (MoneroWallet wallet : participants) tasks.add(() -> closeWallet(wallet, true));
    GenUtils.executeTasks(tasks);
    
    // stop mining
    wallet.stopMining();
  }
  
  private void synchronizeMultisigParticipants(List<MoneroWallet> wallets) {
    
    // collect multisig hex of all participants to synchronize
    List<String> multisigHexes = new ArrayList<String>();
    for (MoneroWallet wallet : wallets) {
      wallet.sync();
      multisigHexes.add(wallet.exportMultisigHex()); // TODO: does wallet need saved?
    }
    
    // import each wallet's peer multisig hex
    for (int i = 0; i < wallets.size(); i++) {
      List<String> peerMultisigHexes = new ArrayList<String>();
      for (int j = 0; j < wallets.size(); j++) if (j != i) peerMultisigHexes.add(multisigHexes.get(j));
      MoneroWallet wallet = wallets.get(i);
      wallet.sync();  // TODO monero-project: creating multisig tx fails if wallet not explicitly synced before import_multisig_hex: https://github.com/monero-project/monero/issues/6850
      wallet.importMultisigHex(peerMultisigHexes);
    }
  }
  
  private static void testMultisigInfo(MoneroMultisigInfo info, int M, int N) {
    assertTrue(info.isMultisig());
    assertTrue(info.isReady());
    assertEquals(M, (int) info.getThreshold());
    assertEquals(N, (int) info.getNumParticipants());
  }
  
  // TODO: test sending to multiple accounts
  // Can update a locked tx sent from/to the same account as blocks are added to the chain
  @Test
  public void testUpdateLockedSameAccount() {
    assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setAccountIndex(0)
            .setCanSplit(false)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  // Can update split locked txs sent from/to the same account as blocks are added to the chain
  @Test
  public void testUpdateLockedSameAccountSplit() {
    assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setCanSplit(true)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  // Can update a locked tx sent from/to different accounts as blocks are added to the chain
  @Test
  public void testUpdateLockedDifferentAccounts() {
    assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getSubaddress(1, 0).getAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setCanSplit(false)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  // Can update locked, split txs sent from/to different accounts as blocks are added to the chain
  @Test
  public void testUpdateLockedDifferentAccountsSplit() {
    assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getSubaddress(1, 0).getAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setAccountIndex(0)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  /**
   * Tests sending a tx with an unlock time then tracking and updating it as
   * blocks are added to the chain.
   * 
   * TODO: test wallet accounting throughout this; dedicated method? probably.
   * 
   * Allows sending to and from the same account which is an edge case where
   * incoming txs are occluded by their outgoing counterpart (issue #4500)
   * and also where it is impossible to discern which incoming output is
   * the tx amount and which is the change amount without wallet metadata.
   * 
   * @param config is the send configuration to send and test
   * @throws InterruptedException
   */
  private void testSendAndUpdateTxs(MoneroTxConfig config) {
    
    // wait for txs to confirm and for sufficient unlocked balance
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    assertNull(config.getSubaddressIndices());
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(wallet, config.getAccountIndex(), null, TestUtils.MAX_FEE.multiply(new BigInteger("2")));
    
    // this test starts and stops mining, so it's wrapped in order to stop mining if anything fails
    try {
      
      // send transactions
      List<MoneroTxWallet> sentTxs = wallet.createTxs(config);
      
      // build test context
      TxContext ctx = new TxContext();
      ctx.wallet = wallet;
      ctx.config = config;
      ctx.isSendResponse = true;
      
      // test sent transactions
      for (MoneroTxWallet tx : sentTxs) {
        testTxWallet(tx, ctx);
        assertEquals(false, tx.isConfirmed());
        assertEquals(true, tx.inTxPool());
      }
      
      // track resulting outgoing and incoming txs as blocks are added to the chain
      List<MoneroTxWallet> updatedTxs = null;
      
      // attempt to start mining to push the network along
      boolean startedMining = false;
      MoneroMiningStatus miningStatus = daemon.getMiningStatus();
      if (!miningStatus.isActive()) {
        try {
          StartMining.startMining();
          startedMining = true;
        } catch (Exception e) {
          System.err.println("Warning: could not start mining: " + e.getMessage()); // not fatal
        }
      }
      
      // loop to update txs through confirmations
      long numConfirmations = 0;
      long numConfirmationsTotal = 2; // number of confirmations to test
      while (numConfirmations < numConfirmationsTotal) {
        System.out.println(numConfirmations + " < " + numConfirmationsTotal + " needed confirmations");
        
        // wait for a block
        MoneroBlockHeader header = daemon.waitForNextBlockHeader();
        System.out.println("*** Block " + header.getHeight() + " added to chain ***");
        
        // give wallet time to catch up, otherwise incoming tx may not appear
        // TODO: this lets new block slip, okay?
        try {
          TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS);
        } catch (InterruptedException e) {
          throw new RuntimeException(e);
        }
        
        // get incoming/outgoing txs with sent hashes
        List<String> txHashes = new ArrayList<String>();
        for (MoneroTxWallet sentTx : sentTxs) txHashes.add(sentTx.getHash());
        MoneroTxQuery txQuery = new MoneroTxQuery().setHashes(txHashes);
        List<MoneroTxWallet> fetchedTxs = getAndTestTxs(wallet, txQuery, null, true);
        assertFalse(fetchedTxs.isEmpty());
        
        // test fetched txs
        testOutInPairs(wallet, fetchedTxs, config, false);

        // merge fetched txs into updated txs and original sent txs
        for (MoneroTxWallet fetchedTx : fetchedTxs) {
          
          // merge with updated txs
          if (updatedTxs == null) updatedTxs = fetchedTxs;
          else {
            for (MoneroTxWallet updatedTx : updatedTxs) {
              if (!fetchedTx.getHash().equals(updatedTx.getHash())) continue;
              if (fetchedTx.isOutgoing() != updatedTx.isOutgoing()) continue; // skip if directions are different
              updatedTx.merge(fetchedTx.copy());
              if (updatedTx.getBlock() == null && fetchedTx.getBlock() != null) updatedTx.setBlock(fetchedTx.getBlock().copy().setTxs(Arrays.asList(updatedTx)));  // copy block for testing
            }
          }
          
          // merge with original sent txs
          for (MoneroTxWallet sentTx : sentTxs) {
            if (!fetchedTx.getHash().equals(sentTx.getHash())) continue;
            if (fetchedTx.isOutgoing() != sentTx.isOutgoing()) continue; // skip if directions are different
            sentTx.merge(fetchedTx.copy());  // TODO: it's mergeable but tests don't account for extra info from send (e.g. hex) so not tested; could specify in test config
          }
        }
        
        // test updated txs
        testOutInPairs(wallet, updatedTxs, config, false);
        
        // update confirmations in order to exit loop
        numConfirmations = fetchedTxs.get(0).getNumConfirmations();
      }
      
      // stop mining if it was started by this test
      if (startedMining) wallet.stopMining();
      
    } catch (MoneroError e) {
      throw e;
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
    }
  }
  
  private void testOutInPairs(MoneroWallet wallet, List<MoneroTxWallet> txs, MoneroTxConfig config, boolean isSendResponse) {
    
    // for each out tx
    for (MoneroTxWallet tx : txs) {
      testUnlockTx(wallet, tx, config, isSendResponse);
      if (tx.getOutgoingTransfer() != null) continue;
      MoneroTxWallet txOut = tx;
      
      // find incoming counterpart
      MoneroTxWallet txIn = null;
      for (MoneroTxWallet tx2 : txs) {
        if (tx2.isIncoming() && tx.getHash().equals(tx2.getHash())) {
          txIn = tx2;
          break;
        }
      }
      
      // test out / in pair
      // TODO monero-wallet-rpc: incoming txs occluded by their outgoing counterpart #4500
      if (txIn == null) {
        System.out.println("WARNING: outgoing tx " + txOut.getHash() + " missing incoming counterpart (issue #4500)");
      } else {
        testOutInPair(txOut, txIn);
      }
    }
  }
  
  private void testOutInPair(MoneroTxWallet txOut, MoneroTxWallet txIn) {
    assertEquals(txOut.isConfirmed(), txIn.isConfirmed());
    assertEquals(txIn.getIncomingAmount(), txOut.getOutgoingAmount());
  }
  
  private void testUnlockTx(MoneroWallet wallet, MoneroTxWallet tx, MoneroTxConfig config, boolean isSendResponse) {
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = config;
    ctx.isSendResponse = isSendResponse;
    try {
      testTxWallet(tx, ctx);
    } catch (MoneroError e) {
      System.out.println(tx.toString());
      throw e;
    }
  }
  
  // --------------------------------- RESET TESTS --------------------------------
  
  // Can sweep subaddresses
  @Test
  public void testSweepSubaddresses() {
    assumeTrue(TEST_RESETS);
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    final int NUM_SUBADDRESSES_TO_SWEEP = 2;
    
    // collect subaddresses with balance and unlocked balance
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesBalance = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesUnlocked = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      if (account.getIndex() == 0) continue;  // skip default account
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        subaddresses.add(subaddress);
        if (subaddress.getBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesBalance.add(subaddress);
        if (subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesUnlocked.add(subaddress);
      }
    }
    
    // test requires at least one more subaddresses than the number being swept to verify it does not change
    assertTrue(subaddressesBalance.size() >= NUM_SUBADDRESSES_TO_SWEEP + 1, "Test requires balance in at least " + (NUM_SUBADDRESSES_TO_SWEEP + 1) + " subaddresses from non-default acccount; run send-to-multiple tests");
    assertTrue(subaddressesUnlocked.size() >= NUM_SUBADDRESSES_TO_SWEEP + 1, "Wallet is waiting on unlocked funds");
    
    // sweep from first unlocked subaddresses
    for (int i = 0; i < NUM_SUBADDRESSES_TO_SWEEP; i++) {
      
      // sweep unlocked account
      MoneroSubaddress unlockedSubaddress = subaddressesUnlocked.get(i);
      MoneroTxConfig config = new MoneroTxConfig()
              .setAddress(wallet.getPrimaryAddress())
              .setAccountIndex(unlockedSubaddress.getAccountIndex())
              .setSubaddressIndices(unlockedSubaddress.getIndex())
              .setRelay(true);
      List<MoneroTxWallet> txs = wallet.sweepUnlocked(config);
      
      // test transactions
      assertTrue(txs.size() > 0);
      for (MoneroTxWallet tx : txs) {
        assertTrue(tx.getTxSet().getTxs().contains(tx));
        TxContext ctx = new TxContext();
        ctx.wallet = wallet;
        ctx.config = config;
        ctx.isSendResponse = true;
        ctx.isSweepResponse = true;
        testTxWallet(tx, ctx);
      }
      
      // assert unlocked balance is less than max fee
      MoneroSubaddress subaddress = wallet.getSubaddress(unlockedSubaddress.getAccountIndex(), unlockedSubaddress.getIndex());
      assertTrue(subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0);
    }
    
    // test subaddresses after sweeping
    List<MoneroSubaddress> subaddressesAfter = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      if (account.getIndex() == 0) continue;  // skip default account
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        subaddressesAfter.add(subaddress);
      }
    }
    assertEquals(subaddresses.size(), subaddressesAfter.size());
    for (int i = 0; i < subaddresses.size(); i++) {
      MoneroSubaddress subaddressBefore = subaddresses.get(i);
      MoneroSubaddress subaddressAfter = subaddressesAfter.get(i);
      
      // determine if subaddress was swept
      boolean swept = false;
      for (int j = 0; j < NUM_SUBADDRESSES_TO_SWEEP; j++) {
        if (subaddressesUnlocked.get(j).getAccountIndex().equals(subaddressBefore.getAccountIndex()) && subaddressesUnlocked.get(j).getIndex().equals(subaddressBefore.getIndex())) {
          swept = true;
          break;
        }
      }
      
      // assert unlocked balance is less than max fee if swept, unchanged otherwise
      if (swept) {
        assertTrue(subaddressAfter.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0);
      } else {
        assertTrue(subaddressBefore.getUnlockedBalance().compareTo(subaddressAfter.getUnlockedBalance()) == 0);
      }
    }
  }
  
  // Can sweep accounts
  @Test
  public void testSweepAccounts() {
    assumeTrue(TEST_RESETS);
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    final int NUM_ACCOUNTS_TO_SWEEP = 1;
    
    // collect accounts with sufficient balance and unlocked balance to cover the fee
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    List<MoneroAccount> accountsBalance = new ArrayList<MoneroAccount>();
    List<MoneroAccount> accountsUnlocked = new ArrayList<MoneroAccount>();
    for (MoneroAccount account : accounts) {
      if (account.getIndex() == 0) continue;  // skip default account
      if (account.getBalance().compareTo(TestUtils.MAX_FEE) > 0) accountsBalance.add(account);
      if (account.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) accountsUnlocked.add(account);
    }
    
    // test requires at least one more accounts than the number being swept to verify it does not change
    assertTrue(accountsBalance.size() >= NUM_ACCOUNTS_TO_SWEEP + 1, "Test requires balance greater than the fee in at least " + (NUM_ACCOUNTS_TO_SWEEP + 1) + " non-default accounts; run send-to-multiple tests");
    assertTrue(accountsUnlocked.size() >= NUM_ACCOUNTS_TO_SWEEP + 1, "Wallet is waiting on unlocked funds");
    
    // sweep from first unlocked accounts
    for (int i = 0; i < NUM_ACCOUNTS_TO_SWEEP; i++) {
      
      // sweep unlocked account
      MoneroAccount unlockedAccount = accountsUnlocked.get(i);
      MoneroTxConfig config = new MoneroTxConfig().setAddress(wallet.getPrimaryAddress()).setAccountIndex(unlockedAccount.getIndex()).setRelay(true);
      List<MoneroTxWallet> txs = wallet.sweepUnlocked(config);
      
      // test transactions
      assertTrue(txs.size() > 0);
      for (MoneroTxWallet tx : txs) {
        TxContext ctx = new TxContext();
        ctx.wallet = wallet;
        ctx.config = config;
        ctx.isSendResponse = true;
        ctx.isSweepResponse = true;
        testTxWallet(tx, ctx);
        assertNotNull(tx.getTxSet());
        assertTrue(tx.getTxSet().getTxs().contains(tx));
      }
      
      // assert unlocked account balance less than max fee
      MoneroAccount account = wallet.getAccount(unlockedAccount.getIndex());
      assertTrue(account.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0);
    }
    
    // test accounts after sweeping
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    for (int i = 0; i < accounts.size(); i++) {
      MoneroAccount accountBefore = accounts.get(i);
      MoneroAccount accountAfter = accountsAfter.get(i);
      
      // determine if account was swept
      boolean swept = false;
      for (int j = 0; j < NUM_ACCOUNTS_TO_SWEEP; j++) {
        if (accountsUnlocked.get(j).getIndex().equals(accountBefore.getIndex())) {
          swept = true;
          break;
        }
      }
      
      // assert unlocked balance is less than max fee if swept, unchanged otherwise
      if (swept) {
        assertTrue(accountAfter.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0);
      } else {
        assertTrue(accountBefore.getUnlockedBalance().compareTo(accountAfter.getUnlockedBalance()) == 0);
      }
    }
  }
  
  // Can sweep the whole wallet by accounts
  @Test
  @Disabled // disabled so tests don't sweep the whole wallet
  public void testSweepWalletByAccounts() {
    assumeTrue(TEST_RESETS);
    testSweepWallet(null);
  }
  
  // Can sweep the whole wallet by subaddresses
  @Test
  @Disabled // disabled so tests don't sweep the whole wallet
  public void testSweepWalletBySubaddresses() {
    assumeTrue(TEST_RESETS);
    testSweepWallet(true);
  }
  
  private void testSweepWallet(Boolean sweepEachSubaddress) {
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // verify 2 subaddresses with enough unlocked balance to cover the fee
    List<MoneroSubaddress> subaddressesBalance = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesUnlocked = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesBalance.add(subaddress);
        if (subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesUnlocked.add(subaddress);
      }
    }
    assertTrue(subaddressesBalance.size() >= 2, "Test requires multiple accounts with a balance greater than the fee; run send to multiple first");
    assertTrue(subaddressesUnlocked.size() >= 2, "Wallet is waiting on unlocked funds");
    
    // sweep
    String destination = wallet.getPrimaryAddress();
    MoneroTxConfig config = new MoneroTxConfig().setAddress(destination).setSweepEachSubaddress(sweepEachSubaddress).setRelay(true);
    MoneroTxConfig copy = config.copy();
    List<MoneroTxWallet> txs = wallet.sweepUnlocked(config);
    assertEquals(copy, config);  // config is unchanged
    for (MoneroTxWallet tx : txs) {
      assertTrue(tx.getTxSet().getTxs().contains(tx));
      assertNull(tx.getTxSet().getMultisigTxHex());
      assertNull(tx.getTxSet().getSignedTxHex());
      assertNull(tx.getTxSet().getUnsignedTxHex());
    }
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      config = new MoneroTxConfig()
              .setAddress(destination)
              .setAccountIndex(tx.getOutgoingTransfer().getAccountIndex())
              .setSweepEachSubaddress(sweepEachSubaddress)
              .setRelay(true);
      TxContext ctx = new TxContext();
      ctx.wallet = wallet;
      ctx.config = config;
      ctx.isSendResponse = true;
      ctx.isSweepResponse = true;
      testTxWallet(tx, ctx);
    }
    
    // all unspent, unlocked outputs must be less than fee
    List<MoneroOutputWallet> spendableOutputs = wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false).setTxQuery(new MoneroTxQuery().setIsLocked(false)));
    for (MoneroOutputWallet spendableOutput : spendableOutputs) {
      assertTrue(spendableOutput.getAmount().compareTo(TestUtils.MAX_FEE) < 0, "Unspent output should have been swept\n" + spendableOutput.toString());
    }
    
    // all subaddress unlocked balances must be less than fee
    subaddressesBalance.clear();
    subaddressesUnlocked.clear();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        assertTrue(subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0, "No subaddress should have more unlocked than the fee");
      }
    }
  }
  
  // Can scan transactions by id
  @Test
  public void testScanTxs() {
    
    // get a few tx hashes
    List<String> txHashes = new ArrayList<String>();
    List<MoneroTxWallet> txs = wallet.getTxs();
    if (txs.size() < 3) fail("Not enough txs to scan");
    for (int i = 0; i < 3; i++) txHashes.add(txs.get(i).getHash());
    
    // start wallet without scanning
    MoneroWallet scanWallet = createWallet(new MoneroWalletConfig().setSeed(wallet.getSeed()).setRestoreHeight(0l));
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
  
  // Can rescan the blockchain
  @Test
  @Disabled // disabled so tests don't delete local cache
  public void testRescanBlockchain() {
    assumeTrue(TEST_RESETS);
    wallet.rescanBlockchain();
    for (MoneroTxWallet tx : wallet.getTxs()) {
      testTxWallet(tx, null);
    }
  }
  
  // ----------------------------- NOTIFICATION TESTS -------------------------
  
  // Can generate notifications sending to different wallet
  @Test
  public void testNotificationsDifferentWallet() {
    testWalletNotifications("testNotificationsDifferentWallet", false, false, false, false, 0);
  }
  
  // Can generate notifications sending to different wallet when relayed
  @Test
  public void testNotificationsDifferentWalletWhenRelayed() {
    testWalletNotifications("testNotificationsDifferentWalletWhenRelayed", false, false, false, true, 3);
  }
  
  // Can generate notifications sending to different account
  @Test
  public void testNotificationsDifferentAccounts() {
    testWalletNotifications("testNotificationsDifferentAccounts", true, false, false, false, 0);
  }
  
  // Can generate notifications sending to same account
  @Test
  public void testNotificationsSameAccount() {
    testWalletNotifications("testNotificationsSameAccount", true, true, false, false, 0);
  }
  
  // Can generate notifications sweeping output to different account
  @Test
  public void testNotificationsDifferentAccountSweepOutput() {
    testWalletNotifications("testNotificationsDifferentAccountSweepOutput", true, false, true, false, 0);
  }
  
  // Can generate notifications sweeping output to same account when relayed
  @Test
  public void testNotificationsSameAccountSweepOutputWhenRelayed() {
    testWalletNotifications("testNotificationsSameAccountSweepOutputWhenRelayed", true, true, true, true, 0);
  }
  
  private void testWalletNotifications(String testName, boolean sameWallet, boolean sameAccount, boolean sweepOutput, boolean createThenRelay, long unlockDelay) {
    assumeTrue(TEST_NOTIFICATIONS);
    List<String> issues = testWalletNotificationsAux(sameWallet, sameAccount, sweepOutput, createThenRelay, unlockDelay);
    if (issues.size() == 0) return;
    String msg = testName + "(" + sameWallet + ", " + sameAccount + ", " + sweepOutput + ", " + createThenRelay + ") generated " + issues.size() + " issues:\n" + issuesToStr(issues);
    System.out.println(msg);
    if (msg.contains("ERROR:")) fail(msg);
  }
  
  // TODO: test sweepUnlocked()
  private List<String> testWalletNotificationsAux(boolean sameWallet, boolean sameAccount, boolean sweepOutput, boolean createThenRelay, long unlockDelay) {
    long MAX_POLL_TIME = 5000l; // maximum time granted for wallet to poll
    
    // collect issues as test runs
    List<String> issues = new ArrayList<String>();
    
    // set sender and receiver
    MoneroWallet sender = wallet;
    MoneroWallet receiver = sameWallet ? sender : createWallet(new MoneroWalletConfig());
    
    // create receiver accounts if necessary
    int numAccounts = receiver.getAccounts().size();
    for (int i = 0; i < 4 - numAccounts; i++) receiver.createAccount();
    
    // wait for unlocked funds in source account
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(sender);
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(sender, 0, null, TestUtils.MAX_FEE.multiply(new BigInteger("10")));
    
    // get balances to compare after sending
    BigInteger senderBalanceBefore = sender.getBalance();
    BigInteger senderUnlockedBalanceBefore = sender.getUnlockedBalance();
    BigInteger receiverBalanceBefore = receiver.getBalance();
    BigInteger receiverUnlockedBalanceBefore = receiver.getUnlockedBalance();
    Long lastHeight = daemon.getHeight();
    
    // start collecting notifications from sender and receiver
    WalletNotificationCollector senderNotificationCollector = new WalletNotificationCollector();
    WalletNotificationCollector receiverNotificationCollector = new WalletNotificationCollector();
    sender.addListener(senderNotificationCollector);
    GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS / 2l);
    receiver.addListener(receiverNotificationCollector);
    
    // send funds
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.isSendResponse = true;
    MoneroTxWallet senderTx = null;
    int[] destinationAccounts = sameAccount ? (sweepOutput ? new int[] {0} : new int[] {0, 1, 2}) : (sweepOutput ? new int[] {1} : new int[] {1, 2, 3});
    List<MoneroOutputWallet> expectedOutputs = new ArrayList<MoneroOutputWallet>();
    if (sweepOutput) {
      ctx.isSweepResponse = true;
      ctx.isSweepOutputResponse = true;
      List<MoneroOutputWallet> outputs = sender.getOutputs(new MoneroOutputQuery().setIsSpent(false).setTxQuery(new MoneroTxQuery().setIsLocked(false)).setAccountIndex(0).setMinAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5"))));
      if (outputs.isEmpty()) {
        issues.add("ERROR: No outputs available to sweep from account 0");
        return issues;
      }
      MoneroTxConfig config = new MoneroTxConfig().setAddress(receiver.getAddress(destinationAccounts[0], 0)).setKeyImage(outputs.get(0).getKeyImage().getHex()).setRelay(!createThenRelay);
      senderTx = sender.sweepOutput(config);
      expectedOutputs.add(new MoneroOutputWallet().setAmount(senderTx.getOutgoingTransfer().getDestinations().get(0).getAmount()).setAccountIndex(destinationAccounts[0]).setSubaddressIndex(0));
      ctx.config = config;
    } else {
      MoneroTxConfig config = new MoneroTxConfig().setAccountIndex(0).setRelay(!createThenRelay);
      for (int destinationAccount : destinationAccounts) {
        config.addDestination(receiver.getAddress(destinationAccount, 0), TestUtils.MAX_FEE); // TODO: send and check random amounts?
        expectedOutputs.add(new MoneroOutputWallet().setAmount(TestUtils.MAX_FEE).setAccountIndex(destinationAccount).setSubaddressIndex(0));
      }
      senderTx = sender.createTx(config);
      ctx.config = config;
    }
    if (createThenRelay) sender.relayTx(senderTx);
    
    // start timer to measure end of sync period
    long startTime = System.currentTimeMillis();
    
    // test send tx
    testTxWallet(senderTx, ctx);
    
    // test sender after sending
    MoneroOutputQuery outputQuery = new MoneroOutputQuery().setTxQuery(new MoneroTxQuery().setHash(senderTx.getHash())); // query for outputs from sender tx
    if (sameWallet) {
      if (senderTx.getIncomingAmount() == null) issues.add("WARNING: sender tx incoming amount is null when sent to same wallet");
      else if (senderTx.getIncomingAmount().equals(new BigInteger("0"))) issues.add("WARNING: sender tx incoming amount is 0 when sent to same wallet");
      else if (senderTx.getIncomingAmount().compareTo(senderTx.getOutgoingAmount().subtract(senderTx.getFee())) != 0) issues.add("WARNING: sender tx incoming amount != outgoing amount - fee when sent to same wallet");
    } else {
      if (senderTx.getIncomingAmount() != null) issues.add("ERROR: tx incoming amount should be null"); // TODO: should be 0? then can remove null checks in this method
    }
    senderTx = sender.getTxs(new MoneroTxQuery().setHash(senderTx.getHash()).setIncludeOutputs(true)).get(0);
    if (!sender.getBalance().equals(senderBalanceBefore.subtract(senderTx.getFee()).subtract(senderTx.getOutgoingAmount()).add(senderTx.getIncomingAmount() == null ? new BigInteger("0") : senderTx.getIncomingAmount()))) issues.add("ERROR: sender balance after send != balance before - tx fee - outgoing amount + incoming amount (" + sender.getBalance() + " != " + senderBalanceBefore + " - " + senderTx.getFee() + " - " + senderTx.getOutgoingAmount() + " + " + senderTx.getIncomingAmount() + ")");
    if (sender.getUnlockedBalance().compareTo(senderUnlockedBalanceBefore) >= 0) issues.add("ERROR: sender unlocked balance should have decreased after sending");
    if (senderNotificationCollector.getBalanceNotifications().size() == 0) issues.add("ERROR: sender did not notify balance change after sending");
    else {
      if (!sender.getBalance().equals(senderNotificationCollector.getBalanceNotifications().get(senderNotificationCollector.getBalanceNotifications().size() - 1).getFirst())) issues.add("ERROR: sender balance != last notified balance after sending (" + sender.getBalance() + " != " + senderNotificationCollector.getBalanceNotifications().get(senderNotificationCollector.getBalanceNotifications().size() - 1).getFirst() + ")");
      if (!sender.getUnlockedBalance().equals(senderNotificationCollector.getBalanceNotifications().get(senderNotificationCollector.getBalanceNotifications().size() - 1).getSecond())) issues.add("ERROR: sender unlocked balance != last notified unlocked balance after sending (" + sender.getUnlockedBalance() + " != " + senderNotificationCollector.getBalanceNotifications().get(senderNotificationCollector.getBalanceNotifications().size() - 1).getSecond() + ")");
    }
    if (senderNotificationCollector.getOutputsSpent(outputQuery).size() == 0) issues.add("ERROR: sender did not announce unconfirmed spent output");
        
    // test receiver after 2 sync periods
    GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS - (System.currentTimeMillis() - startTime));
    startTime = System.currentTimeMillis(); // reset timer
    MoneroTxWallet receiverTx = receiver.getTx(senderTx.getHash());
    if (!senderTx.getOutgoingAmount().equals(receiverTx.getIncomingAmount())) {
      if (sameAccount) issues.add("WARNING: sender tx outgoing amount != receiver tx incoming amount when sent to same account (" + senderTx.getOutgoingAmount() + " != " + receiverTx.getIncomingAmount() + ")");
      else if (sameAccount) issues.add("ERROR: sender tx outgoing amount != receiver tx incoming amount (" + senderTx.getOutgoingAmount() + " != " + receiverTx.getIncomingAmount() + ")");
    }
    if (!receiver.getBalance().equals(receiverBalanceBefore.add(receiverTx.getIncomingAmount() == null ? new BigInteger("0") : receiverTx.getIncomingAmount()).subtract(receiverTx.getOutgoingAmount() == null ? new BigInteger("0") : receiverTx.getOutgoingAmount()).subtract(sameWallet ? receiverTx.getFee() : new BigInteger("0")))) {
      if (sameAccount) issues.add("WARNING: after sending, receiver balance != balance before + incoming amount - outgoing amount - tx fee when sent to same account (" + receiver.getBalance() + " != " + receiverBalanceBefore + " + " + receiverTx.getIncomingAmount() + " - " + receiverTx.getOutgoingAmount() + " - " + (sameWallet ? receiverTx.getFee() : new BigInteger("0")) + ")");
      else issues.add("ERROR: after sending, receiver balance != balance before + incoming amount - outgoing amount - tx fee (" + receiver.getBalance() + " != " + receiverBalanceBefore + " + " + receiverTx.getIncomingAmount() + " - " + receiverTx.getOutgoingAmount() + " - " + (sameWallet ? receiverTx.getFee() : new BigInteger("0")) + ")");
    }
    if (!sameWallet && !receiver.getUnlockedBalance().equals(receiverUnlockedBalanceBefore)) issues.add("ERROR: receiver unlocked balance should not have changed after sending");
    if (receiverNotificationCollector.getBalanceNotifications().size() == 0) issues.add("ERROR: receiver did not notify balance change when funds received");
    else {
      if (!receiver.getBalance().equals(receiverNotificationCollector.getBalanceNotifications().get(receiverNotificationCollector.getBalanceNotifications().size() - 1).getFirst())) issues.add("ERROR: receiver balance != last notified balance after funds received");
      if (!receiver.getUnlockedBalance().equals(receiverNotificationCollector.getBalanceNotifications().get(receiverNotificationCollector.getBalanceNotifications().size() - 1).getSecond())) issues.add("ERROR: receiver unlocked balance != last notified unlocked balance after funds received");
    }
    if (receiverNotificationCollector.getOutputsReceived(outputQuery).size() == 0) issues.add("ERROR: receiver did not announce unconfirmed received output");
    else {
      for (MoneroOutputWallet output : getMissingOutputs(expectedOutputs, receiverNotificationCollector.getOutputsReceived(outputQuery), true)) {
        issues.add("ERROR: receiver did not announce received output for amount " + output.getAmount() + " to subaddress [" + output.getAccountIndex() + ", " + output.getSubaddressIndex() + "]");
      }
    }
    
    // mine until test completes
    StartMining.startMining();
    
    // loop every sync period until unlock tested
    List<Thread> threads = new ArrayList<Thread>();
    long expectedUnlockTime = lastHeight + unlockDelay;
    Long confirmHeight = null;
    while (true) {
      
      // test height notifications
      long height = daemon.getHeight();
      if (height > lastHeight) {
        long testStartHeight = lastHeight;
        lastHeight = height;
        Thread thread = new Thread(new Runnable() {
          @Override public void run() {
            GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS * 2 + MAX_POLL_TIME); // wait 2 sync periods + poll time for notifications
            List<Long> senderBlockNotifications = senderNotificationCollector.getBlockNotifications();
            List<Long> receiverBlockNotifications = receiverNotificationCollector.getBlockNotifications();
            for (long i = testStartHeight; i < height; i++) {
              if (!senderBlockNotifications.contains(i)) issues.add("ERROR: sender did not announce block " + i);
              if (!receiverBlockNotifications.contains(i)) issues.add("ERROR: receiver did not announce block " + i);
            }
          }
        });
        threads.add(thread);
        thread.start();
      }
      
      // check if tx confirmed
      if (confirmHeight == null) {
        
        // get updated tx
        MoneroTxWallet tx = receiver.getTx(senderTx.getHash());
        
        // break if tx fails
        if (tx.isFailed()) {
          issues.add("ERROR: tx failed in tx pool");
          break;
        }
        
        // test confirm notifications
        if (tx.isConfirmed() && confirmHeight == null) {
          confirmHeight = tx.getHeight();
          expectedUnlockTime = Math.max(confirmHeight + NUM_BLOCKS_LOCKED, expectedUnlockTime); // exact unlock time known
          Thread thread = new Thread(new Runnable() {
            @Override public void run() {
              GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS * 2 + MAX_POLL_TIME); // wait 2 sync periods + poll time for notifications
              MoneroOutputQuery confirmedQuery = outputQuery.getTxQuery().copy().setIsConfirmed(true).setIsLocked(true).getOutputQuery();
              if (senderNotificationCollector.getOutputsSpent(confirmedQuery).size() == 0) issues.add("ERROR: sender did not announce confirmed spent output"); // TODO: test amount
              if (receiverNotificationCollector.getOutputsReceived(confirmedQuery).size() == 0) issues.add("ERROR: receiver did not announce confirmed received output");
              else for (MoneroOutputWallet output : getMissingOutputs(expectedOutputs, receiverNotificationCollector.getOutputsReceived(confirmedQuery), true)) issues.add("ERROR: receiver did not announce confirmed received output for amount " + output.getAmount() + " to subaddress [" + output.getAccountIndex() + ", " + output.getSubaddressIndex() + "]");
              
              // if same wallet, net amount spent = tx fee = outputs spent - outputs received
              if (sameWallet) {
                BigInteger netAmount = new BigInteger("0");
                for (MoneroOutputWallet outputSpent : senderNotificationCollector.getOutputsSpent(confirmedQuery)) netAmount = netAmount.add(outputSpent.getAmount());
                for (MoneroOutputWallet outputReceived : senderNotificationCollector.getOutputsReceived(confirmedQuery)) netAmount = netAmount.subtract(outputReceived.getAmount());
                if (tx.getFee().compareTo(netAmount) != 0) {
                  if (sameAccount) issues.add("WARNING: net output amount != tx fee when funds sent to same account: " + netAmount + " vs " + tx.getFee());
                  else if (sender instanceof MoneroWalletRpc) issues.add("WARNING: net output amount != tx fee when funds sent to same wallet because monero-wallet-rpc does not provide tx inputs: " + netAmount + " vs " + tx.getFee()); // TODO (monero-project): open issue to provide tx inputs
                  else issues.add("ERROR: net output amount must equal tx fee when funds sent to same wallet: " + netAmount + " vs " + tx.getFee());
                }
              }
            }
          });
          threads.add(thread);
          thread.start();
        }
      }
      
      // otherwise test unlock notifications
      else if (height >= expectedUnlockTime) {
        Thread thread = new Thread(new Runnable() {
          @Override public void run() {
            GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS * 2 + MAX_POLL_TIME); // wait 2 sync periods + poll time for notifications
            MoneroOutputQuery unlockedQuery = outputQuery.getTxQuery().copy().setIsLocked(false).getOutputQuery();
            if (senderNotificationCollector.getOutputsSpent(unlockedQuery).size() == 0) issues.add("ERROR: sender did not announce unlocked spent output"); // TODO: test amount?
            for (MoneroOutputWallet output : getMissingOutputs(expectedOutputs, receiverNotificationCollector.getOutputsReceived(unlockedQuery), true)) issues.add("ERROR: receiver did not announce unlocked received output for amount " + output.getAmount() + " to subaddress [" + output.getAccountIndex() + ", " + output.getSubaddressIndex() + "]");
            if (!sameWallet && !receiver.getBalance().equals(receiver.getUnlockedBalance())) issues.add("ERROR: receiver balance != unlocked balance after funds unlocked");
            if (senderNotificationCollector.getBalanceNotifications().size() == 0) issues.add("ERROR: sender did not announce any balance notifications");
            else {
              if (!sender.getBalance().equals(senderNotificationCollector.getBalanceNotifications().get(senderNotificationCollector.getBalanceNotifications().size() - 1).getFirst())) issues.add("ERROR: sender balance != last notified balance after funds unlocked");
              if (!sender.getUnlockedBalance().equals(senderNotificationCollector.getBalanceNotifications().get(senderNotificationCollector.getBalanceNotifications().size() - 1).getSecond())) issues.add("ERROR: sender unlocked balance != last notified unlocked balance after funds unlocked");
            }
            if (receiverNotificationCollector.getBalanceNotifications().size() == 0) issues.add("ERROR: receiver did not announce any balance notifications");
            else {
              if (!receiver.getBalance().equals(receiverNotificationCollector.getBalanceNotifications().get(receiverNotificationCollector.getBalanceNotifications().size() - 1).getFirst())) issues.add("ERROR: receiver balance != last notified balance after funds unlocked");
              if (!receiver.getUnlockedBalance().equals(receiverNotificationCollector.getBalanceNotifications().get(receiverNotificationCollector.getBalanceNotifications().size() - 1).getSecond())) issues.add("ERROR: receiver unlocked balance != last notified unlocked balance after funds unlocked");
            }
          }
        });
        threads.add(thread);
        thread.start();
        break;
      }
      
      // wait for end of sync period
      GenUtils.waitFor(TestUtils.SYNC_PERIOD_IN_MS - (System.currentTimeMillis() - startTime));
      startTime = System.currentTimeMillis(); // reset timer
    }
    
    // wait for test threads
    try {
      for (Thread thread : threads) thread.join();
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    
    // test notified outputs
    for (MoneroOutputWallet output : senderNotificationCollector.getOutputsSpent(outputQuery)) testNotifiedOutput(output, true, issues);
    for (MoneroOutputWallet output : senderNotificationCollector.getOutputsReceived(outputQuery)) testNotifiedOutput(output, false, issues);
    for (MoneroOutputWallet output : receiverNotificationCollector.getOutputsSpent(outputQuery)) testNotifiedOutput(output, true, issues);
    for (MoneroOutputWallet output : receiverNotificationCollector.getOutputsReceived(outputQuery)) testNotifiedOutput(output, false, issues);
    
    // clean up
    if (daemon.getMiningStatus().isActive()) daemon.stopMining();
    sender.removeListener(senderNotificationCollector);
    senderNotificationCollector.setListening(false);
    receiver.removeListener(receiverNotificationCollector);
    receiverNotificationCollector.setListening(false);
    if (sender != receiver) closeWallet(receiver);
    return issues;
  }
  
  private static List<MoneroOutputWallet> getMissingOutputs(List<MoneroOutputWallet> expectedOutputs, List<MoneroOutputWallet> actualOutputs, boolean matchSubaddress) {
    List<MoneroOutputWallet> missing = new ArrayList<MoneroOutputWallet>();
    List<MoneroOutputWallet> used = new ArrayList<MoneroOutputWallet>();
    for (MoneroOutputWallet expectedOutput : expectedOutputs) {
      boolean found = false;
      for (MoneroOutputWallet actualOutput : actualOutputs) {
        if (used.contains(actualOutput)) continue;
        if (actualOutput.getAmount().equals(expectedOutput.getAmount()) && (!matchSubaddress || (actualOutput.getAccountIndex() == expectedOutput.getAccountIndex() && actualOutput.getSubaddressIndex() == expectedOutput.getSubaddressIndex()))) {
          used.add(actualOutput);
          found = true;
          break;
        }
      }
      if (!found) missing.add(expectedOutput);
    }
    return missing;
  }
  
  private static String issuesToStr(List<String> issues) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < issues.size(); i++) {
      sb.append((i + 1) + ": " + issues.get(i));
      if (i < issues.size() - 1) sb.append('\n');
    }
    return sb.toString();
  }
  
  private static void testNotifiedOutput(MoneroOutputWallet output, boolean isTxInput, List<String> issues) {
    
    // test tx link
    assertNotNull(output.getTx());
    if (isTxInput) assertTrue(output.getTx().getInputs().contains(output));
    else assertTrue(output.getTx().getOutputs().contains(output));
    
    // test output values
    TestUtils.testUnsignedBigInteger(output.getAmount());
    if (output.getAccountIndex() != null) assertTrue(output.getAccountIndex() >= 0);
    else {
      if (isTxInput) issues.add("WARNING: notification of " + getOutputState(output) + " spent output missing account index"); // TODO (monero-project): account index not provided when output swept by key image.  could retrieve it but slows tx creation significantly
      else issues.add("ERROR: notification of " + getOutputState(output) + " received output missing account index");
    }
    if (output.getSubaddressIndex() != null) assertTrue(output.getSubaddressIndex() >= 0);
    else {
      if (isTxInput) issues.add("WARNING: notification of " + getOutputState(output) + " spent output missing subaddress index"); // TODO (monero-project): because inputs are not provided, creating fake input from outgoing transfer, which can be sourced from multiple subaddress indices, whereas an output can only come from one subaddress index; need to provide tx inputs to resolve this
      else issues.add("ERROR: notification of " + getOutputState(output) + " received output missing subaddress index");
    }
  }
  
  private static String getOutputState(MoneroOutputWallet output) {
    if (Boolean.FALSE.equals(output.getTx().isLocked())) return "unlocked";
    if (Boolean.TRUE.equals(output.getTx().isConfirmed())) return "confirmed";
    if (Boolean.FALSE.equals(output.getTx().isConfirmed())) return "unconfirmed";
    throw new RuntimeException("Unknown output state: " + output.toString());
  }
  
  // Can stop listening
  @Test
  public void testStopListening() {
    
    // create wallet and start background synchronizing
    MoneroWallet wallet = createWallet(new MoneroWalletConfig());
    
    // add listener
    WalletNotificationCollector listener = new WalletNotificationCollector();
    wallet.addListener(listener);
    try { TimeUnit.SECONDS.sleep(1); }
    catch (Exception e) { throw new RuntimeException(e); }
    
    // remove listener and close
    wallet.removeListener(listener);
    closeWallet(wallet);
  }
  
  // Can be created and receive funds
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
      assertFalse(myListener.getOutputsReceived().isEmpty());
    } finally {
      closeWallet(receiver);
      try { daemon.stopMining(); } catch (Exception e) { }
    }
  }
  
  // Can freeze and thaw outputs
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
      wallet.sweepOutput(new MoneroTxConfig().setAddress(wallet.getPrimaryAddress()).setKeyImage(output.getKeyImage().getHex()));
      fail("Should have thrown error");
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
  
  // Provides key images of spent outputs
  @Test
  public void testInputKeyImages() {
    int accountIndex = 0;
    int subaddressIndex = wallet.getSubaddresses(0).size() > 1 ? 1 : 0; // TODO: avoid subaddress 0 which is more likely to fail transaction sanity check
    
    // test unrelayed single transaction
    testSpendTx(wallet.createTx(new MoneroTxConfig().addDestination(wallet.getPrimaryAddress(), TestUtils.MAX_FEE).setAccountIndex(accountIndex)));
    
    // test unrelayed split transactions
    for (MoneroTxWallet tx : wallet.createTxs(new MoneroTxConfig().addDestination(wallet.getPrimaryAddress(), TestUtils.MAX_FEE).setAccountIndex(accountIndex))) {
      testSpendTx(tx);
    }
    
    // test unrelayed sweep dust
    List<String> dustKeyImages = new ArrayList<String>();
    for (MoneroTxWallet tx : wallet.sweepDust(false)) {
      testSpendTx(tx);
      for (MoneroOutput input : tx.getInputs()) dustKeyImages.add(input.getKeyImage().getHex());
    }
    
    // get available outputs above min amount
    List<MoneroOutputWallet> outputs = wallet.getOutputs(new MoneroOutputQuery().setAccountIndex(accountIndex).setSubaddressIndex(subaddressIndex).setIsSpent(false).setIsFrozen(false).setTxQuery(new MoneroTxQuery().setIsLocked(false)).setMinAmount(TestUtils.MAX_FEE));
    
    // filter dust outputs
    List<MoneroOutputWallet> dustOutputs = new ArrayList<MoneroOutputWallet>();
    for (MoneroOutputWallet output : outputs) {
      if (dustKeyImages.contains(output.getKeyImage().getHex())) dustOutputs.add(output);
    }
    outputs.removeAll(dustOutputs);
    
    // test unrelayed sweep output
    testSpendTx(wallet.sweepOutput(new MoneroTxConfig().setAddress(wallet.getPrimaryAddress()).setKeyImage(outputs.get(0).getKeyImage().getHex())));
    
    // test unrelayed sweep wallet ensuring all non-dust outputs are spent
    Set<String> availableKeyImages = new HashSet<String>();
    for (MoneroOutputWallet output : outputs) availableKeyImages.add(output.getKeyImage().getHex());
    Set<String> sweptKeyImages = new HashSet<String>();
    List<MoneroTxWallet> txs = wallet.sweepUnlocked(new MoneroTxConfig().setAccountIndex(accountIndex).setSubaddressIndex(subaddressIndex).setAddress(wallet.getPrimaryAddress()));
    for (MoneroTxWallet tx : txs) {
      testSpendTx(tx);
      for (MoneroOutput input : tx.getInputs()) sweptKeyImages.add(input.getKeyImage().getHex());
    }
    assertTrue(sweptKeyImages.size() > 0);
    
    // max skipped output is less than max fee amount
    MoneroOutputWallet maxSkippedOutput = null;
    for (MoneroOutputWallet output : outputs) {
      if (!sweptKeyImages.contains(output.getKeyImage().getHex())) {
        if (maxSkippedOutput == null || maxSkippedOutput.getAmount().compareTo(output.getAmount()) < 0) {
          maxSkippedOutput = output;
        }
      }
    }
    assertTrue(maxSkippedOutput == null || maxSkippedOutput.getAmount().compareTo(TestUtils.MAX_FEE) < 0);
  }
  
  private static void testSpendTx(MoneroTxWallet spendTx) {
    assertNotNull(spendTx.getInputs());
    assertTrue(spendTx.getInputs().size() > 0);
    for (MoneroOutput input : spendTx.getInputs()) assertNotNull(input.getKeyImage().getHex());
  }
  
  // Can prove unrelayed txs
  @Test
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
      MoneroWallet verifyingWallet = createWallet(new MoneroWalletConfig());
      
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

  // Can get the default fee priority
  @Test
  public void testGetDefaultFeePriority() {
    MoneroTxPriority defaultPriority = wallet.getDefaultFeePriority();
    assertTrue(defaultPriority.ordinal() > 0);
  }
  
  // --------------------------------- HELPERS --------------------------------
  
  /**
   * Fetches and tests transactions according to the given query.
   * 
   * TODO: ensure each tx passes query filter, same with testGetTransfer and getAndTestOutputs
   */
  private List<MoneroTxWallet> getAndTestTxs(MoneroWallet wallet, MoneroTxQuery query, TxContext ctx, Boolean isExpected) {
    MoneroTxQuery copy = null;
    if (query != null) copy = query.copy();
    List<MoneroTxWallet> txs = wallet.getTxs(query);
    assertNotNull(txs);
    if (Boolean.FALSE.equals(isExpected)) assertTrue(txs.isEmpty());
    if (Boolean.TRUE.equals(isExpected)) assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) testTxWallet(tx, ctx);
    testGetTxsStructure(txs, query);
    if (query != null) assertEquals(copy, query);
    return txs;
  }
  
  /**
   * Fetches and tests transfers according to the given query.
   */
  private List<MoneroTransfer> getAndTestTransfers(MoneroWallet wallet, MoneroTransferQuery query, TxContext ctx, Boolean isExpected) {
    MoneroTransferQuery copy = null;
    if (query != null) copy = query.copy();
    List<MoneroTransfer> transfers = wallet.getTransfers(query);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, transfers.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue(transfers.size() > 0, "Transfers were expected but not found; run send tests?");
    if (ctx == null) ctx = new TxContext();
    ctx.wallet = wallet;
    for (MoneroTransfer transfer : transfers) testTxWallet(transfer.getTx(), ctx);
    if (query != null) assertEquals(copy, query);
    return transfers;
  }

  /**
   * Fetches and tests wallet outputs (i.e. wallet tx outputs) according to the given query.
   */
  private static List<MoneroOutputWallet> getAndTestOutputs(MoneroWallet wallet, MoneroOutputQuery query, Boolean isExpected) {
    MoneroOutputQuery copy = null;
    if (query != null) copy = query.copy();
    List<MoneroOutputWallet> outputs = wallet.getOutputs(query);
    assertEquals(copy, query);
    if (Boolean.FALSE.equals(isExpected)) assertEquals(0, outputs.size());
    if (Boolean.TRUE.equals(isExpected)) assertTrue(outputs.size() > 0, "Outputs were expected but not found; run send tests");
    for (MoneroOutputWallet output : outputs) testOutputWallet(output);
    if (query != null) assertEquals(copy, query);
    return outputs;
  }
  
  /**
   * Provides context or configuration for test methods to test a type.
   */
  public static class TxContext {
    MoneroWallet wallet;
    MoneroTxConfig config;
    Boolean hasOutgoingTransfer;
    Boolean hasIncomingTransfers;
    Boolean hasDestinations;
    Boolean isCopy;                 // indicates if a copy is being tested which means back references won't be the same
    Boolean includeOutputs;
    Boolean isSendResponse;
    Boolean isSweepResponse;
    Boolean isSweepOutputResponse;  // TODO monero-wallet-rpc: this only necessary because sweep_output does not return account index
    public TxContext() { }
    public TxContext(TxContext ctx) {
      if (ctx == null) return;
      this.wallet = ctx.wallet;
      this.config = ctx.config;
      this.hasOutgoingTransfer = ctx.hasOutgoingTransfer;
      this.hasIncomingTransfers = ctx.hasIncomingTransfers;
      this.hasDestinations = ctx.hasDestinations;
      this.isCopy = ctx.isCopy;
      this.includeOutputs = ctx.includeOutputs;
      this.isSendResponse = ctx.isSendResponse;
      this.isSweepResponse = ctx.isSweepResponse;
      this.isSweepOutputResponse = ctx.isSweepOutputResponse;
    }
  }
  
  protected void testInvalidAddressError(MoneroError e) {
    assertEquals("Invalid address", e.getMessage());
  }
  
  protected void testInvalidTxHashError(MoneroError e) {
    assertEquals("TX hash has invalid format", e.getMessage());
  }
  
  protected void testInvalidTxKeyError(MoneroError e) {
    assertEquals("Tx key has invalid format", e.getMessage());
  }
  
  protected void testInvalidSignatureError(MoneroError e) {
    assertEquals("Signature size mismatch with additional tx pubkeys", e.getMessage());
  }
  
  protected void testNoSubaddressError(MoneroError e) {
    assertEquals("Address must not be a subaddress", e.getMessage());
  }
  
  protected void testSignatureHeaderCheckError(MoneroError e) {
    assertEquals("Signature header check error", e.getMessage());
  }
  
  private static void testAccount(MoneroAccount account) {
    
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

  private static void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getAccountIndex() >= 0);
    assertTrue(subaddress.getIndex() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getLabel() == null || !subaddress.getLabel().isEmpty());
    TestUtils.testUnsignedBigInteger(subaddress.getBalance());
    TestUtils.testUnsignedBigInteger(subaddress.getUnlockedBalance());
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    assertNotNull(subaddress.isUsed());
    if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0) assertTrue(subaddress.isUsed());
    assertTrue(subaddress.getNumBlocksToUnlock() >= 0);
  }

  protected void testTxsWallet(List<MoneroTxWallet> txs, TxContext ctx) {

    // test each transaction
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) testTxWallet(tx, ctx);

    // test destinations across transactions
    if (ctx.config != null && ctx.config.getDestinations() != null) {
      int destinationIdx = 0;
      boolean subtractFeeFromDestinations = ctx.config.getSubtractFeeFrom() != null && ctx.config.getSubtractFeeFrom().size() > 0;
      for (MoneroTxWallet tx : txs) {

        // TODO: remove this after >18.3.1 when amounts_by_dest_list is official
        if (tx.getOutgoingTransfer().getDestinations() == null) {
          System.err.println("Tx missing destinations");
          return;
        }

        BigInteger amountDiff = BigInteger.valueOf(0);
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          MoneroDestination ctxDestination = ctx.config.getDestinations().get(destinationIdx);
          assertEquals(ctxDestination.getAddress(), destination.getAddress());
          if (subtractFeeFromDestinations) amountDiff = amountDiff.add(ctxDestination.getAmount().subtract(destination.getAmount()));
          else assertEquals(ctxDestination.getAmount(), destination.getAmount());
          destinationIdx++;
        }
        if (subtractFeeFromDestinations) assertEquals(amountDiff, tx.getFee());
      }
      assertEquals(destinationIdx, ctx.config.getDestinations().size());
    }
  }

  /**
   * Tests a wallet transaction with a test configuration.
   * 
   * @param tx is the wallet transaction to test
   * @param ctx provides test context
   *        ctx.wallet is used to cross reference tx info if available
   *        ctx.config specifies the tx's originating config
   *        ctx.isSendResponse indicates if the tx is built from a send response, which contains additional fields (e.g. key)
   *        ctx.hasDestinations specifies if the tx has an outgoing transfer with destinations, undefined if doesn't matter
   *        ctx.includeOutputs specifies if outputs were fetched and should therefore be expected with incoming transfers
   */
  protected void testTxWallet(MoneroTxWallet tx) { testTxWallet(tx, null); }
  protected void testTxWallet(MoneroTxWallet tx, TxContext ctx) {
    
    // validate / sanitize inputs
    ctx = new TxContext(ctx);
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
        //assertTrue(tx.getReceivedTimestamp() > 0);  // TODO: re-enable when received timestamp returned in wallet rpc
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
      //assertTrue(tx.getReceivedTimestamp() > 0);  // TODO: re-enable when received timestamp returned in wallet rpc
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
      assertNotNull(tx.getMetadata());
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

  private void testTxWalletCopy(MoneroTxWallet tx, TxContext ctx) {
    
    // copy tx and assert deep equality
    MoneroTxWallet copy = tx.copy();
    assertTrue(copy instanceof MoneroTxWallet);
    assertEquals(copy, tx);
    
    // test different references
    if (tx.getOutgoingTransfer() != null) {
      assertTrue(tx.getOutgoingTransfer() != copy.getOutgoingTransfer());
      assertTrue(tx.getOutgoingTransfer().getTx() != copy.getOutgoingTransfer().getTx());
      if (tx.getOutgoingTransfer().getDestinations() != null) {
        assertTrue(tx.getOutgoingTransfer().getDestinations() != copy.getOutgoingTransfer().getDestinations());
        for (int i = 0; i < tx.getOutgoingTransfer().getDestinations().size(); i++) {
          assertEquals(copy.getOutgoingTransfer().getDestinations().get(i), tx.getOutgoingTransfer().getDestinations().get(i));
          assertTrue(tx.getOutgoingTransfer().getDestinations().get(i) != copy.getOutgoingTransfer().getDestinations().get(i));
        }
      }
    }
    if (tx.getIncomingTransfers() != null) {
      for (int i = 0; i < tx.getIncomingTransfers().size(); i++) {
        assertEquals(copy.getIncomingTransfers().get(i), tx.getIncomingTransfers().get(i));
        assertTrue(tx.getIncomingTransfers().get(i) != copy.getIncomingTransfers().get(i));
      }
    }
    if (tx.getInputs() != null) {
      for (int i = 0; i < tx.getInputs().size(); i++) {
        assertEquals(copy.getInputs().get(i), tx.getInputs().get(i));
        assertTrue(tx.getInputs().get(i) != copy.getInputs().get(i));
      }
    }
    if (tx.getOutputs() != null) {
      for (int i = 0; i < tx.getOutputs().size(); i++) {
        assertEquals(copy.getOutputs().get(i), tx.getOutputs().get(i));
        assertTrue(tx.getOutputs().get(i) != copy.getOutputs().get(i));
      }
    }
    
    // test copied tx
    ctx = new TxContext(ctx);
    ctx.isCopy = true;
    if (tx.getBlock() != null) copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy))); // copy block for testing
    testTxWallet(copy, ctx);
    
    // test merging with copy
    MoneroTxWallet merged = copy.merge(copy.copy());
    assertEquals(merged.toString(), tx.toString());
  }
  
  private static void testTransfer(MoneroTransfer transfer, TxContext ctx) {
    if (ctx == null) ctx = new TxContext();
    assertNotNull(transfer);
    TestUtils.testUnsignedBigInteger(transfer.getAmount());
    if (!Boolean.TRUE.equals(ctx.isSweepOutputResponse)) assertTrue(transfer.getAccountIndex() >= 0);
    if (transfer.isIncoming()) testIncomingTransfer((MoneroIncomingTransfer) transfer);
    else testOutgoingTransfer((MoneroOutgoingTransfer) transfer, ctx);
    
    // transfer and tx reference each other
    assertNotNull(transfer.getTx());
    if (!transfer.equals(transfer.getTx().getOutgoingTransfer())) {
      assertNotNull(transfer.getTx().getIncomingTransfers());
      assertTrue(transfer.getTx().getIncomingTransfers().contains(transfer), "Transaction does not reference given transfer");
    }
  }
  
  private static void testIncomingTransfer(MoneroIncomingTransfer transfer) {
    assertTrue(transfer.isIncoming());
    assertFalse(transfer.isOutgoing());
    assertNotNull(transfer.getAddress());
    assertTrue(transfer.getSubaddressIndex() >= 0);
    assertTrue(transfer.getNumSuggestedConfirmations() > 0);
  }
  
  private static void testOutgoingTransfer(MoneroOutgoingTransfer transfer, TxContext ctx) {
    assertFalse(transfer.isIncoming());
    assertTrue(transfer.isOutgoing());
    if (!Boolean.TRUE.equals(ctx.isSendResponse)) assertNotNull(transfer.getSubaddressIndices());
    if (transfer.getSubaddressIndices() != null) {
      assertTrue(transfer.getSubaddressIndices().size() >= 1);
      for (int subaddressIdx : transfer.getSubaddressIndices()) assertTrue(subaddressIdx >= 0);
    }
    if (transfer.getAddresses() != null) {
      assertEquals(transfer.getSubaddressIndices().size(), transfer.getAddresses().size());
      for (String address : transfer.getAddresses()) assertNotNull(address);
    }
    
    // test destinations sum to outgoing amount
    if (transfer.getDestinations() != null) {
      assertTrue(transfer.getDestinations().size() > 0);
      BigInteger sum = BigInteger.valueOf(0);
      for (MoneroDestination destination : transfer.getDestinations()) {
        testDestination(destination);
        sum = sum.add(destination.getAmount());
      }
      if (!transfer.getAmount().equals(sum)) System.out.println(transfer.getTx().getTxSet() == null ? transfer.getTx().toString() : transfer.getTx().getTxSet().toString());
      assertEquals(sum, transfer.getAmount());
    }
  }
  
  private static void testDestination(MoneroDestination destination) {
    MoneroUtils.validateAddress(destination.getAddress(), TestUtils.NETWORK_TYPE);
    TestUtils.testUnsignedBigInteger(destination.getAmount(), true);
  }
  
  private static void testInputWallet(MoneroOutputWallet input) {
    assertNotNull(input);
    assertNotNull(input.getKeyImage());
    assertNotNull(input.getKeyImage().getHex());
    assertTrue(input.getKeyImage().getHex().length() > 0);
    assertNull(input.getAmount()); // must get info separately
  }
  
  private static void testOutputWallet(MoneroOutputWallet output) {
    assertNotNull(output);
    assertTrue(output.getAccountIndex() >= 0);
    assertTrue(output.getSubaddressIndex() >= 0);
    assertTrue(output.getIndex() >= 0);
    assertNotNull(output.isSpent());
    assertNotNull(output.isLocked());
    assertNotNull(output.isFrozen());
    assertNotNull(output.getKeyImage());
    assertTrue(output.getKeyImage().getHex().length() > 0);
    TestUtils.testUnsignedBigInteger(output.getAmount(), true);
    
    // output has circular reference to its transaction which has some initialized fields
    MoneroTxWallet tx = output.getTx();
    assertNotNull(tx);
    assertTrue(tx.getOutputs().contains(output));
    assertNotNull(tx.getHash());
    assertNotNull(tx.isLocked());
    assertEquals(true, tx.isConfirmed());  // TODO monero-wallet-rpc: possible to get unconfirmed outputs?
    assertEquals(true, tx.isRelayed());
    assertEquals(false, tx.isFailed());
    assertTrue(tx.getHeight() > 0);
    
    // test copying
    MoneroOutputWallet copy = output.copy();
    assertTrue(copy != output);
    assertEquals(output.toString(), copy.toString());
    assertNull(copy.getTx());  // TODO: should output copy do deep copy of tx so models are graph instead of tree?  Would need to work out circular references
  }
  
  /**
   * Gets random transactions.
   * 
   * @param wallet is the wallet to query for transactions
   * @param txQuery specifies the transactions to retrieve
   * @param minTxs specifies the minimum number of transactions (null for no minimum)
   * @param maxTxs specifies the maximum number of transactions (null for all filtered transactions)
   * @return List<MoneroTxWallet> are the random transactions
   */
  private static List<MoneroTxWallet> getRandomTransactions(MoneroWallet wallet, MoneroTxQuery txQuery, Integer minTxs, Integer maxTxs) {
    List<MoneroTxWallet> txs = wallet.getTxs(txQuery);
    if (minTxs != null) assertTrue(txs.size() >= minTxs, txs.size() + "/" + minTxs + " transactions found with the query");
    Collections.shuffle(txs);
    if (maxTxs == null) return txs;
    else return txs.subList(0, Math.min(maxTxs, txs.size()));
  }
  
  private static void testCommonTxSets(List<MoneroTxWallet> txs, boolean hasSigned, boolean hasUnsigned, boolean hasMultisig) {
    assertTrue(txs.size() > 0);
    
    // assert that all sets are same reference
    MoneroTxSet set = null;
    for (int i = 0; i < txs.size(); i++) {
      assertTrue(txs.get(i) instanceof MoneroTxWallet);
      if (i == 0) set = txs.get(i).getTxSet();
      else assertTrue(txs.get(i).getTxSet() == set);
    }
    
    // test expected set
    assertNotNull(set);
    if (hasSigned) {
      assertNotNull(set.getSignedTxHex());
      assertTrue(set.getSignedTxHex().length() > 0);
    }
    if (hasUnsigned) {
      assertNotNull(set.getUnsignedTxHex());
      assertTrue(set.getUnsignedTxHex().length() > 0);
    }
    if (hasMultisig) {
      assertNotNull(set.getMultisigTxHex());
      assertTrue(set.getMultisigTxHex().length() > 0);
    }
  }
  
  private static void testCheckTx(MoneroTxWallet tx, MoneroCheckTx check) {
    assertNotNull(check.isGood());
    if (check.isGood()) {
      assertTrue(check.getNumConfirmations() >= 0);
      assertNotNull(check.inTxPool());
      TestUtils.testUnsignedBigInteger(check.getReceivedAmount());
      if (check.inTxPool()) assertEquals(0, (long) check.getNumConfirmations());
      else assertTrue(check.getNumConfirmations() > 0); // TODO (monero-wall-rpc) this fails (confirmations is 0) for (at least one) transaction that has 1 confirmation on testCheckTxKey()
    } else {
      assertNull(check.inTxPool());
      assertNull(check.getNumConfirmations());
      assertNull(check.getReceivedAmount());
    }
  }

  private static void testCheckReserve(MoneroCheckReserve check) {
    assertNotNull(check.isGood());
    if (check.isGood()) {
      TestUtils.testUnsignedBigInteger(check.getTotalAmount());
      assertTrue(check.getTotalAmount().compareTo(BigInteger.valueOf(0)) >= 0);
      TestUtils.testUnsignedBigInteger(check.getUnconfirmedSpentAmount());
      assertTrue(check.getUnconfirmedSpentAmount().compareTo(BigInteger.valueOf(0)) >= 0);
    } else {
      assertNull(check.getTotalAmount());
      assertNull(check.getUnconfirmedSpentAmount());
    }
  }
  
  private static void testDescribedTxSet(MoneroTxSet describedTxSet) {
    assertNotNull(describedTxSet.getTxs());
    assertFalse(describedTxSet.getTxs().isEmpty());
    assertNull(describedTxSet.getSignedTxHex());
    assertNull(describedTxSet.getUnsignedTxHex());
    
    // test each transaction
    // TODO: use common tx wallet test?
    assertNull(describedTxSet.getMultisigTxHex());
    for (MoneroTxWallet parsedTx : describedTxSet.getTxs()) {
      assertTrue(parsedTx.getTxSet() == describedTxSet);
      TestUtils.testUnsignedBigInteger(parsedTx.getInputSum(), true);
      TestUtils.testUnsignedBigInteger(parsedTx.getOutputSum(), true);
      TestUtils.testUnsignedBigInteger(parsedTx.getFee());
      TestUtils.testUnsignedBigInteger(parsedTx.getChangeAmount());
      if (parsedTx.getChangeAmount().equals(BigInteger.valueOf(0))) assertNull(parsedTx.getChangeAddress());
      else MoneroUtils.validateAddress(parsedTx.getChangeAddress(), TestUtils.NETWORK_TYPE);
      assertTrue(parsedTx.getRingSize() > 1);
      assertTrue(parsedTx.getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
      assertTrue(parsedTx.getNumDummyOutputs() >= 0);
      assertFalse(parsedTx.getExtraHex().isEmpty());
      assertTrue(parsedTx.getPaymentId() == null || !parsedTx.getPaymentId().isEmpty());
      assertTrue(parsedTx.isOutgoing());
      assertNotNull(parsedTx.getOutgoingTransfer());
      assertNotNull(parsedTx.getOutgoingTransfer().getDestinations());
      assertFalse(parsedTx.getOutgoingTransfer().getDestinations().isEmpty());
      assertNull(parsedTx.isIncoming());
      for (MoneroDestination destination : parsedTx.getOutgoingTransfer().getDestinations()) {
        testDestination(destination);
      }
    }
  }
  
  private static void testAddressBookEntry(MoneroAddressBookEntry entry) {
    assertTrue(entry.getIndex() >= 0);
    MoneroUtils.validateAddress(entry.getAddress(), TestUtils.NETWORK_TYPE);
    assertNotNull(entry.getDescription());
  }
  
  /**
   * Tests the integrity of the full structure in the given txs from the block down
   * to transfers / destinations.
   */
  private void testGetTxsStructure(List<MoneroTxWallet> txs, MoneroTxQuery query) {
    if (query == null) query = new MoneroTxQuery();
    
    // collect unique blocks in order (using set and list instead of TreeSet for direct portability to other languages)
    Set<MoneroBlock> seenBlocks = new HashSet<MoneroBlock>();
    List<MoneroBlock> blocks = new ArrayList<MoneroBlock>();
    List<MoneroTxWallet> unconfirmedTxs = new ArrayList<MoneroTxWallet>();
    for (MoneroTxWallet tx : txs) {
      if (tx.getBlock() == null) unconfirmedTxs.add(tx);
      else {
        assertTrue(tx.getBlock().getTxs().contains(tx));
        if (!seenBlocks.contains(tx.getBlock())) {
          seenBlocks.add(tx.getBlock());
          blocks.add(tx.getBlock());
        }
      }
    }
    
    // tx hashes must be in order if requested
    if (query.getHashes() != null) {
      assertEquals(txs.size(), query.getHashes().size());
      for (int i = 0; i < query.getHashes().size(); i++) {
        assertEquals(query.getHashes().get(i), txs.get(i).getHash());
      }
    }
    
    // test that txs and blocks reference each other and blocks are in ascending order unless specific tx hashes queried
    int index = 0;
    Long prevBlockHeight = null;
    for (MoneroBlock block : blocks) {
      if (prevBlockHeight == null) prevBlockHeight = block.getHeight();
      else if (query.getHashes() == null) assertTrue(block.getHeight() > prevBlockHeight, "Blocks are not in order of heights: " + prevBlockHeight + " vs " + block.getHeight());
      for (MoneroTx tx : block.getTxs()) {
        assertTrue(tx.getBlock() == block);
        if (query.getHashes() == null) {
          assertEquals(txs.get(index).getHash(), tx.getHash()); // verify tx order is self-consistent with blocks unless txs manually re-ordered by querying by hash
          assertTrue(txs.get(index) == tx);
        }
        index++;
      }
    }
    assertEquals(txs.size(), index + unconfirmedTxs.size());
    
    // test that incoming transfers are in order of ascending accounts and subaddresses
    for (MoneroTxWallet tx : txs) {
      Integer prevAccountIdx = null;
      Integer prevSubaddressIdx = null;
      if (tx.getIncomingTransfers() == null) continue;
      for (MoneroIncomingTransfer transfer : tx.getIncomingTransfers()) {
        if (prevAccountIdx == null) prevAccountIdx = transfer.getAccountIndex();
        else {
          assertTrue(prevAccountIdx <= transfer.getAccountIndex());
          if (prevAccountIdx < transfer.getAccountIndex()) {
            prevSubaddressIdx = null;
            prevAccountIdx = transfer.getAccountIndex();
          }
          if (prevSubaddressIdx == null) prevSubaddressIdx = transfer.getSubaddressIndex();
          else assertTrue(prevSubaddressIdx < transfer.getSubaddressIndex());
        }
      }
    }
    
    // test that outputs are in order of ascending accounts and subaddresses
    for (MoneroTxWallet tx : txs) {
      Integer prevAccountIdx = null;
      Integer prevSubaddressIdx = null;
      if (tx.getOutputs() == null) continue;
      for (MoneroOutputWallet output : tx.getOutputsWallet()) {
        if (prevAccountIdx == null) prevAccountIdx = output.getAccountIndex();
        else {
          assertTrue(prevAccountIdx <= output.getAccountIndex());
          if (prevAccountIdx < output.getAccountIndex()) {
            prevSubaddressIdx = null;
            prevAccountIdx = output.getAccountIndex();
          }
          if (prevSubaddressIdx == null) prevSubaddressIdx = output.getSubaddressIndex();
          else assertTrue(prevSubaddressIdx <= output.getSubaddressIndex(), output.getKeyImage().toString() + " " + prevSubaddressIdx + " > " + output.getSubaddressIndex()); // TODO: this does not test that index < other index if subaddresses are equal
        }
      }
    }
    
    // TODO monero-project wallet2 does not provide ordered blocks or txs
//    // collect given tx hashes
//    List<String> txHashes = new ArrayList<String>();
//    for (MoneroTx tx : txs) txHashes.add(tx.getId());
//
//    // fetch network blocks at tx heights
//    List<Long> heights = new ArrayList<Long>();
//    for (MoneroBlock block : blocks) heights.add(block.getHeight());
//    List<MoneroBlock> networkBlocks = daemon.getBlocksByHeight(heights);
//
//    // collect matching tx hashes from network blocks in order
//    List<String> expectedTxIds = new ArrayList<String>();
//    for (MoneroBlock networkBlock : networkBlocks) {
//      for (String txHash : networkBlock.getTxHashes()) {
//        if (!txHashes.contains(txHash)) expectedTxIds.add(txHash);
//      }
//    }
//
//    // order of hashes must match
//    assertEquals(expectedTxIds, txHashes);
  }
  
  private static Map<Long, Integer> countNumInstances(List<Long> instances) {
    Map<Long, Integer> heightCounts = new HashMap<Long, Integer>();
    for (Long instance : instances) {
      Integer count = heightCounts.get(instance);
      heightCounts.put(instance, count == null ? 1 : count + 1);
    }
    return heightCounts;
  }
  
  private static Set<Long> getModes(Map<Long, Integer> counts) {
    Set<Long> modes = new HashSet<Long>();
    Integer maxCount = null;
    for (Integer count : counts.values()) {
      if (maxCount == null || count > maxCount) maxCount = count;
    }
    for (Entry<Long, Integer> entry : counts.entrySet()) {
      if (entry.getValue() == maxCount) modes.add(entry.getKey());
    }
    return modes;
  }
  
  /**
   * Wallet listener to collect notifications.
   */
  protected class WalletNotificationCollector extends MoneroWalletListener {
    
    private boolean listening = true;
    private List<Long> blockNotifications = new ArrayList<Long>();
    private List<Pair<BigInteger, BigInteger>> balanceNotifications = new ArrayList<Pair<BigInteger, BigInteger>>();
    private List<MoneroOutputWallet> outputsReceived = new ArrayList<MoneroOutputWallet>();
    private List<MoneroOutputWallet> outputsSpent = new ArrayList<MoneroOutputWallet>();
    
    @Override
    public void onNewBlock(long height) {
      assertTrue(listening);
      if (blockNotifications.size() > 0) assertTrue(height == blockNotifications.get(blockNotifications.size() - 1) + 1);
      blockNotifications.add(height);
    }
    
    @Override
    public void onBalancesChanged(BigInteger newBalance, BigInteger newUnlockedBalance) {
      assertTrue(listening);
      if (!balanceNotifications.isEmpty()) {
        Pair<BigInteger, BigInteger> lastNotification = balanceNotifications.get(balanceNotifications.size() - 1);
        assertTrue(!newBalance.equals(lastNotification.getFirst()) || !newUnlockedBalance.equals(lastNotification.getSecond())); // test that balances change
      }
      balanceNotifications.add(new Pair<BigInteger, BigInteger>(newBalance, newUnlockedBalance));
    }
    
    @Override
    public void onOutputReceived(MoneroOutputWallet output) {
      assertTrue(listening);
      outputsReceived.add(output);
    }
    
    @Override
    public void onOutputSpent(MoneroOutputWallet output) {
      assertTrue(listening);
      outputsSpent.add(output);
    }
    
    public List<Long> getBlockNotifications() {
      return blockNotifications;
    }
    
    public List<Pair<BigInteger, BigInteger>> getBalanceNotifications() {
      return balanceNotifications;
    }
    
    public List<MoneroOutputWallet> getOutputsReceived() {
      return getOutputsReceived(null);
    }
    
    public List<MoneroOutputWallet> getOutputsReceived(MoneroOutputQuery query) {
      return Filter.apply(query, outputsReceived);
    }
    
    public List<MoneroOutputWallet> getOutputsSpent() {
      return getOutputsSpent(null);
    }
    
    public List<MoneroOutputWallet> getOutputsSpent(MoneroOutputQuery query) {
      return Filter.apply(query, outputsSpent);
    }
    
    public void setListening(boolean listening) {
      this.listening = listening;
    }
  }
}
