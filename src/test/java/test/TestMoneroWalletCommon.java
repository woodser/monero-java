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

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import common.types.Filter;
import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroUtils;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroVersion;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
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
import utils.StartMining;
import utils.TestUtils;
import utils.WalletEqualityUtils;

/**
 * Runs common tests that every Monero wallet implementation should support.
 */
public abstract class TestMoneroWalletCommon {
  
  // test constants
  protected static final boolean LITE_MODE = false;
  protected static final boolean TEST_NON_RELAYS = true;
  protected static final boolean TEST_RELAYS = true;
  protected static final boolean TEST_NOTIFICATIONS = true;
  protected static final boolean TEST_RESETS = false;
  private static final int MAX_TX_PROOFS = 25;   // maximum number of transactions to check for each proof, undefined to check all
  private static final int SEND_MAX_DIFF = 60;
  private static final int SEND_DIVISOR = 2;
  
  // instance variables
  protected MoneroWallet wallet;        // wallet instance to test
  protected MoneroDaemonRpc daemon;     // daemon instance to test
  private List<MoneroTxWallet> txCache; // local tx cache
  
  public TestMoneroWalletCommon() {
    wallet = getTestWallet();
    daemon = getTestDaemon();
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
  protected MoneroWallet openWallet(String path) { return openWallet(new MoneroWalletConfig().setPath(path)); }
  protected abstract MoneroWallet openWallet(MoneroWalletConfig config);
  
  /**
   * Create a test wallet with default configuration for each wallet type.
   * 
   * @param config configures the wallet to create
   * @return MoneroWallet is the created wallet
   */
  protected abstract MoneroWallet createWallet(MoneroWalletConfig config);
  
  /**
   * Get the wallet's supported languages for the mnemonic phrase.  This is an
   * instance method for wallet rpc and a static utility for other wallets.
   * 
   * @return List<String> are the wallet's supported languages
   */
  protected abstract List<String> getMnemonicLanguages();

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    TestUtils.TX_POOL_WALLET_TRACKER.reset(); // all wallets need to wait for txs to confirm to reliably sync
  }
  
  // ------------------------------ BEGIN TESTS -------------------------------
  
  // Can create a random wallet
  @Test
  public void testCreateWalletRandom() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {
      
      // create random wallet
      wallet = createWallet(new MoneroWalletConfig());
      String path = wallet.getPath();
      Exception e2 = null;
      try {
        MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
        MoneroUtils.validatePrivateViewKey(wallet.getPrivateViewKey());
        MoneroUtils.validatePrivateSpendKey(wallet.getPrivateSpendKey());
        MoneroUtils.validateMnemonic(wallet.getMnemonic());
        if (!(wallet instanceof MoneroWalletRpc)) assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getMnemonicLanguage());  // TODO monero-wallet-rpc: get mnemonic language
      } catch (Exception e) {
        e2 = e;
      }
      wallet.close();
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
    
    // open main test wallet for other tests
    wallet = getTestWallet();
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  // Can create a wallet from a mnemonic phrase
  @Test
  public void testCreateWalletFromMnemonic() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {
      
      // save for comparison
      String primaryAddress = wallet.getPrimaryAddress();
      String privateViewKey = wallet.getPrivateViewKey();
      String privateSpendKey = wallet.getPrivateSpendKey();
      
      // recreate test wallet from mnemonic
      wallet = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      String path = wallet.getPath();
      Exception e2 = null;
      try {
        assertEquals(primaryAddress, wallet.getPrimaryAddress());
        assertEquals(privateViewKey, wallet.getPrivateViewKey());
        assertEquals(privateSpendKey, wallet.getPrivateSpendKey());
        assertEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
        if (!(wallet instanceof MoneroWalletRpc)) assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getMnemonicLanguage());
      } catch (Exception e) {
        e2 = e;
      }
      wallet.close();
      if (e2 != null) throw e2;
      
      // attempt to create wallet with missing mnemonic word
      // TODO: this fails for rpc and jni
//      try {
//        String invalidMnemonic = "memoir hull oilfield desk algebra inbound innocent unplugs fully okay five inflamed giant factual ritual toyed topic snake unhappy guarded tweezers haunted inundate giant";
//        wallet = createWallet(new MoneroWalletConfig().setMnemonic(invalidMnemonic).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
//        assertEquals(invalidMnemonic, wallet.getMnemonic());  // TODO monero-core: detect mnemonic mismatch
//        throw new Error("Should have thrown error");
//      } catch(Exception e) {
//        assertEquals("Invalid mnemonic", e.getMessage());
//      }
      
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
    
    // open main test wallet for other tests
    wallet = getTestWallet();
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  // Can create a wallet from a mnemonic phrase with a seed offset
  @Test
  public void testCreateWalletFromMnemonicWithOffset() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {

      // create test wallet with offset
      wallet = createWallet(new MoneroWalletConfig().setMnemonic(TestUtils.MNEMONIC).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setSeedOffset("my secret offset!"));
      Exception e2 = null;
      try {
        MoneroUtils.validateMnemonic(wallet.getMnemonic());
        assertNotEquals(TestUtils.MNEMONIC, wallet.getMnemonic());
        MoneroUtils.validateAddress(wallet.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
        assertNotEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
        if (!(wallet instanceof MoneroWalletRpc)) assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getMnemonicLanguage());  // TODO monero-wallet-rpc: support
      } catch (Exception e) {
        e2 = e;
      }
      wallet.close();
      if (e2 != null) throw e2;
    } catch (Exception e) {
      e1 = e;
    }
    
    // open main test wallet for other tests
    wallet = getTestWallet();
    if (e1 != null) throw new RuntimeException(e1);
  }
    
  // Can create a wallet from keys
  @Test
  public void testCreateWalletFromKeys() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    Exception e1 = null;  // emulating Java "finally" but compatible with other languages
    try {

      // save for comparison
      String primaryAddress = wallet.getPrimaryAddress();
      String privateViewKey = wallet.getPrivateViewKey();
      String privateSpendKey = wallet.getPrivateSpendKey();
      
      // recreate test wallet from keys
      wallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(primaryAddress).setPrivateViewKey(privateViewKey).setPrivateSpendKey(privateSpendKey).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      String path = wallet.getPath();
      Exception e2 = null;
      try {
        assertEquals(primaryAddress, wallet.getPrimaryAddress());
        assertEquals(privateViewKey, wallet.getPrivateViewKey());
        assertEquals(privateSpendKey, wallet.getPrivateSpendKey());
        if (!wallet.isConnected()) System.out.println("WARNING: wallet created from keys is not connected to authenticated daemon");  // TODO monero-core: keys wallets not connected
        assertTrue("Wallet created from keys is not connected to authenticated daemon", wallet.isConnected());
        if (!(wallet instanceof MoneroWalletRpc)) {
          MoneroUtils.validateMnemonic(wallet.getMnemonic()); // TODO monero-wallet-rpc: cannot get mnemonic from wallet created from keys?
          assertEquals(MoneroWallet.DEFAULT_LANGUAGE, wallet.getMnemonicLanguage());
        }
      } catch (Exception e) {
        e2 = e;
      }
      wallet.close();
      if (e2 != null) throw e2;
      
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
    
    // open main test wallet for other tests
    wallet = getTestWallet();
    if (e1 != null) throw new RuntimeException(e1);
  }
  
  // Can get the wallet's version
  @Test
  public void testGetVersion() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroVersion version = wallet.getVersion();
    assertNotNull(version.getNumber());
    assertTrue(version.getNumber() > 0);
    assertNotNull(version.getIsRelease());
  }
  
  // Can get the wallet's path
  @Test
  public void testGetPath() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create a random wallet
    MoneroWallet wallet = createWallet(new MoneroWalletConfig());
    
    // set a random attribute
    String uuid = UUID.randomUUID().toString();
    wallet.setAttribute("uuid", uuid);
    
    // record the wallet's path then save and close
    String path = wallet.getPath();
    wallet.close(true);
    
    // re-open the wallet using its path
    wallet = openWallet(path);
    
    // test the attribute
    assertEquals(uuid, wallet.getAttribute("uuid"));
    
    // re-open main test wallet
    wallet.close();
    this.wallet = getTestWallet();
  }

  // Can get the mnemonic phrase
  @Test
  public void testGetMnemonic() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String mnemonic = wallet.getMnemonic();
    MoneroUtils.validateMnemonic(mnemonic);
    assertEquals(TestUtils.MNEMONIC, mnemonic);
  }
  
  // Can get the language of the mnemonic phrase
  @Test
  public void testGetMnemonicLanguage() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String language = wallet.getMnemonicLanguage();
    assertEquals(MoneroWallet.DEFAULT_LANGUAGE, language);
  }
  
  // Can get a list of supported languages for the mnemonic phrase
  @Test
  public void testGetMnemonicLanguages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<String> languages = getMnemonicLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }
  
  // Can get the private view key
  @Test
  public void testGetPrivateViewKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String privateViewKey = wallet.getPrivateViewKey();
    MoneroUtils.validatePrivateViewKey(privateViewKey);
  }
  
  // Can get the private spend key
  @Test
  public void testGetPrivateSpendKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String privateSpendKey = wallet.getPrivateSpendKey();
    MoneroUtils.validatePrivateSpendKey(privateSpendKey);
  }
  
  // Can get the public view key
  @Test
  public void testGetPublicViewKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String publicViewKey = wallet.getPublicViewKey();
    MoneroUtils.validatePrivateSpendKey(publicViewKey);
  }
  
  // Can get the public view key
  @Test
  public void testGetPublicSpendKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String publicSpendKey = wallet.getPublicSpendKey();
    MoneroUtils.validatePrivateSpendKey(publicSpendKey);
  }
  
  // Can get the primary address
  @Test
  public void testGetPrimaryAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String primaryAddress = wallet.getPrimaryAddress();
    MoneroUtils.validateAddress(primaryAddress, TestUtils.NETWORK_TYPE);
    assertEquals((wallet.getSubaddress(0, 0)).getAddress(), primaryAddress);
  }
  
  // Can get the address of a subaddress at a specified account and subaddress index
  @Test
  public void testGetSubaddressAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  public void testGetIntegratedAddressFromPaymentId() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // save address for later comparison
    String address = wallet.getSubaddress(0, 0).getAddress();
    
    // test valid payment id
    String paymentId = "03284e41c342f036";
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId);
    assertEquals(integratedAddress.getStandardAddress(), address);
    assertEquals(integratedAddress.getPaymentId(), paymentId);
    
    // test null payment id which generates a new one
    integratedAddress = wallet.getIntegratedAddress(null);
    assertEquals(integratedAddress.getStandardAddress(), address);
    assertFalse(integratedAddress.getPaymentId().isEmpty());
    
    // test invalid payment id
    String invalidPaymentId = "invalid_payment_id_123456";
    try {
      integratedAddress = wallet.getIntegratedAddress(invalidPaymentId);
      fail("Getting integrated address with invalid payment id " + invalidPaymentId + " should have thrown a RPC exception");
    } catch (MoneroError e) {
      //assertEquals(-5, (int) e.getCode());  // TODO: error codes specific to rpc?
      assertEquals("Invalid payment ID: " + invalidPaymentId, e.getMessage());
    }
  }
  
  // Can decode an integrated address
  @Test
  public void testDecodeIntegratedAddress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress("03284e41c342f036");
    MoneroIntegratedAddress decodedAddress = wallet.decodeIntegratedAddress(integratedAddress.toString());
    assertEquals(integratedAddress, decodedAddress);
  }
  
  // Can sync (without progress)
  // TODO: test syncing from start height
  @Test
  public void testSyncWithoutProgress() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    MoneroWalletJni walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.MNEMONIC, TestUtils.FIRST_RECEIVE_HEIGHT);
    try {
      WalletEqualityUtils.testWalletEqualityOnChain(walletGt, wallet);
    } finally {
      walletGt.close();
    }
  }
  
  // Can get the current height that the wallet is synchronized to
  @Test
  public void testGetHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    long height = wallet.getHeight();
    assertTrue(height >= 0);
  }
  
  // Can get a blockchain height by date
  @SuppressWarnings("deprecation")
  @Test
  public void testGetHeightByDate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // collect dates to test starting 100 days ago
    long DAY_MS = 24 * 60 * 60 * 1000;
    Date yesterday = new Date(new Date().getTime() - DAY_MS); // TODO monero-core: today's date can throw exception as "in future" so we test up to yesterday
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
    assertTrue(lastHeight > 0);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount();
    testAccount(createdAccount);
    assertEquals(accountsBefore.size(), (wallet.getAccounts()).size() - 1);
  }
  
  // Can create a new account with a label
  @Test
  public void testCreateAccountWithLabel() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  
  // Can get subaddresses at a specified account index
  @Test
  public void testGetSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  
  // Can get transactions in the wallet
  @Test
  public void testGetTxsWallet() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    boolean nonDefaultIncoming = false;
    List<MoneroTxWallet> txs1 = getCachedTxs();
    List<MoneroTxWallet> txs2 = getAndTestTxs(wallet, null, null, true);
    assertEquals(txs2.size(), txs1.size());
    assertFalse("Wallet has no txs to test", txs1.isEmpty());
    assertEquals("First tx's restore height must match the restore height in TestUtils", TestUtils.FIRST_RECEIVE_HEIGHT, (long) txs1.get(0).getHeight());
    
    // build test context
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    
    // test each transaction
    Map<Long, MoneroBlock> blockPerHeight = new HashMap<Long, MoneroBlock>();
    for (int i = 0; i < txs1.size(); i++) {
      testTxWallet(txs1.get(i), ctx);
      testTxWallet(txs2.get(i), ctx);
      assertEquals(txs1.get(i), txs2.get(i));
      
      // test merging equivalent txs
      MoneroTxWallet copy1 = txs1.get(i).copy();
      MoneroTxWallet copy2 = txs2.get(i).copy();
      if (copy1.isConfirmed()) copy1.setBlock(txs1.get(i).getBlock().copy().setTxs(Arrays.asList(copy1)));
      if (copy2.isConfirmed()) copy2.setBlock(txs2.get(i).getBlock().copy().setTxs(Arrays.asList(copy2)));
      MoneroTxWallet merged = copy1.merge(copy2);
      testTxWallet(merged, ctx);
      
      // find non-default incoming
      if (txs1.get(i).getIncomingTransfers() != null) { // TODO: txs1.get(i).isIncoming()
        for (MoneroIncomingTransfer transfer : txs1.get(i).getIncomingTransfers()) {
          if (transfer.getAccountIndex() != 0 && transfer.getSubaddressIndex() != 0) nonDefaultIncoming = true;
        }
      }
      
      // ensure unique block reference per height
      if (txs2.get(i).isConfirmed()) {
        MoneroBlock block = blockPerHeight.get(txs2.get(i).getHeight());
        if (block == null) blockPerHeight.put(txs2.get(i).getHeight(), txs2.get(i).getBlock());
        else {
          assertEquals(block, txs2.get(i).getBlock());
          assertTrue("Block references for same height must be same", block == txs2.get(i).getBlock());
        }
      }
    }
    
    // ensure non-default account and subaddress tested
    assertTrue("No incoming transfers found to non-default account and subaddress; run send-to-multiple tests first", nonDefaultIncoming);
  }
  
  // Can get transactions by hash
  @Test
  public void testGetTxsByHash() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    int maxNumTxs = 10;  // max number of txs to test
    
    // fetch all txs for testing
    List<MoneroTxWallet> txs = wallet.getTxs();
    assertTrue("Test requires at least 2 txs to fetch by hash", txs.size() > 1);
    
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
    
    // test fetching by hashes as collection
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) txHashes.add(tx.getHash());
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
      assertTrue(("Transaction is not associated with account " + accountIdx + ":\n" + tx.toString()), found);
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
      assertTrue("No incoming transfers to account " + accountIdx + " found:\n" + tx.toString(), found);
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
    assertTrue("No outputs found in txs", found);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get all confirmed txs for testing
    List<MoneroTxWallet> txs = wallet.getTxs(new MoneroTxQuery().setIsConfirmed(true));
    
    // collect all tx heights
    List<Long> txHeights = new ArrayList<Long>();
    for (MoneroTxWallet tx : txs) txHeights.add(tx.getHeight());
    
    // get height that most txs occur at
    Map<Long, Integer> heightCounts = countNumInstances(txHeights);
    assertFalse("Wallet has no confirmed txs; run send tests", heightCounts.isEmpty());
    Set<Long> heightModes = getModes(heightCounts);
    Long modeHeight = heightModes.iterator().next();
    
    // fetch txs at mode height
    List<MoneroTxWallet> modeTxs = wallet.getTxs(new MoneroTxQuery().setHeight(modeHeight));
    assertEquals((int) heightCounts.get(modeHeight), (int) modeTxs.size());
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
      assertTrue("No transactions; run send to multiple test", txs.size() > 0);
        
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
  
  // NOTE: payment hashes are deprecated so this test will require an old wallet to pass
  @Test
  public void testGetTxsWithPaymentIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get random transactions with payment hashes for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, new MoneroTxQuery().setHasPaymentId(true), 3, 5);
    assertFalse("No txs with payment ids to test", randomTxs.isEmpty());
    for (MoneroTxWallet randomTx : randomTxs) {
      assertNotNull(randomTx.getPaymentId());
    }
    
    // get transactions by payment id
    List<String> paymentIds = new ArrayList<String>();
    for (MoneroTxWallet tx : randomTxs) paymentIds.add(tx.getPaymentId());
    assertTrue(paymentIds.size() > 1);
    for (String paymentId : paymentIds) {
      List<MoneroTxWallet> txs = getAndTestTxs(wallet, new MoneroTxQuery().setPaymentId(paymentId), null, null);
      assertEquals(1, txs.size());
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  public void testGetTxsValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // fetch random txs for testing
    List<MoneroTxWallet> randomTxs = getRandomTransactions(wallet, null, 3, 5);
    
    // valid, invalid, and unknown tx hashes for tests
    String txHash = randomTxs.get(0).getHash();
    String invalidHash = "invalid_id";
    String unknownHash1 = "6c4982f2499ece80e10b627083c4f9b992a00155e98bcba72a9588ccb91d0a61";
    String unknownHash2 = "ff397104dd875882f5e7c66e4f852ee134f8cf45e21f0c40777c9188bc92e943";
    
    // fetch unknown tx hash
    try {
      wallet.getTx(unknownHash1);
      fail("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(unknownHash1), e.getMessage());
    }
    
    // fetch unknown tx hash using query
    try {
      wallet.getTxs(new MoneroTxQuery().setHash(unknownHash1));
      throw new Error("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(unknownHash1), e.getMessage());
    }
    
    // fetch unknown tx hash in collection
    try {
      wallet.getTxs(txHash, unknownHash1);
      fail("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(unknownHash1), e.getMessage());
    }
    
    // fetch unknown tx hashes in collection
    try {
      wallet.getTxs(txHash, unknownHash1, unknownHash2);
      fail("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(unknownHash1, unknownHash2), e.getMessage()); // TODO: list all invalid hashes in error description?
    }
    
    // fetch invalid hash
    try {
      wallet.getTx(invalidHash);
      fail("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(invalidHash), e.getMessage());
    }
    
    // fetch invalid hash collection
    try {
      wallet.getTxs(txHash, invalidHash);
      fail("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(invalidHash), e.getMessage());
    }
    
    // fetch invalid hashes in collection
    try {
      wallet.getTxs(txHash, invalidHash, "invalid_hash_2");
      fail("Should have thrown error getting tx hash unknown to wallet");
    } catch (MoneroError e) {
      assertEquals("Wallet missing requested tx hashes: " + Arrays.asList(invalidHash, "invalid_hash_2"), e.getMessage());
    }
    
    // test collection of invalid hashes
    List<String> missingTxHashes = new ArrayList<String>();
    wallet.getTxs(new MoneroTxQuery().setHashes(Arrays.asList(txHash, invalidHash, "invalid_hash_2")), missingTxHashes);
    assertEquals(Arrays.asList(invalidHash, "invalid_hash_2"), missingTxHashes);
  }

  // Can get transfers in the wallet, accounts, and subaddresses
  @Test
  public void testGetTransfers() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
          assertTrue("Subaddresses must overlap", intersections.size() > 0);
        }
      }
    }
    
    // ensure transfer found with non-zero account and subaddress indices
    assertTrue("No transfers found in non-default account and subaddress; run send-to-multiple tests", nonDefaultIncoming);
  }
  
  // Can get transfers with additional configuration
  @Test
  public void testGetTransfersWithQuery() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  }
  
  // Validates inputs when getting transfers
  @Test
  public void testGetTransfersValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int accountIdx = 1;
    int subaddressIdx = 1;
    
    // get incoming transfers
    List<MoneroIncomingTransfer> inTransfers = wallet.getIncomingTransfers();
    assertFalse(inTransfers.isEmpty());
    for (MoneroIncomingTransfer transfer : inTransfers) {
      assertTrue(transfer.isIncoming());
      testTransfer(transfer, null);
    }
    
    // get incoming transfers with query
    inTransfers = wallet.getIncomingTransfers(new MoneroTransferQuery().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx));
    assertFalse(inTransfers.isEmpty());
    for (MoneroIncomingTransfer transfer : inTransfers) {
      assertTrue(transfer.isIncoming());
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);

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
    assertTrue("No outputs found in non-default account and subaddress; run send-to-multiple tests", nonDefaultIncoming);
  }

  // Can get outputs with additional configuration
  @Test
  public void testGetOutputsWithQuery() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  public void testGetOutputsValidateInputs() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
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
  
  // Can get outputs in hex format
  @Test
  public void testGetOutputsHex() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    String outputsHex = wallet.getOutputsHex();
    assertNotNull(outputsHex);  // TODO: this will fail if wallet has no outputs; run these tests on new wallet
    assertTrue(outputsHex.length() > 0);
  }
  
  // Can import outputs in hex format
  @Test
  public void testImportOutputsHex() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get outputs hex
    String outputsHex = wallet.getOutputsHex();
    
    // import outputs hex
    if (outputsHex != null) {
      int numImported = wallet.importOutputsHex(outputsHex);
      assertTrue(numImported > 0);
    }
  }
  
  // Has correct accounting across accounts, subaddresses, txs, transfers, and outputs
  @Test
  public void testAccounting() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    BigInteger walletBalance = wallet.getBalance();
    BigInteger walletUnlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);  // includes subaddresses
    List<MoneroTxWallet> txs = wallet.getTxs();
    
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
    boolean hasUnconfirmedTx = false;
    for (MoneroTxWallet tx : txs) if (tx.inTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent outputs
    BigInteger walletSum = BigInteger.valueOf(0);
    for (MoneroOutputWallet output : wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false))) walletSum = walletSum.add(output.getAmount());
    if (!walletBalance.equals(walletSum)) assertTrue("Wallet balance must equal sum of unspent outputs if no unconfirmed txs", hasUnconfirmedTx);
    
    // account balances are sum of their unspent outputs
    for (MoneroAccount account : accounts) {
      BigInteger accountSum = BigInteger.valueOf(0);
      List<MoneroOutputWallet> accountOutputs = wallet.getOutputs(new MoneroOutputQuery().setAccountIndex(account.getIndex()).setIsSpent(false));
      for (MoneroOutputWallet output : accountOutputs) accountSum = accountSum.add(output.getAmount());
      if (!account.getBalance().equals(accountSum)) assertTrue("Account balance must equal sum of its unspent outputs if no unconfirmed txs", hasUnconfirmedTx);
      
      // subaddress balances are sum of their unspent outputs
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        BigInteger subaddressSum = BigInteger.valueOf(0);
        List<MoneroOutputWallet> subaddressOutputs = wallet.getOutputs(new MoneroOutputQuery().setAccountIndex(account.getIndex()).setSubaddressIndex(subaddress.getIndex()).setIsSpent(false));
        for (MoneroOutputWallet output : subaddressOutputs) subaddressSum = subaddressSum.add(output.getAmount());
        if (!subaddress.getBalance().equals(subaddressSum)) assertTrue("Subaddress balance must equal sum of its unspent outputs if no unconfirmed txs", hasUnconfirmedTx);
      }
    }
  }
  
  // Can check a transfer using the transaction's secret key and the destination
  @Test
  public void testCheckTxKey() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroTxWallet> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxQuery().setIsConfirmed(true).setTransferQuery(new MoneroTransferQuery().setHasDestinations(true)), 1, MAX_TX_PROOFS);
    } catch (AssertionError e) {
      if (e.getMessage().contains("found with")) fail("No txs with outgoing destinations found; run send tests");
      throw e;
    }
    
    // test good checks
    assertTrue("No transactions found with outgoing destinations", txs.size() > 0);
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
      testInvalidTxHashException(e);
    }
    
    // test check with invalid tx hash
    MoneroTxWallet tx = txs.get(0);
    String key = wallet.getTxKey(tx.getHash());
    MoneroDestination destination = tx.getOutgoingTransfer().getDestinations().get(0);
    try {
      wallet.checkTxKey("invalid_tx_id", key, destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxHashException(e);
    }
    
    // test check with invalid key
    try {
      wallet.checkTxKey(tx.getHash(), "invalid_tx_key", destination.getAddress());
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxKeyException(e);
    }
    
    // test check with invalid address
    try {
      wallet.checkTxKey(tx.getHash(), key, "invalid_tx_address");
      throw new RuntimeException("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidAddressException(e);
    }
    
    // test check with different address
    String differentAddress = null;
    for (MoneroTxWallet aTx : getCachedTxs()) {
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get random txs that are confirmed and have outgoing destinations
    List<MoneroTxWallet> txs;
    try {
      txs = getRandomTransactions(wallet, new MoneroTxQuery().setIsConfirmed(true).setTransferQuery(new MoneroTransferQuery().setHasDestinations(true)), 2, MAX_TX_PROOFS);
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
      testInvalidTxHashException(e);
    }
    
    // test check with invalid tx hash
    try {
      wallet.checkTxProof("invalid_tx_id", destination.getAddress(), null, signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxHashException(e);
    }
    
    // test check with invalid address
    try {
      wallet.checkTxProof(tx.getHash(), "invalid_tx_address", null, signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidAddressException(e);
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
      testInvalidSignatureException(e);
    }
  }
  
  // Can prove a spend using a generated signature and no destination public address
  @Test
  public void testCheckSpendProof() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
      testInvalidTxHashException(e);
    }
    
    // test check with invalid tx hash
    try {
      wallet.checkSpendProof("invalid_tx_id", null, signature);
      throw new RuntimeException("Should have thrown exception");
    } catch (MoneroError e) {
      testInvalidTxHashException(e);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get proof of entire wallet
    String signature = wallet.getReserveProofWallet("Test message");
    
    // check proof of entire wallet
    MoneroCheckReserve check = wallet.checkReserveProof(wallet.getPrimaryAddress(), "Test message", signature);
    assertTrue(check.isGood());
    testCheckReserve(check);
    BigInteger balance = wallet.getBalance();
    if (!balance.equals(check.getTotalAmount())) {  // TODO monero-wallet-rpc: this check fails with unconfirmed txs
      List<MoneroTxWallet> unconfirmedTxs = wallet.getTxs(new MoneroTxQuery().setInTxPool(true));
      assertTrue("Reserve amount must equal balance unless wallet has unconfirmed txs", unconfirmedTxs.size() > 0);
    }
    
    // test different wallet address
    String differentAddress = TestUtils.getExternalWalletAddress();
    try {
      wallet.checkReserveProof(differentAddress, "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testNoSubaddressException(e);
    }
    
    // test subaddress
    try {
      wallet.checkReserveProof((wallet.getSubaddress(0, 1)).getAddress(), "Test message", signature);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      testNoSubaddressException(e);
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
      testSignatureHeaderCheckException(e);
    }
  }
  
  // Can prove reserves in an account
  @Test
  public void testGetReserveProofAccount() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    assertTrue("Must have more than one account with non-zero balance; run send-to-multiple tests", numNonZeroTests > 1);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set tx notes
    String uuid = UUID.randomUUID().toString();
    List<MoneroTxWallet> txs = getCachedTxs();
    assertTrue("Test requires 3 or more wallet transactions; run send tests", txs.size() >= 3);
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
  
  // Can get signed key images
  @Test
  public void testGetSignedKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroKeyImage> images = wallet.getKeyImages();
    assertTrue("No signed key images in wallet", images.size() > 0);
    for (MoneroKeyImage image : images) {
      assertTrue(image instanceof MoneroKeyImage);
      assertTrue(image.getHex().length() > 0);
      assertTrue(image.getSignature().length() > 0);
    }
  }
  
  // Can get new key images from the last import
  @Test
  public void testGetNewKeyImagesFromLastImport() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get outputs hex
    String outputsHex = wallet.getOutputsHex();
    
    // import outputs hex
    if (outputsHex != null) {
      int numImported = wallet.importOutputsHex(outputsHex);
      assertTrue(numImported > 0);
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
  // TODO monero core: importing key images can cause erasure of incoming transfers per wallet2.cpp:11957
  @Ignore 
  @Test
  public void testImportKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroKeyImage> images = wallet.getKeyImages();
    assertTrue("Wallet does not have any key images; run send tests", images.size() > 0);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS || TEST_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // collect info from main test wallet
    String primaryAddress = wallet.getPrimaryAddress();
    String privateViewKey = wallet.getPrivateViewKey();
    String privateSpendKey = wallet.getPrivateSpendKey();
    
    // create, sign, and submit transactions using view-only and offline wallets
    MoneroWallet viewOnlyWallet = null;
    MoneroWallet offlineWallet = null;
    try {
      
      // create and sync view-only wallet
      viewOnlyWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(primaryAddress).setPrivateViewKey(privateViewKey).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
      assertEquals(primaryAddress, viewOnlyWallet.getPrimaryAddress());
      assertEquals(privateViewKey, viewOnlyWallet.getPrivateViewKey());
      assertEquals(null, viewOnlyWallet.getPrivateSpendKey());
      assertEquals(null, viewOnlyWallet.getMnemonic());
      assertEquals(null, viewOnlyWallet.getMnemonicLanguage());
      assertTrue(viewOnlyWallet.isViewOnly());
      assertTrue(viewOnlyWallet.isConnected());  // TODO: this fails with monero-wallet-rpc and monerod with authentication
      String viewOnlyPath = viewOnlyWallet.getPath();
      viewOnlyWallet.sync();
      assertTrue(viewOnlyWallet.getTxs().size() > 0);
      
      // export outputs from view-only wallet
      String outputsHex = viewOnlyWallet.getOutputsHex();
      
      // create offline wallet
      viewOnlyWallet.close(true);  // only one wallet open at a time to accommodate testing wallet rpc
      offlineWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(primaryAddress).setPrivateViewKey(privateViewKey).setPrivateSpendKey(privateSpendKey).setServerUri(""));
      assertFalse(offlineWallet.isConnected());
      assertFalse(offlineWallet.isViewOnly());
      if (!(offlineWallet instanceof MoneroWalletRpc)) assertEquals(TestUtils.MNEMONIC, offlineWallet.getMnemonic()); // TODO monero-core: cannot get mnemonic from offline wallet rpc
      String offlineWalletPath = offlineWallet.getPath();
      assertEquals(0, offlineWallet.getTxs().size());
      
      // import outputs to offline wallet
      offlineWallet.importOutputsHex(outputsHex);
      
      // export key images from offline wallet
      List<MoneroKeyImage> keyImages = offlineWallet.getKeyImages();
      
      // import key images to view-only wallet
      offlineWallet.close(true);
      viewOnlyWallet = openWallet(viewOnlyPath);
      viewOnlyWallet.importKeyImages(keyImages);
      
      // create unsigned tx using view-only wallet
      MoneroTxWallet unsignedTx = viewOnlyWallet.createTx(new MoneroTxConfig().setAccountIndex(0).setAddress(primaryAddress).setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("3"))));
      assertNotNull(unsignedTx.getTxSet().getUnsignedTxHex());
      
      // sign tx using offline wallet
      viewOnlyWallet.close(true);
      offlineWallet = openWallet(new MoneroWalletConfig().setPath(offlineWalletPath).setServerUri(""));
      String signedTxHex = offlineWallet.signTxs(unsignedTx.getTxSet().getUnsignedTxHex());
      assertFalse(signedTxHex.isEmpty());
      
      // parse or "describe" unsigned tx set
      MoneroTxSet parsedTxSet = offlineWallet.parseTxSet(unsignedTx.getTxSet());
      testParsedTxSet(parsedTxSet);
      
      // submit signed tx using view-only wallet
      if (TEST_RELAYS) {
        offlineWallet.close();
        viewOnlyWallet = openWallet(viewOnlyPath);
        List<String> txHashes = viewOnlyWallet.submitTxs(signedTxHex);
        assertEquals(1, txHashes.size());
        assertEquals(64, txHashes.get(0).length());
      }
    } finally {
      try { viewOnlyWallet.close(); } catch (Exception e) {}
      try { offlineWallet.close(); } catch (Exception e) {}
      wallet = getTestWallet(); // open main test wallet for other tests
    }
  }
  
  // Can sign and verify messages
  // TODO: test with view-only wallet
  @Test
  public void testSignAndVerifyMessages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
      assertTrue("Index " + idx + " not found in address book indices", found);
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
      MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress(paymentId + i); // create unique integrated address
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
      assertTrue("Index " + idx + " not found in address book indices", found);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  public void testCreatePaymentUri() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test with address and amount
    MoneroTxConfig config1 = new MoneroTxConfig().setAddress(wallet.getAddress(0, 0)).setAmount(BigInteger.valueOf(0));
    String uri = wallet.createPaymentUri(config1);
    MoneroTxConfig config2 = wallet.parsePaymentUri(uri);
    assertEquals(config1, config2);
    
    // test with all fields3
    config1.getDestinations().get(0).setAmount(new BigInteger("425000000000"));
    config1.setPaymentId("03284e41c342f03603284e41c342f03603284e41c342f03603284e41c342f036");
    config1.setRecipientName("John Doe");
    config1.setNote("OMZG XMR FTW");
    uri = wallet.createPaymentUri(config1);
    config2 = wallet.parsePaymentUri(uri);
    assertEquals(config1, config2);
    
    // test with undefined address
    String address = config1.getDestinations().get(0).getAddress();
    config1.getDestinations().get(0).setAddress(null);
    try {
      wallet.createPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroError e) {
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
    config1.getDestinations().get(0).setAddress(address);
    
    // test with invalid payment id
    config1.setPaymentId("bizzup");
    try {
      wallet.createPaymentUri(config1);
      fail("Should have thrown RPC exception with invalid parameters");
    } catch (MoneroError e) {
      assertTrue(e.getMessage().indexOf("Cannot make URI from supplied parameters") >= 0);
    }
  }
  
  // Can start and stop mining
  @Test
  public void testMining() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroMiningStatus status = daemon.getMiningStatus();
    if (status.isActive()) wallet.stopMining();
    wallet.startMining(2l, false, true);
    wallet.stopMining();
  }
  
  // ------------------------------- TEST RELAYS ------------------------------
  
  // Can sync with txs in the pool sent from/to the same account
  // TODO: this test fails because wallet does not recognize pool tx sent from/to same account
  @Test
  public void testSyncWithPoolSameAccounts() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncWithPoolSubmit(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")))
            .setRelay(false));
  }
  
  // Can sync with txs submitted and discarded from the pool
  @Test
  public void testSyncWithPoolSubmitAndDiscard() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    testSyncWithPoolSubmit(new MoneroTxConfig()
            .setAccountIndex(2)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")))
            .setRelay(false));
  }
  
  // Can sync with txs submitted and relayed from the pool
  @Test
  public void testSyncWithPoolSubmitAndRelay() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && !LITE_MODE);
    testSyncWithPoolSubmit(new MoneroTxConfig()
            .setAccountIndex(2)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(new BigInteger("5")))
            .setRelay(true));
  }
  
  private void testSyncWithPoolSubmit(MoneroTxConfig config) {
    
    // wait one time for wallet txs in the pool to clear
    // TODO monero core: update from pool does not prevent creating double spend tx
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
      
      // TODO monero core: this code fails which indicates issues
      boolean runFailingCoreCode = false;
      if (runFailingCoreCode) {
        
        // wallet balances should change
        assertNotEquals("Wallet balance should revert to original after flushing tx from pool without relaying", balanceBefore, wallet.getBalance());
        assertNotEquals("Wallet unlocked balance should revert to original after flushing tx from pool without relaying", unlockedBalanceBefore, wallet.getUnlockedBalance());  // TODO: test exact amounts, maybe in ux test
        
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
        assertEquals("Wallet balance should be same as original since tx was flushed and not relayed", balanceBefore, wallet.getBalance());
        assertEquals("Wallet unlocked balance should be same as original since tx was flushed and not relayed", unlockedBalanceBefore, wallet.getUnlockedBalance());
      } else {
        TestUtils.TX_POOL_WALLET_TRACKER.reset(); // all wallets will need to wait for tx to confirm in order to reliably sync
      }
    }
  }
  
  // Can sync with txs relayed to the pool
  @Test
  public void testSyncWithPoolRelay() {
  org.junit.Assume.assumeTrue(TEST_RELAYS && !LITE_MODE);
    
    // wait one time for wallet txs in the pool to clear
    // TODO monero core: update from pool does not prevent creating double spend tx
   TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
        issues.add("WARNING: creating and sending tx through wallet should succeed after syncing wallet with pool but creates a double spend"); // TODO monero core: this fails meaning wallet did not recognize tx relayed directly to pool
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
      assertEquals("testSyncWithPoolRelay() issues: " + issues, 0, issues.size());
    } finally {
      TestUtils.TX_POOL_WALLET_TRACKER.reset(); // all wallets need to wait for tx to confirm to reliably sync tx
    }
  }
  
  // Can send to an external address
  @Test
  public void testSendToExternal() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    
    // collect balances before
    BigInteger balance1 = wallet.getBalance();
    BigInteger unlockedBalance1 = wallet.getUnlockedBalance();
    
    // send funds to external address
    MoneroTxWallet tx = wallet.createTx(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(TestUtils.getExternalWalletAddress())
            .setAmount(TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3)))
            .setRelay(true));
    
    // collect balances after
    BigInteger balance2 = wallet.getBalance();
    BigInteger unlockedBalance2 = wallet.getUnlockedBalance();
    
    // test balances
    assertTrue(unlockedBalance2.compareTo(unlockedBalance1) < 0); // unlocked balance should decrease
    BigInteger expectedBalance = balance1.subtract(tx.getOutgoingAmount().subtract(tx.getFee()));
    assertEquals("Balance after send was not balance before - net tx amount - fee (5 - 1 != 4 test)", expectedBalance, balance2);
  }
  
  // Can send from multiple subaddresses in a single transaction
  @Test
  public void testSendFromSubaddresses() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendFromMultiple(null);
  }
  
  // Can send from multiple subaddresses in split transactions
  @Test
  public void testSendFromSubaddressesSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendFromMultiple(new MoneroTxConfig().setCanSplit(true));
  }
  
  private void testSendFromMultiple(MoneroTxConfig config) {
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    if (config == null) config = new MoneroTxConfig();
    
    int NUM_SUBADDRESSES = 2; // number of subaddresses to send from
    
    // get first account with (NUM_SUBADDRESSES + 1) subaddresses with unlocked balances
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    assertTrue("This test requires at least 2 accounts; run send-to-multiple tests", accounts.size() >= 2);
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
    assertTrue("Wallet does not have account with " + (NUM_SUBADDRESSES + 1) + " subaddresses with balances; run send-to-multiple tests", hasBalance);
    assertTrue("Wallet is waiting on unlocked funds", unlockedSubaddresses.size() >= NUM_SUBADDRESSES + 1);
    
    // determine the indices of the first two subaddresses with unlocked balances
    List<Integer> fromSubaddressIndices = new ArrayList<Integer>();
    for (int i = 0; i < NUM_SUBADDRESSES; i++) {
      fromSubaddressIndices.add(unlockedSubaddresses.get(i).getIndex());
    }
    
    // determine the amount to send (slightly less than the sum to send from)
    BigInteger sendAmount = BigInteger.valueOf(0);
    for (int fromSubaddressIdx : fromSubaddressIndices) {
      sendAmount = sendAmount.add(srcAccount.getSubaddresses().get(fromSubaddressIdx).getUnlockedBalance()).subtract(TestUtils.MAX_FEE);
    }
    
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
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    MoneroTxConfig configCopy = config.copy();
    txs.addAll(wallet.createTxs(config));
    if (Boolean.FALSE.equals(config.getCanSplit())) assertEquals(1, txs.size());  // must have exactly one tx if no split
    
    // test that config is unchanged
    assertTrue(configCopy != config);
    assertEquals(configCopy, config);
    
    // test that balances of intended subaddresses decreased
    List<MoneroAccount> accountsAfter = wallet.getAccounts(true);
    assertEquals(accounts.size(), accountsAfter.size());
    for (int i = 0; i < accounts.size(); i++) {
      assertEquals(accounts.get(i).getSubaddresses().size(), accountsAfter.get(i).getSubaddresses().size());
      for (int j = 0; j < accounts.get(i).getSubaddresses().size(); j++) {
        MoneroSubaddress subaddressBefore = accounts.get(i).getSubaddresses().get(j);
        MoneroSubaddress subaddressAfter = accountsAfter.get(i).getSubaddresses().get(j);
        if (i == srcAccount.getIndex() && fromSubaddressIndices.contains(j)) {
          assertTrue("Subaddress [" + i + "," + j + "] unlocked balance should have decreased but changed from " + subaddressBefore.getUnlockedBalance().toString() + " to " + subaddressAfter.getUnlockedBalance().toString(), subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) < 0); // TODO: Subaddress [0,1] unlocked balance should have decreased          
        } else {
          assertTrue("Subaddress [" + i + "," + j + "] unlocked balance should not have changed", subaddressAfter.getUnlockedBalance().compareTo(subaddressBefore.getUnlockedBalance()) == 0);          
        }
      }
    }
    
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
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(false));
  }
  
  // Can send to an address in a single transaction with a payment id
  // NOTE: this test will be invalid when payment hashes are fully removed
  @Test
  public void testSendWithPaymentId() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    MoneroIntegratedAddress integratedAddress = wallet.getIntegratedAddress();
    String paymentId = integratedAddress.getPaymentId();
    try {
      testSendToSingle(new MoneroTxConfig().setCanSplit(false).setPaymentId(paymentId + paymentId + paymentId + paymentId));  // 64 character payment id
    } catch (MoneroError e) {
      assertEquals("Standalone payment IDs are obsolete. Use subaddresses or integrated addresses instead", e.getMessage());
    }
  }
  
  // Can send to an address with split transactions
  @Test
  public void testSendSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(true));
  }
  
  // Can create then relay a transaction to send to a single address
  @Test
  public void testCreateThenRelay() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(false));
  }
  
  // Can create then relay split transactions to send to a single address
  @Test
  public void testCreateThenRelaySplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToSingle(new MoneroTxConfig().setCanSplit(true));
  }
  
  private void testSendToSingle(MoneroTxConfig config) {
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
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
    assertTrue("No non-primary subaddress found with sufficient balance", sufficientBalance);
    assertTrue("Wallet is waiting on unlocked funds", fromSubaddress != null);
    
    // get balance before send
    BigInteger balanceBefore = fromSubaddress.getBalance();
    BigInteger unlockedBalanceBefore  = fromSubaddress.getUnlockedBalance();
    
    // init tx config
    BigInteger sendAmount = unlockedBalanceBefore.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    config.setDestinations(new MoneroDestination(address, sendAmount));
    config.setAccountIndex(fromAccount.getIndex());
    config.setSubaddressIndices(fromSubaddress.getIndex());
    MoneroTxConfig configCopy = config.copy();
    
    // send to self
    txs.addAll(wallet.createTxs(config));
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
          assertFalse("Created tx should not be in the pool", txPool.getHash().equals(txCreated.getHash()));
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
      assertTrue("Created txs should be among locked txs", found);
    }
    
    // if tx was relayed, all wallets will need to wait for tx to confirm in order to reliably sync
    if (Boolean.TRUE.equals(config.getRelay())) {
      TestUtils.TX_POOL_WALLET_TRACKER.reset(); // TODO: resetExcept(wallet), or does this test wallet also need to be waited on?
    }
  }
  
  // Can send to multiple addresses in a single transaction
  @Test
  public void testSendToMultiple() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToMultiple(5, 3, false);
  }
  
  // Can send to multiple addresses in split transactions
  @Test
  public void testSendToMultipleSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    testSendToMultiple(1, 15, true);
  }
  
  // Can send dust to multiple addresses in split transactions
  @Test
  public void testSendDustToMultipleSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    BigInteger dustAmt = daemon.getFeeEstimate().divide(BigInteger.valueOf(2));
    testSendToMultiple(5, 3, true, dustAmt);
  }
  
  /**
   * Sends funds from the first unlocked account to multiple accounts and subaddresses.
   * 
   * @param numAccounts is the number of accounts to receive funds
   * @param numSubaddressesPerAccount is the number of subaddresses per account to receive funds
   * @param canSplit specifies if the operation can be split into multiple transactions
   * @param sendAmountPerSubaddress is the amount to send to each subaddress (optional, computed if not given)
   */
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit) { testSendToMultiple(numAccounts, numSubaddressesPerAccount, canSplit, null); }
  private void testSendToMultiple(int numAccounts, int numSubaddressesPerAccount, boolean canSplit, BigInteger sendAmountPerSubaddress) {
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
    assertTrue("Wallet does not have enough balance; load '" + TestUtils.WALLET_NAME + "' with XMR in order to test sending", hasBalance);
    assertNotNull("Wallet is waiting on unlocked funds", srcAccount);
    BigInteger balance = srcAccount.getBalance();
    BigInteger unlockedBalance = srcAccount.getUnlockedBalance();
    
    // get amount to send total and per subaddress
    BigInteger sendAmount = null;
    if (sendAmountPerSubaddress == null) {
      sendAmount = unlockedBalance.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
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
    config.setDestinations(new ArrayList<MoneroDestination>());
    config.setRelay(true);
    config.setCanSplit(canSplit);
    config.setPriority(MoneroTxPriority.NORMAL);
    for (int i = 0; i < destinationAddresses.size(); i++) {
      config.getDestinations().add(new MoneroDestination(destinationAddresses.get(i), sendAmountPerSubaddress));
    }
    MoneroTxConfig configCopy = config.copy();
    
    // send tx(s) with config
    List<MoneroTxWallet> txs = wallet.createTxs(config);
    if (!canSplit) assertEquals(1, txs.size());
    
    // test that config is unchanged
    assertTrue(configCopy != config);
    assertEquals(configCopy, config);
    
    // test that wallet balance decreased
    MoneroAccount account = wallet.getAccount(srcAccount.getIndex());
    assertTrue(account.getBalance().compareTo(balance) < 0);
    assertTrue(account.getUnlockedBalance().compareTo(unlockedBalance) < 0);
    
    // build test context
    config.setCanSplit(canSplit);  // for test context
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = config;
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
          assertTrue(destinationAddresses.contains(destination.getAddress()));
          destinationSum = destinationSum.add(destination.getAmount());
        }
        assertTrue(tx.getOutgoingAmount().equals(destinationSum));  // assert that transfers sum up to tx amount
      }
    }
    
    // assert that outgoing amounts sum up to the amount sent within a small margin
    if (Math.abs(sendAmount.subtract(outgoingSum).longValue()) > SEND_MAX_DIFF) { // send amounts may be slightly different
      fail("Actual send amount is too different from requested send amount: " + sendAmount + " - " + outgoingSum + " = " + sendAmount.subtract(outgoingSum));
    }
  }
  
  // Can sweep individual outputs identified by their key images
  @Test
  public void testSweepOutputs() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // test config
    int numOutputs = 3;
    
    // get outputs to sweep (not spent, unlocked, and amount >= fee)
    List<MoneroOutputWallet> spendableUnlockedOutputs = wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false).setIsLocked(false));
    List<MoneroOutputWallet> outputsToSweep = new ArrayList<MoneroOutputWallet>();
    for (int i = 0; i < spendableUnlockedOutputs.size() && outputsToSweep.size() < numOutputs; i++) {
      if (spendableUnlockedOutputs.get(i).getAmount().compareTo(TestUtils.MAX_FEE) > 0) outputsToSweep.add(spendableUnlockedOutputs.get(i));  // output cannot be swept if amount does not cover fee
    }
    assertTrue("Wallet does not have enough sweepable outputs; run send tests", outputsToSweep.size() >= numOutputs);
    
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
          assertTrue("Output should be spent", afterOutput.isSpent());
        }
      }
    }
  }
  
  // Can sweep dust without relaying
  @Test
  public void testSweepDustNoRelay() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // generate non-relayed transactions to sweep dust
    List<MoneroTxWallet> txs = null;
    try {
      txs = wallet.sweepDust(false);
    } catch (MoneroError e) {
      assertEquals("No dust to sweep", e.getMessage());
      return;
    }
    
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
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // sweep dust which will throw exception if no dust to sweep (dust does not exist after rct) 
    List<MoneroTxWallet> txs = null;
    try {
      txs = wallet.sweepDust(true);
    } catch (MoneroError e) {
      assertEquals("No dust to sweep", e.getMessage());
      return;
    }
    
    // if dust swept, test txs
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = null;
    ctx.isSendResponse = true;
    ctx.isSweepResponse = true;
    assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
    }
  }
  
  // Supports multisig wallets
  @Test
  public void testMultisig() {
    org.junit.Assume.assumeTrue(!LITE_MODE && TEST_RELAYS);
    try {
      
      // test n/n
      testMultisig(2, 2, false);
      //testMultisig(3, 3, false);
      //testMultisig(4, 4, false);
      
      // test (n-1)/n
      testMultisig(2, 3, false);
      //testMultisig(3, 4, false);
      //testMultisig(5, 6, false);
      
      // test m/n
      testMultisig(2, 4, true);
      //testMultisig(3, 5, false);
      //testMultisig(3, 7, false);
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
    }
  }
  
  private void testMultisig(int m, int n, boolean testTx) {
    System.out.println("testMultisig(" + m + ", " + n + ")");
    String BEGIN_MULTISIG_NAME = "begin_multisig_wallet";
    MoneroWallet curWallet = null;
    try {
      
      // wait for txs to clear pool
      TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);    
      
      // set name attribute of test wallet at beginning of test
      wallet.setAttribute("name", BEGIN_MULTISIG_NAME);
      wallet.save();
      wallet.close();
      
      // create n wallets and prepare multisig hexes
      List<String> preparedMultisigHexes = new ArrayList<String>();
      List<String> walletIds = new ArrayList<String>();
      for (int i = 0; i < n; i++) {
        MoneroWallet wallet = createWallet(new MoneroWalletConfig());
        walletIds.add(wallet.getPath());
        wallet.setAttribute("name", wallet.getPath());  // set the name of each wallet as an attribute
        preparedMultisigHexes.add(wallet.prepareMultisig());
        //System.out.println("PREPARED HEX: " + preparedMultisigHexes.get(preparedMultisigHexes.size() - 1));
        wallet.close(true);
      }

      // make wallets multisig
      String address = null;
      List<String> madeMultisigHexes = new ArrayList<String>();
      for (int i = 0; i < walletIds.size(); i++) {
        
        // open the wallet
        MoneroWallet wallet = openWallet(walletIds.get(i));
        assertEquals(walletIds.get(i), wallet.getAttribute("name"));
        
        // collect prepared multisig hexes from wallet's peers
        List<String> peerMultisigHexes = new ArrayList<String>();
        for (int j = 0; j < walletIds.size(); j++) if (j != i) peerMultisigHexes.add(preparedMultisigHexes.get(j));

        // make the wallet multisig
        MoneroMultisigInitResult result = wallet.makeMultisig(peerMultisigHexes, m, TestUtils.WALLET_PASSWORD);
        //System.out.println("MADE RESULT: " + JsonUtils.serialize(result));
        if (address == null) address = result.getAddress();
        else assertEquals(address, result.getAddress());
        madeMultisigHexes.add(result.getMultisigHex());
        wallet.close(true);
      }
      
      // handle m/n which exchanges keys n - m times
      if (m != n) {
        address = null;
        
        // exchange keys n - m times
        assertEquals(n, madeMultisigHexes.size());
        List<String> prevMultisigHexes = madeMultisigHexes;
        for (int i = 0; i < n - m; i++) {
          //System.out.println("Exchanging multisig keys round " + (i + 1) + " / " + (n - m));
          
          // exchange multisig keys with each wallet and collect results
          List<String> exchangeMultisigHexes = new ArrayList<String>();
          for (int j = 0; j < walletIds.size(); j++) {
            String walletId = walletIds.get(j);
            
            // open the wallet
            MoneroWallet wallet = openWallet(walletId);
            assertEquals(walletIds.get(j), wallet.getAttribute("name"));
            
            // collect the multisig hexes of the wallet's peers from last round
            List<String> peerMultisigHexes = new ArrayList<String>();
            for (int k = 0; k < walletIds.size(); k++) if (k != j) peerMultisigHexes.add(prevMultisigHexes.get(k));
            
            // import the multisig hexes of the wallet's peers
            MoneroMultisigInitResult result = wallet.exchangeMultisigKeys(peerMultisigHexes, TestUtils.WALLET_PASSWORD);
            //System.out.println("EXCHANGED MULTISIG KEYS RESULT: " + JsonUtils.serialize(result));
            
            // test result
            if (i == n - m - 1) {  // result on last round has address and not multisig hex to share
              assertNotNull(result.getAddress());
              assertFalse(result.getAddress().isEmpty());
              if (address == null) address = result.getAddress();
              else assertEquals(address, result.getAddress());
              assertNull(result.getMultisigHex());
            } else {
              assertNotNull(result.getMultisigHex());
              assertFalse(result.getMultisigHex().isEmpty());
              assertNull(result.getAddress());
              exchangeMultisigHexes.add(result.getMultisigHex());
            }
            
            //wallet.save();
            wallet.close(true);
          }
          
          // use results for next round of exchange
          prevMultisigHexes = exchangeMultisigHexes;
        }
      }
      
      // print final multisig address
      curWallet = openWallet(walletIds.get(0));
      assertEquals(walletIds.get(0), curWallet.getAttribute("name"));
      //System.out.println("FINAL MULTISIG ADDRESS: " + curWallet.getPrimaryAddress());
      curWallet.close();
      
      // test sending a multisig transaction if configured
      if (testTx) {
        
        System.out.println("Creating account");
        
        // create an account in the first multisig wallet to receive funds to
        curWallet = openWallet(walletIds.get(0));
        assertEquals(walletIds.get(0), curWallet.getAttribute("name"));
        curWallet.createAccount();
        
        // get destinations to subaddresses within the account of the multisig wallet
        int accountIdx = 1;
        List<MoneroDestination> destinations = new ArrayList<MoneroDestination>();
        for (int i = 0; i < 3; i++) {
          curWallet.createSubaddress(accountIdx);
          destinations.add(new MoneroDestination(curWallet.getAddress(accountIdx, i), TestUtils.MAX_FEE.multiply(BigInteger.valueOf(2))));
        }
        curWallet.close(true);
        
        
        // send funds from the main test wallet to destinations in the first multisig wallet
        curWallet = getTestWallet();  // get / open the main test wallet
        assertEquals(BEGIN_MULTISIG_NAME, curWallet.getAttribute("name"));
        assertTrue(curWallet.getBalance().compareTo(BigInteger.valueOf(0)) > 0);
        System.out.println("Sending funds from main wallet");
        curWallet.createTx(new MoneroTxConfig().setAccountIndex(0).setDestinations(destinations).setRelay(true));
        String returnAddress = curWallet.getPrimaryAddress(); // funds will be returned to this address from the multisig wallet
        
        // open the first multisig participant
        curWallet = openWallet(walletIds.get(0));
        assertEquals(walletIds.get(0), curWallet.getAttribute("name"));
        testMultisigInfo(curWallet.getMultisigInfo(), m, n);
        curWallet.startSyncing();
        
        System.out.println("Starting mining");
        
        // attempt to start mining
        try { StartMining.startMining(); }
        catch (MoneroError e) { if ("BUSY".equals(e.getMessage())) throw e; }
        
        // wait for the multisig wallet's funds to unlock // TODO: replace with MoneroWalletListener.onOutputReceived() which is called when output unlocked
        Long lastNumConfirmations = null;
        while (true) {
          
          // wait a moment
          try { TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL); }
          catch (InterruptedException e) {  throw new RuntimeException(e); }
          
          // fetch and test outputs
          List<MoneroOutputWallet> outputs = curWallet.getOutputs();
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
        
        // multisig wallet should have unlocked balance in account 1 subaddresses 0-3
        assertEquals(walletIds.get(0), curWallet.getAttribute("name"));
        for (int i = 0; i < 3; i++) {
          assertTrue(curWallet.getUnlockedBalance(1, i).compareTo(BigInteger.valueOf(0)) > 0);
        }
        List<MoneroOutputWallet> outputs = curWallet.getOutputs(new MoneroOutputQuery().setAccountIndex(1));
        assertFalse(outputs.isEmpty());
        if (outputs.size() < 3) System.out.println("WARNING: not one output per subaddress?");
        //assertTrue(outputs.size() >= 3);  // TODO
        for (MoneroOutputWallet output : outputs) assertFalse(output.isLocked());
        
        // wallet requires importing multisig to be reliable
        assertTrue(curWallet.isMultisigImportNeeded());
        
        // attempt creating and relaying transaction without synchronizing with participants
        try {
          curWallet.createTxs(new MoneroTxConfig().setAccountIndex(1).setAddress(returnAddress).setAmount(TestUtils.MAX_FEE.multiply(BigInteger.valueOf(3))));
          throw new RuntimeException("Should have failed sending funds without synchronizing with peers");
        } catch (MoneroError e) {
          assertEquals("No transaction created", e.getMessage());
        }
        
        // synchronize the multisig participants since receiving outputs
        System.out.println("Synchronizing participants");
        curWallet = synchronizeMultisigParticipants(curWallet, walletIds);
        assertEquals(walletIds.get(0), curWallet.getAttribute("name"));
        
        // attempt relaying created transactions without co-signing
        try {
          curWallet.createTxs(new MoneroTxConfig().setAddress(returnAddress).setAmount(TestUtils.MAX_FEE).setAccountIndex(1).setSubaddressIndex(0).setRelay(true));
          throw new RuntimeException("Should have failed");
        } catch (Exception e) {
          assertTrue(e instanceof MoneroError);
          assertEquals("Cannot relay multisig transaction until co-signed", e.getMessage());
        }
        
        // send funds from a subaddress in the multisig wallet
        System.out.println("Sending");
        List<MoneroTxWallet> txs = curWallet.createTxs(new MoneroTxConfig().setAddress(returnAddress).setAmount(TestUtils.MAX_FEE).setAccountIndex(1).setSubaddressIndex(0));
        assertFalse(txs.isEmpty());
        MoneroTxSet txSet = txs.get(0).getTxSet();
        assertNotNull(txSet.getMultisigTxHex());
        assertNull(txSet.getSignedTxHex());
        assertNull(txSet.getUnsignedTxHex());
        
        // parse multisig tx hex and test
        testParsedTxSet(curWallet.parseTxSet(txSet));
        curWallet.close(true);
        
        // sign the tx with participants 1 through m - 1 to meet threshold
        String multisigTxHex = txSet.getMultisigTxHex();
        System.out.println("Signing");
        for (int i = 1; i < m; i++) {
          curWallet = openWallet(walletIds.get(i));
          MoneroMultisigSignResult result = curWallet.signMultisigTxHex(multisigTxHex);
          multisigTxHex = result.getSignedMultisigTxHex();
          curWallet.close(true);
        }
        
        //System.out.println("Submitting signed multisig tx hex: " + multisigTxHex);
        
        // submit the signed multisig tx hex to the network
        System.out.println("Submitting");
        curWallet = openWallet(walletIds.get(0));
        List<String> txHashes = curWallet.submitMultisigTxHex(multisigTxHex);
        curWallet.save();
        
        // synchronize the multisig participants since spending outputs
        System.out.println("Synchronizing participants");
        curWallet = synchronizeMultisigParticipants(curWallet, walletIds);
        
        // fetch the wallet's multisig txs
        List<MoneroTxWallet> multisigTxs = curWallet.getTxs(new MoneroTxQuery().setHashes(txHashes));
        assertEquals(multisigTxs.size(), txHashes.size());
        
        // sweep an output from subaddress [1,1]
        outputs = curWallet.getOutputs(new MoneroOutputQuery().setAccountIndex(1).setSubaddressIndex(1));
        assertFalse(outputs.isEmpty());
        assertFalse(outputs.get(0).isSpent());
        txSet = curWallet.sweepOutput(new MoneroTxConfig().setAddress(returnAddress).setKeyImage(outputs.get(0).getKeyImage().getHex()).setRelay(true)).getTxSet();
        assertNotNull(txSet.getMultisigTxHex());
        assertNull(txSet.getSignedTxHex());
        assertNull(txSet.getUnsignedTxHex());
        assertFalse(txSet.getTxs().isEmpty());
        
        // parse multisig tx hex and test
        testParsedTxSet(curWallet.parseTxSet(txSet));
        curWallet.close();
        
        // sign the tx with participants 1 through m - 1 to meet threshold
        multisigTxHex = txSet.getMultisigTxHex();
        System.out.println("Signing sweep output");
        for (int i = 1; i < m; i++) {
          curWallet = openWallet(walletIds.get(i));
          MoneroMultisigSignResult result = curWallet.signMultisigTxHex(multisigTxHex);
          multisigTxHex = result.getSignedMultisigTxHex();
          curWallet.close(true);
        }
        
        // submit the signed multisig tx hex to the network
        System.out.println("Submitting sweep output");
        curWallet = openWallet(walletIds.get(0));
        txHashes = curWallet.submitMultisigTxHex(multisigTxHex);
        curWallet.save();
        
        // synchronize the multisig participants since spending outputs
        System.out.println("Synchronizing participants");
        curWallet = synchronizeMultisigParticipants(curWallet, walletIds);
        
        // fetch the wallet's multisig txs
        multisigTxs = curWallet.getTxs(new MoneroTxQuery().setHashes(txHashes));
        assertEquals(multisigTxs.size(), txHashes.size());
        
        // sweep remaining balance
        System.out.println("Sweeping");
        txs = curWallet.sweepUnlocked(new MoneroTxConfig().setAddress(returnAddress).setAccountIndex(1).setRelay(true)); // TODO: test multisig with sweepEachSubaddress which will generate multiple tx sets without synchronizing participants
        assertFalse("No txs created on sweepUnlocked", txs.isEmpty());
        txSet = txs.get(0).getTxSet();
        for (MoneroTxWallet tx : txs) {
          assertTrue(tx.getTxSet() == txSet);
          assertTrue(tx.getTxSet().getTxs().contains(tx));
        }
        assertNotNull(txSet.getMultisigTxHex());
        assertNull(txSet.getSignedTxHex());
        assertNull(txSet.getUnsignedTxHex());
        
        // parse multisig tx hex and test
        testParsedTxSet(curWallet.parseTxSet(txSet));
        curWallet.close();
        
        // sign the tx with participants 1 through m - 1 to meet threshold
        multisigTxHex = txSet.getMultisigTxHex();
        System.out.println("Signing sweep");
        for (int i = 1; i < m; i++) {
          curWallet = openWallet(walletIds.get(i));
          MoneroMultisigSignResult result = curWallet.signMultisigTxHex(multisigTxHex);
          multisigTxHex = result.getSignedMultisigTxHex();
          curWallet.close(true);
        }
        
        // submit the signed multisig tx hex to the network
        System.out.println("Submitting sweep");
        curWallet = openWallet(walletIds.get(0));
        txHashes = curWallet.submitMultisigTxHex(multisigTxHex);
        curWallet.save();
        
        // synchronize the multisig participants since spending outputs
        System.out.println("Synchronizing participants");
        curWallet = synchronizeMultisigParticipants(curWallet, walletIds);
        
        // fetch the wallet's multisig txs
        multisigTxs = curWallet.getTxs(new MoneroTxQuery().setHashes(txHashes));
        assertEquals(multisigTxs.size(), txHashes.size());
        
        curWallet.close(true);
      }
    } finally {
      
      // re-open main test wallet
      if (curWallet != null && !curWallet.isClosed()) curWallet.close();  // TODO: MoneroWallet.isClosed()
      wallet = getTestWallet();
      assertEquals(BEGIN_MULTISIG_NAME, wallet.getAttribute("name"));
    }    
  }
  
  private MoneroWallet synchronizeMultisigParticipants(MoneroWallet currentWallet, List<String> walletIds) {
    
    // close the current wallet
    String path = currentWallet.getPath();
    currentWallet.close(true);

    // collect multisig hex of all participants to synchronize
    List<String> multisigHexes = new ArrayList<String>();
    for (String walletId : walletIds) {
      MoneroWallet wallet = openWallet(walletId);
      wallet.sync();
      multisigHexes.add(wallet.getMultisigHex());
      wallet.close(true);
    }
    
    // import each wallet's peer multisig hex
    for (int i = 0; i < walletIds.size(); i++) {
      List<String> peerMultisigHexes = new ArrayList<String>();
      for (int j = 0; j < walletIds.size(); j++) if (j != i) peerMultisigHexes.add(multisigHexes.get(j));
      MoneroWallet wallet = openWallet(walletIds.get(i));
      wallet.sync();  // TODO monero-core: creating multisig tx fails if wallet not explicitly synced before import_multisig_hex: https://github.com/monero-project/monero/issues/6850
      wallet.importMultisigHex(peerMultisigHexes);
      wallet.close(true);
    }
    
    // re-open current wallet and return
    MoneroWallet endWallet = openWallet(path);
    endWallet.sync();
    return endWallet;
  }
  
  private static void testMultisigInfo(MoneroMultisigInfo info, int m, int n) {
    assertTrue(info.isMultisig());
    assertTrue(info.isReady());
    assertEquals(m, (int) info.getThreshold());
    assertEquals(n, (int) info.getNumParticipants());
  }
  
  // --------------------------- NOTIFICATION TESTS ---------------------------
  
  // TODO: test sending to multiple accounts
  
  // Can update a locked tx sent from/to the same account as blocks are added to the chain
  @Test
  public void testUpdateLockedSameAccount() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setAccountIndex(0)
            .setUnlockHeight(daemon.getHeight() + 3l)
            .setCanSplit(false)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  // Can update split locked txs sent from/to the same account as blocks are added to the chain
  @Test
  public void testUpdateLockedSameAccountSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getPrimaryAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setUnlockHeight(daemon.getHeight() + 3l)
            .setCanSplit(true)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  // Can update a locked tx sent from/to different accounts as blocks are added to the chain
  @Test
  public void testUpdateLockedDifferentAccounts() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getSubaddress(1, 0).getAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setUnlockHeight(daemon.getHeight() + 3l)
            .setCanSplit(false)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  // Can update locked, split txs sent from/to different accounts as blocks are added to the chain
  @Test
  public void testUpdateLockedDifferentAccountsSplit() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && TEST_NOTIFICATIONS && !LITE_MODE);
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(wallet.getSubaddress(1, 0).getAddress())
            .setAmount(TestUtils.MAX_FEE)
            .setAccountIndex(0)
            .setUnlockHeight(daemon.getHeight() + 3l)
            .setRelay(true);
    testSendAndUpdateTxs(config);
  }
  
  /**
   * Tests sending a tx with an unlock height then tracking and updating it as
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
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
        MoneroBlockHeader header = daemon.getNextBlockHeader();
        System.out.println("*** Block " + header.getHeight() + " added to chain ***");
        
        // give wallet time to catch up, otherwise incoming tx may not appear
        // TODO: this lets new block slip, okay?
        try {
          TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL);
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
    org.junit.Assume.assumeTrue(TEST_RESETS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
    assertTrue("Test requires balance in at least " + (NUM_SUBADDRESSES_TO_SWEEP + 1) + " subaddresses from non-default acccount; run send-to-multiple tests", subaddressesBalance.size() >= NUM_SUBADDRESSES_TO_SWEEP + 1);
    assertTrue("Wallet is waiting on unlocked funds", subaddressesUnlocked.size() >= NUM_SUBADDRESSES_TO_SWEEP + 1);
    
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
    org.junit.Assume.assumeTrue(TEST_RESETS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
    assertTrue("Test requires balance greater than the fee in at least " + (NUM_ACCOUNTS_TO_SWEEP + 1) + " non-default accounts; run send-to-multiple tests", accountsBalance.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    assertTrue("Wallet is waiting on unlocked funds", accountsUnlocked.size() >= NUM_ACCOUNTS_TO_SWEEP + 1);
    
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
  @Ignore // disabled so tests don't sweep the whole wallet
  public void testSweepWalletByAccounts() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    testSweepWallet(null);
  }
  
  // Can sweep the whole wallet by subaddresses
  @Test
  @Ignore // disabled so tests don't sweep the whole wallet
  public void testSweepWalletBySubaddresses() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    testSweepWallet(true);
  }
  
  private void testSweepWallet(Boolean sweepEachSubaddress) {
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // verify 2 subaddresses with enough unlocked balance to cover the fee
    List<MoneroSubaddress> subaddressesBalance = new ArrayList<MoneroSubaddress>();
    List<MoneroSubaddress> subaddressesUnlocked = new ArrayList<MoneroSubaddress>();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        if (subaddress.getBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesBalance.add(subaddress);
        if (subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0) subaddressesUnlocked.add(subaddress);
      }
    }
    assertTrue("Test requires multiple accounts with a balance greater than the fee; run send to multiple first", subaddressesBalance.size() >= 2);
    assertTrue("Wallet is waiting on unlocked funds", subaddressesUnlocked.size() >= 2);
    
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
    List<MoneroOutputWallet> spendableOutputs = wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false).setIsLocked(false));
    for (MoneroOutputWallet spendableOutput : spendableOutputs) {
      assertTrue("Unspent output should have been swept\n" + spendableOutput.toString(), spendableOutput.getAmount().compareTo(TestUtils.MAX_FEE) < 0);
    }
    
    // all subaddress unlocked balances must be less than fee
    subaddressesBalance.clear();
    subaddressesUnlocked.clear();
    for (MoneroAccount account : wallet.getAccounts(true)) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        assertTrue("No subaddress should have more unlocked than the fee", subaddress.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) < 0);
      }
    }
  }
  
  // Can rescan the blockchain
  @Test
  @Ignore // disabled so tests don't delete local cache
  public void testRescanBlockchain() {
    org.junit.Assume.assumeTrue(TEST_RESETS);
    wallet.rescanBlockchain();
    for (MoneroTxWallet tx : wallet.getTxs()) {
      testTxWallet(tx, null);
    }
  }
  
  // Can save and close the wallet in a single call
  @Test
  public void testSaveAndClose() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create a random wallet
    MoneroWallet wallet = createWallet(new MoneroWalletConfig());
    String path = wallet.getPath();
            
    // set an attribute
    String uuid = UUID.randomUUID().toString();
    wallet.setAttribute("id", uuid);
    
    // close the wallet without saving
    wallet.close();
    
    // re-open the wallet and ensure attribute was not saved
    wallet = openWallet(path);
    assertEquals(null, wallet.getAttribute("id"));
    
    // set the attribute and close with saving
    wallet.setAttribute("id", uuid);
    wallet.close(true);
    
    // re-open the wallet and ensure attribute was saved
    wallet = openWallet(path);
    assertEquals(uuid, wallet.getAttribute("id"));
    
    // re-open main test wallet
    wallet.close(); // defaults to not saving
    this.wallet = getTestWallet();
  }
  
  // --------------------------------- HELPERS --------------------------------
  
  private List<MoneroTxWallet> getCachedTxs() {
    if (txCache != null) return txCache;
    txCache = wallet.getTxs();
    testGetTxsStructure(txCache);
    return txCache;
  }
  
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
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Transfers were expected but not found; run send tests?", transfers.size() > 0);
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
    if (Boolean.TRUE.equals(isExpected)) assertTrue("Outputs were expected but not found; run send tests", outputs.size() > 0);
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
  
  protected void testInvalidAddressException(MoneroError e) {
    assertEquals("Invalid address", e.getMessage());
  }
  
  protected void testInvalidTxHashException(MoneroError e) {
    assertEquals("TX hash has invalid format", e.getMessage());
  }
  
  protected void testInvalidTxKeyException(MoneroError e) {
    assertEquals("Tx key has invalid format", e.getMessage());
  }
  
  protected void testInvalidSignatureException(MoneroError e) {
    assertEquals("Signature size mismatch with additional tx pubkeys", e.getMessage());
  }
  
  protected void testNoSubaddressException(MoneroError e) {
    assertEquals("Address must not be a subaddress", e.getMessage());
  }
  
  protected void testSignatureHeaderCheckException(MoneroError e) {
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
      assertTrue("Subaddress balances " + balance + " does not equal account " + account.getIndex() + " balance " + account.getBalance(), account.getBalance().equals(balance));
      assertTrue("Subaddress unlocked balances " + unlockedBalance + " does not equal account " + account.getIndex() + " unlocked balance " + account.getUnlockedBalance(), account.getUnlockedBalance().equals(unlockedBalance));
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
      assertNull("if either sendRequest or isSendResponse is defined, they must both be defined", ctx.isSendResponse);
      assertNull("if either sendRequest or isSendResponse is defined, they must both be defined", ctx.config);
    }
    
    // test common field types
    testTxWalletTypes(tx, ctx);
    
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
      assertEquals(config.getUnlockHeight() != null ? config.getUnlockHeight() : 0, (long) tx.getUnlockHeight());
      assertNull(tx.getBlock());
      assertTrue(tx.getKey().length() > 0);
      assertNotNull(tx.getFullHex());
      assertTrue(tx.getFullHex().length() > 0);
      assertNotNull(tx.getMetadata());
      assertNull(tx.getReceivedTimestamp());
      assertTrue(tx.isLocked());
      
      // test locked state
      if (tx.getUnlockHeight() == 0) assertEquals(tx.isConfirmed(), !tx.isLocked());
      else assertEquals(true, tx.isLocked());
      for (MoneroOutputWallet output : tx.getOutputsWallet()) {
        assertEquals(tx.isLocked(), output.isLocked());
      }
      
      // test destinations of sent tx
      assertEquals(config.getDestinations().size(), tx.getOutgoingTransfer().getDestinations().size());
      for (int i = 0; i < config.getDestinations().size(); i++) {
        assertEquals(config.getDestinations().get(i).getAddress(), tx.getOutgoingTransfer().getDestinations().get(i).getAddress());
        if (Boolean.TRUE.equals(ctx.isSweepResponse)) {
          assertEquals(1, config.getDestinations().size());
          assertNull(config.getDestinations().get(i).getAmount());
          assertEquals(tx.getOutgoingTransfer().getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
        } else {
          assertEquals(config.getDestinations().get(i).getAmount().toString(), tx.getOutgoingTransfer().getDestinations().get(i).getAmount().toString());
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

  /**
   * Tests that common tx field types are valid regardless of tx state.
   * 
   * @param tx is the tx to test
   */
  private static void testTxWalletTypes(MoneroTxWallet tx, TxContext ctx) {
    assertNotNull(tx.getHash());
    assertNotNull(tx.isConfirmed());
    assertNotNull(tx.isMinerTx());
    assertNotNull(tx.isFailed());
    assertNotNull(tx.isRelayed());
    assertNotNull(tx.inTxPool());
    assertNotNull(tx.isLocked());
    TestUtils.testUnsignedBigInteger(tx.getFee());
    assertNull(tx.getInputs());
    if (tx.getPaymentId() != null) assertNotEquals(MoneroTx.DEFAULT_PAYMENT_ID, tx.getPaymentId()); // default payment id converted to null
    if (tx.getNote() != null) assertTrue(tx.getNote().length() > 0);  // empty notes converted to undefined
    assertTrue(tx.getUnlockHeight() >= 0);
    assertNull(tx.getSize());   // TODO monero-wallet-rpc: add tx_size to get_transfers and get_transfer_by_txid
    if (Boolean.TRUE.equals(ctx.isSendResponse)) assertTrue(tx.getWeight() > 0);
    else assertNull(tx.getWeight());
    assertNull(tx.getReceivedTimestamp());  // TODO monero-wallet-rpc: return received timestamp (asked to file issue if wanted)
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
      assertTrue("Transaction does not reference given transfer", transfer.getTx().getIncomingTransfers().contains(transfer));
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
      assertEquals(transfer.getAmount(), sum);  // TODO: sum of destinations != outgoing amount in split txs
    }
  }
  
  private static void testDestination(MoneroDestination destination) {
    MoneroUtils.validateAddress(destination.getAddress(), TestUtils.NETWORK_TYPE);
    TestUtils.testUnsignedBigInteger(destination.getAmount(), true);
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
    if (minTxs != null) assertTrue(txs.size() + "/" + minTxs + " transactions found with the query", txs.size() >= minTxs);
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
      assertNotNull(check.getInTxPool());
      TestUtils.testUnsignedBigInteger(check.getReceivedAmount());
      if (check.getInTxPool()) assertEquals(0, (long) check.getNumConfirmations());
      else assertTrue(check.getNumConfirmations() > 0); // TODO (monero-wall-rpc) this fails (confirmations is 0) for (at least one) transaction that has 1 confirmation on testCheckTxKey()
    } else {
      assertNull(check.getInTxPool());
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
  
  private static void testParsedTxSet(MoneroTxSet parsedTxSet) {
    assertNotNull(parsedTxSet.getTxs());
    assertFalse(parsedTxSet.getTxs().isEmpty());
    assertNull(parsedTxSet.getSignedTxHex());
    assertNull(parsedTxSet.getUnsignedTxHex());
    
    // test each transaction        
    // TODO: use common tx wallet test?
    assertNull(parsedTxSet.getMultisigTxHex());
    for (MoneroTxWallet parsedTx : parsedTxSet.getTxs()) {
      TestUtils.testUnsignedBigInteger(parsedTx.getInputSum(), true);
      TestUtils.testUnsignedBigInteger(parsedTx.getOutputSum(), true);
      TestUtils.testUnsignedBigInteger(parsedTx.getFee());
      TestUtils.testUnsignedBigInteger(parsedTx.getChangeAmount());
      if (parsedTx.getChangeAmount().equals(BigInteger.valueOf(0))) assertNull(parsedTx.getChangeAddress());
      else MoneroUtils.validateAddress(parsedTx.getChangeAddress(), TestUtils.NETWORK_TYPE);
      assertTrue(parsedTx.getRingSize() > 1);
      assertTrue(parsedTx.getUnlockHeight() >= 0);
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
  private void testGetTxsStructure(List<MoneroTxWallet> txs) { testGetTxsStructure(txs, null); }
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
      else if (query.getHashes() == null) assertTrue("Blocks are not in order of heights: " + prevBlockHeight + " vs " + block.getHeight(), block.getHeight() > prevBlockHeight);
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
          else assertTrue(output.getKeyImage().toString() + " " + prevSubaddressIdx + " > " + output.getSubaddressIndex(), prevSubaddressIdx <= output.getSubaddressIndex()); // TODO: this does not test that index < other index if subaddresses are equal
        }
      }
    }
    
    // TODO monero core wallet2 does not provide ordered blocks or txs
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
}
