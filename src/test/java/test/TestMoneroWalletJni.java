package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.daemon.model.MoneroNetworkType;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.model.MoneroSyncListener;
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
  public static void setUpBeforeClass() throws Exception {
    
  }

  @Override
  protected MoneroWallet getTestWallet() {
    return TestUtils.getWalletJni();
  }

  @Test
  public void testCreateWalletRandom() {
    
    // create random wallet with defaults
    MoneroWalletJni wallet = new MoneroWalletJni();
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress());
    assertEquals(MoneroNetworkType.MAINNET, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertEquals("English", wallet.getLanguage());
    assertEquals(null, wallet.getPath());
    assertEquals(1, wallet.getHeight());  // TODO monero core: why does height of new unsynced wallet start at 1?
    assertTrue(wallet.getRestoreHeight() >= 0);
    
    // create random wallet with non-defaults
    wallet = new MoneroWalletJni(MoneroNetworkType.TESTNET, daemon.getRpcConnection(), "Spanish");
    MoneroUtils.validateMnemonic(wallet.getMnemonic());
    MoneroUtils.validateAddress(wallet.getPrimaryAddress());
    assertEquals(MoneroNetworkType.TESTNET, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertEquals("Spanish", wallet.getLanguage());
    assertEquals(null, wallet.getPath());
    assertEquals(1, wallet.getHeight());  // TODO monero core: why is height of unsynced wallet 1?
    if (daemon.getIsConnected()) {
      //assertTrue(wallet.getRestoreHeight() >= 0);
      assertEquals(daemon.getHeight(), wallet.getRestoreHeight());  // TODO: expected 325769 but was 1215072
    }
    else assertTrue(wallet.getRestoreHeight() >= 0);
  }
  
  @Test
  public void testCreateWalletFromMnemonic() {
    
    // create wallet with mnemonic and defaults
    wallet = new MoneroWalletJni(TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, null, null);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertEquals(null, wallet.getDaemonConnection());
    assertEquals("English", wallet.getLanguage());
    assertEquals(null, wallet.getPath());
    assertEquals(1, wallet.getHeight());
    assertEquals(0, wallet.getRestoreHeight());
    
    // create wallet without restore height
    wallet = new MoneroWalletJni(TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), null);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertEquals("English", wallet.getLanguage());
    assertEquals(null, wallet.getPath());
    assertEquals(1, wallet.getHeight());    // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(0, wallet.getRestoreHeight());
    
    // create wallet with mnemonic, connection, and restore height
    int restoreHeight = 10000;
    wallet = new MoneroWalletJni(TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, daemon.getRpcConnection(), restoreHeight);
    assertEquals(TestUtils.TEST_MNEMONIC, wallet.getMnemonic());
    assertEquals(TestUtils.TEST_ADDRESS, wallet.getPrimaryAddress());
    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
    assertNotNull(wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection() != wallet.getDaemonConnection());
    assertTrue(daemon.getRpcConnection().equals(wallet.getDaemonConnection()));
    assertEquals("English", wallet.getLanguage());
    assertEquals(null, wallet.getPath());
    assertEquals(1, wallet.getHeight());    // TODO monero core: why does height of new unsynced wallet start at 1?
    assertEquals(restoreHeight, wallet.getRestoreHeight());
  }
  
  @Test
  public void testCreateWalletFromKeys() {
    throw new RuntimeException("Not implemented");
  }
  
  // Can sync a wallet with a randomly generated seed
  @Test
  public void testSyncRandom() {
    assertTrue("Not connected to daemon", daemon.getIsConnected());
    
    // create test wallet
    MoneroWalletJni wallet = new MoneroWalletJni(TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection(), null);
    
    // test wallet's height before syncing
    assertEquals(1, wallet.getHeight());
    //assertEquals(daemon.getHeight(), wallet.getRestoreHeight());  // TODO
    
    // sync the wallet
    SyncProgressTester progressTester = new SyncProgressTester(wallet.getRestoreHeight(), wallet.getChainHeight() - 1, true, true);
    wallet.sync(null, null, new MoneroSyncListener() {
      @Override
      public void onProgress(int numBlocksDone, int numBlocksTotal, float percent, String message) {
        progressTester.onProgress(numBlocksDone, numBlocksTotal, percent, message);
      }
    });
    progressTester.onDone();
    
    // test wallet's height after syncing
    assertEquals(daemon.getHeight(), wallet.getHeight());
    
    // sync the wallet with default params
    wallet.sync();
    assertEquals(daemon.getHeight(), wallet.getHeight());
  }
  
  // Can sync a wallet with a given seed and start height
  @Test
  public void testSyncMnemonic() {
    throw new RuntimeException("Not implemented");
  }
  
  // Can close the currently open wallet
  @Test
  @Ignore   // disabled so wallet is not actually closed TODO: just re-open wallet?
  public void testClose() {
    wallet.close();
  }
  
  // ---------------------------------- PRIVATE -------------------------------
  
  // Can save the wallet
  @Test
  public void testSave() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented");
    
//    // unique wallet path for test
//    String path = "test_wallet_" + UUID.randomUUID().toString();
//    
//    // wallet does not exist
//    assertFalse(MoneroWalletJni.walletExists(path));
//    
//    // cannot open wallet
//    try {
//      MoneroWalletJni.openWallet(path, TestUtils.WALLET_JNI_PW);
//      fail("Cannot open non-existant wallet");
//    } catch (MoneroException e) {
//      assertEquals("Wallet does not exist: " + path, e.getMessage());
//    }
//    
//    // create the wallet
//    MoneroWalletJni wallet;
//    if (mnemonic == null) wallet = MoneroWalletJni.createWallet(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpc(), TestUtils.TEST_LANGUAGE);
//    else wallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpc(), mnemonic, restoreHeight);
//    
//    // test created wallet
//    assertEquals(path, wallet.getPath());
//    assertEquals(TestUtils.NETWORK_TYPE, wallet.getNetworkType());
//    MoneroUtils.validateMnemonic(wallet.getMnemonic());
//    assertEquals(TestUtils.TEST_LANGUAGE, wallet.getLanguage());
//    if (mnemonic != null) assertEquals(mnemonic, wallet.getMnemonic());
//    if (address != null) assertEquals(address, wallet.getPrimaryAddress());
  }
  
  /**
   * Internal class to test progress updates.
   */
  private class SyncProgressTester {
    
    private Integer startHeight;
    private Integer endHeight;
    private Boolean noMidway; // syncing should not have midway progress
    private boolean noProgress; // syncing should not make any progress
    private boolean midwayCalled;
    private Float prevPercent;
    private Integer prevNumBlocksDone;
    private Integer prevNumBlocksTotal;
    
    public SyncProgressTester(Integer startHeight, Integer endHeight, Boolean noMidway, boolean noProgress) {
      this.startHeight = startHeight;
      this.endHeight = endHeight;
      this.noMidway = noMidway;
      this.noProgress = noProgress;
      this.midwayCalled = false;
    }
    
    public void onProgress(int numBlocksDone, int numBlocksTotal, float percent, String message) {
      assertFalse("Should not call progress", noProgress);
      assertTrue(numBlocksDone >= 0);
      assertTrue(numBlocksTotal > 0 && numBlocksTotal >= numBlocksDone);
      assertTrue(percent >= 0);
      assertNotNull(message);
      assertFalse(message.isEmpty());
      if (prevPercent == null) {
        assertEquals(0, numBlocksDone);
        assertEquals(0, percent, 0);
      } else {
        assertTrue(percent > prevPercent);
        assertTrue(numBlocksDone >= prevNumBlocksDone);
      }
      prevPercent = percent;
      prevNumBlocksDone = numBlocksDone;
      prevNumBlocksTotal = numBlocksTotal;
      if (percent > 0 && percent < 1) midwayCalled = true;
    }
    
    public void onDone() {
      
      // nothing to test if no progress called
      if (this.noProgress) {
        assertNull(prevPercent);
        return;
      }
      
      // ensure progress was called
      assertNotNull(prevPercent);
      
      // test midway progress
      if (endHeight > startHeight && !Boolean.TRUE.equals(this.noMidway)) assertTrue("No midway progress reported but it should have been", midwayCalled);
      else assertFalse("No midway progress should have been reported but it was", midwayCalled);
      
      // test last progress
      assertEquals(1, (float) prevPercent, 0);
      assertEquals(endHeight - startHeight + 1, (int) prevNumBlocksDone);
      assertEquals(prevNumBlocksDone, prevNumBlocksTotal);
    }
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
  public void testGetSupportedLanguages() {
    super.testGetSupportedLanguages();
  }

  @Override
  public void testGetPrivateViewKey() {
    super.testGetPrivateViewKey();
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
  public void getAllBalances() {
    super.getAllBalances();
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
