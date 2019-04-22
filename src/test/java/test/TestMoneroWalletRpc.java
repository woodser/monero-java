package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.rpc.MoneroRpc;
import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.config.MoneroTransferFilter;
import monero.wallet.config.MoneroTxFilter;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;
import utils.TestUtils;

/**
 * Tests monero-wallet-rpc non-relaying calls.
 */
public class TestMoneroWalletRpc extends TestMoneroWalletCommon {
  
  protected MoneroWalletRpc wallet;
  
  public TestMoneroWalletRpc() {
    this.wallet = (MoneroWalletRpc) getTestWallet();
  }

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }

  @Override
  protected MoneroWallet getTestWallet() {
    return TestUtils.getWalletRpc();
  }
  
  // Can indicate if multisig import is needed for correct balance information
  @Test
  public void testIsMultisigNeeded() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    assertEquals(false, wallet.isMultisigImportNeeded()); // TODO: test with multisig wallet
  }
  
  // Can create and open a wallet
  @Test
  public void testCreateAndOpenWallet() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // create test wallet 2 which throws rpc code -21 if it already exists
    try {
      wallet.createWallet(TestUtils.WALLET_RPC_NAME_2, TestUtils.WALLET_RPC_PW, "English");
    } catch (MoneroException e) {
      assertEquals(-21, (int) e.getCode());
    }
    try {
      
      // open test wallet 2
      wallet.openWallet(TestUtils.WALLET_RPC_NAME_2, TestUtils.WALLET_RPC_PW);
      
      // assert wallet 2 is empty
      List<MoneroTxWallet> txs = wallet.getTxs();
      assertTrue(txs.size() == 0);
      
      // open test wallet 1
      wallet.openWallet(TestUtils.WALLET_RPC_NAME_1, TestUtils.WALLET_RPC_PW);
      txs = wallet.getTxs();
      assertTrue(txs.size() != 0);  // wallet is used
    } catch (MoneroException e) {
      throw e;
    } finally {
      
      // open test wallet 1 no matter what for other tests
      try {
        wallet.openWallet(TestUtils.WALLET_RPC_NAME_1, TestUtils.WALLET_RPC_PW);
      } catch (MoneroException e) {
        assertEquals(-1, (int) e.getCode()); // ok if wallet is already open
      }
    }
  }
  
  // Preserves order from rpc
  @SuppressWarnings("unchecked")
  @Test
  public void testRpcOrder() {
    
    // fetch transfers directly from rpc for comparison to library
    MoneroRpc rpc = wallet.getRpc();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("all_accounts", true);
    params.put("in", true);
    params.put("out", true);
//    params.put("pool", true);
//    params.put("pending", true);
//    params.put("failed", true);
    Map<String, Object> resp = rpc.sendJsonRequest("get_transfers", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // compare incoming confirmed transfers
    compareTransferOrder((List<Map<String, Object>>) result.get("in"), wallet.getTransfers(new MoneroTransferFilter().setIsIncoming(true).setTxFilter(new MoneroTxFilter().setIsConfirmed(true))));
    
    // compare outgoing confirmed transfers
    compareTransferOrder((List<Map<String, Object>>) result.get("out"), wallet.getTransfers(new MoneroTransferFilter().setIsOutgoing(true).setTxFilter(new MoneroTxFilter().setIsConfirmed(true))));
  }

  // Can tag accounts and query accounts by tag
  @Test
  public void testAccountTags() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get accounts
    List<MoneroAccount> accounts = wallet.getAccounts();
    assertTrue("Not enough accounts to test; run create account test", accounts.size() >= 3);
    
    // tag some of the accounts
    MoneroAccountTag tag = new MoneroAccountTag("my_tag_" + UUID.randomUUID(), "my tag label", Arrays.asList(0, 1));
    wallet.tagAccounts(tag.getTag(), tag.getAccountIndices());
    
    // query accounts by tag
    List<MoneroAccount> taggedAccounts = wallet.getAccounts(false, tag.getTag());
    assertEquals(2, taggedAccounts.size());
    assertEquals(0, (int) taggedAccounts.get(0).getIndex());
    assertEquals(tag.getTag(), taggedAccounts.get(0).getTag());
    assertEquals(1, (int) taggedAccounts.get(1).getIndex());
    assertEquals(tag.getTag(), taggedAccounts.get(1).getTag());

    // set tag label
    wallet.setAccountTagLabel(tag.getTag(), tag.getLabel());
    
    // fetch tags and ensure new tag is contained
    List<MoneroAccountTag> tags = wallet.getAccountTags();
    assertTrue(tags.contains(tag));
    
    // re-tag an account
    MoneroAccountTag tag2 = new MoneroAccountTag("my_tag_" + UUID.randomUUID(), "my tag label 2", Arrays.asList(1));
    wallet.tagAccounts(tag2.getTag(), tag2.getAccountIndices());
    List<MoneroAccount> taggedAccounts2 = wallet.getAccounts(false, tag2.getTag());
    assertEquals(1, taggedAccounts2.size(), 1);
    assertEquals(1, taggedAccounts2.get(0).getIndex(), 1);
    assertEquals(tag2.getTag(), taggedAccounts2.get(0).getTag());
    
    // re-query original tag which only applies to one account now
    taggedAccounts = wallet.getAccounts(false, tag.getTag());
    assertEquals(1, taggedAccounts.size());
    assertEquals(0, (int) taggedAccounts.get(0).getIndex());
    assertEquals(tag.getTag(), taggedAccounts.get(0).getTag());
    
    // untag and query accounts
    wallet.untagAccounts(Arrays.asList(0, 1));
    assertEquals(0, wallet.getAccountTags().size());
    try {
      wallet.getAccounts(false, tag.getTag());
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // test that non-existing tag returns no accounts
    try {
      wallet.getAccounts(false, "non_existing_tag");
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
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
    String address = wallet.getSubaddress(0, 0).getAddress();
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
          assertEquals(entry.getDescription(), integratedDescriptions.get(idx));
          assertEquals(entry.getAddress(), integratedAddresses.get(idx).getStandardAddress());
          assertTrue(MoneroUtils.paymentIdsEqual(integratedAddresses.get(idx).getPaymentId(), entry.getPaymentId()));
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
  
  // Can rescan spent
  @Test
  public void testRescanSpent() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    wallet.rescanSpent();
  }
  
  // Can save the wallet file
  @Test
  public void testSave() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    wallet.save();
  }
  
  // Can close the currently open wallet
  @Test
  @Ignore   // disabled so wallet is not actually closed
  public void testClose() {
    wallet.close();
  }
  
  // ---------------------------------- PRIVATE -------------------------------
  
  private static void testAddressBookEntry(MoneroAddressBookEntry entry) {
    assertTrue(entry.getIndex() >= 0);
    assertNotNull(entry.getAddress());
    assertNotNull(entry.getDescription());
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
  public void testGetVouts() {
    super.testGetVouts();
  }

  @Override
  public void testGetVoutsWithConfiguration() {
    super.testGetVoutsWithConfiguration();
  }

  @Override
  public void testGetVoutsValidateInputs() {
    super.testGetVoutsValidateInputs();
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
  public void testSweepOutputs() {
    super.testSweepOutputs();
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
  public void testSweepSubaddresses() {
    super.testSweepSubaddresses();
  }

  @Override
  public void testSweepAccounts() {
    super.testSweepAccounts();
  }

  @Override
  public void testRescanBlockchain() {
    super.testRescanBlockchain();
  }

  @Override
  public void testSweepWallet() {
    super.testSweepWallet();
  }

  @Override
  protected MoneroDaemon getTestDaemon() {
    return super.getTestDaemon();
  }
  
  @SuppressWarnings("unchecked")
  private static void compareTransferOrder(List<Map<String, Object>> rpcTransfers, List<?> transfers) {
    assertEquals(transfers.size(), rpcTransfers.size());
    for (int i = 0; i < transfers.size(); i++) {
      MoneroTransfer transfer = (MoneroTransfer) transfers.get(i);
      Map<String, Object> rpcTransfer = rpcTransfers.get(i);
      assertEquals((String) rpcTransfer.get("txid"), transfer.getTx().getId());
      List<Map<String, BigInteger>> rpcSubaddrIndices = (List<Map<String, BigInteger>>) rpcTransfer.get("subaddr_indices");
      
      
      // TODO: account for multiple here
      
      
      
      
      
      if (rpcSubaddrIndices.size() > 1) System.out.println(rpcTransfer);
      assertEquals(1, rpcSubaddrIndices.size());
      Map<String, BigInteger> rpcSubaddrIndex = rpcSubaddrIndices.get(0);
      int rpcAccountIdx = rpcSubaddrIndex.get("major").intValue();
      assertEquals(rpcAccountIdx, (int) transfer.getAccountIndex());
      int rpcSubaddressIdx = rpcSubaddrIndex.get("minor").intValue();
      if (rpcSubaddressIdx != transfer.getSubaddressIndex()) System.out.println(rpcTransfer);
      assertEquals(rpcSubaddressIdx, (int) transfer.getSubaddressIndex());
    }
  }
}
