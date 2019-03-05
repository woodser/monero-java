package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.BeforeClass;
import org.junit.Test;

import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroIntegratedAddress;
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
    assertEquals(false, wallet.isMultisigImportNeeded()); // TODO: test with multisig wallet
  }
  
  // Can create and open a wallet
  @Test
  public void testCreateAndOpenWallet() {
    
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

  // Can tag accounts and query accounts by tag
  @Test
  public void testAccountTags() {
    
    // test that non-existing tag returns no accounts
    try {
      wallet.getAccounts(false, "non_existing_tag");
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
    
    // create expected tag for test
    MoneroAccountTag expectedTag = new MoneroAccountTag("my_tag_" + UUID.randomUUID().toString(), "my tag label", Arrays.asList(0, 1));
    
    // tag and query accounts
    List<MoneroAccount> accounts1 = wallet.getAccounts();
    assertTrue("Not enough accounts to test; run create account test", accounts1.size() >= 3);
    wallet.tagAccounts(expectedTag.getTag(), Arrays.asList(0, 1));
    List<MoneroAccount> accounts2 = wallet.getAccounts(false, expectedTag.getTag());
    assertEquals(2, accounts2.size());
    assertEquals(accounts2.get(0), accounts1.get(0));
    assertEquals(accounts2.get(1), accounts1.get(1));
    
    // set tag label
    wallet.setAccountTagLabel(expectedTag.getTag(), expectedTag.getLabel());
    
    // retrieve and find new tag
    List<MoneroAccountTag> tags = wallet.getAccountTags();
    assertTrue(tags.contains(expectedTag));
    
    // untag and query accounts
    wallet.untagAccounts(Arrays.asList(0, 1));
    assertEquals(false, (wallet.getAccountTags()).contains(expectedTag));
    try {
      wallet.getAccounts(false, expectedTag.getTag());
      fail("Should have thrown exception with unregistered tag");
    } catch (MoneroException e) {
      assertEquals(-1, (int) e.getCode());
    }
  }
  
  // Has an address book
  @Test
  public void testAddressBook() {
    
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
    wallet.rescanSpent();
  }
  
  // Can save the wallet file
  @Test
  public void testSave() {
    wallet.save();
  }
  
//  // Can close the currently open wallet
//  // disabled so wallet is not actually closed
//  @Test
//  public void testClose() {
//    wallet.close();
//  }
  
  // ---------------------------------- PRIVATE -------------------------------
  
  private static void testAddressBookEntry(MoneroAddressBookEntry entry) {
    assertTrue(entry.getIndex() >= 0);
    assertNotNull(entry.getAddress());
    assertNotNull(entry.getDescription());
  }
}
