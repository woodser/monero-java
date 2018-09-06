package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import model.MoneroAccount;
import model.MoneroSubaddress;
import utils.TestUtils;
import wallet.MoneroWallet;

/**
 * Tests a Monero wallet's account and subaddress creation function.
 */
public class TestMoneroWalletCreateAccountsAndSubaddresses {
  
  private MoneroWallet wallet;

  @Before
  public void setup() throws Exception {
    wallet = TestUtils.getWallet();
  }

  @Test
  public void testCreateAccount() {
    
    // create account with null label
    List<MoneroAccount> accountsBefore = wallet.getAccounts();
    MoneroAccount createdAccount = wallet.createAccount(null);
    TestUtils.testAccount(createdAccount);
    assertNull(createdAccount.getLabel());
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 1);
    
    // create account with label
    accountsBefore = wallet.getAccounts();
    String label = UUID.randomUUID().toString();
    createdAccount = wallet.createAccount(label);
    assertEquals(label, createdAccount.getLabel());
    TestUtils.testAccount(createdAccount);
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 1);
    
    // create account with same label
    createdAccount = wallet.createAccount(label);
    assertEquals(label, createdAccount.getLabel());
    TestUtils.testAccount(createdAccount);
    assertTrue(accountsBefore.size() == wallet.getAccounts().size() - 2);
    
    // test querying by tag
    fail("This test is not correct because tags != labels");
    List<MoneroAccount> accountsByTag = wallet.getAccounts(label);
    assertEquals(1, accountsByTag.size());
    assertEquals(createdAccount, accountsByTag.get(0));
    
    // create another account with the same tag
    createdAccount = wallet.createAccount(label);
    assertEquals(label, createdAccount.getLabel());
    accountsByTag = wallet.getAccounts(label);
    assertEquals(2, accountsByTag.size());
    assertEquals(createdAccount, accountsByTag.get(1));
  }

  @Test
  public void testCreateSubaddress() {
    
    // create subaddress with no label
    List<MoneroSubaddress> subaddresses = wallet.getSubaddresses(0);
    MoneroSubaddress subaddress = wallet.createSubaddress(0, null);
    assertNull(subaddress.getLabel());
    TestUtils.testSubaddress(subaddress);
    List<MoneroSubaddress> subaddressesNew = wallet.getSubaddresses(0);
    assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
    assertEquals(subaddressesNew.get(subaddressesNew.size() - 1), subaddress);
    
    // create subaddress with label
    subaddresses = wallet.getSubaddresses(0);
    String uuid = UUID.randomUUID().toString();
    subaddress = wallet.createSubaddress(0, uuid);
    assertEquals(subaddress.getLabel(), uuid);
    TestUtils.testSubaddress(subaddress);
    subaddressesNew = wallet.getSubaddresses(0);
    assertEquals(subaddresses.size(), subaddressesNew.size() - 1);
    assertEquals(subaddressesNew.get(subaddressesNew.size() - 1), subaddress);
  }
}
