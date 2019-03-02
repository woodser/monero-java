package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.BeforeClass;
import org.junit.Test;

import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import utils.TestUtils;

/**
 * Runs common tests that every Monero wallet implementation should support.
 * 
 * TODO: test filtering with not relayed
 */
public abstract class TestMoneroWalletCommon<T extends MoneroWallet> {
  
  // wallet instance to test
  private MoneroWallet wallet;
  
  /**
   * Subclasseses return wallet instance to test.
   */
  protected abstract T initWallet();

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }
  
  @Test
  public void testGetHeight() {
    int height = wallet.getHeight();
    assertTrue(height >= 0);
  }
  
  @Test
  public void testGetMnemonic() {
    String mnemonic = wallet.getMnemonic();
    MoneroUtils.validateMnemonic(mnemonic);
    assertEquals(TestUtils.TEST_MNEMONIC, mnemonic);
  }
  
  @Test
  public void testGetSupportedLanguages() {
    List<String> languages = wallet.getLanguages();
    assertFalse(languages.isEmpty());
    for (String language : languages) assertFalse(language.isEmpty());
  }
  
  @Test
  public void testGetPrivateViewKey() {
    String privateViewKey = wallet.getPrivateViewKey();
    MoneroUtils.validatePrivateViewKey(privateViewKey);
  }
  
  @Test
  public void getPrimaryAddress() {
    String primaryAddress = wallet.getPrimaryAddress();
    MoneroUtils.validateAddress(primaryAddress);
    assertEquals((wallet.getSubaddress(0, 0)).getAddress(), primaryAddress);
  }
}
