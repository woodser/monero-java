package test;

import org.junit.BeforeClass;

import monero.wallet.MoneroWallet;

/**
 * Tests monero-wallet-rpc non-relaying calls.
 */
public class TestMoneroWalletRpc extends TestMoneroWalletCommon {

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }

  @Override
  protected MoneroWallet getTestWallet() {
    throw new RuntimeException("Not implemented");
  }
}
