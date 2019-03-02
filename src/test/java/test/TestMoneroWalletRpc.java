package test;

import org.junit.BeforeClass;

import monero.wallet.MoneroWalletRpc;

/**
 * Tests the Monero Wallet RPC client and server.
 */
public class TestMoneroWalletRpc extends TestMoneroWalletCommon<MoneroWalletRpc> {

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }

  @Override
  protected MoneroWalletRpc initWallet() {
    throw new RuntimeException("Not implemented");
  }
}
