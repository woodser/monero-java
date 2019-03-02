package test;

import static org.junit.Assert.fail;

import org.junit.BeforeClass;
import org.junit.Test;

import monero.wallet.MoneroWalletRpc;

/**
 * Tests the Monero Wallet RPC client and server.
 */
public class TestMoneroWalletRpc extends TestMoneroWalletCommon<MoneroWalletRpc> {

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
  }

  @Test
  public void test() {
    fail("Not yet implemented");
  }

  @Override
  protected MoneroWalletRpc initWallet() {
    throw new RuntimeException("Not implemented");
  }
}
