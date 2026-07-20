import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.UUID;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.model.MoneroWalletConfig;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Tests the native library offline, in order to verify a built or distributed binary.
 *
 * Has a main() so it can also run on a bare jvm, without maven: java -cp <classpath> TestNativeLibrary
 */
public class TestNativeLibrary {

  /**
   * Restores a known seed to its known address, exercising native key derivation.
   */
  @Test
  public void testKnownKeyDerivation() {
    MoneroWallet wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig().setPath(TestUtils.TEST_WALLETS_DIR + "/temp_" + UUID.randomUUID()).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(TestUtils.NETWORK_TYPE).setSeed(TestUtils.SEED).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    try {
      assertEquals(TestUtils.ADDRESS, wallet.getPrimaryAddress());
    } finally {
      wallet.close();
    }
  }

  public static void main(String[] args) {
    new TestNativeLibrary().testKnownKeyDerivation();
    System.out.println("Native library verified");
  }
}
