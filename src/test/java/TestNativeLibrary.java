import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
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

  @Test
  public void testDaemonSslVerifyRoundTrip() {
    MoneroRpcConnection connection = new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI, "user", "password").setProxyUri("127.0.0.1:19050").setSslVerify(false);
    MoneroWallet wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig().setNetworkType(TestUtils.NETWORK_TYPE).setSeed(TestUtils.SEED).setServer(connection).setRestoreHeight(0l));
    try {
      assertFalse(wallet.getDaemonConnection().getSslVerify());
      assertEquals(connection, wallet.getDaemonConnection());
      wallet.setDaemonConnection(wallet.getDaemonConnection());
      assertEquals(connection, wallet.getDaemonConnection());
      assertFalse(wallet.getDaemonConnection().getSslVerify());
      assertEquals("user", wallet.getDaemonConnection().getUsername());
      assertEquals("password", wallet.getDaemonConnection().getPassword());
      wallet.setDaemonConnection(connection.setSslVerify(true));
      assertTrue(wallet.getDaemonConnection().getSslVerify());
      wallet.setDaemonConnection(connection.setSslVerify(false));
      assertFalse(wallet.getDaemonConnection().getSslVerify());
      assertThrows(MoneroError.class, () -> wallet.setDaemonConnection(new MoneroRpcConnection("https://127.0.0.1:65536")));
      assertEquals(connection, wallet.getDaemonConnection());
      wallet.setDaemonConnection(new MoneroRpcConnection(connection).setProxyUri("127.0.0.1:19051"));
      assertEquals("127.0.0.1:19051", wallet.getDaemonConnection().getProxyUri());
      wallet.setDaemonConnection((MoneroRpcConnection) null);
      assertNull(wallet.getDaemonConnection());
      wallet.setDaemonConnection(new MoneroRpcConnection(TestUtils.OFFLINE_SERVER_URI));
      assertTrue(wallet.getDaemonConnection().getSslVerify());
      assertNull(wallet.getDaemonConnection().getProxyUri());
    } finally {
      wallet.close(false);
    }
  }

  @Test
  public void testConcurrentDaemonConnectionSnapshots() throws Exception {
    MoneroRpcConnection first = new MoneroRpcConnection("http://127.0.0.1:18081", "first", "password1").setProxyUri("127.0.0.1:19050").setSslVerify(false);
    MoneroRpcConnection second = new MoneroRpcConnection("http://127.0.0.1:18082", "second", "password2").setProxyUri("127.0.0.1:19051");
    MoneroWallet wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig().setNetworkType(TestUtils.NETWORK_TYPE));
    ExecutorService executor = Executors.newFixedThreadPool(2);
    CountDownLatch start = new CountDownLatch(1);
    try {
      wallet.setDaemonConnection(first);
      Future<?>[] updates = new Future<?>[2];
      MoneroRpcConnection[] connections = {first, second};
      for (int i = 0; i < connections.length; i++) {
        MoneroRpcConnection connection = connections[i];
        updates[i] = executor.submit(() -> {
          start.await();
          for (int j = 0; j < 200; j++) {
            wallet.setDaemonConnection(connection);
            MoneroRpcConnection snapshot = wallet.getDaemonConnection();
            assertTrue(first.equals(snapshot) || second.equals(snapshot));
          }
          return null;
        });
      }
      start.countDown();
      for (Future<?> update : updates) update.get(30, TimeUnit.SECONDS);
    } finally {
      executor.shutdownNow();
      wallet.close(false);
    }
  }

  public static void main(String[] args) throws Exception {
    new TestNativeLibrary().testKnownKeyDerivation();
    new TestNativeLibrary().testDaemonSslVerifyRoundTrip();
    new TestNativeLibrary().testConcurrentDaemonConnectionSnapshots();
    System.out.println("Native library verified");
  }
}
