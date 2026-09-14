import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import monero.common.MoneroConnectionManager;
import monero.common.MoneroConnectionManagerListener;
import monero.common.MoneroError;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
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

  @TempDir
  Path tempDir;

  @Test
  public void testCloseCancelsForegroundSync() throws Exception {
    testCloseCancelsSync(false, false);
  }

  @Test
  public void testCloseSavesAfterCancellingForegroundSync() throws Exception {
    testCloseCancelsSync(false, true);
  }

  @Test
  public void testCloseSavesAfterCancellingBackgroundSync() throws Exception {
    testCloseCancelsSync(true, true);
  }

  private void testCloseCancelsSync(boolean background, boolean save) throws Exception {
    String path = tempDir.resolve("wallet").toString();
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPath(path).setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    String address = wallet.getPrimaryAddress();
    wallet.setAttribute("close-test", "saved");
    ExecutorService executor = Executors.newSingleThreadExecutor();
    Future<?> sync = null;
    try (StalledDaemon daemon = new StalledDaemon()) {
      wallet.setDaemonConnection(daemon.uri());
      wallet.stopSyncing(); // ordinary stop must leave subsequent syncing and daemon requests usable
      if (background) wallet.startSyncing(600000L);
      else sync = executor.submit(() -> {
        try { wallet.sync(0L, new MoneroWalletListener()); }
        catch (MoneroError e) { } // cancellation may report an interrupted daemon request
      });
      assertTrue(daemon.request.await(5, TimeUnit.SECONDS), "wallet did not send its daemon request");
      long start = System.nanoTime();
      wallet.close(save);
      assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) < 5000, "close waited instead of cancelling native I/O");
      if (sync != null) sync.get(5, TimeUnit.SECONDS);
      assertEquals(0L, nativeHandle(wallet));
      wallet.close(save); // closing twice is harmless
      MoneroWalletFull reopened = MoneroWalletFull.openWallet(path, "", MoneroNetworkType.TESTNET);
      try {
        assertEquals(address, reopened.getPrimaryAddress());
        if (save) assertEquals("saved", reopened.getAttribute("close-test"));
      } finally {
        reopened.close(false);
      }
    } finally {
      executor.shutdownNow();
      if (sync != null) sync.get(5, TimeUnit.SECONDS);
      wallet.close(false);
    }
  }

  @Test
  public void testInterruptedCloseCanBeRetried() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    try {
      Thread.currentThread().interrupt();
      assertThrows(MoneroError.class, () -> wallet.close(false));
      assertTrue(Thread.interrupted(), "close must preserve interruption");
      assertTrue(wallet.isClosed(), "new operations must be rejected once closing starts");
      assertThrows(MoneroError.class, () -> wallet.getPrimaryAddress());
      assertNotEquals(0L, nativeHandle(wallet), "interruption must not free the native wallet");
      wallet.close(false);
      assertEquals(0L, nativeHandle(wallet));
    } finally {
      Thread.interrupted();
      wallet.close(false);
    }
  }

  @Test
  public void testFailedCloseDetachesConnectionManager() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    MoneroConnectionManager manager = new MoneroConnectionManager();
    AtomicBoolean notified = new AtomicBoolean();
    wallet.setConnectionManager(manager);
    MoneroConnectionManagerListener walletListener = manager.getListeners().get(0);
    manager.addListener(connection -> notified.set(true));
    try {
      Thread.currentThread().interrupt();
      assertThrows(MoneroError.class, () -> wallet.close(false));
      assertTrue(Thread.interrupted());
      manager.setConnection("http://127.0.0.1:1");
      assertTrue(notified.get(), "closing one wallet must not interrupt other connection listeners");
      assertEquals(1, manager.getListeners().size());
      assertNull(wallet.getConnectionManager());
      assertThrows(MoneroError.class, () -> wallet.setConnectionManager(manager));
      assertEquals(1, manager.getListeners().size());
      walletListener.onConnectionChanged(null); // a callback already dispatched during close must be harmless
      assertNotEquals(0L, nativeHandle(wallet));
      wallet.close(false);
      assertEquals(0L, nativeHandle(wallet));
    } finally {
      Thread.interrupted();
      wallet.close(false);
      manager.reset();
    }
  }

  @Test
  public void testCloseAfterConnectionManagerReset() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    MoneroConnectionManager manager = new MoneroConnectionManager();
    wallet.setConnectionManager(manager);
    try {
      manager.reset();
      wallet.close(false);
      assertEquals(0L, nativeHandle(wallet));
      assertNull(wallet.getConnectionManager());
    } finally {
      wallet.close(false);
      manager.reset();
    }
  }

  @Test
  public void testCloseWaitsForActiveCalls() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    Field field = MoneroWalletFull.class.getDeclaredField("callLock");
    field.setAccessible(true);
    ReentrantReadWriteLock callLock = (ReentrantReadWriteLock) field.get(wallet);
    ExecutorService executor = Executors.newSingleThreadExecutor();
    callLock.readLock().lock(); // hold an active call at the Java/native lifetime barrier
    Future<?> close = executor.submit(() -> wallet.close(false));
    try {
      assertThrows(TimeoutException.class, () -> close.get(200, TimeUnit.MILLISECONDS));
      assertNotEquals(0L, nativeHandle(wallet));
    } finally {
      callLock.readLock().unlock();
      try {
        close.get(5, TimeUnit.SECONDS);
        assertEquals(0L, nativeHandle(wallet));
      } finally {
        executor.shutdownNow();
        wallet.close(false);
      }
    }
  }

  @Test
  public void testCloseTimeoutCanBeRetried() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    Field field = MoneroWalletFull.class.getDeclaredField("callLock");
    field.setAccessible(true);
    ReentrantReadWriteLock callLock = (ReentrantReadWriteLock) field.get(wallet);
    ExecutorService executor = Executors.newSingleThreadExecutor();
    callLock.readLock().lock();
    long start = System.nanoTime();
    Future<?> close = executor.submit(() -> wallet.close(false));
    try {
      ExecutionException error = assertThrows(ExecutionException.class, () -> close.get(65, TimeUnit.SECONDS));
      assertTrue(error.getCause() instanceof MoneroError);
      assertTrue(error.getCause().getMessage().contains("Timed out"));
      assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) >= 59000);
      assertNotEquals(0L, nativeHandle(wallet), "timeout must not free a wallet with an active call");
      assertThrows(MoneroError.class, () -> wallet.getPrimaryAddress());
    } finally {
      callLock.readLock().unlock();
      executor.shutdownNow();
      wallet.close(false);
    }
    assertEquals(0L, nativeHandle(wallet));
  }

  @Test
  public void testConcurrentClose() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    ExecutorService executor = Executors.newFixedThreadPool(2);
    CountDownLatch start = new CountDownLatch(1);
    Callable<Void> close = () -> {
      start.await();
      wallet.close(false);
      return null;
    };
    try {
      Future<?> first = executor.submit(close);
      Future<?> second = executor.submit(close);
      start.countDown();
      first.get(5, TimeUnit.SECONDS);
      second.get(5, TimeUnit.SECONDS);
      assertEquals(0L, nativeHandle(wallet));
    } finally {
      executor.shutdownNow();
      wallet.close(false);
    }
  }

  @Test
  public void testCloseSaveFailureCanBeRetried() throws Exception {
    Path directory = Files.createDirectory(tempDir.resolve("original"));
    Path moved = tempDir.resolve("moved");
    String path = directory.resolve("wallet").toString();
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPath(path).setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    wallet.setAttribute("close-test", "saved");
    try {
      Files.move(directory, moved);
      Files.createFile(directory); // make the wallet's parent path unwritable as a directory
      try {
        assertThrows(MoneroError.class, () -> wallet.close(true));
        assertNotEquals(0L, nativeHandle(wallet));
      } finally {
        Files.delete(directory);
        Files.move(moved, directory);
      }
      wallet.close(true);
      assertEquals(0L, nativeHandle(wallet));
      MoneroWalletFull reopened = MoneroWalletFull.openWallet(path, "", MoneroNetworkType.TESTNET);
      try {
        assertEquals("saved", reopened.getAttribute("close-test"));
      } finally {
        reopened.close(false);
      }
    } finally {
      wallet.close(false);
    }
  }

  @Test
  public void testCloseFromListenerIsRejected() throws Exception {
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPassword("").setNetworkType(MoneroNetworkType.TESTNET));
    AtomicBoolean notified = new AtomicBoolean();
    try {
      wallet.addListener(new MoneroWalletListener() {
        @Override
        public void onNewBlock(long height) {
          assertThrows(MoneroError.class, () -> wallet.close(false));
          notified.set(true);
        }
      });
      Field field = MoneroWalletFull.class.getDeclaredField("jniListener");
      field.setAccessible(true);
      Object listener = field.get(wallet);
      Method callback = listener.getClass().getDeclaredMethod("onNewBlock", long.class);
      callback.setAccessible(true);
      callback.invoke(listener, 1L); // exercise the same notification entry point used by JNI
      assertTrue(notified.get());
      assertFalse(wallet.isClosed());
      ExecutorService executor = Executors.newSingleThreadExecutor();
      try {
        executor.submit(() -> wallet.close(false)).get(5, TimeUnit.SECONDS);
        assertEquals(0L, nativeHandle(wallet));
      } finally {
        executor.shutdownNow();
      }
    } finally {
      wallet.close(false);
    }
  }

  private static long nativeHandle(MoneroWalletFull wallet) throws Exception {
    Field field = MoneroWalletFull.class.getDeclaredField("jniWalletHandle");
    field.setAccessible(true);
    return field.getLong(wallet);
  }

  private static class StalledDaemon implements AutoCloseable {
    final CountDownLatch request = new CountDownLatch(1);
    final CountDownLatch release = new CountDownLatch(1);
    final ServerSocket server;
    final Thread thread;

    StalledDaemon() throws IOException {
      server = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"));
      thread = new Thread(() -> {
        try (Socket socket = server.accept()) {
          if (socket.getInputStream().read() != -1) request.countDown();
          release.await(); // accept the request but never send a response
        } catch (IOException e) {
          if (!server.isClosed()) throw new RuntimeException(e);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
      }, "stalled-daemon");
      thread.setDaemon(true);
      thread.start();
    }

    String uri() {
      return "http://127.0.0.1:" + server.getLocalPort();
    }

    @Override
    public void close() throws Exception {
      release.countDown();
      server.close();
      thread.join(5000);
      assertFalse(thread.isAlive());
    }
  }

  public static void main(String[] args) {
    new TestNativeLibrary().testKnownKeyDerivation();
    System.out.println("Native library verified");
  }
}
