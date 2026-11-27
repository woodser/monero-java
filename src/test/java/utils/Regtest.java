package utils;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroDaemonInfo;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroWalletConfig;

/**
 * Starts and funds the regtest servers used by the tests unless they are already running.
 */
public class Regtest {

  public static final String DAEMON_DIR = TestUtils.REGTEST_DIR + "/monerod";
  public static final int COINBASE_BLOCKS = 100; // blocks mined to the test wallet from its first receive height
  public static final long FUNDING_HEIGHT = TestUtils.FIRST_RECEIVE_HEIGHT + COINBASE_BLOCKS + 60; // the test wallet's coinbase outputs are unlocked
  public static final int COVERAGE_BLOCKS = 15; // one block funding accounts 1 to 3, 10 to unlock, one spend per account, one block funding every subaddress
  public static final int HISTORY_ROUNDS = 20; // random transfers confirmed one per block after coverage
  public static final long FIXTURE_HEIGHT = FUNDING_HEIGHT + COVERAGE_BLOCKS + HISTORY_ROUNDS + 10; // history outputs are unlocked

  public static final String LWS_CONTAINER_NAME = "monero-java-regtest-lws";
  public static final String LWS_DIR = TestUtils.REGTEST_DIR + "/lws";

  private static MoneroDaemonRpc daemon;
  private static MoneroWalletRpc walletRpc;
  private static Process lws;
  private static boolean started;
  private static RuntimeException startError;

  /**
   * Start the servers unless they answer on the configured ports, then fund the test wallet on a fresh chain.
   */
  public static synchronized void start() {
    if (startError != null) throw startError;
    if (started) return;
    try {
      daemon = startDaemon();
      walletRpc = startWalletRpc();
      started = true; // calls while funding use the running servers
      mineTo(TestUtils.MINING_ADDRESS, TestUtils.FIRST_RECEIVE_HEIGHT);
      mineTo(TestUtils.ADDRESS, TestUtils.FIRST_RECEIVE_HEIGHT + COINBASE_BLOCKS);
      mineTo(TestUtils.MINING_ADDRESS, FUNDING_HEIGHT);
      if (daemon.getHeight() < FIXTURE_HEIGHT) buildHistory();
      if (!daemon.getTxPool().isEmpty()) StartMining.generateBlocks(10); // confirm and unlock txs left by a previous run
      initMiningDataset();
    } catch (IOException e) {
      startError = new MoneroError(e);
      throw startError;
    } catch (RuntimeException e) {
      startError = e;
      throw e;
    } catch (Error e) {
      startError = new MoneroError(e);
      throw e;
    }
  }

  /**
   * Stop the servers started by this process and any wallet servers left running by tests.
   */
  public static synchronized void stop() {
    stopLws();
    for (MoneroWalletRpc wallet : new ArrayList<MoneroWalletRpc>(TestUtils.WALLET_PORT_OFFSETS.keySet())) TestUtils.stopWalletRpcProcess(wallet);
    if (walletRpc != null && walletRpc.getProcess() != null) walletRpc.stopProcess();
    if (daemon != null && daemon.getProcess() != null) daemon.stopProcess();
  }

  /**
   * Start monero-lws, which the light wallet tests connect to, unless it answers on the configured port.
   * 
   * It follows the regtest daemon and is run with the binary at MONERO_LWS_PATH, or with docker otherwise.
   * The accounts of its database are not kept, the light wallets are created again by the tests.
   */
  public static synchronized void startLws() {
    start();
    if (isLwsRunning()) return;
    if (lws != null && lws.isAlive()) throw new MoneroError("monero-lws is running but does not answer at " + TestUtils.WALLET_LWS_URI);
    try {
      List<String> lwsArgs = Arrays.asList(
          "--daemon=tcp://127.0.0.1:" + TestUtils.DAEMON_ZMQ_RPC_PORT, "--sub=tcp://127.0.0.1:" + TestUtils.DAEMON_ZMQ_PUB_PORT,
          "--rest-server=http://127.0.0.1:" + TestUtils.WALLET_LWS_PORT_START, "--admin-rest-server=http://127.0.0.1:" + TestUtils.WALLET_LWS_ADMIN_PORT_START,
          "--disable-admin-auth", "--auto-accept-creation", "--auto-accept-import", "--max-subaddresses=1000",
          "--webhook-ssl-verification=none", "--regtest", "--log-level=2");
      List<String> cmd = new ArrayList<String>();
      if (TestUtils.LWS_LOCAL_PATH == null) {
        new File(LWS_DIR).mkdirs();
        cmd.addAll(Arrays.asList("docker", "run", "--rm", "--name", LWS_CONTAINER_NAME, "--network", TestUtils.LWS_DOCKER_NETWORK, TestUtils.LWS_DOCKER_IMAGE));
      } else {
        File dbDir = new File(LWS_DIR + "/db");
        dbDir.mkdirs();
        cmd.addAll(Arrays.asList(TestUtils.LWS_LOCAL_PATH, "--db-path", dbDir.getAbsolutePath()));
      }
      cmd.addAll(lwsArgs);
      new File(LWS_DIR).mkdirs();
      if (TestUtils.LWS_LOCAL_PATH == null) removeLwsContainer(); // left by a previous run
      lws = new ProcessBuilder(cmd).redirectErrorStream(true).redirectOutput(new File(LWS_DIR + "/monero-lws.log")).start();
      
      // wait for monero-lws to answer
      long timeout = System.currentTimeMillis() + 60000;
      while (!isLwsRunning()) {
        if (!lws.isAlive()) throw new MoneroError("monero-lws exited with code " + lws.exitValue() + ", see " + LWS_DIR + "/monero-lws.log");
        if (System.currentTimeMillis() > timeout) throw new MoneroError("Timed out waiting for monero-lws at " + TestUtils.WALLET_LWS_URI + ", see " + LWS_DIR + "/monero-lws.log");
        TimeUnit.MILLISECONDS.sleep(500);
      }
    } catch (IOException e) {
      throw new MoneroError("Could not start monero-lws, install docker or set MONERO_LWS_PATH to a monero-lws-daemon binary: " + e.getMessage());
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new MoneroError(e);
    }
  }

  private static boolean isLwsRunning() {
    try {
      new MoneroRpcConnection(TestUtils.WALLET_LWS_URI).sendPathRequest("get_version");
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  // stop the monero-lws started by this process
  private static synchronized void stopLws() {
    if (lws == null) return;
    if (TestUtils.LWS_LOCAL_PATH == null) removeLwsContainer(); // killing the docker client does not stop the container
    else lws.destroy();
    try { lws.waitFor(10, TimeUnit.SECONDS); }
    catch (InterruptedException e) { Thread.currentThread().interrupt(); }
    lws = null;
  }

  private static void removeLwsContainer() {
    try { new ProcessBuilder("docker", "rm", "-f", LWS_CONTAINER_NAME).redirectErrorStream(true).redirectOutput(new File(LWS_DIR + "/docker.log")).start().waitFor(30, TimeUnit.SECONDS); }
    catch (Exception e) { }
  }

  // connect to the daemon on the configured port, starting one if nothing answers
  private static MoneroDaemonRpc startDaemon() throws IOException {
    MoneroDaemonRpc daemon = new MoneroDaemonRpc(new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD));
    MoneroDaemonInfo info;
    try {
      info = daemon.getInfo();
    } catch (MoneroError e) {
      new File(DAEMON_DIR).mkdirs();
      daemon = new MoneroDaemonRpc(Arrays.asList(TestUtils.DAEMON_LOCAL_PATH,
          "--regtest", "--keep-fakechain", "--offline", "--fixed-difficulty", "1", "--non-interactive",
          "--zmq-rpc-bind-ip", "127.0.0.1", "--zmq-rpc-bind-port", "" + TestUtils.DAEMON_ZMQ_RPC_PORT, "--zmq-pub", "tcp://127.0.0.1:" + TestUtils.DAEMON_ZMQ_PUB_PORT, // monero-lws follows the daemon through zmq
          "--p2p-bind-ip", "127.0.0.1", "--p2p-bind-port", "" + (TestUtils.DAEMON_RPC_PORT - 1),
          "--rpc-bind-ip", "127.0.0.1", "--rpc-bind-port", "" + TestUtils.DAEMON_RPC_PORT,
          "--rpc-ssl", "disabled", "--rpc-access-control-origins", TestUtils.WALLET_RPC_ACCESS_CONTROL_ORIGINS,
          "--data-dir", DAEMON_DIR, "--check-updates", "disabled", "--disable-dns-checkpoints"));
      info = daemon.getInfo();
    }
    if (!Boolean.TRUE.equals(info.isRegtest())) throw new MoneroError("Daemon at " + TestUtils.DAEMON_RPC_URI + " is not regtest");
    return daemon;
  }

  // connect to the wallet server on the configured port, starting one if nothing answers
  private static MoneroWalletRpc startWalletRpc() throws IOException {
    MoneroWalletRpc walletRpc = new MoneroWalletRpc(new MoneroRpcConnection(TestUtils.WALLET_RPC_URI, TestUtils.WALLET_RPC_USERNAME, TestUtils.WALLET_RPC_PASSWORD));
    try {
      walletRpc.getVersion();
      return walletRpc;
    } catch (MoneroError e) {
      new File(TestUtils.WALLET_RPC_LOCAL_WALLET_DIR).mkdirs();
      List<String> cmd = new ArrayList<String>(Arrays.asList(TestUtils.WALLET_RPC_LOCAL_PATH,
          "--rpc-bind-ip", "127.0.0.1", "--rpc-bind-port", "" + TestUtils.WALLET_RPC_PORT_START,
          "--rpc-login", TestUtils.WALLET_RPC_USERNAME + ":" + TestUtils.WALLET_RPC_PASSWORD,
          "--rpc-ssl", "disabled", "--daemon-ssl", "disabled",
          "--rpc-access-control-origins", TestUtils.WALLET_RPC_ACCESS_CONTROL_ORIGINS,
          "--daemon-address", TestUtils.DAEMON_RPC_URI,
          "--wallet-dir", TestUtils.WALLET_RPC_LOCAL_WALLET_DIR,
          "--log-file", TestUtils.WALLET_RPC_LOCAL_WALLET_DIR + "/monero-wallet-rpc.log",
          "--allow-mismatched-daemon-version", "--trusted-daemon", "--non-interactive"));
      if (!TestUtils.DAEMON_RPC_USERNAME.isEmpty()) cmd.addAll(Arrays.asList("--daemon-login", TestUtils.DAEMON_RPC_USERNAME + ":" + TestUtils.DAEMON_RPC_PASSWORD));
      return new MoneroWalletRpc(cmd);
    }
  }

  // mine a block with the daemon so its randomx dataset is initialized before tests stop mining through
  // the wallet servers, whose daemon requests time out after 15 seconds
  private static void initMiningDataset() {
    long height = daemon.getHeight();
    boolean miningStarted = !daemon.getMiningStatus().isActive();
    if (miningStarted) daemon.startMining(TestUtils.MINING_ADDRESS, 1l, false, true);
    while (daemon.getHeight() == height) {
      try { TimeUnit.MILLISECONDS.sleep(100); }
      catch (InterruptedException e) { throw new RuntimeException(e); }
    }
    if (miningStarted) daemon.stopMining();
  }

  private static void mineTo(String address, long height) {
    StartMining.generateBlocks(address, height - daemon.getHeight());
  }

  // confirm a seeded random sequence of transfers so the test wallet has history beyond coinbase
  private static void buildHistory() {
    String seedEnv = System.getenv("MONERO_HISTORY_SEED");
    long seed = seedEnv == null ? new Random().nextLong() : Long.parseLong(seedEnv);
    System.out.println("Building regtest history with seed " + seed);
    Random random = new Random(seed);
    TestUtils.openTestWallet(walletRpc);
    MoneroWalletRpc funder = TestUtils.createWalletRpc(new MoneroWalletConfig().setSeed(TestUtils.MINING_SEED).setServer(daemon.getRpcConnection()));
    try {
      funder.sync();
      if (funder.getUnlockedBalance().signum() == 0) throw new MoneroError("Mining wallet is unfunded on this chain, delete " + TestUtils.REGTEST_DIR + " to start over");
      while (walletRpc.getAccounts().size() < 4) walletRpc.createAccount();
      for (int accountIdx = 0; accountIdx < 4; accountIdx++) while (walletRpc.getSubaddresses(accountIdx).size() < 3) walletRpc.createSubaddress(accountIdx);
      
      // fund accounts 1 to 3, spend from each so each has spent outputs, then fund every subaddress from account 0
      MoneroTxConfig funding = new MoneroTxConfig().setAccountIndex(0).setRelay(true);
      for (int accountIdx = 1; accountIdx < 4; accountIdx++) funding.addDestination(walletRpc.getAddress(accountIdx, 0), TestUtils.MAX_FEE.multiply(BigInteger.valueOf(2)));
      walletRpc.sync();
      walletRpc.createTx(funding);
      StartMining.generateBlocks(10);
      for (int accountIdx = 1; accountIdx < 4; accountIdx++) {
        walletRpc.sync();
        walletRpc.createTx(new MoneroTxConfig().setAccountIndex(accountIdx).setAddress(funder.getPrimaryAddress()).setAmount(TestUtils.MAX_FEE).setRelay(true));
        StartMining.generateBlocks(1);
      }
      funding = new MoneroTxConfig().setAccountIndex(0).setRelay(true);
      for (int accountIdx = 0; accountIdx < 4; accountIdx++) for (int subaddressIdx = 0; subaddressIdx < 3; subaddressIdx++) funding.addDestination(walletRpc.getAddress(accountIdx, subaddressIdx), randomAmount(random));
      walletRpc.sync();
      walletRpc.createTx(funding);
      StartMining.generateBlocks(1);
      
      // random transfers one per block
      while (daemon.getHeight() < FUNDING_HEIGHT + COVERAGE_BLOCKS + HISTORY_ROUNDS) {
        walletRpc.sync();
        funder.sync();
        MoneroTxConfig config = new MoneroTxConfig().setAccountIndex(0).setRelay(true);
        int shape = random.nextInt(4);
        if (shape == 0) funder.createTx(config.addDestination(randomAddress(random), randomAmount(random)));
        else if (shape == 1) walletRpc.createTx(config.addDestination(funder.getPrimaryAddress(), randomAmount(random)));
        else {
          for (int i = random.nextInt(3); i >= 0; i--) config.addDestination(randomAddress(random), randomAmount(random));
          walletRpc.createTx(config);
        }
        StartMining.generateBlocks(1);
      }
      mineTo(TestUtils.MINING_ADDRESS, FIXTURE_HEIGHT);
    } finally {
      funder.close(true);
      TestUtils.stopWalletRpcProcess(funder);
    }
  }

  private static String randomAddress(Random random) {
    return walletRpc.getAddress(random.nextInt(4), random.nextInt(3));
  }

  private static BigInteger randomAmount(Random random) {
    return TestUtils.MAX_FEE.multiply(BigInteger.valueOf(random.nextInt(20) + 1));
  }
}
