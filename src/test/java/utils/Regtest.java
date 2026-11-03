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

  private static MoneroDaemonRpc daemon;
  private static MoneroWalletRpc walletRpc;
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
    for (MoneroWalletRpc wallet : new ArrayList<MoneroWalletRpc>(TestUtils.WALLET_PORT_OFFSETS.keySet())) TestUtils.stopWalletRpcProcess(wallet);
    if (walletRpc != null && walletRpc.getProcess() != null) walletRpc.stopProcess();
    if (daemon != null && daemon.getProcess() != null) daemon.stopProcess();
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
          "--regtest", "--keep-fakechain", "--offline", "--fixed-difficulty", "1", "--non-interactive", "--no-zmq",
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
