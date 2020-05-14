package test;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.math.BigInteger;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import monero.common.MoneroRpcConnection;
import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxPriority;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import utils.TestUtils;

/**
 * Test the sample code for README.md.
 */
public class TestSampleCode {
  
  private static boolean JNI_OUTPUT_RECEIVED = false;
  
  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    
    // all wallets need to wait for txs to confirm to reliably sync
    TestUtils.TX_POOL_WALLET_TRACKER.reset(); 
    
    // pre-create test wallet
    MoneroWalletRpc wallet = TestUtils.getWalletRpc();
    wallet.close();

    // create directory for test wallets if it doesn't exist
    File testWalletsDir = new File(TestUtils.TEST_WALLETS_DIR);
    if (!testWalletsDir.exists()) testWalletsDir.mkdirs();
  }
  
  @SuppressWarnings("unused")
  @Test
  public void testSampleCodeShort() throws InterruptedException {
    
    // connect to a daemon
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "superuser", "abctesting123");
    long height = daemon.getHeight();                       // 1523651
    BigInteger feeEstimate = daemon.getFeeEstimate();       // 1014313512
    List<MoneroTx> txsInPool = daemon.getTxPool();          // get transactions in the pool
    
    // open wallet on monero-wallet-rpc
    MoneroWalletRpc walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
    walletRpc.openWallet("test_wallet_1", "supersecretpassword123");  // *** CHANGE README TO "sample_wallet_rpc" ***
    String primaryAddress = walletRpc.getPrimaryAddress();  // 555zgduFhmKd2o8rPUz...
    BigInteger balance = walletRpc.getBalance();            // 533648366742
    List<MoneroTxWallet> txs = walletRpc.getTxs();          // get transactions containing transfers to/from the wallet
    
    // create wallet from mnemonic phrase using JNI bindings to Monero Core
    MoneroWalletJni walletJni = MoneroWalletJni.createWallet(new MoneroWalletConfig()
            .setPath("./test_wallets/" + UUID.randomUUID().toString())  // *** CHANGE README TO "sample_wallet_jni" ***
            .setPassword("supersecretpassword123")
            .setNetworkType(MoneroNetworkType.STAGENET)
            .setServerUri("http://localhost:38081")
            .setServerUsername("superuser")
            .setServerPassword("abctesting123")
            .setMnemonic("spying swept ashtray going hence jester swagger cease spying unusual boss vain dyslexic divers among unfit asleep bays ostrich maverick skirting jaunt scenic shuffled spying")
            .setRestoreHeight(573936l));
    
    // synchronize the wallet and receive progress notifications
    walletJni.sync(new MoneroWalletListener() {
      @Override
      public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
        // feed a progress bar?
      }
    });
    
    // synchronize in the background
    walletJni.startSyncing();
    
    // listen for incoming transfers
    walletJni.addListener(new MoneroWalletListener() {
      @Override
      public void onOutputReceived(MoneroOutputWallet output) {
        BigInteger amount = output.getAmount();
        String txHash = output.getTx().getHash();
        JNI_OUTPUT_RECEIVED = true;
      }
    });
    
    // send funds from RPC wallet to JNI wallet
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(walletRpc); // *** REMOVE FROM README SAMPLE ***
    MoneroTxWallet sentTx = walletRpc.createTx(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(walletJni.getAddress(1, 0))
            .setAmount(new BigInteger("50000"))
            .setRelay(true));
    String txHash = sentTx.getHash();
    
    // wallet receives unconfirmed funds within 10 seconds
    TimeUnit.SECONDS.sleep(10);
    assertTrue(JNI_OUTPUT_RECEIVED);
    
    // save and close JNI wallet
    walletJni.close(true);
  }
  
  @SuppressWarnings("unused")
  @Test
  @Ignore
  public void testSampleCodeLong() throws InterruptedException {
    
    // connect to a daemon
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "superuser", "abctesting123");
    long height = daemon.getHeight();                 // 1523651
    BigInteger feeEstimate = daemon.getFeeEstimate(); // 1014313512
    
    // get transactions in the pool
    List<MoneroTx> txsInPool = daemon.getTxPool();
    for (MoneroTx tx : txsInPool) {
      String hash = tx.getHash();
      BigInteger fee = tx.getFee();
      boolean isDoubleSpendSeen = tx.isDoubleSpendSeen();
    }
    
    // get last 100 blocks as a binary request
    List<MoneroBlock> blocks = daemon.getBlocksByRange(height - 100, height - 1);
    for (MoneroBlock block : blocks) {
      int numTxs = block.getTxs().size();
    }
    
    // connect to a monero-wallet-rpc endpoint with authentication
    MoneroWalletRpc walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
    
    // open a wallet on the server
    walletRpc.openWallet("test_wallet_1", "supersecretpassword123");
    String primaryAddress = walletRpc.getPrimaryAddress(); // 59aZULsUF3YNSKGiHz4J...
    BigInteger balance = walletRpc.getBalance();           // 533648366742
    MoneroSubaddress subaddress = walletRpc.getSubaddress(1, 0);
    BigInteger subaddressBalance = subaddress.getBalance();
    
    // query a transaction by hash
    MoneroTxWallet tx = walletRpc.getTx(walletRpc.getTxs(new MoneroTxQuery().setIsOutgoing(true)).get(0).getHash());  // *** REMOVE FROM README SAMPLE ***
    //MoneroTxWallet tx = walletRpc.getTx("32088012e68be1c090dc022f7852ca4d7c23066241649cdfaeb14ec1fd5a10f8");
    long txHeight = tx.getHeight();
    List<MoneroIncomingTransfer> incomingTransfers = tx.getIncomingTransfers();
    List<MoneroDestination> destinations = tx.getOutgoingTransfer().getDestinations();
    
    // query incoming transfers to account 1
    MoneroTransferQuery transferQuery = new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(1);
    List<MoneroTransfer> transfers = walletRpc.getTransfers(transferQuery);
    
    // query unspent outputs
    MoneroOutputQuery outputQuery = new MoneroOutputQuery().setIsSpent(false);
    List<MoneroOutputWallet> outputs = walletRpc.getOutputs(outputQuery);
    
    // create a wallet from a mnemonic phrase using Java native bindings to Monero Core
    MoneroWalletJni walletJni = MoneroWalletJni.createWallet(new MoneroWalletConfig()   // *** REPLACE WITH BELOW FOR README ***
            .setPath("./test_wallets/" + UUID.randomUUID())
            .setPassword("supersecretpassword123")
            .setNetworkType(MoneroNetworkType.STAGENET)
            .setMnemonic(TestUtils.MNEMONIC)
            .setServer(new MoneroRpcConnection("http://localhost:38081", "superuser", "abctesting123"))
            .setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
//    MoneroWalletJni walletJni = MoneroWalletJni.createWallet(new MoneroWalletConfig()
//            .setPath("MyWallet")
//            .setPassword("supersecretpassword123")
//            .setNetworkType(MoneroNetworkType.STAGENET)
//            .setMnemonic("hefty value ...")
//            .setServer(new MoneroRpcConnection("http://localhost:38081", "superuser", "abctesting123"))
//            .setRestoreHeight(501788L));
    
    // synchronize the wallet and receive progress notifications
    walletJni.sync(new MoneroWalletListener() {
      @Override
      public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
        // feed a progress bar?
      }
    });
    
    // start syncing the wallet continuously in the background
    walletJni.startSyncing();
    
    // receive notifications when the JNI wallet receives funds
    walletJni.addListener(new MoneroWalletListener() {
      
      @Override
      public void onOutputReceived(MoneroOutputWallet output) {
        System.out.println("Wallet received funds!");
        String txHash = output.getTx().getHash();
        int accountIdx = output.getAccountIndex();
        int subaddressIdx = output.getSubaddressIndex();
        JNI_OUTPUT_RECEIVED = true;
      }
    });
    
    // send funds from the RPC wallet to the JNI wallet
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(walletRpc); // wait for txs to clear pool *** REMOVE FROM README SAMPLE ***
    MoneroTxWallet sentTx = walletRpc.createTx(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(walletJni.getPrimaryAddress())
            .setAmount(new BigInteger("50000"))
            .setRelay(true));
    assertTrue(sentTx.inTxPool());
    
    // mine with 7 threads to push the network along
    long numThreads = 7;
    boolean isBackground = false;
    boolean ignoreBattery = false;
    walletRpc.startMining(numThreads, isBackground, ignoreBattery);
    
    // wait for the next block to be added to the chain
    MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
    long nextNumTxs = nextBlockHeader.getNumTxs();
    
    // stop mining
    walletRpc.stopMining();
    
    // the transaction is (probably) confirmed
    TimeUnit.SECONDS.sleep(10); // wait 10s for auto refresh
    boolean isConfirmed = walletRpc.getTx(sentTx.getHash()).isConfirmed();
    
    // create a config to send funds from the RPC wallet to multiple destinations in the JNI wallet
    MoneroTxConfig config = new MoneroTxConfig()
            .setAccountIndex(1)                           // send from account 1
            .setSubaddressIndices(0, 1)                   // send from subaddresses in account 1
            .setPriority(MoneroTxPriority.UNIMPORTANT)  // no rush
            .setDestinations(
                    new MoneroDestination(walletJni.getAddress(1, 0), new BigInteger("50000")),
                    new MoneroDestination(walletJni.getAddress(2, 0), new BigInteger("50000")));
    
    // create the transaction, confirm with the user, and relay to the network
    MoneroTxWallet createdTx = walletRpc.createTx(config);
    BigInteger fee = createdTx.getFee();  // "Are you sure you want to send ...?"
    walletRpc.relayTx(createdTx); // submit the transaction which will notify the JNI wallet
    
    // JNI wallet will receive notification of incoming output after a moment
    TimeUnit.SECONDS.sleep(10);
    assertTrue(JNI_OUTPUT_RECEIVED);
    
    // save and close the JNI wallet
    walletJni.close(true);
  }
}
