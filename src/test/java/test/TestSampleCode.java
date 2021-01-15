package test;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.math.BigInteger;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Test the sample code for README.md.
 */
public class TestSampleCode {
  
  private static boolean FUNDS_RECEIVED = false;
  
  @BeforeAll
  public static void setUpBeforeClass() throws Exception {
    
    // all wallets need to wait for txs to confirm to reliably sync
    TestUtils.WALLET_TX_TRACKER.reset(); 
    
    // pre-create test wallet
    MoneroWalletRpc wallet = TestUtils.getWalletRpc();
    wallet.close();

    // create directory for test wallets if it doesn't exist
    File testWalletsDir = new File(TestUtils.TEST_WALLETS_DIR);
    if (!testWalletsDir.exists()) testWalletsDir.mkdirs();
  }
  
  @SuppressWarnings("unused")
  @Test
  public void testSampleCode() throws InterruptedException {
    
    // connect to daemon
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "superuser", "abctesting123");
    long height = daemon.getHeight();                       // 1523651
    BigInteger feeEstimate = daemon.getFeeEstimate();       // 1014313512
    List<MoneroTx> txsInPool = daemon.getTxPool();          // get transactions in the pool
    
    // open wallet on monero-wallet-rpc
    MoneroWalletRpc walletRpc = new MoneroWalletRpc(TestUtils.WALLET_RPC_URI, TestUtils.WALLET_RPC_USERNAME, TestUtils.WALLET_RPC_PASSWORD); // *** REPLACE WITH CONSTANTS IN README ***
    walletRpc.openWallet("test_wallet_1", "supersecretpassword123");  // *** CHANGE README TO "sample_wallet_rpc" ***
    String primaryAddress = walletRpc.getPrimaryAddress();  // 555zgduFhmKd2o8rPUz...
    BigInteger balance = walletRpc.getBalance();            // 533648366742
    List<MoneroTxWallet> txs = walletRpc.getTxs();          // get transactions containing transfers to/from the wallet
    
    // create wallet from mnemonic phrase using JNI bindings to monero-project
    MoneroWalletFull walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
            .setPath("./test_wallets/" + UUID.randomUUID().toString())  // *** CHANGE README TO "sample_wallet_full" ***
            .setPassword("supersecretpassword123")
            .setNetworkType(MoneroNetworkType.STAGENET)
            .setServerUri("http://localhost:38081")
            .setServerUsername("superuser")
            .setServerPassword("abctesting123")
            .setMnemonic(TestUtils.MNEMONIC)                    // *** REPLACE WITH MNEMONIC IN README ***
            .setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT)); // *** REPLACE WITH FIRST RECEIVE HEIGHT IN README ***
    
    // synchronize the wallet and receive progress notifications
    walletFull.sync(new MoneroWalletListener() {
      @Override
      public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
        // feed a progress bar?
      }
    });
    
    // synchronize in the background every 5 seconds
    walletFull.startSyncing(5000l);
    
    // receive notifications when funds are received, confirmed, and unlocked
    walletFull.addListener(new MoneroWalletListener() {
      @Override
      public void onOutputReceived(MoneroOutputWallet output) {
        BigInteger amount = output.getAmount();
        String txHash = output.getTx().getHash();
        Boolean isConfirmed = output.getTx().isConfirmed();
        Boolean isLocked = output.getTx().isLocked();
        FUNDS_RECEIVED = true;
      }
    });
    
    // send funds from RPC wallet to full wallet
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(walletRpc);                              // *** REMOVE FROM README SAMPLE ***
    TestUtils.WALLET_TX_TRACKER.waitForUnlockedBalance(walletRpc, 0, null, new BigInteger("50000")); // *** REMOVE FROM README SAMPLE ***
    MoneroTxWallet createdTx = walletRpc.createTx(new MoneroTxConfig()
            .setAccountIndex(0)
            .setAddress(walletFull.getAddress(1, 0))
            .setAmount(new BigInteger("50000"))
            .setRelay(false)); // create transaction and relay to the network if true
    BigInteger fee = createdTx.getFee(); // "Are you sure you want to send... ?"
    walletRpc.relayTx(createdTx); // relay the transaction
    
    // recipient receives unconfirmed funds within 5 seconds
    TimeUnit.SECONDS.sleep(5);
    assertTrue(FUNDS_RECEIVED);
    
    // save and close full wallet
    walletFull.close(true);
  }
}
