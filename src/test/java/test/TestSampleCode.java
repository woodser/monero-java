package test;

import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.BeforeClass;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.rpc.MoneroRpcConnection;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSendPriority;
import monero.wallet.model.MoneroSendRequest;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncListener;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletListener;
import utils.TestUtils;

/**
 * Test the sample code for README.md.
 */
public class TestSampleCode {
  
  private static boolean JNI_OUTPUT_RECEIVED = false;
  
  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    TestUtils.TX_POOL_WALLET_TRACKER.reset(); // all wallets need to wait for txs to confirm to reliably sync
  }
  
  @SuppressWarnings("unused")
  @Test
  public void testSampleCode() throws InterruptedException {
    
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
    MoneroWalletJni walletJni = MoneroWalletJni.createWalletFromMnemonic("./test_wallets/" + UUID.randomUUID().toString(), "supersecretpassword123", MoneroNetworkType.STAGENET, TestUtils.MNEMONIC, new MoneroRpcConnection("http://localhost:38081"), TestUtils.FIRST_RECEIVE_HEIGHT, null);
    //MoneroWalletJni walletJni = MoneroWalletJni.createWalletFromMnemonic("MyWallet", "supersecretpassword123", MoneroNetworkType.STAGENET, "hefty value ...", new MoneroRpcConnection("http://localhost:38081"), 501788);
    
    // synchronize the wallet and receive progress notifications
    walletJni.sync(new MoneroSyncListener() {
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
    MoneroTxSet txSet = walletRpc.sendTx(0, walletJni.getPrimaryAddress(), new BigInteger("50000"));
    MoneroTxWallet sentTx = txSet.getTxs().get(0);  // send methods return tx set(s) which contain sent txs unless further steps needed in a multisig or watch-only wallet
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
    
    // create a request to send funds from the RPC wallet to multiple destinations in the JNI wallet
    MoneroSendRequest request = new MoneroSendRequest()
            .setAccountIndex(1)                           // send from account 1
            .setSubaddressIndices(0, 1)                   // send from subaddresses in account 1
            .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
            .setDestinations(
                    new MoneroDestination(walletJni.getAddress(1, 0), new BigInteger("50000")),
                    new MoneroDestination(walletJni.getAddress(2, 0), new BigInteger("50000")));
    
    // create the transaction, confirm with the user, and relay to the network
    MoneroTxWallet createdTx = walletRpc.createTx(request).getTxs().get(0);
    BigInteger fee = createdTx.getFee();  // "Are you sure you want to send ...?"
    walletRpc.relayTx(createdTx); // submit the transaction which will notify the JNI wallet
    
    // JNI wallet will receive notification of incoming output after a moment
    TimeUnit.SECONDS.sleep(10);
    assertTrue(JNI_OUTPUT_RECEIVED);
    
    // save and close the JNI wallet
    walletJni.close(true);
  }
}
