package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.Ignore;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.rpc.MoneroRpcConnection;
import monero.wallet.MoneroWallet;
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
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletListener;
import utils.TestUtils;

/**
 * Test the sample code for README.md.
 */
public class TestSampleCode {
  
  private static boolean JNI_OUTPUT_RECEIVED = false;
  
  @SuppressWarnings("unused")
  @Test
  public void testSampleCode() throws InterruptedException {
    
    // connect to a daemon
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081");
    long height = daemon.getHeight();                 // 1523651
    BigInteger feeEstimate = daemon.getFeeEstimate(); // 1014313512
    
    // get transactions in the pool
    List<MoneroTx> txsInPool = daemon.getTxPool();
    for (MoneroTx tx : txsInPool) {
      String id = tx.getId();
      BigInteger fee = tx.getFee();
      boolean isDoubleSpendSeen = tx.isDoubleSpendSeen();
    }
    
    // get last 100 blocks as a binary request
    List<MoneroBlock> blocks = daemon.getBlocksByRange(height - 100, height - 1);
    for (MoneroBlock block : blocks) {
      int numTxs = block.getTxs().size();
    }
    
    // connect to a wallet using RPC
    MoneroWallet walletRPC = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
    String primaryAddress = walletRPC.getPrimaryAddress(); // 59aZULsUF3YNSKGiHz4J...
    BigInteger balance = walletRPC.getBalance();           // 533648366742
    MoneroSubaddress subaddress = walletRPC.getSubaddress(1, 0);
    BigInteger subaddressBalance = subaddress.getBalance();
    
    // query a transaction by id
    MoneroTxWallet tx = walletRPC.getTx(walletRPC.getTxs(new MoneroTxQuery().setIsOutgoing(true)).get(0).getId());
    //MoneroTxWallet tx = walletRPC.getTx("314a0f1375db31cea4dac4e0a51514a6282b43792269b3660166d4d2b46437ca");
    long txHeight = tx.getHeight();
    List<MoneroIncomingTransfer> incomingTransfers = tx.getIncomingTransfers();
    List<MoneroDestination> destinations = tx.getOutgoingTransfer().getDestinations();
    
    // query incoming transfers to account 1
    MoneroTransferQuery transferQuery = new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(1);
    List<MoneroTransfer> transfers = walletRPC.getTransfers(transferQuery);
    
    // query unspent outputs
    MoneroOutputQuery outputQuery = new MoneroOutputQuery().setIsSpent(false);
    List<MoneroOutputWallet> outputs = walletRPC.getOutputs(outputQuery);
    
    // create a wallet from a mnemonic phrase using Java native bindings to Monero Core
    MoneroWalletJni walletJNI = MoneroWalletJni.createWalletFromMnemonic("test_wallets/" + UUID.randomUUID().toString(), "supersecretpassword123", MoneroNetworkType.STAGENET, TestUtils.MNEMONIC, new MoneroRpcConnection("http://localhost:38081"), TestUtils.FIRST_RECEIVE_HEIGHT);
    //MoneroWalletJni walletJNI = MoneroWalletJni.createWalletFromMnemonic("MyWallet", "supersecretpassword123", MoneroNetworkType.STAGENET, "hefty value ...", new MoneroRpcConnection("http://localhost:38081"), 384151l);
    
    // synchronize the wallet and receive progress notifications
    walletJNI.sync(new MoneroSyncListener() {
      @Override
      public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
        System.out.println("onSyncProgress(" + height + ", " + startHeight + ", " + endHeight + ", " + percentDone + ", " + message);
      }
    });
    
    // start syncing the wallet continuously in the background
    walletJNI.startSyncing();
    
    // be notified when the JNI wallet receives funds
    walletJNI.addListener(new MoneroWalletListener() {
      
      @Override
      public void onOutputReceived(MoneroOutputWallet output) {
        System.out.println("Wallet received funds!");
        int accountIdx = output.getAccountIndex();
        int subaddressIdx = output.getSubaddressIndex();
        MoneroKeyImage keyImage = output.getKeyImage();
        JNI_OUTPUT_RECEIVED = true;
      }
    });
    
    // send funds from the RPC wallet to the JNI wallet
    MoneroTxWallet sentTx = walletRPC.send(0, walletJNI.getPrimaryAddress(), new BigInteger("50000"));
    assertTrue(sentTx.inTxPool());
    
    // mine with 7 threads to push the network along
    int numThreads = 7;
    boolean isBackground = false;
    boolean ignoreBattery = false;
    walletRPC.startMining(numThreads, isBackground, ignoreBattery);
    
    // wait for the next block to be added to the chain
    MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
    long nextNumTxs = nextBlockHeader.getNumTxs();
    
    // stop mining
    walletRPC.stopMining();
    
    // the transaction is (probably) confirmed
    TimeUnit.SECONDS.sleep(10); // let the wallet refresh
    boolean isConfirmed = walletRPC.getTx(sentTx.getId()).isConfirmed();
    
    // create a request to send funds from the RPC wallet to multiple destinations in the JNI wallet
    MoneroSendRequest request = new MoneroSendRequest()
            .setAccountIndex(1)                           // send from account 1
            .setSubaddressIndices(0, 1)                   // send from subaddreses in account 1
            .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
            .setDestinations(
                    new MoneroDestination(walletJNI.getAddress(1, 0), new BigInteger("50000")),
                    new MoneroDestination(walletJNI.getAddress(2, 0), new BigInteger("50000")));
    
    // create the transaction, confirm with the user, and relay to the network
    MoneroTxWallet createdTx = walletRPC.createTx(request);
    BigInteger fee = createdTx.getFee();  // "Are you sure you want to send ...?"
    walletRPC.relayTx(createdTx); // submit the transaction which will notify the JNI wallet
    
    // JNI wallet will receive notification of incoming output after a moment
    TimeUnit.SECONDS.sleep(10);
    assertTrue(JNI_OUTPUT_RECEIVED);
  }
  
  @Test
  @Ignore
  @Deprecated
  @SuppressWarnings("unused")
  public void testWalletSample() {
    
    // create a wallet that uses a monero-wallet-rpc endpoint with authentication
    MoneroWallet wallet = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");

    // get wallet balance as BigInteger
    BigInteger balance = wallet.getBalance();  // e.g. 533648366742
    
    // get wallet primary address
    String primaryAddress = wallet.getPrimaryAddress();  // e.g. 59aZULsUF3YNSKGiHz4J...
    
    // get address and balance of subaddress [1, 0]
    MoneroSubaddress subaddress = wallet.getSubaddress(1, 0);
    BigInteger subaddressBalance = subaddress.getBalance();
    String subaddressAddress = subaddress.getAddress();
    
    // get incoming and outgoing transfers
    List<MoneroTransfer> transfers = wallet.getTransfers();
    for (MoneroTransfer transfer : transfers) {
      boolean isIncoming = transfer.isIncoming();
      BigInteger amount = transfer.getAmount();
      int accountIdx = transfer.getAccountIndex();
      Long height = transfer.getTx().getHeight();  // will be null if unconfirmed
    }
    
    // get incoming transfers to account 0
    transfers = wallet.getTransfers(new MoneroTransferQuery().setAccountIndex(0).setIsIncoming(true));
    for (MoneroTransfer transfer : transfers) {
      assertTrue(transfer.isIncoming());
      assertEquals(0, (int) transfer.getAccountIndex());
      BigInteger amount = transfer.getAmount();
      Long height = transfer.getTx().getHeight();  // will be null if unconfirmed
    }

    // send to an address from account 0
    MoneroTxWallet sentTx = wallet.send(0, "74oAtjgE2dfD1bJBo4DWW3E6qXCAwUDMgNqUurnX9b2xUvDTwMwExiXDkZskg7Vct37tRGjzHRqL4gH4H3oag3YyMYJzrNp", new BigInteger("50000"));

    // send to multiple destinations from multiple subaddresses in account 1 which can be split into multiple transactions
    // see MoneroSendRequest.java for all request options
    List<MoneroTxWallet> sentTxs = wallet.sendSplit(new MoneroSendRequest()
            .setAccountIndex(1)
            .setSubaddressIndices(0, 1)
            .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
            .setDestinations(
                    new MoneroDestination("7BV7iyk9T6kfs7cPfmn7vPZPyWRid7WEwecBkkVr8fpw9MmUgXTPtvMKXuuzqKyr2BegWMhEcGGEt5vNkmJEtgnRFUAvf29", new BigInteger("50000")),
                    new MoneroDestination("78NWrWGgyZeYgckJhuxmtDMqo8Kzq5r9j1kV8BQXGq5CDnECz2KjQeBDc3KKvdMQmR6TWtfbRaedgbSGmmwr1g8N1rBMdvW", new BigInteger("50000"))));
    
    // get all confirmed wallet transactions
    for (MoneroTxWallet tx : wallet.getTxs(new MoneroTxQuery().setIsConfirmed(true))) {
      String txId = tx.getId();                   // e.g. f8b2f0baa80bf6b...
      BigInteger txFee = tx.getFee();             // e.g. 750000
      boolean isConfirmed = tx.isConfirmed();  // e.g. true
    }
    
    // get a wallet transaction by id
    MoneroTxWallet tx = wallet.getTx("3276252c5a545b90c8e147fcde45d3e1917726470a8f7d4c8977b527a44dfd15");
    String txId = tx.getId();                   // e.g. 69a0d27a3e019526c...
    BigInteger txFee = tx.getFee();             // e.g. 750000
    boolean isConfirmed = tx.isConfirmed();  // e.g. true
  }
  
  @Test
  @Ignore
  @Deprecated
  @SuppressWarnings("unused")
  public void testDaemonSample() {
    
    // create a daemon that uses a monero-daemon-rpc endpoint
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081");
    //MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "admin", "password");
    
    // get daemon info
    long height = daemon.getHeight();                 // e.g. 1523651
    BigInteger feeEstimate = daemon.getFeeEstimate(); // e.g. 750000
    
    // get last block's header
    MoneroBlockHeader lastBlockHeader = daemon.getLastBlockHeader();
    long lastBlockSize = lastBlockHeader.getSize();
    
    // get first 100 blocks as a binary request
    List<MoneroBlock> blocks = daemon.getBlocksByRange(0l, 100l);
    
    // get block info
    for (MoneroBlock block : blocks) {
      long blockHeight = block.getHeight();
      String blockId = block.getId();
      List<MoneroTx> txs = block.getTxs();
      
      // get tx ids and keys
      for (MoneroTx tx : txs) {
        String txId = tx.getId();
        String txKey = tx.getKey();
      }
    }
    
    // start mining to an address with 4 threads, not in the background, and ignoring the battery
    String address = TestUtils.ADDRESS;
    //String address = "74oAtjgE2dfD1bJBo4DWW3E6qXCAwUDMgNqUurnX9b2xUvDTwMwExiXDkZskg7Vct37tRGjzHRqL4gH4H3oag3YyMYJzrNp";
    int numThreads = 7;
    boolean isBackground = false;
    boolean ignoreBattery = false;
    daemon.startMining(address, numThreads, isBackground, ignoreBattery);
    
    // wait for the header of the next block added to the chain
    MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
    long nextNumTxs = nextBlockHeader.getNumTxs();
    
    // stop mining
    daemon.stopMining();
  }
}
