package test;

import java.math.BigInteger;
import java.util.List;

import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
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
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletListener;

/**
 * Test the sample code in README.md.
 */
public class TestSampleCode2 {
  
  @SuppressWarnings("unused")
  @Test
  public void testSampleCode() {
    
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
    List<MoneroBlock> blocks = daemon.getBlocksByRange(height - 100, height);
    for (MoneroBlock block : blocks) {
      int numTxs = block.getTxs().size();
    }
    
    // mine with 2 threads in the background
    String address = "74oAtjgE2dfD1bJBo4DW...";
    int numThreads = 2;
    boolean isBackground = true;
    boolean ignoreBattery = false;
    daemon.startMining(address, numThreads, isBackground, ignoreBattery);
    daemon.stopMining();

    // wait for the next block to be added to the chain
    MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
    long nextNumTxs = nextBlockHeader.getNumTxs();
    
    // connect to a wallet using RPC
    MoneroWallet walletRPC = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
    BigInteger balance = walletRPC.getBalance();           // 533648366742
    String primaryAddress = walletRPC.getPrimaryAddress(); // 59aZULsUF3YNSKGiHz4J...
    MoneroSubaddress subaddress = walletRPC.getSubaddress(1, 0);
    BigInteger subaddressBalance = subaddress.getBalance();
    
    // query a transaction
    MoneroTxWallet tx = walletRPC.getTx("3276252c5a545b90c8e147fcde45d3e1917726470a8f7d4c8977b527a44dfd15");
    List<MoneroIncomingTransfer> incomingTransfers = tx.getIncomingTransfers();
    List<MoneroDestination> destinations = tx.getOutgoingTransfer().getDestinations();
    
    // query incoming transfers to account 1
    MoneroTransferQuery transferQuery = new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(1);
    List<MoneroTransfer> transfers = walletRPC.getTransfers(transferQuery);
    
    // query unspent outputs
    MoneroOutputQuery outputQuery = new MoneroOutputQuery().setIsSpent(false);
    List<MoneroOutputWallet> outputs = walletRPC.getOutputs(outputQuery);
    
    // create a new wallet using native Java binding to Monero Core
    MoneroWalletJni walletJNI = MoneroWalletJni.createWalletRandom("MyWallet", "supersecretpassword123", MoneroNetworkType.MAINNET);
    walletJNI.startSyncing();
    
    // listen for incoming funds to the JNI wallet
    walletJNI.addListener(new MoneroWalletListener() {
      
      @Override
      public void onOutputReceived(MoneroOutputWallet output) {
        System.out.println("Funds received!"); 
        int accountIdx = output.getAccountIndex();
        int subaddressIdx = output.getSubaddressIndex();
        MoneroKeyImage keyImage = output.getKeyImage();
      }
    });
    
    // send funds from the RPC wallet to the JNI wallet
    MoneroTxWallet sentTx = walletRPC.send(0, walletJNI.getPrimaryAddress(), new BigInteger("50000"));
    
    // create and relay tx which sends funds to multiple destinations in the JNI wallet
    MoneroSendRequest request = new MoneroSendRequest()
            .setAccountIndex(1)                           // send from account 1
            .setSubaddressIndices(0, 1)                   // send from subaddreses in account 1
            .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
            .setDestinations(
                    new MoneroDestination(walletJNI.getAddress(1, 0), new BigInteger("50000")),
                    new MoneroDestination(walletJNI.getAddress(2, 0), new BigInteger("50000")));
    MoneroTxWallet createdTx = walletRPC.createTx(request);
    BigInteger fee = createdTx.getFee();  // could confirm "Are you sure you want to send?"
    walletRPC.relayTx(createdTx);
  }
}
