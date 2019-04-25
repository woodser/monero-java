package test;

import java.math.BigInteger;
import java.util.List;

import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.config.MoneroSendConfig;
import monero.wallet.config.MoneroTransferFilter;
import monero.wallet.config.MoneroTxFilter;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroSendPriority;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;
import utils.TestUtils;

/**
 * Test the sample code in README.md.
 */
public class TestSampleCode {
  
  @Test
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

    // send to an address
    MoneroTxWallet sentTx = wallet.send("74oAtjgE2dfD1bJBo4DWW3E6qXCAwUDMgNqUurnX9b2xUvDTwMwExiXDkZskg7Vct37tRGjzHRqL4gH4H3oag3YyMYJzrNp", new BigInteger("50000"));

    // send to multiple destinations from subaddress 1, 0 which can be split into multiple transactions
    // see MoneroSendConfig.js for all config options or to build a config object
    List<MoneroTxWallet> sentTxs = wallet.sendSplit(new MoneroSendConfig()
            .setAccountIndex(1)
            .setSubaddressIndices(0, 1)
            .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
            .setDestinations(
                    new MoneroDestination("7BV7iyk9T6kfs7cPfmn7vPZPyWRid7WEwecBkkVr8fpw9MmUgXTPtvMKXuuzqKyr2BegWMhEcGGEt5vNkmJEtgnRFUAvf29", new BigInteger("50000")),
                    new MoneroDestination("78NWrWGgyZeYgckJhuxmtDMqo8Kzq5r9j1kV8BQXGq5CDnECz2KjQeBDc3KKvdMQmR6TWtfbRaedgbSGmmwr1g8N1rBMdvW", new BigInteger("50000"))));
    
    // get confirmed transactions
    for (MoneroTxWallet tx : wallet.getTxs(new MoneroTxFilter().setIsConfirmed(true))) {
      String txId = tx.getId();                   // e.g. f8b2f0baa80bf6b...
      BigInteger txFee = tx.getFee();             // e.g. 750000
      boolean isConfirmed = tx.getIsConfirmed();  // e.g. true
    }
    
    // get incoming transfers to account 0
    List<MoneroTransfer> transfers = wallet.getTransfers(new MoneroTransferFilter().setIsIncoming(true).setAccountIndex(0));
    for (MoneroTransfer transfer : transfers) {
      BigInteger amount = transfer.getAmount();   // e.g. 752343011023
      int height = transfer.getTx().getHeight();
      int subaddressIdx = ((MoneroIncomingTransfer) transfer).getSubaddressIndex();
    }
  }
  
  @Test
  public void testDaemonSample() {
    
    // create a daemon that uses a monero-daemon-rpc endpoint
    MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081");
    //MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "admin", "password");
    
    // get daemon info
    int height = daemon.getHeight();                  // e.g. 1523651
    BigInteger feeEstimate = daemon.getFeeEstimate(); // e.g. 750000
    
    // get last block's header
    MoneroBlockHeader lastBlockHeader = daemon.getLastBlockHeader();
    long lastBlockSize = lastBlockHeader.getSize();
    
    // get first 100 blocks as a binary request
    List<MoneroBlock> blocks = daemon.getBlocksByRange(0, 100);
    
    // get block info
    for (MoneroBlock block : blocks) {
      int blockHeight = block.getHeight();
      String blockId = block.getId();
      List<MoneroTx> txs = block.getTxs();
      
      // get tx ids and keys
      for (MoneroTx tx : txs) {
        String txId = tx.getId();
        String txKey = tx.getKey();
      }
    }
    
    // start mining to an address with 4 threads, not in the background, and ignoring the battery
    String address = TestUtils.TEST_ADDRESS;
    //String address = "59aZULsUF3YNSKGiHz4JPMfjGYk...";
    int numThreads = 4;
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
