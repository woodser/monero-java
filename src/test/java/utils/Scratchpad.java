package utils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroNetworkType;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroSendRequest;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroTxWallet;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  @SuppressWarnings("unused")
  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    MoneroWalletJni walletJni = TestUtils.getWalletJni();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
//    // common variables
    //MoneroTx tx = null;
    List<MoneroTxWallet> txs = null;
    //List<MoneroTransfer> transfers = null;
    String txId = null;
    
    // -------------------------------- SCRATCHPAD ----------------------------
    
    
    
    // MEASURE LAST 30 DAYS
//    int numBlocks = 30 * 24 * 60 / 2;
//    
//    List<MoneroBlockHeader> headers = daemon.getBlockHeadersByRange(daemon.getHeight() - (numBlocks * 2), daemon.getHeight() - (numBlocks * 1));
//    long totalSize = 0;
//    int numOutputs = 0;
//    int numTxs = 0;
//    for (MoneroBlockHeader header : headers) {
//      totalSize += header.getSize();
//      numTxs += header.getNumTxs();
//    }
//    
//    for (MoneroBlock block : daemon.getBlocksByRange(daemon.getHeight() - numBlocks, daemon.getHeight() - 1)) {
//      for (MoneroTx tx : block.getTxs()) {
//        numOutputs += tx.getVouts().size();
//      }
//    }
//    
//    System.out.println("Number of blocks: " + numBlocks);
//    System.out.println("Num txs: " + numTxs);
//    System.out.println("Num outputs: " + numOutputs);
//    System.out.println("Total size: " + totalSize);
    
//    // TIMING TEST
//    String path = TestMoneroWalletJni.getRandomWalletPath();
//    MoneroWalletJni myWallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, MoneroNetworkType.STAGENET, TestUtils.MNEMONIC, TestUtils.getDaemonRpc().getRpcConnection());
//    myWallet.save();
//    long now = System.currentTimeMillis();;
//    myWallet.addListener(new MoneroWalletListener());
//    myWallet.sync(new WalletSyncPrinter());
//    long newNow = System.currentTimeMillis();
//    System.out.println("Sync took " + (((double) newNow - (double) now) / (double) 1000) + " seconds");
    
//    // generate 20 random stagenet wallets
//    MoneroRpcConnection daemonConnection = new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD);
//    List<String> mnemonics = new ArrayList<String>();
//    List<String> addresses = new ArrayList<String>();
//    for (int i = 0; i < 20; i++) {
//      String temp = UUID.randomUUID().toString();
//      walletJni = new MoneroWalletJni(TestUtils.TEST_WALLETS_DIR + "/" + temp, TestUtils.WALLET_JNI_PW, MoneroNetworkType.STAGENET, daemonConnection, "English");
//      mnemonics.add(walletJni.getMnemonic());
//      addresses.add(walletJni.getPrimaryAddress());
//      ((MoneroWalletJni) walletJni).close();
//    }
//    for (int i = 0; i < 20; i++) {
//      System.out.println(mnemonics.get(i));
//      System.out.println(addresses.get(i));
//    }
    
    // create and fund 10 wallets
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(walletJni);
    for (int i = 0; i < 10; i++) {
      
      // create wallet and create and collect subaddresses
      List<String> subaddresses = new ArrayList<String>();
      String path = TestUtils.TEST_WALLETS_DIR + "/funded_" + UUID.randomUUID().toString();
      MoneroWalletJni fundedWallet = MoneroWalletJni.createWalletRandom(path, TestUtils.WALLET_PASSWORD, MoneroNetworkType.STAGENET);
      for (int accountIdx = 0; accountIdx < 3; accountIdx++) {
        fundedWallet.createAccount();
        for (int subaddressIdx = 0; subaddressIdx < 5; subaddressIdx++) {
          fundedWallet.createSubaddress(accountIdx);
          subaddresses.add(fundedWallet.getAddress(accountIdx, subaddressIdx));
        }
      }
      fundedWallet.close(true);
      
      // wait for unlocked funds
      while (walletJni.getUnlockedBalance(0).compareTo(TestUtils.MAX_FEE) < 0) {
        System.out.println("Waiting...");
        try { TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL); }
        catch (InterruptedException e) { throw new RuntimeException(e.getMessage()); }
      }
      
      // transfer funds to subaddresses
      List<MoneroDestination> destinations = new ArrayList<MoneroDestination>();
      for (String address : subaddresses) destinations.add(new MoneroDestination(address, TestUtils.MAX_FEE.multiply(BigInteger.valueOf(2))));
      System.out.println("Transferring....");
      MoneroTxSet txSet = walletJni.sendSplit(new MoneroSendRequest().setDestinations(destinations).setAccountIndex(0));
      System.out.println("Tx set has " + txSet.getTxs().size() + " transactions");
      for (MoneroTxWallet tx : txSet.getTxs()) System.out.println(tx);
    }
  }
}
