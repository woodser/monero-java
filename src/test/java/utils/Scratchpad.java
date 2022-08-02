package utils;

import java.util.UUID;

import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.model.MoneroWalletConfig;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
//    MoneroDaemon daemon = TestUtils.getDaemonRpc();
//    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
//    MoneroWalletFull walletFull = TestUtils.getWalletFull();
    
    // -------------------------------- SCRATCHPAD ----------------------------
    
    // create wallet from mnemonic
    MoneroWallet walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
      .setPath("./test_wallets/" + UUID.randomUUID().toString())  // leave blank for in-memory wallet
      .setPassword("abctesting123")
      .setNetworkType(MoneroNetworkType.TESTNET)
      .setServerUri("http://localhost:38081")
      .setServerUsername("superuser")
      .setServerPassword("abctesting123")
      .setMnemonic("biggest duets beware eskimos coexist igloo pamphlet lagoon odometer hounded jukebox enough pride cocoa nylon wolf geometry buzzer vivid federal idols gang semifinal subtly coexist")
      .setRestoreHeight(573800l));
    walletFull.sync(new WalletSyncPrinter());
    System.out.println("WASM wallet daemon height: " + walletFull.getDaemonHeight());
    System.out.println("WASM wallet mnemonic: " + walletFull.getMnemonic());
    
//    walletFull.createTx(new MoneroTxConfig()
//            .addDestination("52FnB7ABUrKJzVQRpbMNrqDFWbcKLjFUq8Rgek7jZEuB6WE2ZggXaTf4FK6H8gQymvSrruHHrEuKhMN3qTMiBYzREKsmRKM", walletFull.getUnlockedBalance(0).divide(new BigInteger("8")).multiply(new BigInteger("1")))
//            .addDestination("52aPELZwrwvVBNK4pvRZPNj4U5EEkZBsNTR2jozCLYyrhQySvYbWebTQEdt7RS9nFnRY9r88eFpt6UcsHKnVpCQDAFKu1Az", walletFull.getUnlockedBalance(0).divide(new BigInteger("8")).multiply(new BigInteger("1")))
//            .setAccountIndex(0)
//            .setRelay(true));
    
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
//        numOutputs += tx.getOutputs().size();
//      }
//    }
//
//    System.out.println("Number of blocks: " + numBlocks);
//    System.out.println("Num txs: " + numTxs);
//    System.out.println("Num outputs: " + numOutputs);
//    System.out.println("Total size: " + totalSize);
    
//    // TIMING TEST
//    String path = TestMoneroWalletFull.getRandomWalletPath();
//    MoneroWalletFull myWallet = MoneroWalletFull.createWalletFromMnemonic(path, TestUtils.WALLET_FULL_PW, MoneroNetworkType.TESTNET, TestUtils.MNEMONIC, TestUtils.getDaemonRpc().getRpcConnection());
//    myWallet.save();
//    long now = System.currentTimeMillis();;
//    myWallet.addListener(new MoneroWalletListener());
//    myWallet.sync(new WalletSyncPrinter());
//    long newNow = System.currentTimeMillis();
//    System.out.println("Sync took " + (((double) newNow - (double) now) / (double) 1000) + " seconds");
    
//    // generate 20 random testnet wallets
//    MoneroRpcConnection daemonConnection = new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD);
//    List<String> mnemonics = new ArrayList<String>();
//    List<String> addresses = new ArrayList<String>();
//    for (int i = 0; i < 20; i++) {
//      String temp = UUID.randomUUID().toString();
//      walletFull = new MoneroWalletFull(TestUtils.TEST_WALLETS_DIR + "/" + temp, TestUtils.WALLET_FULL_PW, MoneroNetworkType.TESTNET, daemonConnection, "English");
//      mnemonics.add(walletFull.getMnemonic());
//      addresses.add(walletFull.getPrimaryAddress());
//      ((MoneroWalletFull) walletFull).close();
//    }
//    for (int i = 0; i < 20; i++) {
//      System.out.println(mnemonics.get(i));
//      System.out.println(addresses.get(i));
//    }
  }
}
