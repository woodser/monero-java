package utils;

import java.util.UUID;

import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListener;

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

    // create wallet from seed on mainnet
    MoneroDaemon daemon = new MoneroDaemonRpc("http://xmr-node.cakewallet.com:18081");
    MoneroWallet walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
      .setPath("./test_wallets/" + UUID.randomUUID().toString())  // leave blank for in-memory wallet
      .setPassword("abctesting123")
      .setNetworkType(MoneroNetworkType.MAINNET)
      .setServerUri("http://xmr-node.cakewallet.com:18081")
      //.setServerProxyUri("127.0.0.1:59787")
      .setRestoreHeight(daemon.getHeight() - 1000)
      .setSeed("inbound boldly fuselage jukebox unveil rounded village summon swiftly aside shuffled rising examine friendly goat rockets girth mugged january yesterday went dented amnesty awful unveil"));
    walletFull.sync(new MoneroWalletListener() {
      @Override
      public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
        System.out.println("Sync progress: " + percentDone + "%, height: " + height + ", startHeight: " + startHeight + ", endHeight: " + endHeight + ", message: " + message);
      }
    });
    System.out.println("Full wallet daemon height: " + walletFull.getDaemonHeight());
    System.out.println("Full wallet seed: " + walletFull.getSeed());

//    walletFull.createTx(new MoneroTxConfig()
//            .addDestination("52FnB7ABUrKJzVQRpbMNrqDFWbcKLjFUq8Rgek7jZEuB6WE2ZggXaTf4FK6H8gQymvSrruHHrEuKhMN3qTMiBYzREKsmRKM", walletFull.getUnlockedBalance(0).divide(new BigInteger("8")).multiply(new BigInteger("1")))
//            .addDestination("52aPELZwrwvVBNK4pvRZPNj4U5EEkZBsNTR2jozCLYyrhQySvYbWebTQEdt7RS9nFnRY9r88eFpt6UcsHKnVpCQDAFKu1Az", walletFull.getUnlockedBalance(0).divide(new BigInteger("8")).multiply(new BigInteger("1")))
//            .setAccountIndex(0)
//            .setRelay(true));
  }
}
