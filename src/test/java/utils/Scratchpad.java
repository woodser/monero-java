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
    
    // create wallet from seed
    MoneroWallet walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
      .setPath("./test_wallets/" + UUID.randomUUID().toString())  // leave blank for in-memory wallet
      .setPassword("abctesting123")
      .setNetworkType(MoneroNetworkType.TESTNET)
      .setServerUri("http://localhost:38081")
      .setServerUsername("superuser")
      .setServerPassword("abctesting123")
      .setSeed("biggest duets beware eskimos coexist igloo pamphlet lagoon odometer hounded jukebox enough pride cocoa nylon wolf geometry buzzer vivid federal idols gang semifinal subtly coexist")
      .setRestoreHeight(573800l));
    walletFull.sync(new WalletSyncPrinter());
    System.out.println("Full wallet daemon height: " + walletFull.getDaemonHeight());
    System.out.println("Full wallet seed: " + walletFull.getSeed());
    
//    walletFull.createTx(new MoneroTxConfig()
//            .addDestination("52FnB7ABUrKJzVQRpbMNrqDFWbcKLjFUq8Rgek7jZEuB6WE2ZggXaTf4FK6H8gQymvSrruHHrEuKhMN3qTMiBYzREKsmRKM", walletFull.getUnlockedBalance(0).divide(new BigInteger("8")).multiply(new BigInteger("1")))
//            .addDestination("52aPELZwrwvVBNK4pvRZPNj4U5EEkZBsNTR2jozCLYyrhQySvYbWebTQEdt7RS9nFnRY9r88eFpt6UcsHKnVpCQDAFKu1Az", walletFull.getUnlockedBalance(0).divide(new BigInteger("8")).multiply(new BigInteger("1")))
//            .setAccountIndex(0)
//            .setRelay(true));
  }
}
