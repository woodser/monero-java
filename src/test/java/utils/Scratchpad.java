package utils;

import java.util.List;

import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.model.MoneroTransfer;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
//    MoneroDaemon daemon = TestUtils.getDaemonRpc();
//    MoneroWallet walletRpc = TestUtils.getWalletRpc();
    MoneroWallet walletJni = TestUtils.getWalletJni();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
//    // common variables
    MoneroTx tx = null;
    List<MoneroTx> txs = null;
    List<MoneroTransfer> transfers = null;
    String txId = null;
    
    // -------------------------------- SCRATCHPAD ----------------------------
    
//    String path = "wallet_temp";
//    String password = TestUtils.WALLET_RPC_PW;
//    MoneroRpc daemonConnection = TestUtils.getDaemonRpc().getRpc();
//    String language = "English";
//    String mnemonic = TestUtils.TEST_MNEMONIC;
//    MoneroNetworkType networkType = MoneroNetworkType.STAGENET;
//    Integer restoreHeight = null;
//    
//    MoneroWalletJni.createWalletFromMnemonic(path, password, networkType, daemonConnection, mnemonic, restoreHeight);
//    
//    System.out.println(MoneroWalletJni.walletExists("asdf"));
    
    walletJni = new MoneroWalletJni(null, MoneroNetworkType.STAGENET, "English");
    //walletJni = new MoneroWalletJni(TestUtils.TEST_MNEMONIC, null, 10000, MoneroNetworkType.STAGENET);
    System.out.println("Wallet seed: " + walletJni.getMnemonic());
    System.out.println("Wallet address: " + walletJni.getPrimaryAddress());
    System.out.println("Wallet height: " + walletJni.getHeight());
  }
}
