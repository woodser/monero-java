package utils;

import java.util.List;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
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
    
    System.out.println("Wallet seed: " + walletJni.getMnemonic());
    System.out.println("Wallet height: " + walletJni.getHeight());
  }
}
