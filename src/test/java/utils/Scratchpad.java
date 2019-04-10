package utils;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroTx;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
//    MoneroWallet wallet = TestUtils.getWalletRpc();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
//    try { wallet.startMining(8, false, true); }
//    catch (MoneroException e) { }
//    wallet.stopMining();
    daemon.stopMining();
//    daemon.flushTxPool();
    
//    // common variables
//    List<MoneroTx> txs = null;
//    List<MoneroTransfer> transfers = null;
//    String txId = null;
//    List<MoneroOutput> vouts = null;
    
    // -------------------------------- SCRATCHPAD ----------------------------
    
    MoneroTx tx = daemon.getTx("de832e12e213c07af54bece4d070b0ec31c59bb35da0c1460db24a0018ab3396");
    System.out.println(tx.toString());
    
//    List<MoneroAccount> accounts = wallet.getAccounts(true);
//    for (MoneroAccount account : accounts) {
//      System.out.println(account.getUnlockedBalance());
//      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
//        System.out.println("\t" + subaddress.getUnlockedBalance());
//      }
//    }
  }
}
