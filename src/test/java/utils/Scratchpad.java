package utils;

import monero.daemon.MoneroDaemon;
import monero.utils.MoneroException;
import monero.wallet.MoneroWallet;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    MoneroWallet wallet = TestUtils.getWalletRpc();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
    
//    for (int i = 0; i < 10; i++) {
//      wallet.startMining(8, false, true);
//      daemon.stopMining();
//    }
    
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
    
//    MoneroTxWallet tx = wallet.getTxs(new MoneroTxFilter().setTxId("80abdb2be7fdf07249967a581150da77069a41cd28385701ce1027606fceb677")).get(0);
//    System.out.println(tx.toString());
//    assertTrue(tx.getBlock().getTxs().contains(tx));
    
//    List<MoneroAccount> accounts = wallet.getAccounts(true);
//    for (MoneroAccount account : accounts) {
//      System.out.println(account.getUnlockedBalance());
//      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
//        System.out.println("\t" + subaddress.getUnlockedBalance());
//      }
//    }
  }
}
