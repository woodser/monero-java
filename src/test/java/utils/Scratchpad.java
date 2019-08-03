package utils;

import java.util.List;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWalletJni;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletListener;
import test.TestMoneroWalletJni;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  @SuppressWarnings("unused")
  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    //MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    //MoneroWalletJni walletJni = TestUtils.getWalletJni();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
//    // common variables
    //MoneroTx tx = null;
    List<MoneroTxWallet> txs = null;
    //List<MoneroTransfer> transfers = null;
    String txId = null;
    
    // -------------------------------- SCRATCHPAD ----------------------------
    
    // TIMING TEST
    String path = TestMoneroWalletJni.getRandomWalletPath();
    MoneroWalletJni myWallet = MoneroWalletJni.createWalletFromMnemonic(path, TestUtils.WALLET_JNI_PW, TestUtils.MNEMONIC, MoneroNetworkType.STAGENET, TestUtils.getDaemonRpc().getRpcConnection());
    myWallet.save();
    long now = System.currentTimeMillis();;
    myWallet.addListener(new MoneroWalletListener());
    myWallet.sync(new WalletSyncPrinter());
    long newNow = System.currentTimeMillis();
    System.out.println("Sync took " + (((double) newNow - (double) now) / (double) 1000) + " seconds");
    
//    MoneroTransferRequest req = new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setTxId("af908410ce4f9e4e6474be51f0524c30e4aaefb6b2bee490fe72ddea516b34d8"));
////    List<MoneroTransfer> transfers1 = walletRpc.getTransfers(req);
////    System.out.println(transfers1);
//    List<MoneroTransfer> transfers2 = walletJni.getTransfers(req);
//    System.out.println(transfers2);
    
//    List<String> txIds = new ArrayList<String>();
//    txIds.add("04d110bae5645928eb10b242a3cad27a5b6fd5fde0c336f75c40ab234f29d774");
//    txIds.add("a76ae3097320ecb63961dfc0be85823a8c9fbc20a36275e403ed53169b71d817");
//    txIds.add("daa951386877a42226a012117929b626b00a98836cf0ca3c9f1f11c597595dc9");
//    MoneroTransferRequest req = new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setTxIds(txIds));
//    assertFalse(req.getTxRequest().getTransferRequest() == req);
//    List<MoneroTransfer> transfers = walletRpc.getTransfers(req);
//    System.out.println("Found " + transfers.size() + " transfers");
//    for (MoneroTransfer transfer : transfers) {
//      assertTrue(req.meetsCriteria(transfer));
//      assertTrue(txIds.contains(transfer.getTx().getId()));
//    }
    
    //MoneroCppUtils.setLogLevel(1);
    
//    String path = TestMoneroWalletJni.getRandomWalletPath();
//    String password = TestUtils.WALLET_RPC_PW;
//    MoneroRpcConnection daemonConnection = TestUtils.getDaemonRpc().getRpcConnection();
//    String language = "English";
//    String mnemonic = TestUtils.MNEMONIC;
//    MoneroNetworkType networkType = MoneroNetworkType.STAGENET;
//    Long restoreHeight = TestUtils.RESTORE_HEIGHT;
//    
//    MoneroWallet wallet = MoneroWalletJni.createWalletFromMnemonic(path, password, mnemonic, networkType, daemonConnection, restoreHeight);
//    wallet.sync();
//    
//    txs = wallet.getTxs();
//    System.out.println("Wallet height: " + wallet.getHeight());
//    System.out.println("Wallet has " + txs.size() + " txs");
//    
//    long height = 375707;
//    //txs = walletJni.getTxs(new MoneroTxRequest().setMinHeight(height - 30).setMaxHeight(height));
//    txs = wallet.getTxs(new MoneroTxRequest().setMinHeight(375734l));
//    System.out.println("Got " + txs.size() + " txs since that height");
//    for (MoneroTxWallet tx : txs) {
//      System.out.println(tx);
//    }
    
//    // get confirmed transfers to account 0
//    transfers = getAndTestTransfers(wallet, new MoneroTransferRequest().setAccountIndex(0).setTxRequest(new MoneroTxRequest().setIsConfirmed(true)), null, true);
//    for (MoneroTransfer transfer : transfers) {
//      assertEquals(0, (int) transfer.getAccountIndex());
//      assertTrue(transfer.getTx().getIsConfirmed());
//    }
//    assertFalse(transfers.isEmpty());
//    for (MoneroTransfer transfer : transfers) {
//      assertEquals(1, (int) transfer.getAccountIndex());
//      if (transfer instanceof MoneroIncomingTransfer) {
//        assertEquals(2, (int) ((MoneroIncomingTransfer) transfer).getSubaddressIndex());
//      } else {
//        assertTrue(((MoneroOutgoingTransfer) transfer).getSubaddressIndices().contains(2));
//      }
//    }
    
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
  }
}
