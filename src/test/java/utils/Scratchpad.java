package utils;

import java.util.List;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpcConnection;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroTxRequest;
import test.TestMoneroWalletJni;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  @SuppressWarnings("unused")
  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    //MoneroWalletJni walletJni = TestUtils.getWalletJni();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
//    // common variables
    //MoneroTx tx = null;
    List<MoneroTxWallet> txs = null;
    //List<MoneroTransfer> transfers = null;
    String txId = null;
    
    // -------------------------------- SCRATCHPAD ----------------------------
    
    //MoneroCppUtils.setLogLevel(1);
    
    String path = TestMoneroWalletJni.getRandomWalletPath();
    String password = TestUtils.WALLET_RPC_PW;
    MoneroRpcConnection daemonConnection = TestUtils.getDaemonRpc().getRpcConnection();
    String language = "English";
    String mnemonic = TestUtils.MNEMONIC;
    MoneroNetworkType networkType = MoneroNetworkType.STAGENET;
    Long restoreHeight = TestUtils.RESTORE_HEIGHT;
    
    MoneroWallet wallet = MoneroWalletJni.createWalletFromMnemonic(path, password, mnemonic, networkType, daemonConnection, restoreHeight);
    wallet.sync();
    
    txs = wallet.getTxs();
    System.out.println("Wallet height: " + wallet.getHeight());
    System.out.println("Wallet has " + txs.size() + " txs");
    
    long height = 375707;
    //txs = walletJni.getTxs(new MoneroTxRequest().setMinHeight(height - 30).setMaxHeight(height));
    txs = wallet.getTxs(new MoneroTxRequest().setMinHeight(375734l));
    System.out.println("Got " + txs.size() + " txs since that height");
    for (MoneroTxWallet tx : txs) {
      System.out.println(tx);
    }
    
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
