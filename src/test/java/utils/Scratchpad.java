package utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import monero.daemon.model.MoneroBlock;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroTxRequest;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
//    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    MoneroWallet walletRpc = TestUtils.getWalletRpc();
    MoneroWallet walletJni = TestUtils.getWalletJni();
    //MoneroRpc rpc = new MoneroRpc(TestUtils.WALLET_RPC_CONFIG);
    
//    // common variables
    //MoneroTx tx = null;
    //List<MoneroTx> txs = null;
    //List<MoneroTransfer> transfers = null;
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
    
    //walletJni = new MoneroWalletJni(MoneroNetworkType.STAGENET, null, "English");
//    MoneroWalletJni walletJni = new MoneroWalletJni(TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, TestUtils.getDaemonRpc().getRpcConnection(), 300000l);
//    walletJni.sync(new MoneroSyncListener() {
//      @Override
//      public void onSyncProgress(long startHeight, long numBlocksDone, long numBlocksTotal, double percentDone, String message) {
//        if (numBlocksDone % 10000 == 0 || percentDone > .999) System.out.println("onSyncProgress(" + startHeight + ", " + numBlocksDone + ", " + numBlocksTotal + ", " + percentDone + ", " + message + ")");
//      }
//    });
    
//    List<MoneroTxWallet> txs = walletRpc.getTxs();
//    for (MoneroTxWallet tx : txs) {
//      System.out.println(tx.getBlock());
//    }
    
//    for (MoneroTxWallet tx : walletRpc.getTxs()) {
//      if (tx.getIsOutgoing() && tx.getIsIncoming()) {
//        assertTrue(tx.getIsOutgoing());
//        assertFalse(tx.getOutgoingTransfer() == null);
//        System.out.println(tx);
//      }
//    }
    
//    System.out.println("RPC");
//    for (MoneroTxWallet tx : walletRpc.getTxs("c40d5dbf49172a1a42111e414ee243e8c7a45cf0c09c5d91c5cef21672145755")) {
//      System.out.println(tx);
//    }
    
    // TODO: implement height
    List<MoneroTxWallet> txs = walletJni.getTxs();
    MoneroBlock block = null;
    assertFalse(txs.isEmpty());
    for (MoneroTxWallet tx : txs) {
      if (tx.getHeight() != 360559l) continue;
      System.out.println("We have one!!!");
      if (block == null) block = tx.getBlock();
      else {
        if (block != tx.getBlock()) {
          System.out.println("boom");
          System.out.println(block);
          System.out.println("----- VS -----");
          System.out.println(tx.getBlock());
        }
        assertTrue(block == tx.getBlock());
      }
    }
    
//    MoneroTxWallet txInQuestion = walletJni.getTx("c40d5dbf49172a1a42111e414ee243e8c7a45cf0c09c5d91c5cef21672145755");
//    System.out.println(txInQuestion);
    
//    MoneroRpcConnection daemonConnection = new MoneroRpcConnection(TestUtils.DAEMON_RPC_URI, TestUtils.DAEMON_RPC_USERNAME, TestUtils.DAEMON_RPC_PASSWORD);
    //walletJni = new MoneroWalletJni(TestUtils.WALLET_JNI_PATH_1, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, NETWORK_TYPE, daemonConnection, 0l);
    
//    String path = TestUtils.TEST_WALLETS_DIR + "/test_wallet_" + UUID.randomUUID().toString();
//    MoneroWallet walletJni = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, daemonConnection, 0l);
//    walletJni.sync(new MoneroSyncPrinter(1));
    
//    System.out.println("JNI");
//    for (MoneroTxWallet tx : walletJni.getTxs("c40d5dbf49172a1a42111e414ee243e8c7a45cf0c09c5d91c5cef21672145755")) {
//      System.out.println(tx);
//    }
    
//    System.out.println("FETCHING TXS");
//    List<MoneroTransfer> transfers = walletJni.getTransfers(new MoneroTransferRequest().setAccountIndex(0).setTxRequest(new MoneroTxRequest().setIsConfirmed(false)));
//    System.out.println("GOT " + transfers.size() + " TRANSFERS");
//    for (MoneroTransfer transfer : transfers) {
//      assertFalse(transfer.getTx().getIsConfirmed());
//      System.out.println(transfer.getTx().getId() + ": " + transfer);
//    }
//    System.out.println("DONE!!!");
    
    
    //MoneroWalletJni walletJni = new MoneroWalletJni("./test_wallets/test_wallet_1", TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
//
//    System.out.println("Wallet balance: " + walletJni.getBalance());
//    System.out.println("Account 0 balance: " + walletJni.getBalance(0));
//    System.out.println("Account 0 subaddress 1 balance: " + walletJni.getBalance(0, 1));
//    System.out.println("Wallet unlocked balance: " + walletJni.getUnlockedBalance());
//    System.out.println("Account 0 unlocked balance: " + walletJni.getUnlockedBalance(0));
//    System.out.println("Account 0 subaddress 1 unlocked balance: " + walletJni.getUnlockedBalance(0, 1));
//    System.out.println("Wallet seed: " + walletJni.getMnemonic());
//    System.out.println("Wallet address: " + walletJni.getPrimaryAddress());
//    System.out.println("Wallet height: " + walletJni.getHeight());
//    List<MoneroAccount> accounts = walletJni.getAccounts();
//    System.out.println("Wallet has " + accounts.size() + " accounts");
    //walletJni.getTxs(new MoneroTxRequest().setIsOutgoing(true).setId("abcdef"));
    
//    transfers = walletJni.getTransfers(new MoneroTransferRequest().setTxRequest(new MoneroTxRequest().setIsConfirmed(true)));
//    assertFalse(transfers.isEmpty());
//    for (MoneroTransfer transfer : transfers) {
//      //assertEquals(0, (int) transfer.getAccountIndex());
//      assertTrue(transfer.getTx().getIsConfirmed());
//    }
//    System.out.println("Done");
    
    
    
//    System.out.println("Starting...");
//    List<MoneroOutputWallet> outputs = walletJni.getOutputs(new MoneroOutputRequest().setAccountIndex(1));
//    System.out.println("Returned!!!");
//    for (MoneroOutputWallet output : outputs) {
//      assertEquals(1, (int) output.getAccountIndex());
//    }
//    System.out.println("Done");
    

    
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
  }
}
