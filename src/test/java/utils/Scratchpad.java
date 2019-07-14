package utils;

import static org.junit.Assert.assertFalse;

import java.util.List;

import monero.daemon.MoneroDaemon;
import monero.wallet.MoneroWalletJni;
import monero.wallet.MoneroWalletRpc;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroTxRequest;

/**
 * Scratchpad for quick scripting.
 */
public class Scratchpad {

  @SuppressWarnings("unused")
  public static void main(String[] args) {
    
    // initialize daemon, wallet, and direct rpc interface
    MoneroDaemon daemon = TestUtils.getDaemonRpc();
    MoneroWalletRpc walletRpc = TestUtils.getWalletRpc();
    MoneroWalletJni walletJni = TestUtils.getWalletJni();
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
    
//    MoneroBlock block = daemon.getBlockByHeight(359300l);
//    System.out.println(block);
    
    List<MoneroTxWallet> txs = walletJni.getTxs(new MoneroTxRequest().setMinHeight(364866l).setMaxHeight(364866l));
    assertFalse(txs.isEmpty());
    
    //System.out.println(walletJni.getTx("c40d5dbf49172a1a42111e414ee243e8c7a45cf0c09c5d91c5cef21672145755"));
    
//    System.out.println(walletRpc.getAccounts(true));
//    System.out.println(walletJni.getAccounts(true));
    
//    System.out.println("=== TX 1 ===");
//
//    MoneroTxWallet tx0 = walletRpc.getTx("e0fbdbec23253a8f38520b59eea8984045ac90f1344be9e7858318e56826bcac");
//    System.out.println(tx0);
//    MoneroSubaddress s1 = walletRpc.getSubaddress(0, 0);
//    
//    //System.out.println(s1);
//    
//    //System.out.println(tx0);
//    System.out.println("=== TX 2 ===");
//    String path = "./test_wallets/test_wallet_5";
//    MoneroWalletJni wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.TEST_MNEMONIC, TestUtils.NETWORK_TYPE, walletJni.getDaemonConnection(), 300000l);
//    wallet.sync();
//    System.out.println(wallet.getTx("e0fbdbec23253a8f38520b59eea8984045ac90f1344be9e7858318e56826bcac"));
//    wallet.save();
//    wallet.close();
//    
//    System.out.println("=== TX 3 ===");
//    walletJni.getTx("e0fbdbec23253a8f38520b59eea8984045ac90f1344be9e7858318e56826bcac");
    

    
    
    
//    System.out.println("TXS 1");
//    System.out.println(walletRpc.getTxs());
//    System.out.println("TXS 2");
//    System.out.println(walletJni.getTxs());
//    

//    
//    System.out.println("3");
//    wallet = new MoneroWalletJni(path, TestUtils.WALLET_JNI_PW, TestUtils.NETWORK_TYPE);
//    wallet.setDaemonConnection(walletJni.getDaemonConnection());
//    System.out.println(wallet.getTx("69a0d27a3e019526cb5a969ce9f65f1433b8069b68b3ff3c6a5b992a2983f7a2"));
//    wallet.close();
    
//    // TODO: implement height
//    List<MoneroTxWallet> txs = walletJni.getTxs();
//    MoneroBlock block = null;
//    assertFalse(txs.isEmpty());
//    for (MoneroTxWallet tx : txs) {
//      if (tx.getHeight() != 360559l) continue;
//      System.out.println("We have one!!!");
//      if (block == null) block = tx.getBlock();
//      else {
//        if (block != tx.getBlock()) {
//          System.out.println("boom");
//          System.out.println(block);
//          System.out.println("----- VS -----");
//          System.out.println(tx.getBlock());
//        }
//        assertTrue(block == tx.getBlock());
//      }
//    }
    
//    MoneroTxWallet txInQuestion = walletJni.getTx("c40d5dbf49172a1a42111e414ee243e8c7a45cf0c09c5d91c5cef21672145755");
//    System.out.println(txInQuestion);
    

    
    
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
