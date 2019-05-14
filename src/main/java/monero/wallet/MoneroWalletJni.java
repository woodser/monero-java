package monero.wallet;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpc;
import monero.utils.MoneroException;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroOutputRequest;
import monero.wallet.request.MoneroSendRequest;
import monero.wallet.request.MoneroTransferRequest;
import monero.wallet.request.MoneroTxRequest;

/**
 * Implements a Monero wallet using JNI to bridge to Monero core c++.
 */
public class MoneroWalletJni extends MoneroWalletDefault {
  
  // load Monero core c++ as a dynamic library
  static {
    System.loadLibrary("monero-java");
  }
  
  // ---------------------------- WALLET MANAGEMENT ---------------------------
  
  /**
   * Indicates if the wallet at the given path exists.
   * 
   * @param path is the path to check for existence
   * @return true if a wallet exists at the given path, false otherwise
   */
  public static boolean walletExists(String path) {
    return walletExistsJni(path);
  }
  
  /**
   * Open a wallet.
   * 
   * @param path is the path on the filesystem of the wallet to open
   * @param password is the password of the wallet
   * @return the opened wallet
   */
  public static MoneroWalletJni openWallet(String path, String password) {
    if (!walletExistsJni(path)) throw new MoneroException("Wallet does not exist: " + path);
    return new MoneroWalletJni(openWalletJni(path, password, 0));
  }
  
  /**
   * Create a wallet with a randomly generated seed.
   * 
   * @param networkType is the wallet's network type (default = MoneroNetworkType.MAINNET)
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param language is the wallet and mnemonic's language (default = "English")
   * @return the created wallet
   */
  public static MoneroWalletJni createWallet(MoneroNetworkType networkType, MoneroRpc daemonConnection, String language) {
    MoneroWalletJni wallet = new MoneroWalletJni(createWalletJni(language, networkType.ordinal()));
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  
  /**
   * Create a wallet from an existing mnemonic phrase.
   * 
   * @param path is the path on the filesystem to create the wallet
   * @param password is the password to encrypt the wallet
   * @param networkType is the wallet's network type (default = MoneroNetworkType.MAINNET)
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param mnemonic is the mnemonic of a wallet to restore (default = randomly generate a new wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   * @return the created wallet
   */
  public static MoneroWalletJni createWalletFromMnemonic(MoneroNetworkType networkType, MoneroRpc daemonConnection, String mnemonic, Integer restoreHeight) {
    MoneroWalletJni wallet = new MoneroWalletJni(createWalletFromMnemonicJni(networkType.ordinal(), mnemonic, restoreHeight));
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  
  /**
   * Create a wallet from existing keys.
   * 
   * @param path is the path on the filesystem to create the wallet
   * @param password is the password to encrypt the wallet
   * @param networkType is the wallet's network type (default = MoneroNetworkType.MAINNET)
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param language is the wallet and mnemonic's language (default = "English")
   * @param mnemonic is the mnemonic of a wallet to restore (default = randomly generate a new wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   * @return the created wallet
   */
  public static MoneroWalletJni createWalletFromKeys(MoneroNetworkType networkType, MoneroRpc daemonConnection, String language, String address, String viewKey, String spendKey, Integer restoreHeight) {
    MoneroWalletJni wallet = new MoneroWalletJni(createWalletFromKeysJni(language, networkType.ordinal(), address, viewKey, spendKey, restoreHeight));
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  
  // instance variables
  private long walletHandle;    // memory address of corresponding wallet in c++; this variable is read directly by name in c++
  private long listenerHandle;  // memory address of corresponding listener in c++; this variable is read directly by name in c++
  
  /**
   * Construct a wallet instance.  The constructor is private so static methods
   * are used.  This wallet instance is given a handle to the memory address of
   * the corresponding wallet in c++.
   * 
   * @param handle is the memory address of the wallet in c++
   */
  private MoneroWalletJni(long handle) {
    this.walletHandle = handle;
    this.listenerHandle = setListenerJni(new WalletListenerJniImpl());
  }
  
  // ------------ WALLET METHODS SPECIFIC TO JNI IMPLEMENTATION ---------------
  
  public void setDaemonConnection(MoneroRpc daemonConnection) {
    if (daemonConnection == null) setDaemonConnectionJni("", "", "");
    else setDaemonConnectionJni(daemonConnection.getUri().toString(), daemonConnection.getUsername(), daemonConnection.getPassword());
  }
  
  public MoneroRpc getDaemonConnection() {
    throw new RuntimeException("Not implemented");
  }
  
  // TODO: comments and other jni specific methods
  
  public String getPath() {
    return getPathJni();
  }
  
  public MoneroNetworkType getNetworkType() {
    return MoneroNetworkType.values()[getNetworkTypeJni()];
  }
  
  public String getLanguage() {
    return getLanguageJni();
  }
  
  // TODO: can set restore height, start refresh, pause refresh, isSynchronized()
  public void pauseSync() {
    throw new RuntimeException("Not implemented");
  }
  
  // TODO: createFromJson? don't automatically create file?
  public String toJson() {
    throw new RuntimeException("Not implemented");
  }
  
  public void save(String path, String password) {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Re-save the wallet at its current path.
   */
  public void save() {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Close the wallet.
   */
  public void close() {
    throw new RuntimeException("Not implemented");
  }
  
  // -------------------------- COMMON WALLET METHODS -------------------------

  @Override
  public String getSeed() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getMnemonic() {
    return getMnemonicJni();
  }

  @Override
  public String getPublicViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getPrivateViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getLanguages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int getHeight() {
    return (int) getHeightJni();  // TODO: switch heights to longs
  }

  @Override
  public int getChainHeight() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSyncResult sync(Integer startHeight, Integer endHeight, MoneroSyncListener listener) {
    if (endHeight != null) throw new MoneroException("Monero core wallet does not support syncing to an end height");
    if (listener != null) throw new RuntimeException("sync listening not yet implemented");
    syncJni(startHeight);
    throw new RuntimeException("Done syncing but need to return sync results");
  }

  @Override
  public void rescanBlockchain() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean isMultisigImportNeeded() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount createAccount(String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    return getAddressJni(accountIdx, subaddressIdx);
  }

  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> getTxs(MoneroTxRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputWallet> getOutputs(MoneroOutputRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxWallet send(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sendSplit(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxWallet sweepOutput(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sweepAllUnlocked(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sweepDust(boolean doNotRelay) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> relayTxs(Collection<String> txMetadatas) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxNotes(Collection<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setTxNotes(Collection<String> txIds, Collection<String> notes) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String sign(String msg) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean verify(String msg, String address, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxKey(String txId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckTx checkTxKey(String txId, String txKey, String address) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxProof(String txId, String address, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckTx checkTxProof(String txId, String address, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getSpendProof(String txId, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean checkSpendProof(String txId, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProofWallet(String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(Collection<Integer> entryIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int addAddressBookEntry(String address, String description, String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void tagAccounts(String tag, Collection<Integer> accountIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void untagAccounts(Collection<Integer> accountIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccountTag> getAccountTags() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setAccountTagLabel(String tag, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String createPaymentUri(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSendRequest parsePaymentUri(String uri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getOutputsHex() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int importOutputsHex(String outputsHex) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setAttribute(String key, String val) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getAttribute(String key) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void startMining(Integer numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopMining() {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------------------ NATIVE METHODS ----------------------------
  
  private native static boolean walletExistsJni(String path);
  
  private native static long openWalletJni(String path, String password, int networkType);
  
  private native static long createWalletJni(String language, int networkType);
  
  private native static long createWalletFromMnemonicJni(int networkType, String mnemonic, Integer restoreHeight);
  
  private native static long createWalletFromKeysJni(String language, int networkType, String address, String viewKey, String spendKey, Integer restoreHeight);
  
  private native void setDaemonConnectionJni(String uri, String username, String password);
  
  private native String getPathJni();
  
  private native int getNetworkTypeJni();
  
  private native String getLanguageJni();
  
  private native long getHeightJni();
  
  private native String getMnemonicJni();
  
  private native String getAddressJni(int accountIdx, int subaddressIdx);
  
  private native long setListenerJni(WalletListenerJni listener);
  
  private native void syncJni(Integer startHeight);
  
  // -------------------------- WALLET LISTENER JNI ---------------------------
  
  /**
   * Interface to receive notifications from C++ over JNI.
   * 
   * TODO: don't longs lose precision?
   */
  private interface WalletListenerJni {
    
    /**
     * Called when any event occurs (send, receive, block processed, etc).
     */
    public void updated();
    
    /**
     * Called when the wallet is refreshed (explicitly or by the background thread).
     */
    public void refreshed();
    
    /**
     * Called when a new block is received.
     * 
     * @param height is the height of the received block
     */
    public void newBlock(long height);
    
    /**
     * Called when funds are sent from the wallet.
     * 
     * @param txId is the id of the outgoing transaction
     * @param amount is the amount sent from the wallet
     */
    public void moneySpent(String txId, long amount);
    
    /**
     * Called when funds are received to the wallet.
     * 
     * @param txId is the id of the incoming transaction
     * @param amount is the amount received to the wallet
     */
    public void moneyReceived(String txId, long amount);

    /**
     * Called when funds are received to the wallet but the tx is still in the tx pool.
     * 
     * @param txId is the id of the incoming and unconfirmed transaction
     * @param amount is the amount received to the wallet
     */
    public void unconfirmedMoneyReceived(String txId, long amount);
  }
  
  /**
   * Handles wallet notifications as they are received from C++ over JNI.
   */
  private class WalletListenerJniImpl implements WalletListenerJni {
    
    @Override
    public void updated() {
      System.out.println("updated()");
    }

    @Override
    public void refreshed() {
      System.out.println("refreshed()");
    }

    @Override
    public void newBlock(long height) {
      System.out.println("newBlock()");
    }

    @Override
    public void moneySpent(String txId, long amount) {
      System.out.println("moneySpent(" + txId + ", " + amount + ")");
    }

    @Override
    public void moneyReceived(String txId, long amount) {
      System.out.println("moneyReceived(" + txId + ", " + amount + ")");
    }

    @Override
    public void unconfirmedMoneyReceived(String txId, long amount) {
      System.out.println("unconfirmedMoneyReceived(" + txId + ", " + amount + ")");
    }
  }
}
