package monero.wallet;

import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroDaemonListener;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpcConnection;
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
import monero.wallet.model.MoneroWalletListener;
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
  
  // ----------------------------- PUBLIC STATIC ------------------------------
  
  /**
   * Indicates if a wallet exists at the given path.
   * 
   * @param path is the path to check for a wallet
   * @return true if a wallet exists at the given path, false otherwise
   */
  public static boolean walletExists(String path) {
    return walletExistsJni(path);
  }
  
  // ------------------------------- INSTANCE ---------------------------------
  
  // instance variables
  private long jniWalletHandle;                 // memory address of wallet in c++; this variable is read directly by name in c++ // TODO: rename to cppWalletHandle, cppListenerHandle
  private long jniListenerHandle;               // memory address of wallet listener in c++; this variable is read directly by name in c++
  private WalletJniListener jniListener;        // receives notifications from jni c++
  private Set<MoneroWalletListener> listeners;  // externally subscribed wallet listeners
  
  // private static variables
  private static String DEFAULT_LANGUAGE = "English";
  
  /**
   * Construct an unconnected English wallet with a randomly generated seed on mainnet.
   */
  public MoneroWalletJni() {
    this(MoneroNetworkType.MAINNET, null, null);
  }
  
  /**
   * Construct a wallet with a randomly generated seed.
   * 
   * @param networkType is the wallet's network type (default = MoneroNetworkType.MAINNET)
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param language is the wallet and mnemonic's language (default = "English")
   */
  public MoneroWalletJni(MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, String language) {
    if (networkType == null) networkType = MoneroNetworkType.MAINNET;
    if (language == null) language = DEFAULT_LANGUAGE;
    if (daemonConnection == null) this.jniWalletHandle = createWalletRandomJni(networkType.ordinal(), null, null, null, language);
    else this.jniWalletHandle = createWalletRandomJni(networkType.ordinal(), daemonConnection.getUri(), daemonConnection.getUsername(), daemonConnection.getPassword(), language);
    initCommon();
  }
  
  /**
   * Construct a wallet from a mnemonic phrase.
   * 
   * @param mnemonic is the mnemonic of the wallet to construct
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   */
  public MoneroWalletJni(String mnemonic, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, Long restoreHeight) {
    if (networkType == null) throw new MoneroException("Must provide a network type");
    if (restoreHeight == null) restoreHeight = 0l;
    this.jniWalletHandle = createWalletFromMnemonicJni(mnemonic, networkType.ordinal(), restoreHeight);
    if (daemonConnection != null) setDaemonConnection(daemonConnection);
    initCommon();
  }

  /**
   * Construct a wallet from an address, view key, and spend key.
   * 
   * @param address is the address of the wallet to construct
   * @param viewKey is the view key of the wallet to construct
   * @param spendKey is the spend key of the wallet to construct
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   * @param language is the wallet and mnemonic's language (default = "English")
   */
  public MoneroWalletJni(String address, String viewKey, String spendKey, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, Long restoreHeight, String language) {
    if (restoreHeight == null) restoreHeight = 0l;
    if (networkType == null) throw new MoneroException("Must provide a network type");
    if (language == null) language = DEFAULT_LANGUAGE;
    this.jniWalletHandle = createWalletFromKeysJni(address, viewKey, spendKey, networkType.ordinal(), restoreHeight, language);
    if (daemonConnection != null) setDaemonConnection(daemonConnection);
    initCommon();
  }
  
  /**
   * Construct a wallet by opening a wallet file on disk.
   * 
   * @param path is the path to the wallet file to open
   * @param password is the password of the wallet file to open
   * @param networkType is the wallet's network type
   */
  public MoneroWalletJni(String path, String password, MoneroNetworkType networkType) {
    if (!walletExistsJni(path)) throw new MoneroException("Wallet does not exist: " + path);
    if (networkType == null) throw new MoneroException("Must provide a network type");
    this.jniWalletHandle = openWalletJni(path, password, networkType.ordinal());
    initCommon();
  }
  
  private void initCommon() {
    this.jniListener = new WalletJniListener();
    this.listeners = new LinkedHashSet<MoneroWalletListener>();
  }
  
  // ------------ WALLET METHODS SPECIFIC TO JNI IMPLEMENTATION ---------------
  
  public void setDaemonConnection(MoneroRpcConnection daemonConnection) {
    if (daemonConnection == null) setDaemonConnectionJni("", "", "");
    else setDaemonConnectionJni(daemonConnection.getUri().toString(), daemonConnection.getUsername(), daemonConnection.getPassword());
  }
  
  public MoneroRpcConnection getDaemonConnection() {
    String[] vals = getDaemonConnectionJni();
    return vals[0] == null ? null : new MoneroRpcConnection(vals[0], vals[1], vals[2]); // TODO: return same connection if same values
  }
  
  // TODO: comments and other jni specific methods
  
  public String getPath() {
    String path = getPathJni();
    return path.isEmpty() ? null : path;
  }
  
  public MoneroNetworkType getNetworkType() {
    return MoneroNetworkType.values()[getNetworkTypeJni()];
  }
  
  public String getLanguage() {
    return getLanguageJni();
  }
  
  public long getRestoreHeight() {
    return getRestoreHeightJni();
  }
  
  public void addListener(MoneroWalletListener listener) {
    listeners.add(listener);
    jniListener.setIsListening(true);
  }
  
  public void removeListener(MoneroWalletListener listener) {
    if (!listeners.contains(listener)) throw new MoneroException("Listener is not registered to wallet");
    listeners.remove(listener);
    if (listeners.isEmpty()) jniListener.setIsListening(false);
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
  public long getHeight() {
    return getHeightJni();  // TODO: switch heights to longs
  }

  @Override
  public long getChainHeight() {
    return getChainHeightJni();  // TODO: switch heights to longs
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
  public MoneroSyncResult sync(Long startHeight, Long endHeight, MoneroSyncListener listener) {
    if (startHeight == null) startHeight = Math.max(getHeight(), getRestoreHeight());
    if (endHeight != null) throw new MoneroException("Monero core wallet does not support syncing to an end height");
    
    // verify connection to daemon which informs sync end height
    MoneroDaemonRpc daemon = new MoneroDaemonRpc(getDaemonConnection());
    assertTrue("No connection to daemon", daemon.getIsConnected()); // TODO: way to get end height from wallet2?  need to fallback if daemon not connected, let wallet report sync error
    
    // wrap and register sync listener as normal wallet listener
    SyncListenerWrapper syncListenerWrapper = new SyncListenerWrapper(listener);
    addListener(syncListenerWrapper);
    
    // register listener which notifies all listeners of sync updates
    SyncNotifier syncNotifier = new SyncNotifier(startHeight, getChainHeight() - 1);
    addListener(syncNotifier);
    
    // listen for new blocks added to the chain in order to update sync height // TODO: no way to get this from wallet2?
    MoneroDaemonListener syncRangeUpdater = new MoneroDaemonListener() {
      public void onBlockHeader(MoneroBlockHeader header) {
        syncNotifier.setEndHeight(header.getHeight());
      }
    };
    daemon.addListener(syncRangeUpdater);
    
    // sync wallet
    syncNotifier.onStart(); // notify sync listeners of 0% progress
    Object[] results = syncJni(startHeight);
    
    // unregister listeners
    removeListener(syncNotifier);
    removeListener(syncListenerWrapper);
    daemon.removeListener(syncRangeUpdater);
    
    // return results
    return new MoneroSyncResult((long) results[0], (boolean) results[1]);
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
    
//    m_wallet->get_num_subaddress_accounts()
//    m_wallet->get_num_subaddresses(accountIdx)  // returns 0 if account out of index
//    m_wallet->balance_per_subaddress

    
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
    return new BigInteger(getBalanceWalletJni());
  }

  @Override
  public BigInteger getBalance(int accountIdx) {
    return new BigInteger(getBalanceAccountJni(accountIdx));
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    return new BigInteger(getBalanceSubaddressJni(accountIdx, subaddressIdx));
  }

  @Override
  public BigInteger getUnlockedBalance() {
    return new BigInteger(getUnlockedBalanceWalletJni());
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    return new BigInteger(getUnlockedBalanceAccountJni(accountIdx));
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    return new BigInteger(getUnlockedBalanceSubaddressJni(accountIdx, subaddressIdx));
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
  
  private native static long createWalletRandomJni(int networkType, String daemonUrl, String daemonUsername, String daemonPassword, String language);
  
  private native static long createWalletFromMnemonicJni(String mnemonic, int networkType, long restoreHeight);
  
  private native static long createWalletFromKeysJni(String address, String viewKey, String spendKey, int networkType, long restoreHeight, String language);
  
  private native String[] getDaemonConnectionJni(); // returns [uri, username, password]
  
  private native void setDaemonConnectionJni(String uri, String username, String password);
  
  private native String getPathJni();
  
  private native int getNetworkTypeJni();
  
  private native String getLanguageJni();
  
  private native long getHeightJni();
  
  private native long getChainHeightJni();
  
  private native long getRestoreHeightJni();
  
  private native String getMnemonicJni();
  
  private native String getAddressJni(int accountIdx, int subaddressIdx);
  
  private native String getBalanceWalletJni();
  
  private native String getBalanceAccountJni(int accountIdx);
  
  private native String getBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  private native String getUnlockedBalanceWalletJni();
  
  private native String getUnlockedBalanceAccountJni(int accountIdx);
  
  private native String getUnlockedBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  private native long setListenerJni(WalletJniListener listener);
  
  private native Object[] syncJni(long startHeight);
  
  // ------------------------------- LISTENERS --------------------------------
  
  /**
   * Receives notifications from jni c++.
   */
  @SuppressWarnings("unused") // called directly from jni c++
  private class WalletJniListener {
    
    /**
     * Enables or disables listening in the c++ wallet.
     */
    public void setIsListening(boolean isEnabled) {
      jniListenerHandle = setListenerJni(isEnabled ? this : null);
    }
    
    /**
     * Called when a new block is received.
     * 
     * @param height is the height of the received block
     */
    public void onNewBlock(long height) {
      
      // create header  // TODO: build more complete header?
      MoneroBlockHeader header = new MoneroBlockHeader();
      header.setHeight(height);
      
      // notify external listeners
      for (MoneroWalletListener listener : listeners) listener.onNewBlock(header);
    }

    public void onSyncProgress(long startHeight, long numBlocksDone, long numBlocksTotal, double percentDone, String message) {
      System.out.println("Java received sync notification!");
    }
    
//    /**
//     * Called when funds are sent from the wallet.
//     * 
//     * @param txId is the id of the outgoing transaction
//     * @param amount is the amount sent from the wallet
//     */
//    public void moneySpent(String txId, long amount) { }
//    
//    /**
//     * Called when funds are received to the wallet.
//     * 
//     * @param txId is the id of the incoming transaction
//     * @param amount is the amount received to the wallet
//     */
//    public void moneyReceived(String txId, long amount) { }
//
//    /**
//     * Called when funds are received to the wallet but the tx is still in the tx pool.
//     * 
//     * @param txId is the id of the incoming and unconfirmed transaction
//     * @param amount is the amount received to the wallet
//     */
//    public void unconfirmedMoneyReceived(String txId, long amount)  { }
  }
  
  /**
   * Wraps a sync listener as a general wallet listener.
   */
  private class SyncListenerWrapper extends MoneroWalletListener {
    
    private MoneroSyncListener listener;
    
    public SyncListenerWrapper(MoneroSyncListener listener) {
      this.listener = listener;
    }
    
    @Override
    public void onSyncProgress(long startHeight, long numBlocksDone, long numBlocksTotal, double percentDone, String message) {
      listener.onSyncProgress(startHeight, numBlocksDone, numBlocksTotal, percentDone, message);
    }
  }
  
  /**
   * Listens for new blocks in order to notify wallet listeners of sync updates.
   */
  private class SyncNotifier extends MoneroWalletListener {
    
    private long startHeight;
    private long numBlocksTotal;
    
    public SyncNotifier(long startHeight, long endHeight) {
      this.startHeight = startHeight;
      this.numBlocksTotal = endHeight - startHeight + 1;
    }
  
    // update the sync end height as blocks are added to the chain to report accurate progress
    public void setEndHeight(long endHeight) {
      this.numBlocksTotal = endHeight - startHeight + 1;
    }
    
    public void onStart() {
      if (numBlocksTotal <= 0) return;  // don't notify if no blocks to process
      
      // notify external listeners
      for (MoneroWalletListener listener : listeners) {
        listener.onSyncProgress(startHeight, 0, numBlocksTotal, 0, "Synchronizing");
      }
    }
    
    @Override
    public void onNewBlock(MoneroBlockHeader header) {
      
      // ignore if block is not applicable to wallet
      if (header.getHeight() < startHeight) return;
  
      // update num blocks total if this block exceeds original end height (i.e. block added to chain)
      if (header.getHeight() > startHeight + numBlocksTotal - 1) numBlocksTotal = header.getHeight() - startHeight + 1;
      
      // prepare notification params
      long numBlocksDone = header.getHeight() - startHeight + 1;
      double percentDone = numBlocksDone / (double) numBlocksTotal;
      String message = "Synchronizing";
      
      // notify external listeners
      for (MoneroWalletListener listener : listeners) {
        listener.onSyncProgress(startHeight, numBlocksDone, numBlocksTotal, percentDone, message);
      }
    }
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
}
