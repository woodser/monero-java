package monero.wallet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.rpc.MoneroRpcConnection;
import monero.utils.MoneroException;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroIncomingTransfer;
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
   * Construct a wallet with a randomly generated seed and default values (i.e. mainnet,
   * no connection, english).
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   */
  public MoneroWalletJni(String path, String password) {
    this(path, password, MoneroNetworkType.MAINNET, null, null);
  }
  
  /**
   * Construct a wallet with a randomly generated seed.
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   * @param networkType is the wallet's network type (default = MoneroNetworkType.MAINNET)
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param language is the wallet and mnemonic's language (default = "English")
   */
  public MoneroWalletJni(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, String language) {
    if (networkType == null) networkType = MoneroNetworkType.MAINNET;
    if (language == null) language = DEFAULT_LANGUAGE;
    if (daemonConnection == null) this.jniWalletHandle = createWalletRandomJni(path, password, networkType.ordinal(), null, null, null, language);
    else this.jniWalletHandle = createWalletRandomJni(path, password, networkType.ordinal(), daemonConnection.getUri(), daemonConnection.getUsername(), daemonConnection.getPassword(), language);
    initCommon();
  }
  
  /**
   * Construct a wallet from a mnemonic phrase.
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   * @param mnemonic is the mnemonic of the wallet to construct
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   */
  public MoneroWalletJni(String path, String password, String mnemonic, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, Long restoreHeight) {
    if (networkType == null) throw new MoneroException("Must provide a network type");
    if (restoreHeight == null) restoreHeight = 0l;
    this.jniWalletHandle = createWalletFromMnemonicJni(path, password, mnemonic, networkType.ordinal(), restoreHeight);
    if (daemonConnection != null) setDaemonConnection(daemonConnection);
    initCommon();
  }

  /**
   * Construct a wallet from an address, view key, and spend key.
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   * @param address is the address of the wallet to construct
   * @param viewKey is the view key of the wallet to construct
   * @param spendKey is the spend key of the wallet to construct
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   * @param language is the wallet and mnemonic's language (default = "English")
   */
  public MoneroWalletJni(String path, String password, String address, String viewKey, String spendKey, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, Long restoreHeight, String language) {
    if (restoreHeight == null) restoreHeight = 0l;
    if (networkType == null) throw new MoneroException("Must provide a network type");
    if (language == null) language = DEFAULT_LANGUAGE;
    this.jniWalletHandle = createWalletFromKeysJni(path, password, address, viewKey, spendKey, networkType.ordinal(), restoreHeight, language);
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
    if (!walletExistsJni(path)) throw new MoneroException("Wallet does not exist at path: " + path);
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

  /**
   * Re-save the wallet at its current path.
   * 
   * Throws an exception if the wallet was not loaded from a path and has not
   * been saved to an explicit path.
   */
  public void save() {
    save(null, null);
  }
  
  /**
   * Save the wallet to the given path, deleting old wallet files if
   * applicable.  // TODO monero-core: why delete old wallet files on store_to? seems dangerous
   * 
   * @param path is the path to save the wallet at
   * @param password is the password to encrypt the wallet
   */
  public void save(String path, String password) {
    String err = saveJni(path, password);
    if (err != null) throw new MoneroException(err);
  }
  
  /**
   * Close the wallet.  // TODO: calling methods after close will access released c++ resources
   */
  public void close() {
    closeJni();
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
    assertTrue("No connection to daemon", daemon.getIsConnected()); // TODO: replace with c++
    
    // wrap and register sync listener as wallet listener if given
    SyncListenerWrapper syncListenerWrapper = null;
    if (listener != null) {
      syncListenerWrapper = new SyncListenerWrapper(listener);
      addListener(syncListenerWrapper);
    }
    
    // sync wallet
    Object[] results = syncJni(startHeight);
    
    // unregister sync listener
    if (syncListenerWrapper != null) removeListener(syncListenerWrapper);
    
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
    String accountsJson = getAccountsJni(includeSubaddresses, tag);
    List<MoneroAccount> accounts = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountsJson, AccountsContainer.class).accounts;
    for (MoneroAccount account : accounts) sanitizeAccount(account);  // TODO: better way?
    return accounts;
  }
  
  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    String accountJson = getAccountJni(accountIdx, includeSubaddresses);
    MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, AccountsContainer.class).accounts.get(0);
    sanitizeAccount(account);
    return account;
  }

  @Override
  public MoneroAccount createAccount(String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    String subaddressesJson = getSubaddressesJni(accountIdx, GenUtils.listToIntArray(subaddressIndices));
    List<MoneroSubaddress> subaddresses = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressesJson, SubaddressesContainer.class).subaddresses;
    for (MoneroSubaddress subaddress : subaddresses) sanitizeSubaddress(subaddress);  // TODO: better way?
    return subaddresses;
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
    String subaddressesJson = getAddressIndexJni(address);
    List<MoneroSubaddress> subaddresses = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressesJson, SubaddressesContainer.class).subaddresses;
    assertEquals("Address does not belong to a subaddress", 1, subaddresses.size());
    return sanitizeSubaddress(subaddresses.get(0));
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
    String blocksJson = getTxsJni(request == null ? null : JsonUtils.serialize(request.getBlock() != null ? request.getBlock() : new MoneroBlock().setTxs(Arrays.asList(request))));
    System.out.println("Received getTxs() response from JNI: " + blocksJson);
    List<MoneroBlock> blocks = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksContainer.class).blocks;
    for (MoneroBlock block : blocks) sanitizeBlock(block);
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    for (MoneroBlock block : blocks) {
      for (MoneroTx tx : block.getTxs()) {
        txs.add((MoneroTxWallet) tx);
      }
    }
    return txs;
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferRequest request) {
    
    // normalize request up to block
    if (request == null) request = new MoneroTransferRequest();
    MoneroTxRequest txRequest = request.getTxRequest() == null ? new MoneroTxRequest() : request.getTxRequest();
    MoneroBlock requestBlock = txRequest.getBlock() == null ? new MoneroBlock() : txRequest.getBlock();
    
    // serialize request from block and deserialize response
    String blocksJson = getTransfersJni(JsonUtils.serialize(requestBlock));
    System.out.println("Received getTransfers() response from JNI: " + blocksJson);
    
    // create custom mapper to deserialize to wallet subclasses
    TxWalletDeserializer txDeserializer = new TxWalletDeserializer();
    SimpleModule module = new SimpleModule("TxWalletDeserializer", new Version(1, 0, 0, null, null, null));
    module.addDeserializer(MoneroTx.class, txDeserializer);
    ObjectMapper mapper = new ObjectMapper();
    mapper.registerModule(module);
    
    // deserialize blocks
    List<MoneroBlock> blocks = JsonUtils.deserialize(mapper, blocksJson, BlocksContainer.class).blocks;
    
    // collect transfers
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    for (MoneroBlock block : blocks) {
      sanitizeBlock(block);
      System.out.println(JsonUtils.serialize(block));
      for (MoneroTx tx : block.getTxs()) {
        MoneroTxWallet txWallet = (MoneroTxWallet) tx;
        if (txWallet.getIncomingTransfers() != null) {
          for (MoneroIncomingTransfer transfer : txWallet.getIncomingTransfers()) transfers.add(transfer);
        }
        if (txWallet.getOutgoingTransfer() != null) transfers.add(txWallet.getOutgoingTransfer());
      }
      throw new RuntimeException("No need to continue ya?");
    }
    return transfers;
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
  
  private native static long createWalletRandomJni(String path, String password, int networkType, String daemonUrl, String daemonUsername, String daemonPassword, String language);
  
  private native static long createWalletFromMnemonicJni(String path, String password, String mnemonic, int networkType, long restoreHeight);
  
  private native static long createWalletFromKeysJni(String path, String password, String address, String viewKey, String spendKey, int networkType, long restoreHeight, String language);
  
  private native String[] getDaemonConnectionJni(); // returns [uri, username, password]
  
  private native void setDaemonConnectionJni(String uri, String username, String password);
  
  private native String getPathJni();
  
  private native int getNetworkTypeJni();
  
  private native String getMnemonicJni();
  
  private native String getLanguageJni();
  
  private native String getAddressJni(int accountIdx, int subaddressIdx);
  
  private native String getAddressIndexJni(String address);
  
  private native long setListenerJni(WalletJniListener listener);
  
  private native Object[] syncJni(long startHeight);
  
  private native long getHeightJni();
  
  private native long getChainHeightJni();
  
  private native long getRestoreHeightJni();
  
  private native String getBalanceWalletJni();
  
  private native String getBalanceAccountJni(int accountIdx);
  
  private native String getBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  private native String getUnlockedBalanceWalletJni();
  
  private native String getUnlockedBalanceAccountJni(int accountIdx);
  
  private native String getUnlockedBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  private native String getAccountsJni(boolean includeSubaddresses, String tag);
  
  private native String getAccountJni(int accountIdx, boolean includeSubaddresses);
  
  private native String getSubaddressesJni(int accountIdx, int[] subaddressIndices);
  
  /**
   * Gets txs from the native layer using strings to communicate.
   * 
   * @param txRequestJson is a tx request serialized to a json string
   * @return a serialized BlocksContainer to preserve model relationships
   */
  private native String getTxsJni(String txRequestJson);
  
  private native String getTransfersJni(String transferRequestJson);
  
  private native String saveJni(String path, String password);
  
  private native void closeJni();
  
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
      for (MoneroWalletListener listener : listeners) {
        listener.onSyncProgress(startHeight, numBlocksDone, numBlocksTotal, percentDone, message);
      }
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
  
  // ------------------------ RESPONSE DESERIALIZATION ------------------------
  
  static class AccountsContainer {
    public List<MoneroAccount> accounts;
  };
  
  static class SubaddressesContainer {
    public List<MoneroSubaddress> subaddresses;
  };
  
  static class BlocksContainer {
    public List<MoneroBlock> blocks;
  }
  
  /**
   * Deserializes a block with wallet subclasses.
   */
  private class TxWalletDeserializer extends StdDeserializer<MoneroTx> {
    
    protected TxWalletDeserializer() {
      super(MoneroTx.class);
    }

    @Override
    public MoneroTx deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
      TreeNode node = p.readValueAsTree();
      return p.getCodec().treeToValue(node, MoneroTxWallet.class);
    }
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  private static MoneroAccount sanitizeAccount(MoneroAccount account) {
    if ("".equals(account.getLabel())) account.setLabel(null);
    if (account.getSubaddresses() != null) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) sanitizeSubaddress(subaddress);
    }
    return account;
  }
  
  private static MoneroSubaddress sanitizeSubaddress(MoneroSubaddress subaddress) {
    if ("".equals(subaddress.getLabel())) subaddress.setLabel(null);
    return subaddress;
  }
  
  private static MoneroBlock sanitizeBlock(MoneroBlock block) {
    return block;
  }
}
