package monero.wallet;

import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonProperty;

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
    return vals == null ? null : new MoneroRpcConnection(vals[0], vals[1], vals[2]); // TODO: return same connection if same values
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
  
  public void setRestoreHeight(long restoreHeight) {
    setRestoreHeightJni(restoreHeight);
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
   * Save the wallet at its current path.
   */
  public void save() {
    saveJni();
  }
  
  /**
   * Move the wallet from its current path to the given path.
   * 
   * @param path is the new wallet's path
   * @param password is the new wallet's password // TODO: can this be used to change wallet password?
   */
  public void moveTo(String path, String password) {
    moveToJni(path, password);
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
    return getPrivateViewKeyJni();
  }

  @Override
  public String getPrivateViewKey() {
    return getPrivateViewKeyJni();
  }

  @Override
  public List<String> getLanguages() {
    return Arrays.asList(getLanguagesJni());
  }

  @Override
  public long getHeight() {
    return getHeightJni();
  }

  @Override
  public long getChainHeight() {
    try {
      return getChainHeightJni();
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    try {
      String integratedAddressJson = getIntegratedAddressJni("", paymentId);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, integratedAddressJson, MoneroIntegratedAddress.class);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    try {
      String integratedAddressJson = decodeIntegratedAddressJni(integratedAddress);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, integratedAddressJson, MoneroIntegratedAddress.class);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
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
    MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
    sanitizeAccount(account);
    return account;
  }

  @Override
  public MoneroAccount createAccount(String label) {
    String accountJson = createAccountJni(label);
    MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
    sanitizeAccount(account);
    return account;
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    String subaddressesJson = getSubaddressesJni(accountIdx, GenUtils.listToIntArray(subaddressIndices));
    System.out.println("Deserializing subaddresses: " + subaddressesJson);
    List<MoneroSubaddress> subaddresses = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressesJson, SubaddressesContainer.class).subaddresses;
    for (MoneroSubaddress subaddress : subaddresses) sanitizeSubaddress(subaddress);  // TODO: better way?
    return subaddresses;
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    String subaddressJson = createSubaddressJni(accountIdx, label);
    MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
    sanitizeSubaddress(subaddress);
    return subaddress;
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    return getAddressJni(accountIdx, subaddressIdx);
  }

  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    String subaddressJson = getAddressIndexJni(address);
    System.out.println("Deserialize: " + subaddressJson);
    MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
    return sanitizeSubaddress(subaddress);
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
    
    // initialize request up to block
    if (request == null) request = new MoneroTxRequest();
    if (request.getBlock() == null) request.setBlock(new MoneroBlock().setTxs(request));
    
    // serialize request from block and fetch txs from jni
    String blocksJson;
    try {
      blocksJson = getTxsJni(JsonUtils.serialize(request.getBlock()));
      System.out.println("Received getTxs() response from JNI: " + blocksJson.substring(0, Math.min(5000, blocksJson.length())) + "...");
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
    
    // deserialize blocks
    List<MoneroBlockWallet> blocks = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksContainer.class).blocks;
    
    // collect txs
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (blocks != null) {
      for (MoneroBlock block : blocks) {
        sanitizeBlock(block);
        for (MoneroTx tx : block.getTxs()) {
          if (block.getHeight() == null) tx.setBlock(null); // dereference placeholder block for unconfirmed txs
          txs.add((MoneroTxWallet) tx);
        }
      }
    }
    
    // re-sort txs which is lost over jni serialization
    if (request.getTxIds() != null) {
      Map<String, MoneroTxWallet> txMap = new HashMap<String, MoneroTxWallet>();
      for (MoneroTxWallet tx : txs) txMap.put(tx.getId(), tx);
      List<MoneroTxWallet> txsSorted = new ArrayList<MoneroTxWallet>();
      for (String txId : request.getTxIds()) txsSorted.add(txMap.get(txId));
      txs = txsSorted;
    }
    return txs;
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferRequest request) {
    
    // initialize request up to block
    if (request == null) request = new MoneroTransferRequest();
    if (request.getTxRequest() == null) request.setTxRequest(new MoneroTxRequest().setTransferRequest(request));
    else request.getTxRequest().setTransferRequest(request);
    if (request.getTxRequest().getBlock() == null) request.getTxRequest().setBlock(new MoneroBlock().setTxs(request.getTxRequest()));
    
    // serialize request from block and fetch transfers from jni
    String blocksJson;
    try {
      blocksJson = getTransfersJni(JsonUtils.serialize(request.getTxRequest().getBlock()));
      System.out.println("Received getTransfers() response from JNI: " + blocksJson.substring(0, Math.min(5000, blocksJson.length())) + "...");
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
    
    // deserialize blocks
    List<MoneroBlockWallet> blocks = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksContainer.class).blocks;
    
    // collect transfers
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    if (blocks != null) {
      for (MoneroBlock block : blocks) {
        sanitizeBlock(block);
        for (MoneroTx tx : block.getTxs()) {
          if (block.getHeight() == null) tx.setBlock(null); // dereference placeholder block for unconfirmed txs
          MoneroTxWallet txWallet = (MoneroTxWallet) tx;
          if (txWallet.getIncomingTransfers() != null) {
            for (MoneroIncomingTransfer transfer : txWallet.getIncomingTransfers()) transfers.add(transfer);
          }
          if (txWallet.getOutgoingTransfer() != null) transfers.add(txWallet.getOutgoingTransfer());
        }
      }
    }
    return transfers;
  }

  @Override
  public List<MoneroOutputWallet> getOutputs(MoneroOutputRequest request) {
    
    // initialize request up to block
    if (request == null) request = new MoneroOutputRequest();
    if (request.getTxRequest() == null) request.setTxRequest(new MoneroTxRequest().setOutputRequest(request));
    else request.getTxRequest().setOutputRequest(request);
    if (request.getTxRequest().getBlock() == null) request.getTxRequest().setBlock(new MoneroBlock().setTxs(request.getTxRequest()));
    
    System.out.println("FETCHING: " + request.getAccountIndex());
    System.out.println("SERIALIZED: " + JsonUtils.serialize(request.getTxRequest().getBlock()));
    
    // serialize request from block and fetch outputs from jni
    String blocksJson = getOutputsJni(JsonUtils.serialize(request.getTxRequest().getBlock()));
    System.out.println("Received getOutputs() response from JNI: " + blocksJson.substring(0, Math.min(5000, blocksJson.length())) + "...");
    
    // deserialize blocks
    List<MoneroBlockWallet> blocks = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksContainer.class).blocks;
    
    // collect outputs
    List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
    if (blocks != null) {
      for (MoneroBlock block : blocks) {
        sanitizeBlock(block);
        for (MoneroTx tx : block.getTxs()) {
          MoneroTxWallet txWallet = (MoneroTxWallet) tx;
          outputs.addAll(txWallet.getVoutsWallet());
        }
      }
    }
    return outputs;
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    String keyImagesJson = getKeyImagesJni();
    System.out.println("Received key images json from jni: " + keyImagesJson);
    List<MoneroKeyImage> keyImages = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, keyImagesJson, KeyImagesContainer.class).keyImages;
    return keyImages;
  }

  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    
    // wrap and serialize key images in container for jni
    KeyImagesContainer keyImageContainer = new KeyImagesContainer(keyImages);
    String importResultJson = importKeyImagesJni(JsonUtils.serialize(keyImageContainer));
    
    // deserialize response
    System.out.println("Received import result json from jni: " + importResultJson);
    return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, importResultJson, MoneroKeyImageImportResult.class);
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public MoneroTxWallet send(MoneroSendRequest request) {
    if (request == null) throw new MoneroException("Send request cannot be null");
    if (Boolean.TRUE.equals(request.getCanSplit())) throw new MoneroException("Cannot request split transactions with send() which prevents splitting; use sendSplit() instead");
    request.setCanSplit(false);
    return sendSplit(request).get(0);
  }

  @Override
  public List<MoneroTxWallet> sendSplit(MoneroSendRequest request) {
    System.out.println("java sendSplit(request)");
    System.out.println("Send request: " + JsonUtils.serialize(request));
    
    // validate request
    if (request == null) throw new MoneroException("Send request cannot be null");
    
    // submit send request to JNI and get response as json rooted at blocks
    String blocksJson;
    try {
      blocksJson = sendSplitJni(JsonUtils.serialize(request));
      System.out.println("Received sendSplit() response from JNI: " + blocksJson.substring(0, Math.min(5000, blocksJson.length())) + "...");
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
    
    // deserialize blocks
    List<MoneroBlockWallet> blocks = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksContainer.class).blocks;
    
    // collect and return txs
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (blocks != null) {
      for (MoneroBlock block : blocks) {
        if (block.getHeight() != null) sanitizeBlock(block);
        for (MoneroTx tx : block.getTxs()) {
          if (block.getHeight() == null) tx.setBlock(null); // dereference placeholder block for unconfirmed txs
          txs.add((MoneroTxWallet) tx);
        }
      }
    }
    return txs;
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
    String[] txMetadatasArr = txMetadatas.toArray(new String[txMetadatas.size()]);  // convert to array for jni
    try {
      return Arrays.asList(relayTxsJni(txMetadatasArr));
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public List<String> getTxNotes(Collection<String> txIds) {
    return Arrays.asList(getTxNotesJni(txIds.toArray(new String[txIds.size()])));  // convert to array for jni
  }

  @Override
  public void setTxNotes(Collection<String> txIds, Collection<String> notes) {
    setTxNotesJni(txIds.toArray(new String[txIds.size()]), notes.toArray(new String[notes.size()]));
  }

  @Override
  public String sign(String msg) {
    return signJni(msg);
  }

  @Override
  public boolean verify(String msg, String address, String signature) {
    return verifyJni(msg, address, signature);
  }

  @Override
  public String getTxKey(String txId) {
    try {
      return getTxKeyJni(txId);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public MoneroCheckTx checkTxKey(String txId, String txKey, String address) {
    try {
      String checkStr = checkTxKeyJni(txId, txKey, address);
      System.out.println("Java received MoneroCheckTx json from jni: " + checkStr);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckTx.class);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public String getTxProof(String txId, String address, String message) {
    try {
      return getTxProofJni(txId, address, message);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public MoneroCheckTx checkTxProof(String txId, String address, String message, String signature) {
    try {
      String checkStr = checkTxProofJni(txId, address, message, signature);
      System.out.println("Java received MoneroCheckTx json from jni: " + checkStr);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckTx.class);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public String getSpendProof(String txId, String message) {
    try {
      return getSpendProofJni(txId, message);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public boolean checkSpendProof(String txId, String message, String signature) {
    try {
      return checkSpendProofJni(txId, message, signature);
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public String getReserveProofWallet(String message) {
    return getReserveProofWalletJni(message);
  }

  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    return getReserveProofAccountJni(accountIdx, amount.toString(), message);
  }

  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    String checkStr = checkReserveProofJni(address, message, signature);
    System.out.println("Java received MoneroCheckReserve json from jni: " + checkStr);
    return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckReserve.class);
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
    try {
      return createPaymentUriJni(JsonUtils.serialize(request));
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
  }

  @Override
  public MoneroSendRequest parsePaymentUri(String uri) {
    try {
      String sendRequestJson = parsePaymentUriJni(uri);
      System.out.println("Received send request json from jni: " + sendRequestJson);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, sendRequestJson, MoneroSendRequest.class);
    } catch (Exception e) {
      throw new MoneroException(e);
    }
  }

  @Override
  public String getOutputsHex() {
    String outputsHex = getOutputsHexJni();
    return outputsHex.isEmpty() ? null : outputsHex;
  }

  @Override
  public int importOutputsHex(String outputsHex) {
    return importOutputsHexJni(outputsHex);
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
    try {
      startMiningJni(numThreads == null ? null : (long) numThreads, backgroundMining, ignoreBattery); // TODO: startMining(Long, ...)? wallet2 uses uint64_t
    } catch (Exception e) {
      throw new MoneroException(e.getMessage());
    }
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
  
  private native String getPublicViewKeyJni();
  
  private native String getPrivateViewKeyJni();
  
  private native String getLanguageJni();
  
  private native String[] getLanguagesJni();
  
  private native String getAddressJni(int accountIdx, int subaddressIdx);
  
  private native String getAddressIndexJni(String address);
  
  private native String getIntegratedAddressJni(String standardAddress, String paymentId);
  
  private native String decodeIntegratedAddressJni(String integratedAddress);
  
  private native long setListenerJni(WalletJniListener listener);
  
  private native Object[] syncJni(long startHeight);
  
  private native long getHeightJni();
  
  private native long getChainHeightJni();
  
  private native long getRestoreHeightJni();
  
  private native void setRestoreHeightJni(long height);
  
  private native String getBalanceWalletJni();
  
  private native String getBalanceAccountJni(int accountIdx);
  
  private native String getBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  private native String getUnlockedBalanceWalletJni();
  
  private native String getUnlockedBalanceAccountJni(int accountIdx);
  
  private native String getUnlockedBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  private native String getAccountsJni(boolean includeSubaddresses, String tag);
  
  private native String getAccountJni(int accountIdx, boolean includeSubaddresses);
  
  private native String createAccountJni(String label);
  
  private native String getSubaddressesJni(int accountIdx, int[] subaddressIndices);
  
  private native String createSubaddressJni(int accountIdx, String label);
  
  /**
   * Gets txs from the native layer using strings to communicate.
   * 
   * @param txRequestJson is a tx request serialized to a json string
   * @return a serialized BlocksContainer to preserve model relationships
   */
  private native String getTxsJni(String txRequestJson);
  
  private native String getTransfersJni(String transferRequestJson);
  
  private native String getOutputsJni(String outputsRequestJson);
  
  private native String getKeyImagesJni();
  
  private native String importKeyImagesJni(String keyImagesJson);
  
  private native String sendSplitJni(String sendRequestJson);
  
  private native String[] relayTxsJni(String[] txMetadatas);
  
  private native String[] getTxNotesJni(String[] txIds);
  
  private native void setTxNotesJni(String[] txIds, String[] notes);
  
  private native String signJni(String msg);
  
  private native boolean verifyJni(String msg, String address, String signature);
  
  private native String getTxKeyJni(String txId);
  
  private native String checkTxKeyJni(String txId, String txKey, String address);
  
  private native String getTxProofJni(String txId, String address, String message);
  
  private native String checkTxProofJni(String txId, String address, String message, String signature);
  
  private native String getSpendProofJni(String txId, String message);
  
  private native boolean checkSpendProofJni(String txId, String message, String signature);
  
  private native String getReserveProofWalletJni(String message);
  
  private native String getReserveProofAccountJni(int accountIdx, String amount, String message);
  
  private native String checkReserveProofJni(String address, String message, String signature);
  
  private native String createPaymentUriJni(String sendRequestJson);
  
  private native String parsePaymentUriJni(String uri);
  
  private native String getOutputsHexJni();
  
  private native int importOutputsHexJni(String outputsHex);
  
  private native void startMiningJni(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery);
  
  private native void saveJni();
  
  private native void moveToJni(String path, String password);
  
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
  
  /**
   * Override MoneroBlock with wallet types for polymorphic deserialization.
   */
  private static class MoneroBlockWallet extends MoneroBlock {
    
    // default constructor necessary for serialization
    @SuppressWarnings("unused")
    public MoneroBlockWallet() {
      super();
    }
    
    @JsonProperty("txs")
    public MoneroBlockWallet setTxWallets(List<MoneroTxWallet> txs) {
      super.setTxs(new ArrayList<MoneroTx>(txs));
      return this;
    }
  }
  
  private static class AccountsContainer {
    public List<MoneroAccount> accounts;
  };
  
  private static class SubaddressesContainer {
    public List<MoneroSubaddress> subaddresses;
  };
  
  private static class BlocksContainer {
    public List<MoneroBlockWallet> blocks;
  }
  
  private static class KeyImagesContainer {
    public List<MoneroKeyImage> keyImages;
    @SuppressWarnings("unused") public KeyImagesContainer() { } // necessary for serialization
    public KeyImagesContainer(List<MoneroKeyImage> keyImages) { this.keyImages = keyImages; };
  }
  
//  /**
//   * Deserializes MoneroTx as MoneroTxWallet.
//   */
//  private class TxWalletDeserializer extends StdDeserializer<MoneroTx> {
//    private static final long serialVersionUID = 8098255188385074924L;
//
//    protected TxWalletDeserializer() {
//      super(MoneroTx.class);
//    }
//
//    @Override
//    public MoneroTx deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
//      TreeNode node = p.readValueAsTree();
//      return p.getCodec().treeToValue(node, MoneroTxWallet.class);
////      JsonParser parser = node.traverse();
////      parser.setCodec(p.getCodec());
////      return parser.readValueAs(MoneroTxWallet.class);
//    }
//  }
  
//  private class TxWalletDeserializer2 extends DelegatingDeserializer {
//    
//    public TxWalletDeserializer2(JsonDeserializer<?> delegate) {
//      super(delegate);
//    }
//    
//    @Override
//    protected JsonDeserializer<?> newDelegatingInstance(JsonDeserializer<?> newDelegate) {
//      return new TxWalletDeserializer2(newDelegate);
//    }
//
//    @Override
//    public MoneroTxWallet deserialize(JsonParser p, DeserializationContext ctx) throws IOException {
//      //TreeNode node = p.readValueAsTree();
//      //return p.getCodec().treeToValue(node, MoneroTxWallet.class);
//      
//      
//      //String key = p.getCurrentName();
//      //System.out.println("Key: " + key);
//      return (MoneroTxWallet) super.deserialize(p, ctx);
////      result.userId = key;
////      return result;
//    }
//  }
  
//  private class TxWalletDeserializer2 extends StdDeserializer<MoneroTxWallet> implements ResolvableDeserializer {
//    
//    private static final long serialVersionUID = 4375869015667210070L;
//    private final JsonDeserializer<?> defaultDeserializer;
//    
//    public TxWalletDeserializer2(JsonDeserializer<?> defaultDeserializer) {
//      super(MoneroTxWallet.class);
//      this.defaultDeserializer = defaultDeserializer;
//    }
//
//    @Override
//    public MoneroTxWallet deserialize(JsonParser jp, DeserializationContext ctx) throws IOException {
//      return (MoneroTxWallet) defaultDeserializer.deserialize(jp, ctx);
//    }
//
//    @Override
//    public void resolve(DeserializationContext ctxt) throws JsonMappingException {
//      ((ResolvableDeserializer) defaultDeserializer).resolve(ctxt);
//    }
//  }
  
  
//  /**
//   * Deserializes MoneroTransfer as MoneroIncomingTransfer xor MoneroOutgoingTransfer.
//   */
//  private class TransferDeserializer extends StdDeserializer<MoneroTransfer> {
//    private static final long serialVersionUID = -8524060141695794874L;
//
//    protected TransferDeserializer() {
//      super(MoneroTransfer.class);
//    }
//
//    @Override
//    public MoneroTransfer deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
//      TreeNode node = p.readValueAsTree();
//      throw new RuntimeException("Not implemented");
//    }
//  }
  
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
    for (MoneroTx tx : block.getTxs()) sanitizeTxWallet((MoneroTxWallet) tx);
    return block;
  }
  
  private static MoneroTxWallet sanitizeTxWallet(MoneroTxWallet tx) {
    return tx;
  }
}
