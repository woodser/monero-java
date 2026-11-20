package monero.wallet;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Logger;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroConnectionManager;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroVersion;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageExportResult;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroMessageSignatureResult;
import monero.wallet.model.MoneroMessageSignatureType;
import monero.wallet.model.MoneroMultisigInfo;
import monero.wallet.model.MoneroMultisigInitResult;
import monero.wallet.model.MoneroMultisigSignResult;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxPriority;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletListenerI;

public abstract class MoneroWalletJni extends MoneroWalletDefault {
      // ----------------------------- PRIVATE SETUP ------------------------------

  // try to load jni bindings
  static {
    MoneroUtils.tryLoadNativeLibrary();
  }
  
  // class variables
  protected static final Logger LOGGER = Logger.getLogger(MoneroWalletJni.class.getName());
  protected static final long DEFAULT_SYNC_PERIOD_IN_MS = 10000; // default period betweeen syncs in ms
  private static final long CLOSE_WAIT_MS = 45000; // maximum time to wait for other calls to finish before closing

  // instance variables
  protected long jniWalletHandle;                 // memory address of the wallet in c++; this variable is read directly by name in c++
  protected long jniListenerHandle;               // memory address of the wallet listener in c++; this variable is read directly by name in c++
  protected WalletJniListener jniListener;        // receives notifications from jni c++
  private final ReentrantReadWriteLock callLock = new ReentrantReadWriteLock(); // calls hold the read lock so close() cannot free the wallet while they execute
  private final Object closeLock = new Object(); // serialize close attempts independently of wallet operations
  
  /**
   * Private constructor with a handle to the memory address of the wallet in c++.
   * 
   * @param jniWalletHandle memory address of the wallet in c++
   * @param password password of the wallet instance
   */
  protected MoneroWalletJni(long jniWalletHandle) {
    this.jniWalletHandle = jniWalletHandle;
    this.jniListener = new WalletJniListener();
    this.listeners = new CopyOnWriteArraySet<MoneroWalletListenerI>();
  }

  // ------------ WALLET METHODS SPECIFIC TO JNI IMPLEMENTATION ---------------
  
  /**
   * Get the maximum height of the peers the wallet's daemon is connected to.
   *
   * @return the maximum height of the peers the wallet's daemon is connected to
   */
  public long getDaemonMaxPeerHeight() {
    beginCall();
    try {
      try {
        return getDaemonMaxPeerHeightJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  /**
   * Indicates if the wallet's daemon is synced with the network.
   * 
   * @return true if the daemon is synced with the network, false otherwise
   */
  public boolean isDaemonSynced() {
    beginCall();
    try {
      try {
        return isDaemonSyncedJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  /**
   * Indicates if the wallet is synced with the daemon.
   * 
   * @return true if the wallet is synced with the daemon, false otherwise
   */
  public boolean isSynced() {
    beginCall();
    try {
      try {
        return isSyncedJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  /**
   * Get the wallet's network type (mainnet, testnet, or stagenet).
   * 
   * @return the wallet's network type
   */
  public MoneroNetworkType getNetworkType() {
    beginCall();
    try {
      return MoneroNetworkType.values()[getNetworkTypeJni()];
    } finally {
      endCall();
    }
  }
  
  /**
   * Get the height of the first block that the wallet scans.
   * 
   * @return the height of the first block that the wallet scans
   */
  public long getRestoreHeight() {
    beginCall();
    try {
      return getRestoreHeightJni();
    } finally {
      endCall();
    }
  }
  
  /**
   * Set the height of the first block that the wallet scans.
   * 
   * @param syncHeight is the height of the first block that the wallet scans
   */
  public void setRestoreHeight(long syncHeight) {
    beginCall();
    try {
      setRestoreHeightJni(syncHeight);
    } finally {
      endCall();
    }
  }

  
  // -------------------------- COMMON WALLET METHODS -------------------------
  
  @Override
  public void addListener(MoneroWalletListenerI listener) {
    beginCall();
    try {
      super.addListener(listener);
      initListening();
    } finally {
      endCall();
    }
  }
  
  @Override
  public void removeListener(MoneroWalletListenerI listener) {
    beginCall();
    try {
      super.removeListener(listener);
    } finally {
      endCall();
    }
  }
  
  @Override
  public Set<MoneroWalletListenerI> getListeners() {
    assertNotClosed();
    return super.getListeners();
  }
  
  @Override
  public boolean isViewOnly() {
    beginCall();
    try {
      return isViewOnlyJni();
    } finally {
      endCall();
    }
  }
  
  @Override
  public void setConnectionManager(MoneroConnectionManager connectionManager) {
    beginCall();
    try {
      super.setConnectionManager(connectionManager);
    } finally {
      try {
        if (isClosed) { // undo a registration that raced with close detaching the manager
          removeConnectionManagerListener(connectionManager);
          this.connectionManager = null;
        }
      } finally {
        endCall();
      }
    }
  }

  @Override
  public void setDaemonConnection(MoneroRpcConnection daemonConnection) {
    setDaemonConnection(daemonConnection, null);
  }

  @Override
  public void setDaemonConnection(MoneroRpcConnection daemonConnection, Boolean isTrusted) {
    beginCall();
    try {
      int isTrustedJni = isTrusted == null ? -1 : (isTrusted ? 1 : 0); // negative if unset
      if (daemonConnection == null) setDaemonConnectionWithSslJni("", "", "", "", isTrustedJni, true);
      else {
        try {
          setDaemonConnectionWithSslJni(daemonConnection.getUri() == null ? "" : daemonConnection.getUri().toString(), daemonConnection.getUsername(), daemonConnection.getPassword(), daemonConnection.getProxyUri(), isTrustedJni, daemonConnection.getSslVerify());
        } catch (Exception e) {
          throw new MoneroError(e.getMessage());
        }
      }
    } finally {
      endCall();
    }
  }

  /**
   * Indicates if the wallet's daemon is trusted.
   *
   * @return true if the daemon is trusted, false otherwise
   */
  public boolean isDaemonTrusted() {
    beginCall();
    try {
      try {
        return isDaemonTrustedJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroRpcConnection getDaemonConnection() {
    beginCall();
    try {
      try {
        String[] vals = getDaemonConnectionWithSslJni();
        return vals == null ? null : new MoneroRpcConnection(vals[0], vals[1], vals[2], null, vals[3]).setSslVerify(!"false".equals(vals[4]));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public boolean isConnectedToDaemon() {
    beginCall();
    try {
      try {
        return isConnectedToDaemonJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroVersion getVersion() {
    beginCall();
    try {
      try {
        String versionJson = getVersionJni();
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, versionJson, MoneroVersion.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getPath() {
    beginCall();
    try {
      String path = getPathJni();
      return path.isEmpty() ? null : path;
    } finally {
      endCall();
    }
  }

  @Override
  public String getSeed() {
    beginCall();
    try {
      String seed = getSeedJni();
      if ("".equals(seed)) return null;
      return seed;
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getSeedLanguage() {
    beginCall();
    try {
      String seedLanguage = getSeedLanguageJni();
      if ("".equals(seedLanguage)) return null;
      return seedLanguage;
    } finally {
      endCall();
    }
  }

  @Override
  public String getPrivateViewKey() {
    beginCall();
    try {
      return getPrivateViewKeyJni();
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getPrivateSpendKey() {
    beginCall();
    try {
      String privateSpendKey = getPrivateSpendKeyJni();
      if ("".equals(privateSpendKey)) return null;
      return privateSpendKey;
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getPublicViewKey() {
    beginCall();
    try {
      return getPublicViewKeyJni();
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getPublicSpendKey() {
    beginCall();
    try {
      return getPublicSpendKeyJni();
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String standardAddress, String paymentId) {
    beginCall();
    try {
      try {
        String integratedAddressJson = getIntegratedAddressJni(standardAddress, paymentId);
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, integratedAddressJson, MoneroIntegratedAddress.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    beginCall();
    try {
      try {
        String integratedAddressJson = decodeIntegratedAddressJni(integratedAddress);
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, integratedAddressJson, MoneroIntegratedAddress.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public long getHeight() {
    beginCall();
    try {
      return getHeightJni();
    } finally {
      endCall();
    }
  }

  @Override
  public long getDaemonHeight() {
    beginCall();
    try {
      try {
        return getDaemonHeightJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public long getHeightByDate(int year, int month, int day) {
    beginCall();
    try {
      try {
        return getHeightByDateJni(year, month ,day);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener) {
    beginCall();
    try {
      if (startHeight == null) startHeight = Math.max(getHeight(), getRestoreHeight());
    
      // register listener if given
      if (listener != null) addListener(listener);
    
      // sync wallet and handle exception
      try {
        Object[] results = syncJni(startHeight);
        return new MoneroSyncResult((long) results[0], (boolean) results[1]);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      } finally {
        if (listener != null) listeners.remove(listener); // cleanup can race with close or removal from the callback
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public void startSyncing(Long syncPeriodInMs) {
    beginCall();
    try {
      try {
        startSyncingJni(syncPeriodInMs == null ? DEFAULT_SYNC_PERIOD_IN_MS : syncPeriodInMs);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public void stopSyncing() {
    beginCall();
    try {
      try {
        stopSyncingJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public void scanTxs(Collection<String> txHashes) {
    beginCall();
    try {
      String[] txMetadatasArr = txHashes.toArray(new String[txHashes.size()]); // convert to array for jni
      try {
        scanTxsJni(txMetadatasArr);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public void rescanSpent() {
    beginCall();
    try {
      try {
        rescanSpentJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public void rescanBlockchain() {
    beginCall();
    try {
      try {
        rescanBlockchainJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    beginCall();
    try {
      String accountsJson = getAccountsJni(includeSubaddresses, tag);
      List<MoneroAccount> accounts = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountsJson, AccountsContainer.class).accounts;
      for (MoneroAccount account : accounts) sanitizeAccount(account);
      return accounts;
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    beginCall();
    try {
      String accountJson = getAccountJni(accountIdx, includeSubaddresses);
      MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
      sanitizeAccount(account);
      return account;
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroAccount createAccount(String label) {
    beginCall();
    try {
      String accountJson = createAccountJni(label);
      MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
      sanitizeAccount(account);
      return account;
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    beginCall();
    try {
      String subaddresses_json = getSubaddressesJni(accountIdx, GenUtils.listToIntArray(subaddressIndices));
      List<MoneroSubaddress> subaddresses = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddresses_json, SubaddressesContainer.class).subaddresses;
      for (MoneroSubaddress subaddress : subaddresses) sanitizeSubaddress(subaddress);
      return subaddresses;
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    beginCall();
    try {
      String subaddressJson = createSubaddressJni(accountIdx, label);
      MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
      sanitizeSubaddress(subaddress);
      return subaddress;
    } finally {
      endCall();
    }
  }

  @Override
  public void setSubaddressLabel(int accountIdx, int subaddressIdx, String label) {
    beginCall();
    try {
      if (label == null) label = "";
      setSubaddressLabelJni(accountIdx, subaddressIdx, label);
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    beginCall();
    try {
      return getAddressJni(accountIdx, subaddressIdx);
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    beginCall();
    try {
      try {
        String subaddressJson = getAddressIndexJni(address);
        MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
        return sanitizeSubaddress(subaddress);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public BigInteger getBalance(Integer accountIdx, Integer subaddressIdx) {
    beginCall();
    try {
      try {
        if (accountIdx == null) {
          if (subaddressIdx != null) throw new MoneroError("Must provide account index with subaddress index");
          return new BigInteger(getBalanceWalletJni());
        } else {
          if (subaddressIdx == null) return new BigInteger(getBalanceAccountJni(accountIdx));
          else return new BigInteger(getBalanceSubaddressJni(accountIdx, subaddressIdx));
        }
      } catch (MoneroError e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public BigInteger getUnlockedBalance(Integer accountIdx, Integer subaddressIdx) {
    beginCall();
    try {
      try {
        if (accountIdx == null) {
          if (subaddressIdx != null) throw new MoneroError("Must provide account index with subaddress index");
          return new BigInteger(getUnlockedBalanceWalletJni());
        } else {
          if (subaddressIdx == null) return new BigInteger(getUnlockedBalanceAccountJni(accountIdx));
          else return new BigInteger(getUnlockedBalanceSubaddressJni(accountIdx, subaddressIdx));
        }
      } catch (MoneroError e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public List<MoneroTxWallet> getTxs(MoneroTxQuery query) {
    beginCall();
    try {
      // copy and normalize tx query up to block
      query = query == null ? new MoneroTxQuery() : query.copy();
      if (query.getBlock() == null) query.setBlock(new MoneroBlock().setTxs(query));
    
      // serialize query from block and fetch txs from jni
      String blocksJson;
      try {
        blocksJson = getTxsJni(JsonUtils.serialize(query.getBlock()));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    
      // deserialize and return txs
      return deserializeTxs(query, blocksJson);
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
    beginCall();
    try {
      // copy and normalize query up to block
      query = normalizeTransferQuery(query);
    
      // serialize query from block and fetch transfers from jni
      String blocksJson;
      try {
        blocksJson = getTransfersJni(JsonUtils.serialize(query.getTxQuery().getBlock()));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    
      // deserialize and return transfers
      return deserializeTransfers(query, blocksJson);
    } finally {
      endCall();
    }
  }
  
  @Override
  public List<MoneroOutputWallet> getOutputs(MoneroOutputQuery query) {
    beginCall();
    try {
      // copy and normalize query up to block
      if (query == null) query = new MoneroOutputQuery();
      else {
        if (query.getTxQuery() == null) query = query.copy();
        else {
          MoneroTxQuery txQuery = query.getTxQuery().copy();
          if (query.getTxQuery().getOutputQuery() == query) query = txQuery.getOutputQuery();
          else {
            GenUtils.assertNull("Output query's tx query must be circular reference or null", query.getTxQuery().getOutputQuery());
            query = query.copy();
            query.setTxQuery(txQuery);
          }
        }
      }
      if (query.getTxQuery() == null) query.setTxQuery(new MoneroTxQuery());
      query.getTxQuery().setOutputQuery(query);
      if (query.getTxQuery().getBlock() == null) query.getTxQuery().setBlock(new MoneroBlock().setTxs(query.getTxQuery()));
    
      // serialize query from block and fetch outputs from jni
      String blocksJson = getOutputsJni(JsonUtils.serialize(query.getTxQuery().getBlock()));
    
      // deserialize and return outputs
      return deserializeOutputs(query, blocksJson);
    } finally {
      endCall();
    }
  }
  
  @Override
  public String exportOutputs(boolean all) {
    beginCall();
    try {
      String outputsHex = exportOutputsJni(all);
      return outputsHex.isEmpty() ? null : outputsHex;
    } finally {
      endCall();
    }
  }

  @Override
  public int importOutputs(String outputsHex) {
    beginCall();
    try {
      return importOutputsJni(outputsHex);
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroKeyImageExportResult exportKeyImages(boolean all) {
    beginCall();
    try {
      String resultJson = exportKeyImagesJni(all);
      MoneroKeyImageExportResult result = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, resultJson, MoneroKeyImageExportResult.class);
      if (result.getKeyImages() == null) result.setKeyImages(new ArrayList<MoneroKeyImage>());
      return result;
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages, long offset) {
    beginCall();
    try {
      // wrap and serialize offset and key images for jni
      MoneroKeyImageExportResult keyImageContainer = new MoneroKeyImageExportResult();
      keyImageContainer.setOffset(offset);
      keyImageContainer.setKeyImages(keyImages);
      String importResultJson = importKeyImagesJni(JsonUtils.serialize(keyImageContainer));
    
      // deserialize response
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, importResultJson, MoneroKeyImageImportResult.class);
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletFull.getNewKeyImagesFromLastImport() not implemented");
  }
  
  @Override
  public void freezeOutput(String keyImage) {
    beginCall();
    try {
      try {
        freezeOutputJni(keyImage);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public void thawOutput(String keyImage) {
    beginCall();
    try {
      try {
        thawOutputJni(keyImage);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public boolean isOutputFrozen(String keyImage) {
    beginCall();
    try {
      try {
        return isOutputFrozenJni(keyImage);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroTxPriority getDefaultFeePriority() {
    beginCall();
    try {
      try {
        int moneroTxPriorityOrdinal = getDefaultFeePriorityJni();
        return MoneroTxPriority.values()[moneroTxPriorityOrdinal];
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroTxWallet> createTxs(MoneroTxConfig config) {
    beginCall();
    try {
      LOGGER.fine("java createTxs(request)");
      LOGGER.fine("Tx config: " + JsonUtils.serialize(config));
    
      // validate request
      if (config == null) throw new MoneroError("Tx config cannot be null");
    
      // submit tx config to JNI and get response as json rooted at tx set
      String txSetJson;
      try {
        txSetJson = createTxsJni(JsonUtils.serialize(config));
        LOGGER.fine("Received createTxs() response from JNI: " + txSetJson.substring(0, Math.min(5000, txSetJson.length())) + "...");
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    
      // deserialize and return txs
      MoneroTxSet txSet = JsonUtils.deserialize(txSetJson, MoneroTxSet.class);
      return txSet.getTxs();
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroTxWallet sweepOutput(MoneroTxConfig config) {
    beginCall();
    try {
      try {
        String txSetJson = sweepOutputJni(JsonUtils.serialize(config));
        MoneroTxSet txSet = JsonUtils.deserialize(txSetJson, MoneroTxSet.class);
        return txSet.getTxs().get(0);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public List<MoneroTxWallet> sweepUnlocked(MoneroTxConfig config) {
    beginCall();
    try {
      // validate request
      if (config == null) throw new MoneroError("Send request cannot be null");
    
      // submit send request to JNI and get response as json rooted at tx set
      String txSetsJson;
      try {
        txSetsJson = sweepUnlockedJni(JsonUtils.serialize(config));
        LOGGER.fine("Received sweepUnlocked() response from JNI: " + txSetsJson.substring(0, Math.min(5000, txSetsJson.length())) + "...");
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    
      // deserialize tx sets
      List<MoneroTxSet> txSets = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, txSetsJson, TxSetsContainer.class).txSets;
    
      // return txs
      List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
      for (MoneroTxSet txSet : txSets) txs.addAll(txSet.getTxs());
      return txs;
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroTxWallet> sweepDust(boolean relay) {
    beginCall();
    try {
      try {
        String txSetJson = sweepDustJni(relay);
        MoneroTxSet txSet = JsonUtils.deserialize(txSetJson, MoneroTxSet.class);
        if (txSet.getTxs() == null) txSet.setTxs(new ArrayList<MoneroTxWallet>());
        return txSet.getTxs();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public List<String> relayTxs(Collection<String> txMetadatas) {
    beginCall();
    try {
      String[] txMetadatasArr = txMetadatas.toArray(new String[txMetadatas.size()]); // convert to array for jni
      try {
        return Arrays.asList(relayTxsJni(txMetadatasArr));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroTxSet describeTxSet(MoneroTxSet txSet) {
    beginCall();
    try {
      txSet = new MoneroTxSet()
              .setUnsignedTxHex(txSet.getUnsignedTxHex())
              .setSignedTxHex(txSet.getSignedTxHex())
              .setMultisigTxHex(txSet.getMultisigTxHex());
      String describedTxSetJson;
      try {
        describedTxSetJson = describeTxSetJni(JsonUtils.serialize(txSet));
        return JsonUtils.deserialize(describedTxSetJson, MoneroTxSet.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroTxSet signTxs(String unsignedTxHex) {
    beginCall();
    try {
      try {
        String signedTxSetJson = signTxsJni(unsignedTxHex);
        return JsonUtils.deserialize(signedTxSetJson, MoneroTxSet.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public List<String> submitTxs(String signedTxHex) {
    beginCall();
    try {
      try {
        return Arrays.asList(submitTxsJni(signedTxHex));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroCheckTx checkTxKey(String txHash, String txKey, String address) {
    beginCall();
    try {
      try {
        String checkStr = checkTxKeyJni(txHash, txKey, address);
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckTx.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String getTxProof(String txHash, String address, String message) {
    beginCall();
    try {
      try {
        return getTxProofJni(txHash, address, message);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroCheckTx checkTxProof(String txHash, String address, String message, String signature) {
    beginCall();
    try {
      try {
        String checkStr = checkTxProofJni(txHash, address, message, signature);
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckTx.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String getSpendProof(String txHash, String message) {
    beginCall();
    try {
      try {
        return getSpendProofJni(txHash, message);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public boolean checkSpendProof(String txHash, String message, String signature) {
    beginCall();
    try {
      try {
        return checkSpendProofJni(txHash, message, signature);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String getReserveProofWallet(String message) {
    beginCall();
    try {
      try {
        return getReserveProofWalletJni(message);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    beginCall();
    try {
      try {
        return getReserveProofAccountJni(accountIdx, amount.toString(), message);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage(), -1);
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    beginCall();
    try {
      try {
        String checkStr = checkReserveProofJni(address, message, signature);
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckReserve.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage(), -1);
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String signMessage(String msg, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx) {
    beginCall();
    try {
      return signMessageJni(msg, signatureType.ordinal(), accountIdx, subaddressIdx);
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroMessageSignatureResult verifyMessage(String msg, String address, String signature) {
    beginCall();
    try {
      try {
        String resultJson = verifyMessageJni(msg, address, signature);
        Map<String, Object> result = JsonUtils.deserialize(resultJson, new TypeReference<Map<String, Object>>(){});
        boolean isGood = (boolean) result.get("isGood");
        return new MoneroMessageSignatureResult(
            isGood,
            !isGood ? null : (Boolean) result.get("isOld"),
            !isGood ? null : "spend".equals(result.get("signatureType")) ? MoneroMessageSignatureType.SIGN_WITH_SPEND_KEY : MoneroMessageSignatureType.SIGN_WITH_VIEW_KEY,
            !isGood ? null : (Integer) result.get("version"));
      } catch (Exception e) {
        return new MoneroMessageSignatureResult(false, null, null, null); // jni can differentiate incorrect from invalid address, but rpc returns -2 for both, so returning bad result for consistency
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String getTxKey(String txHash) {
    beginCall();
    try {
      try {
        return getTxKeyJni(txHash);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public List<String> getTxNotes(List<String> txHashes) {
    beginCall();
    try {
      return Arrays.asList(getTxNotesJni(txHashes.toArray(new String[txHashes.size()])));  // convert to array for jni
    } finally {
      endCall();
    }
  }

  @Override
  public void setTxNotes(List<String> txHashes, List<String> notes) {
    beginCall();
    try {
      setTxNotesJni(txHashes.toArray(new String[txHashes.size()]), notes.toArray(new String[notes.size()]));
    } finally {
      endCall();
    }
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
    beginCall();
    try {
      if (entryIndices == null) entryIndices = new ArrayList<Integer>();
      String entriesJson = getAddressBookEntriesJni(GenUtils.listToIntArray(entryIndices));
      List<MoneroAddressBookEntry> entries = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, entriesJson, AddressBookEntriesContainer.class).entries;
      if (entries == null) entries = new ArrayList<MoneroAddressBookEntry>();
      return entries;
    } finally {
      endCall();
    }
  }

  @Override
  public int addAddressBookEntry(String address, String description) {
    beginCall();
    try {
      return addAddressBookEntryJni(address, description);
    } finally {
      endCall();
    }
  }

  @Override
  public void editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description) {
    beginCall();
    try {
      editAddressBookEntryJni(index, setAddress, address, setDescription, description);
    } finally {
      endCall();
    }
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    beginCall();
    try {
      deleteAddressBookEntryJni(entryIdx);
    } finally {
      endCall();
    }
  }

  @Override
  public void tagAccounts(String tag, Collection<Integer> accountIndices) {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletFull.tagAccounts() not implemented");
  }

  @Override
  public void untagAccounts(Collection<Integer> accountIndices) {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletFull.untagAccounts() not implemented");
  }

  @Override
  public List<MoneroAccountTag> getAccountTags() {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletFull.getAccountTags() not implemented");
  }

  @Override
  public void setAccountTagLabel(String tag, String label) {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletFull.setAccountTagLabel() not implemented");
  }

  @Override
  public String getPaymentUri(MoneroTxConfig request) {
    beginCall();
    try {
      try {
        return getPaymentUriJni(JsonUtils.serialize(request));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroTxConfig parsePaymentUri(String uri) {
    beginCall();
    try {
      try {
        String sendRequestJson = parsePaymentUriJni(uri);
        return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, sendRequestJson, MoneroTxConfig.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public String getAttribute(String key) {
    beginCall();
    try {
      return getAttributeJni(key);
    } finally {
      endCall();
    }
  }

  @Override
  public void setAttribute(String key, String val) {
    beginCall();
    try {
      setAttributeJni(key, val);
    } finally {
      endCall();
    }
  }

  @Override
  public void startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    beginCall();
    try {
      try {
        startMiningJni(numThreads == null ? 0l : (long) numThreads, Boolean.TRUE.equals(backgroundMining), Boolean.TRUE.equals(ignoreBattery));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public void stopMining() {
    beginCall();
    try {
      try {
        stopMiningJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public boolean isMultisigImportNeeded() {
    beginCall();
    try {
      return isMultisigImportNeededJni();
    } finally {
      endCall();
    }
  }
  
  @Override
  public MoneroMultisigInfo getMultisigInfo() {
    beginCall();
    try {
      try {
        String multisigInfoJson = getMultisigInfoJni();
        return JsonUtils.deserialize(multisigInfoJson, MoneroMultisigInfo.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String prepareMultisig() {
    beginCall();
    try {
      return prepareMultisigJni();
    } finally {
      endCall();
    }
  }

  @Override
  public String makeMultisig(List<String> multisigHexes, int threshold, String password) {
    beginCall();
    try {
      try {
        return makeMultisigJni(multisigHexes.toArray(new String[multisigHexes.size()]), threshold, password);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password) {
    beginCall();
    try {
      try {
        String initMultisigResultJson = exchangeMultisigKeysJni(multisigHexes.toArray(new String[multisigHexes.size()]), password);
        return JsonUtils.deserialize(initMultisigResultJson, MoneroMultisigInitResult.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public String exportMultisigHex() {
    beginCall();
    try {
      try {
        return exportMultisigHexJni();
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public int importMultisigHex(List<String> multisigHexes, boolean refreshAfterImport) {
    beginCall();
    try {
      try {
        return importMultisigHexJni(multisigHexes.toArray(new String[multisigHexes.size()]), refreshAfterImport);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex) {
    beginCall();
    try {
      try {
        String signMultisigResultJson = signMultisigTxHexJni(multisigTxHex);
        return JsonUtils.deserialize(signMultisigResultJson, MoneroMultisigSignResult.class);
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }

  @Override
  public List<String> submitMultisigTxHex(String signedMultisigTxHex) {
    beginCall();
    try {
      try {
        return Arrays.asList(submitMultisigTxHexJni(signedMultisigTxHex));
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    } finally {
      endCall();
    }
  }
  
  @Override
  public void save() {
    beginCall();
    try {
      saveJni();
    } finally {
      endCall();
    }
  }
  
  @Override
  public void close(boolean save) {
    if (callLock.getReadHoldCount() > 0 || Boolean.TRUE.equals(jniListener.notifying.get())) {
      throw new MoneroError("Cannot close wallet from an active wallet call or listener callback");
    }
    synchronized (closeLock) {
      if (jniWalletHandle == 0) return; // a failed close can be retried while its native handle remains
      isClosed = true; // reject new calls before cancelling native I/O
      boolean locked = false;
      try {
        requestShutdownJni();
        removeConnectionManagerListener(connectionManager); // detach after cancellation so connection callbacks can finish
        locked = callLock.writeLock().tryLock(CLOSE_WAIT_MS, TimeUnit.MILLISECONDS);
        if (!locked) throw new MoneroError("Timed out waiting for active wallet calls; close can be retried");
        removeConnectionManagerListener(connectionManager); // recheck after acquiring visibility of completed registrations
        connectionManager = null;
        closeJni(save);
        super.close(save); // clear Java listeners after native callbacks have finished
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new MoneroError("Interrupted waiting for active wallet calls; close can be retried");
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      } finally {
        if (locked) callLock.writeLock().unlock();
      }
    }
  }

  // -------------------------------- LISTENER --------------------------------
  
  /**
   * Receives notifications directly from jni c++.
   */
  @SuppressWarnings("unused") // called directly from jni c++
  public class WalletJniListener {

    private final ThreadLocal<Boolean> notifying = new ThreadLocal<Boolean>();

    private void notifyListeners(Runnable notification) {
      if (isClosed) return;
      notifying.set(true);
      try {
        notification.run();
      } finally {
        notifying.remove();
      }
    }
    
    public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
      notifyListeners(() -> announceSyncProgress(height, startHeight, endHeight, percentDone, message));
    }
    
    public void onNewBlock(long height) {
      notifyListeners(() -> announceNewBlock(height));
    }
    
    public void onBalancesChanged(String newBalanceStr, String newUnlockedBalanceStr) {
      notifyListeners(() -> announceBalancesChanged(new BigInteger(newBalanceStr), new BigInteger(newUnlockedBalanceStr)));
    }
    
    public void onOutputReceived(long height, String txHash, String amountStr, int accountIdx, int subaddressIdx, int version, String unlockTimeStr, boolean isLocked) {
      
      // build output to announce
      MoneroOutputWallet output = new MoneroOutputWallet();
      output.setAmount(new BigInteger(amountStr));
      output.setAccountIndex(accountIdx);
      output.setSubaddressIndex(subaddressIdx);
      MoneroTxWallet tx = new MoneroTxWallet();
      tx.setHash(txHash);
      tx.setVersion(version);
      tx.setUnlockTime(new BigInteger(unlockTimeStr));
      output.setTx(tx);
      tx.setOutputs(Arrays.asList(output));
      tx.setIsIncoming(true);
      tx.setIsLocked(isLocked);
      if (height > 0) {
        MoneroBlock block = new MoneroBlock().setHeight(height);
        block.setTxs(Arrays.asList(tx));
        tx.setBlock(block);
        tx.setIsConfirmed(true);
        tx.setInTxPool(false);
        tx.setIsFailed(false);
      } else {
        tx.setIsConfirmed(false);
        tx.setInTxPool(true);
      }
      
      // announce output
      notifyListeners(() -> announceOutputReceived((MoneroOutputWallet) tx.getOutputs().get(0)));
    }
    
    public void onOutputSpent(long height, String txHash, String amountStr, String accountIdxStr, String subaddressIdxStr, int version, String unlockTimeStr, boolean isLocked) {
      
      // build spent output
      MoneroOutputWallet output = new MoneroOutputWallet();
      output.setAmount(new BigInteger(amountStr));
      if (accountIdxStr.length() > 0) output.setAccountIndex(Integer.parseInt(accountIdxStr));
      if (subaddressIdxStr.length() > 0) output.setSubaddressIndex(Integer.parseInt(subaddressIdxStr));
      MoneroTxWallet tx = new MoneroTxWallet();
      tx.setHash(txHash);
      tx.setVersion(version);
      tx.setUnlockTime(new BigInteger(unlockTimeStr));
      tx.setIsLocked(isLocked);
      output.setTx(tx);
      tx.setInputs(Arrays.asList(output));
      tx.setIsIncoming(false);
      if (height > 0) {
        MoneroBlock block = new MoneroBlock().setHeight(height);
        block.setTxs(Arrays.asList(tx));
        tx.setBlock(block);
        tx.setIsConfirmed(true);
        tx.setInTxPool(false);
        tx.setIsFailed(false);
      } else {
        tx.setIsConfirmed(false);
        tx.setInTxPool(true);
      }
      
      // announce output
      notifyListeners(() -> announceOutputSpent((MoneroOutputWallet) tx.getInputs().get(0)));
    }
  }
  
  // ------------------------ RESPONSE DESERIALIZATION ------------------------
  
  /**
   * Override MoneroBlock with wallet types for polymorphic deserialization.
   */
  protected static class MoneroBlockWallet extends MoneroBlock {
    
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
    
    @JsonProperty("minerTx") // the light wallet also sends the miner tx of its blocks apart, it is kept in the txs of the block as the full wallet does
    public MoneroBlockWallet ignoreMinerTx(Object minerTx) {
      return this;
    }
    
    /**
     * Initializes a new MoneroBlock with direct references to this block.
     * 
     * TODO: more efficient way to deserialize directly into MoneroBlock?
     * 
     * @return MoneroBlock is the newly initialized block with direct references to this block
     */
    public MoneroBlock toBlock() {
      MoneroBlock block = new MoneroBlock();
      block.setHash(getHash());
      block.setHeight(getHeight());
      block.setTimestamp(getTimestamp());
      block.setSize(getSize());
      block.setWeight(getWeight());
      block.setLongTermWeight(getLongTermWeight());
      block.setDepth(getDepth());
      block.setDifficulty(getDifficulty());
      block.setCumulativeDifficulty(getCumulativeDifficulty());
      block.setMajorVersion(getMajorVersion());
      block.setMinorVersion(getMinorVersion());
      block.setNonce(getNonce());
      block.setMinerTxHash(getMinerTxHash());
      block.setNumTxs(getNumTxs());
      block.setOrphanStatus(getOrphanStatus());
      block.setPrevHash(getPrevHash());
      block.setReward(getReward());
      block.setPowHash(getPowHash());
      block.setHex(getHex());
      block.setMinerTx(getMinerTx());
      block.setTxs(getTxs());
      block.setTxHashes(getTxHashes());
      for (MoneroTx tx : getTxs()) tx.setBlock(block);  // re-assign tx block references
      return block;
    }
  }
  
  protected static class AccountsContainer {
    public List<MoneroAccount> accounts;
  };
  
  protected static class SubaddressesContainer {
    public List<MoneroSubaddress> subaddresses;
  };
  
  protected static class BlocksWalletContainer {
    public List<MoneroBlockWallet> blocks;
  }
  
  protected static class DeserializedBlocksContainer {
    public List<MoneroBlock> blocks;
  }
  
  protected static class TxSetsContainer {
    public List<MoneroTxSet> txSets;
  }
  
  protected static DeserializedBlocksContainer deserializeBlocks(String blocksJson) {
    DeserializedBlocksContainer deserializedBlocksContainer = new DeserializedBlocksContainer();
    deserializedBlocksContainer.blocks = new ArrayList<MoneroBlock>();
    BlocksWalletContainer blocksWalletContainer = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksWalletContainer.class);
    if (blocksWalletContainer.blocks != null) for (MoneroBlockWallet blockWallet : blocksWalletContainer.blocks) deserializedBlocksContainer.blocks.add(blockWallet.toBlock());
    return deserializedBlocksContainer;
  }
  
  protected static List<MoneroTxWallet> deserializeTxs(MoneroTxQuery query, String blocksJson) {
    
    // deserialize blocks
    DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
    List<MoneroBlock> blocks = deserializedBlocks.blocks;
    
    // collect txs
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    for (MoneroBlock block : blocks) {
      sanitizeBlock(block);
      for (MoneroTx tx : block.getTxs()) {
        if (block.getHeight() == null) tx.setBlock(null); // dereference placeholder block for unconfirmed txs
        txs.add((MoneroTxWallet) tx);
      }
    }
    
    // re-sort txs which is lost over jni serialization
    if (query.getHashes() != null) {
      Map<String, MoneroTxWallet> txMap = new HashMap<String, MoneroTxWallet>();
      for (MoneroTxWallet tx : txs) txMap.put(tx.getHash(), tx);
      List<MoneroTxWallet> txsSorted = new ArrayList<MoneroTxWallet>();
      for (String txHash : query.getHashes()) if (txMap.containsKey(txHash)) txsSorted.add(txMap.get(txHash));
      txs = txsSorted;
    }
    return txs;
  }
  
  protected static List<MoneroTransfer> deserializeTransfers(MoneroTransferQuery query, String blocksJson) {
    
    // deserialize blocks
    DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
    List<MoneroBlock> blocks = deserializedBlocks.blocks;
    
    // collect transfers
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    for (MoneroBlock block : blocks) {
      sanitizeBlock(block);
      for (MoneroTx tx : block.getTxs()) {
        if (block.getHeight() == null) tx.setBlock(null); // dereference placeholder block for unconfirmed txs
        MoneroTxWallet txWallet = (MoneroTxWallet) tx;
        if (txWallet.getOutgoingTransfer() != null) transfers.add(txWallet.getOutgoingTransfer());
        if (txWallet.getIncomingTransfers() != null) {
          for (MoneroIncomingTransfer transfer : txWallet.getIncomingTransfers()) transfers.add(transfer);
        }
      }
    }
    return transfers;
  }
  
  protected static List<MoneroOutputWallet> deserializeOutputs(MoneroOutputQuery query, String blocksJson) {
    
    // deserialize blocks
    DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
    List<MoneroBlock> blocks = deserializedBlocks.blocks;
    
    // collect outputs
    List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
    for (MoneroBlock block : blocks) {
      sanitizeBlock(block);
      for (MoneroTx tx : block.getTxs()) {
        outputs.addAll(((MoneroTxWallet) tx).getOutputsWallet());
      }
    }
    return outputs;
  }
  
  protected static class AddressBookEntriesContainer {
    public List<MoneroAddressBookEntry> entries;
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  /**
   * Initializes the c++ listener once and retains it until close drains native callbacks.
   */
  protected void initListening() {
    synchronized (jniListener) {
      if (jniListenerHandle == 0) jniListenerHandle = setListenerJni(jniListener);
    }
  }
  
  protected void assertNotClosed() {
    if (isClosed) throw new MoneroError("Wallet is closed");
  }

  // acquire the shared call lock so close() waits for this call before freeing the wallet
  protected void beginCall() {
    assertNotClosed();
    if (!callLock.readLock().tryLock()) throw new MoneroError("Wallet is closed");
    if (isClosed) {
      callLock.readLock().unlock();
      throw new MoneroError("Wallet is closed");
    }
  }

  protected void endCall() {
    callLock.readLock().unlock();
  }
  
  protected static MoneroAccount sanitizeAccount(MoneroAccount account) {
    if (account.getSubaddresses() != null) {
      for (MoneroSubaddress subaddress : account.getSubaddresses()) sanitizeSubaddress(subaddress);
    }
    return account;
  }
  
  protected static MoneroSubaddress sanitizeSubaddress(MoneroSubaddress subaddress) {
    if ("".equals(subaddress.getLabel())) subaddress.setLabel(null);
    return subaddress;
  }
  
  protected static MoneroBlock sanitizeBlock(MoneroBlock block) {
    for (MoneroTx tx : block.getTxs()) sanitizeTxWallet((MoneroTxWallet) tx);
    return block;
  }
  
  protected static MoneroTxWallet sanitizeTxWallet(MoneroTxWallet tx) {
    return tx;
  }

  // ------------------------------ NATIVE METHODS ----------------------------


  protected abstract long getHeightJni();
  
  protected abstract long getRestoreHeightJni();
  
  protected abstract void setRestoreHeightJni(long height);
  
  protected abstract long getDaemonHeightJni();
  
  protected abstract long getDaemonMaxPeerHeightJni();
  
  protected abstract long getHeightByDateJni(int year, int month, int day);
  
  protected abstract boolean isViewOnlyJni();
  
  protected abstract void setDaemonConnectionWithSslJni(String uri, String username, String password, String proxyUri, int isTrusted, boolean sslVerify);
  
  protected abstract String[] getDaemonConnectionWithSslJni(); // returns [uri, username, password, proxyUri, sslVerify]
  
  protected abstract boolean isConnectedToDaemonJni();
  
  protected abstract boolean isDaemonSyncedJni();

  protected abstract boolean isDaemonTrustedJni();
  
  protected abstract boolean isSyncedJni();
  
  protected abstract int getNetworkTypeJni();
  
  protected abstract String getVersionJni();
  
  protected abstract String getPathJni();
  
  protected abstract String getSeedJni();
  
  protected abstract String getSeedLanguageJni();
  
  protected abstract String getPublicViewKeyJni();
  
  protected abstract String getPrivateViewKeyJni();
  
  protected abstract String getPublicSpendKeyJni();
  
  protected abstract String getPrivateSpendKeyJni();
  
  protected abstract String getAddressJni(int accountIdx, int subaddressIdx);
  
  protected abstract String getAddressIndexJni(String address);
  
  protected abstract String getIntegratedAddressJni(String standardAddress, String paymentId);
  
  protected abstract String decodeIntegratedAddressJni(String integratedAddress);
  
  protected abstract long setListenerJni(WalletJniListener listener);
  
  protected abstract Object[] syncJni(long startHeight);
  
  protected abstract void startSyncingJni(long syncPeriodInMs);
  
  protected abstract void stopSyncingJni();
  
  protected abstract void scanTxsJni(String[] txHashes);
  
  protected abstract void rescanSpentJni();
  
  protected abstract void rescanBlockchainJni();
  
  protected abstract String getBalanceWalletJni();
  
  protected abstract String getBalanceAccountJni(int accountIdx);
  
  protected abstract String getBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  protected abstract String getUnlockedBalanceWalletJni();
  
  protected abstract String getUnlockedBalanceAccountJni(int accountIdx);
  
  protected abstract String getUnlockedBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  
  protected abstract String getAccountsJni(boolean includeSubaddresses, String tag);
  
  protected abstract String getAccountJni(int accountIdx, boolean includeSubaddresses);
  
  protected abstract String createAccountJni(String label);
  
  protected abstract String getSubaddressesJni(int accountIdx, int[] subaddressIndices);
  
  protected abstract String createSubaddressJni(int accountIdx, String label);

  protected abstract void setSubaddressLabelJni(int accountIdx, int subaddressIdx, String label);
  
  protected abstract String getTxsJni(String txQueryJson);
  
  protected abstract String getTransfersJni(String transferQueryJson);
  
  protected abstract String getOutputsJni(String outputQueryJson);
  
  protected abstract String exportOutputsJni(boolean all);
  
  protected abstract int importOutputsJni(String outputsHex);
  
  protected abstract String exportKeyImagesJni(boolean all);
  
  protected abstract String importKeyImagesJni(String keyImagesJson);
  
  protected abstract String[] relayTxsJni(String[] txMetadatas);
  
  protected abstract void freezeOutputJni(String KeyImage);
  
  protected abstract void thawOutputJni(String keyImage);
  
  protected abstract boolean isOutputFrozenJni(String keyImage);

  protected abstract int getDefaultFeePriorityJni();
  
  protected abstract String createTxsJni(String txConfigJson);
  
  protected abstract String sweepUnlockedJni(String txConfigJson);
  
  protected abstract String sweepOutputJni(String txConfigJson);
  
  protected abstract String sweepDustJni(boolean doNotRelay);
  
  protected abstract String describeTxSetJni(String txSetJson);
  
  protected abstract String signTxsJni(String unsignedTxHex);
  
  protected abstract String[] submitTxsJni(String signedTxHex);
  
  protected abstract String[] getTxNotesJni(String[] txHashes);
  
  protected abstract void setTxNotesJni(String[] txHashes, String[] notes);
  
  protected abstract String signMessageJni(String msg, int signatureType, int accountIdx, int subaddressIdx);
  
  protected abstract String verifyMessageJni(String msg, String address, String signature);
  
  protected abstract String getTxKeyJni(String txHash);
  
  protected abstract String checkTxKeyJni(String txHash, String txKey, String address);
  
  protected abstract String getTxProofJni(String txHash, String address, String message);
  
  protected abstract String checkTxProofJni(String txHash, String address, String message, String signature);
  
  protected abstract String getSpendProofJni(String txHash, String message);
  
  protected abstract boolean checkSpendProofJni(String txHash, String message, String signature);
  
  protected abstract String getReserveProofWalletJni(String message);
  
  protected abstract String getReserveProofAccountJni(int accountIdx, String amount, String message);
  
  protected abstract String checkReserveProofJni(String address, String message, String signature);
  
  protected abstract String getAddressBookEntriesJni(int[] indices);
  
  protected abstract int addAddressBookEntryJni(String address, String description);
  
  protected abstract void editAddressBookEntryJni(int index, boolean setAddress, String address, boolean setDescription, String description);
  
  protected abstract void deleteAddressBookEntryJni(int entryIdx);
  
  protected abstract String getPaymentUriJni(String sendRequestJson);
  
  protected abstract String parsePaymentUriJni(String uri);
  
  protected abstract String getAttributeJni(String key);
  
  protected abstract void setAttributeJni(String key, String val);

  protected abstract void startMiningJni(long numThreads, boolean backgroundMining, boolean ignoreBattery);
  
  protected abstract void stopMiningJni();
  
  protected abstract boolean isMultisigImportNeededJni();
  
  protected abstract String getMultisigInfoJni();
  
  protected abstract String prepareMultisigJni();
  
  protected abstract String makeMultisigJni(String[] multisigHexes, int threshold, String password);
  
  protected abstract String exchangeMultisigKeysJni(String[] multisigHexes, String password);
  
  protected abstract String exportMultisigHexJni();
  
  protected abstract int importMultisigHexJni(String[] multisigHexes, boolean refreshAfterImport);
  
  protected abstract String signMultisigTxHexJni(String multisigTxHex);
  
  protected abstract String[] submitMultisigTxHexJni(String signedMultisigTxHex);
  
  protected abstract void changePasswordJni(String oldPassword, String newPassword);
  
  protected abstract void moveToJni(String path, String password);
  
  protected abstract void saveJni();
  
  // cancels the native calls in flight, such as the requests to the daemon, so close() does not wait for them to finish
  protected void requestShutdownJni() { }

  protected abstract void closeJni(boolean save);
}
