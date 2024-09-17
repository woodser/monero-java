package monero.wallet;

import java.util.List;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroWalletConfig;

public class MoneroWalletLight extends MoneroWalletFull {

  public static MoneroWalletLight openWallet(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection) {
    if (!walletExistsJni(path)) throw new MoneroError("Wallet does not exist at path: " + path);
    if (networkType == null) throw new MoneroError("Must provide a network type");
    long jniWalletHandle = openWalletJni(path, password, networkType.ordinal());
    MoneroWalletLight wallet = new MoneroWalletLight(jniWalletHandle, password);
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  public static MoneroWalletLight openWallet(String path, String password, MoneroNetworkType networkType) { return openWallet(path, password, networkType, (MoneroRpcConnection) null); }
  public static MoneroWalletLight openWallet(String path, String password, MoneroNetworkType networkType, String daemonUri) { return openWallet(path, password, networkType, daemonUri == null ? null : new MoneroRpcConnection(daemonUri)); }

  public static MoneroWalletLight openWalletData(String password, MoneroNetworkType networkType, byte[] keysData, byte[] cacheData, MoneroRpcConnection daemonConnection) {
    if (networkType == null) throw new MoneroError("Must provide a network type");
    long jniWalletHandle = openWalletDataJni(password, networkType.ordinal(), keysData == null ? new byte[0] : keysData, cacheData == null ? new byte[0] : cacheData);
    MoneroWalletLight wallet = new MoneroWalletLight(jniWalletHandle, password);
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }

  public static MoneroWalletLight openWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must specify config to open wallet");
    if (config.getPassword() == null) throw new MoneroError("Must specify password to decrypt wallet");
    if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
    if (config.getSeed() != null) throw new MoneroError("Cannot specify seed when opening wallet");
    if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when opening wallet");
    if (config.getPrimaryAddress() != null) throw new MoneroError("Cannot specify primary address when opening wallet");
    if (config.getPrivateViewKey() != null) throw new MoneroError("Cannot specify private view key when opening wallet");
    if (config.getPrivateSpendKey() != null) throw new MoneroError("Cannot specify private spend key when opening wallet");
    if (config.getRestoreHeight() != null) throw new MoneroError("Cannot specify restore height when opening wallet");
    if (config.getLanguage() != null) throw new MoneroError("Cannot specify language when opening wallet");
    if (Boolean.TRUE.equals(config.getSaveCurrent())) throw new MoneroError("Cannot save current wallet when opening full wallet");

    // set server from connection manager if provided
    if (config.getConnectionManager() != null) {
      if (config.getServer() != null) throw new MoneroError("Wallet can be opened with a server or connection manager but not both");
      config.setServer(config.getConnectionManager().getConnection());
    }

    // read wallet data from disk unless provided
    MoneroWalletLight wallet;
    if (config.getKeysData() == null) {
      wallet = openWallet(config.getPath(), config.getPassword(), config.getNetworkType(), config.getServer());
    } else {
      wallet = openWalletData(config.getPassword(), config.getNetworkType(), config.getKeysData(), config.getCacheData(), config.getServer());
    }

    // set connection manager
    wallet.setConnectionManager(config.getConnectionManager());
    return wallet;
  }
  
  public static MoneroWalletLight createWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must specify config to open wallet");
    if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
    if (config.getPath() != null && !config.getPath().isEmpty() && MoneroWalletLight.walletExists(config.getPath())) throw new MoneroError("Wallet already exists: " + config.getPath());
    if (config.getSeed() != null && (config.getPrimaryAddress() != null || config.getPrivateViewKey() != null || config.getPrivateSpendKey() != null)) {
      throw new MoneroError("Wallet may be initialized with a seed or keys but not both");
    }
    if (Boolean.TRUE.equals(config.getSaveCurrent() != null)) throw new MoneroError("Cannot save current wallet when creating full wallet");

    // set server from connection manager if provided
    if (config.getConnectionManager() != null) {
      if (config.getServer() != null) throw new MoneroError("Wallet can be created with a server or connection manager but not both");
      config.setServer(config.getConnectionManager().getConnection());
    }
    
    // create wallet
    MoneroWalletLight wallet;
    if (config.getSeed() != null) {
      if (config.getLanguage() != null) throw new MoneroError("Cannot specify language when creating wallet from seed");
      wallet = createWalletFromSeed(config);
    } else if (config.getPrimaryAddress() != null || config.getPrivateSpendKey() != null) {
      if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when creating wallet from keys");
      wallet = createWalletFromKeys(config);
    } else {
      if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when creating random wallet");
      if (config.getRestoreHeight() != null) throw new MoneroError("Cannot specify restore height when creating random wallet");
      wallet = createWalletRandom(config);
    }

    // set connection manager
    wallet.setConnectionManager(config.getConnectionManager());
    return wallet;
  }
  
  private static MoneroWalletLight createWalletFromSeed(MoneroWalletConfig config) {
    if (config.getRestoreHeight() == null) config.setRestoreHeight(0l);
    long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
    MoneroWalletLight wallet = new MoneroWalletLight(jniWalletHandle, config.getPassword());
    return wallet;
  }
  
  private static MoneroWalletLight createWalletFromKeys(MoneroWalletConfig config) {
    if (config.getRestoreHeight() == null) config.setRestoreHeight(0l);
    if (config.getLanguage() == null) config.setLanguage(DEFAULT_LANGUAGE);
    try {
      long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
      MoneroWalletLight wallet = new MoneroWalletLight(jniWalletHandle, config.getPassword());
      return wallet;
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  private static MoneroWalletLight createWalletRandom(MoneroWalletConfig config) {
    if (config.getLanguage() == null) config.setLanguage(DEFAULT_LANGUAGE);
    long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
    return new MoneroWalletLight(jniWalletHandle, config.getPassword());
  }
  

  private MoneroWalletLight(long jniWalletHandle, String password) {
      super(jniWalletHandle, password);
  }

  @Override
  public List<MoneroOutputWallet> getOutputs(MoneroOutputQuery query) {
    assertNotClosed();
    
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
  }

  // ------------------------------ NATIVE METHODS ----------------------------

  protected native static boolean walletExistsJni(String path);

  protected native static long openWalletJni(String path, String password, int networkType);

  protected native static long openWalletDataJni(String password, int networkType, byte[] keysData, byte[] cacheData);

  protected native static long createWalletJni(String walletConfigJson);
  
  @Override
  protected native long getHeightJni();
  @Override
  protected native long getRestoreHeightJni();
  @Override
  protected native void setRestoreHeightJni(long height);
  @Override
  protected native long getDaemonHeightJni();
  @Override
  protected native long getDaemonMaxPeerHeightJni();
  @Override
  protected native long getHeightByDateJni(int year, int month, int day);
  @Override
  protected native boolean isViewOnlyJni();
  @Override
  protected native void setDaemonConnectionJni(String uri, String username, String password);
  @Override
  protected native void setProxyJni(String uri);
  @Override
  protected native String[] getDaemonConnectionJni(); // returns [uri, username, password]
  @Override
  protected native boolean isConnectedToDaemonJni();
  @Override
  protected native boolean isDaemonSyncedJni();
  @Override
  protected native boolean isSyncedJni();
  @Override
  protected native int getNetworkTypeJni();
  @Override
  protected native String getVersionJni();
  @Override
  protected native String getPathJni();
  @Override
  protected native String getSeedJni();
  @Override
  protected native String getSeedLanguageJni();
  
  protected static native String[] getSeedLanguagesJni();
  @Override
  protected native String getPublicViewKeyJni();
  @Override
  protected native String getPrivateViewKeyJni();
  @Override
  protected native String getPublicSpendKeyJni();
  @Override
  protected native String getPrivateSpendKeyJni();
  @Override
  protected native String getAddressJni(int accountIdx, int subaddressIdx);
  @Override
  protected native String getAddressIndexJni(String address);
  @Override
  protected native String getIntegratedAddressJni(String standardAddress, String paymentId);
  @Override
  protected native String decodeIntegratedAddressJni(String integratedAddress);
  
  //protected native long setListenerJni(WalletJniListener listener);
  @Override
  protected native Object[] syncJni(long startHeight);
  @Override
  protected native void startSyncingJni(long syncPeriodInMs);
  @Override
  protected native void stopSyncingJni();
  @Override
  protected native void scanTxsJni(String[] txHashes);
  @Override
  protected native void rescanSpentJni();
  @Override
  protected native void rescanBlockchainJni();
  @Override
  protected native String getBalanceWalletJni();
  @Override
  protected native String getBalanceAccountJni(int accountIdx);
  @Override
  protected native String getBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  @Override
  protected native String getUnlockedBalanceWalletJni();
  @Override
  protected native String getUnlockedBalanceAccountJni(int accountIdx);
  @Override
  protected native String getUnlockedBalanceSubaddressJni(int accountIdx, int subaddressIdx);
  @Override
  protected native String getAccountsJni(boolean includeSubaddresses, String tag);
  @Override
  protected native String getAccountJni(int accountIdx, boolean includeSubaddresses);
  @Override
  protected native String createAccountJni(String label);
  @Override
  protected native String getSubaddressesJni(int accountIdx, int[] subaddressIndices);
  @Override
  protected native String createSubaddressJni(int accountIdx, String label);
  @Override
  protected native void setSubaddressLabelJni(int accountIdx, int subaddressIdx, String label);
  @Override
  protected native String getTxsJni(String txQueryJson);
  @Override
  protected native String getTransfersJni(String transferQueryJson);
  @Override
  protected native String getOutputsJni(String outputQueryJson);
  @Override
  protected native String exportOutputsJni(boolean all);
  @Override
  protected native int importOutputsJni(String outputsHex);
  @Override
  protected native String exportKeyImagesJni(boolean all);
  @Override
  protected native String importKeyImagesJni(String keyImagesJson);
  @Override
  protected native String[] relayTxsJni(String[] txMetadatas);
  @Override
  protected native void freezeOutputJni(String KeyImage);
  @Override
  protected native void thawOutputJni(String keyImage);
  @Override
  protected native boolean isOutputFrozenJni(String keyImage);
  @Override
  protected native String createTxsJni(String txConfigJson);
  @Override
  protected native String sweepUnlockedJni(String txConfigJson);
  @Override
  protected native String sweepOutputJni(String txConfigJson);
  @Override
  protected native String sweepDustJni(boolean doNotRelay);
  @Override
  protected native String describeTxSetJni(String txSetJson);
  @Override
  protected native String signTxsJni(String unsignedTxHex);
  @Override
  protected native String[] submitTxsJni(String signedTxHex);
  @Override
  protected native String[] getTxNotesJni(String[] txHashes);
  @Override
  protected native void setTxNotesJni(String[] txHashes, String[] notes);
  @Override
  protected native String signMessageJni(String msg, int signatureType, int accountIdx, int subaddressIdx);
  @Override
  protected native String verifyMessageJni(String msg, String address, String signature);
  @Override
  protected native String getTxKeyJni(String txHash);
  @Override
  protected native String checkTxKeyJni(String txHash, String txKey, String address);
  @Override
  protected native String getTxProofJni(String txHash, String address, String message);
  @Override
  protected native String checkTxProofJni(String txHash, String address, String message, String signature);
  @Override
  protected native String getSpendProofJni(String txHash, String message);
  @Override
  protected native boolean checkSpendProofJni(String txHash, String message, String signature);
  @Override
  protected native String getReserveProofWalletJni(String message);
  @Override
  protected native String getReserveProofAccountJni(int accountIdx, String amount, String message);
  @Override
  protected native String checkReserveProofJni(String address, String message, String signature);
  @Override
  protected native String getAddressBookEntriesJni(int[] indices);
  @Override
  protected native int addAddressBookEntryJni(String address, String description);
  @Override
  protected native void editAddressBookEntryJni(int index, boolean setAddress, String address, boolean setDescription, String description);
  @Override
  protected native void deleteAddressBookEntryJni(int entryIdx);
  @Override
  protected native String getPaymentUriJni(String sendRequestJson);
  @Override
  protected native String parsePaymentUriJni(String uri);
  @Override
  protected native String getAttributeJni(String key);
  @Override
  protected native void setAttributeJni(String key, String val);
  @Override
  protected native void startMiningJni(long numThreads, boolean backgroundMining, boolean ignoreBattery);
  @Override
  protected native void stopMiningJni();
  @Override
  protected native boolean isMultisigImportNeededJni();
  @Override
  protected native String getMultisigInfoJni();
  @Override
  protected native String prepareMultisigJni();
  @Override
  protected native String makeMultisigJni(String[] multisigHexes, int threshold, String password);
  @Override
  protected native String exchangeMultisigKeysJni(String[] multisigHexes, String password);
  @Override
  protected native String exportMultisigHexJni();
  @Override
  protected native int importMultisigHexJni(String[] multisigHexes);
  @Override
  protected native String signMultisigTxHexJni(String multisigTxHex);
  @Override
  protected native String[] submitMultisigTxHexJni(String signedMultisigTxHex);
  @Override
  protected native byte[] getKeysFileBufferJni(String password, boolean viewOnly);
  @Override
  protected native byte[] getCacheFileBufferJni();
  @Override
  protected native void changePasswordJni(String oldPassword, String newPassword);
  @Override
  protected native void moveToJni(String path, String password);
  @Override
  protected native void saveJni();
  @Override
  protected native void closeJni(boolean save);
}