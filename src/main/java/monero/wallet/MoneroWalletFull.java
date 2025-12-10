/**
 * Copyright (c) woodser
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package monero.wallet;

import common.utils.JsonUtils;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.model.MoneroWalletConfig;

/**
 * Implements a Monero wallet using fully client-side JNI bindings to monero-project's wallet2 in C++.
 */
public class MoneroWalletFull extends MoneroWalletJni {
  
  // ----------------------------- PRIVATE SETUP ------------------------------
  
  // class variables
  protected static final Logger LOGGER = Logger.getLogger(MoneroWalletFull.class.getName());

  private String password;
  
  /**
   * Private constructor with a handle to the memory address of the wallet in c++.
   * 
   * @param jniWalletHandle memory address of the wallet in c++
   * @param password password of the wallet instance
   */
  protected MoneroWalletFull(long jniWalletHandle, String password) {
    super(jniWalletHandle);
    this.password = password;
  }
  
  // --------------------- WALLET MANAGEMENT UTILITIES ------------------------
  
  /**
   * Indicates if a wallet exists at the given path.
   * 
   * @param path is the path to check for a wallet
   * @return true if a wallet exists at the given path, false otherwise
   */
  public static boolean walletExists(String path) {
    return walletExistsJni(path);
  }
  
  /**
   * Open an existing wallet using JNI bindings to wallet2.h.
   * 
   * @param path is the path to the wallet file to open
   * @param password is the password of the wallet file to open
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection configuration to a daemon (default = an unconnected wallet)
   * @return the opened wallet
   */
  public static MoneroWalletFull openWallet(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection) {
    if (!walletExistsJni(path)) throw new MoneroError("Wallet does not exist at path: " + path);
    if (networkType == null) throw new MoneroError("Must provide a network type");
    long jniWalletHandle = openWalletJni(path, password, networkType.ordinal());
    MoneroWalletFull wallet = new MoneroWalletFull(jniWalletHandle, password);
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  public static MoneroWalletFull openWallet(String path, String password, MoneroNetworkType networkType) { return openWallet(path, password, networkType, (MoneroRpcConnection) null); }
  public static MoneroWalletFull openWallet(String path, String password, MoneroNetworkType networkType, String daemonUri) { return openWallet(path, password, networkType, daemonUri == null ? null : new MoneroRpcConnection(daemonUri)); }

  /**
   * Open an existing wallet from byte[] data using JNI bindings to wallet2.h
   * 
   * @param password the password of the wallet file to open
   * @param networkType the wallet's network type
   * @param keysData the wallet's keys data
   * @param cacheData the wallet's cache data
   * @param daemonConnection connection configuration to a daemon (default = an unconnected wallet)
   * @return the opened wallet
   */
  public static MoneroWalletFull openWalletData(String password, MoneroNetworkType networkType, byte[] keysData, byte[] cacheData, MoneroRpcConnection daemonConnection) {
    if (networkType == null) throw new MoneroError("Must provide a network type");
    long jniWalletHandle = openWalletDataJni(password, networkType.ordinal(), keysData == null ? new byte[0] : keysData, cacheData == null ? new byte[0] : cacheData);
    MoneroWalletFull wallet = new MoneroWalletFull(jniWalletHandle, password);
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  
  /**
   * <p>Open an existing wallet using JNI bindings to wallet2.h.</p>
   * 
   * <p>Example:</p>
   * 
   * <code>
   * MoneroWallet wallet = MoneroWalletFull.openWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("mywallet")<br>
   * &nbsp;&nbsp; .setPassword("supersecretpassword")<br>
   * &nbsp;&nbsp; .setNetworkType(MoneroNetworkType.STAGENET)<br>
   * &nbsp;&nbsp; .setServerUri("http://localhost:38083"));<br>
   * </code>
   * 
   * <p>
   * All supported configuration:<br>
   * &nbsp;&nbsp; path - path of the wallet to open<br>
   * &nbsp;&nbsp; password - password of the wallet to open<br>
   * &nbsp;&nbsp; networkType - network type of the wallet to open (one of MoneroNetworkType.MAINNET|TESTNET|STAGENET)<br>
   * &nbsp;&nbsp; serverUri - uri of the wallet's daemon (optional)<br>
   * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; server - MoneroRpcConnection to a monero daemon (optional)<br>
   * </p>
   * 
   * @param config configures the wallet to open
   * @return the wallet instance
   */
  public static MoneroWalletFull openWallet(MoneroWalletConfig config) {
    
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
    MoneroWalletFull wallet;
    if (config.getKeysData() == null) {
      wallet = openWallet(config.getPath(), config.getPassword(), config.getNetworkType(), config.getServer());
    } else {
      wallet = openWalletData(config.getPassword(), config.getNetworkType(), config.getKeysData(), config.getCacheData(), config.getServer());
    }

    // set connection manager
    wallet.setConnectionManager(config.getConnectionManager());
    return wallet;
  }
  
  /**
   * <p>Create a wallet using JNI bindings to wallet2.h.</p>
   * 
   * <p>Examples:</p>
   * 
   * <code>
   * // create stagenet wallet with randomly generated seed<br>
   * MoneroWallet wallet1 = MoneroWalletFull.createWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("/mywallets/wallet1")<br>
   * &nbsp;&nbsp; .setPassword("supersecretpassword")<br>
   * &nbsp;&nbsp; .setNetworkType(MoneroNetworkType.STAGENET)<br>
   * &nbsp;&nbsp; .setServerUri("http://localhost:38081") // leave blank for offline wallet<br>
   * &nbsp;&nbsp; .setServerUsername("superuser")<br>
   * &nbsp;&nbsp; .setServerPassword("abctesting123"));<br><br>
   * 
   * // restore mainnet wallet from seed<br>
   * MoneroWallet wallet2 = MoneroWalletFull.createWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("/mywallets/wallet2")  // leave blank for in-memory wallet<br>
   * &nbsp;&nbsp; .setPassword("abctesting123")<br>
   * &nbsp;&nbsp; .setNetworkType("mainnet")<br>
   * &nbsp;&nbsp; .setServerUri("http://localhost:18081")<br>
   * &nbsp;&nbsp; .setServerUsername("superuser")<br>
   * &nbsp;&nbsp; .setServerPassword("abctesting123")<br>
   * &nbsp;&nbsp; .setSeed("biggest duets beware eskimos coexist igloo...")<br>
   * &nbsp;&nbsp; .setRestoreHeight(573800l));<br>
   * </code>
   * 
   * <p>
   * All supported configuration:<br>
   * &nbsp;&nbsp; path - path of the wallet to create (optional, in-memory wallet if not given)<br>
   * &nbsp;&nbsp; password - password of the wallet to create<br>
   * &nbsp;&nbsp; networkType - network type of the wallet to create (one of MoneroNetworkType.MAINNET|TESTNET|STAGENET)<br>
   * &nbsp;&nbsp; seed - seed of the wallet to create (optional, random wallet created if neither seed nor keys given)<br>
   * &nbsp;&nbsp; seedOffset - the offset used to derive a new seed from the given seed to recover a secret wallet from the seed<br>
   * &nbsp;&nbsp; isMultisig - restore multisig wallet from seed<br>
   * &nbsp;&nbsp; primaryAddress - primary address of the wallet to create (only provide if restoring from keys)<br>
   * &nbsp;&nbsp; privateViewKey - private view key of the wallet to create (optional)<br>
   * &nbsp;&nbsp; privateSpendKey - private spend key of the wallet to create (optional)<br>
   * &nbsp;&nbsp; restoreHeight - block height to start scanning from (defaults to 0 unless generating random wallet)<br>
   * &nbsp;&nbsp; language - language of the wallet's seed (defaults to "English" or auto-detected)<br>
   * &nbsp;&nbsp; server - MoneroRpcConnection to a monero daemon (optional)<br>
   * &nbsp;&nbsp; serverUri - uri of the wallet's daemon (optional)<br>
   * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; connectionManager - manage connections to monerod (optional)<br>
   * &nbsp;&nbsp; accountLookahead - number of accounts to scan (optional)<br>
   * &nbsp;&nbsp; subaddressLookahead - number of subaddresses per account to scan (optional)<br>
   * </p>
   * 
   * @param config configures the wallet to create
   * @return the wallet instance
   */
  public static MoneroWalletFull createWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must specify config to open wallet");
    if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
    if (config.getPath() != null && !config.getPath().isEmpty() && MoneroWalletFull.walletExists(config.getPath())) throw new MoneroError("Wallet already exists: " + config.getPath());
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
    MoneroWalletFull wallet;
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
  
  private static MoneroWalletFull createWalletFromSeed(MoneroWalletConfig config) {
    if (config.getRestoreHeight() == null) config.setRestoreHeight(0l);
    long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
    MoneroWalletFull wallet = new MoneroWalletFull(jniWalletHandle, config.getPassword());
    return wallet;
  }
  
  private static MoneroWalletFull createWalletFromKeys(MoneroWalletConfig config) {
    if (config.getRestoreHeight() == null) config.setRestoreHeight(0l);
    if (config.getLanguage() == null) config.setLanguage(DEFAULT_LANGUAGE);
    try {
      long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
      MoneroWalletFull wallet = new MoneroWalletFull(jniWalletHandle, config.getPassword());
      return wallet;
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  private static MoneroWalletFull createWalletRandom(MoneroWalletConfig config) {
    if (config.getLanguage() == null) config.setLanguage(DEFAULT_LANGUAGE);
    long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
    return new MoneroWalletFull(jniWalletHandle, config.getPassword());
  }
  
  protected static String serializeWalletConfig(MoneroWalletConfig config) {
    Map<String, Object> configMap = JsonUtils.toMap(config);
    configMap.put("networkType", config.getNetworkType().ordinal());
    return JsonUtils.serialize(configMap);
  }
  
  /**
   * Get a list of available languages for the wallet's seed.
   * 
   * @return the available languages for the wallet's seed.
   */
  public static List<String> getSeedLanguages() {
    return Arrays.asList(getSeedLanguagesJni());
  }
  
  // ------------ WALLET METHODS SPECIFIC TO JNI IMPLEMENTATION ---------------
  
  /**
   * Move the wallet from its current path to the given path.
   * 
   * @param path is the new wallet's path
   */
  public void moveTo(String path) {
    assertNotClosed();
    moveToJni(path, password);
  }
  
  // -------------------------- COMMON WALLET METHODS -------------------------

  /**
   * Get the wallet's keys and cache data.
   * 
   * @return the keys and cache data, respectively
   */
  public synchronized byte[][] getData() {
    return new byte[][] { getKeysFileBufferJni(password, isViewOnly()), getCacheFileBufferJni() };
  }
  
  @Override
  public void changePassword(String oldPassword, String newPassword) {
    try {
      if (!password.equals(oldPassword)) throw new RuntimeException("Invalid original password.");
      changePasswordJni(oldPassword, newPassword);
      password = newPassword;
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
 
  @Override
  public void close(boolean save) {
    if (isClosed) return; // closing a closed wallet has no effect
    super.close(save);
    password = null;
    refreshListening();
    try {
      closeJni(save);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
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
  protected native long getHeightByDateJni(int year, int month, int day);

  @Override
  protected native long getDaemonMaxPeerHeightJni();

  @Override
  protected native boolean isViewOnlyJni();
    
  @Override
  protected native void setDaemonConnectionJni(String uri, String username, String password, String proxyUri);
      
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
    
  @Override
  protected native long setListenerJni(MoneroWalletJni.WalletJniListener listener);
    
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
  protected native int getDefaultFeePriorityJni();

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

  private native byte[] getKeysFileBufferJni(String password, boolean viewOnly);

  private native byte[] getCacheFileBufferJni();
    
  @Override
  protected native void changePasswordJni(String oldPassword, String newPassword);
    
  @Override
  protected native void moveToJni(String path, String password);
  
  @Override
  protected native void saveJni();
    
  @Override
  protected native void closeJni(boolean save);
}
