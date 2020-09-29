/**
 * Copyright (c) 2017-2020 woodser
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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
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
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;
import monero.wallet.model.MoneroWalletListenerI;

/**
 * Implements a Monero wallet using JNI to bridge to Monero Core C++.
 */
public class MoneroWalletJni extends MoneroWalletBase {
  
  // ----------------------------- PRIVATE SETUP ------------------------------

  // load Monero Core C++ as a dynamic library
  static {
    System.loadLibrary("monero-java");
  }
  
  // logger
  private static final Logger LOGGER = Logger.getLogger(MoneroWalletJni.class.getName());
  
  // instance variables
  private long jniWalletHandle;                 // memory address of the wallet in c++; this variable is read directly by name in c++
  private long jniListenerHandle;               // memory address of the wallet listener in c++; this variable is read directly by name in c++
  private WalletJniListener jniListener;        // receives notifications from jni c++
  private Set<MoneroWalletListenerI> listeners; // externally subscribed wallet listeners
  private boolean isClosed;                     // whether or not wallet is closed
  
  /**
   * Private constructor with a handle to the memory address of the wallet in c++.
   * 
   * @param jniWalletHandle is the memory address of the wallet in c++
   */
  private MoneroWalletJni(long jniWalletHandle) {
    this.jniWalletHandle = jniWalletHandle;
    this.jniListener = new WalletJniListener(this);
    this.listeners = new LinkedHashSet<MoneroWalletListenerI>();
    this.isClosed = false;
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
  public static MoneroWalletJni openWallet(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection) {
    if (!walletExistsJni(path)) throw new MoneroError("Wallet does not exist at path: " + path);
    if (networkType == null) throw new MoneroError("Must provide a network type");
    long jniWalletHandle = openWalletJni(path, password, networkType.ordinal());
    MoneroWalletJni wallet = new MoneroWalletJni(jniWalletHandle);
    if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  public static MoneroWalletJni openWallet(String path, String password, MoneroNetworkType networkType) { return openWallet(path, password, networkType, (MoneroRpcConnection) null); }
  public static MoneroWalletJni openWallet(String path, String password, MoneroNetworkType networkType, String daemonUri) { return openWallet(path, password, networkType, daemonUri == null ? null : new MoneroRpcConnection(daemonUri)); }
  
  /**
   * <p>Open an existing wallet using JNI bindings to wallet2.h.</p>
   * 
   * <p>Example:</p>
   * 
   * <code>
   * MoneroWallet wallet = MoneroWalletJni.openWallet(new MoneroWalletConfig()<br>
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
   * &nbsp;&nbsp; server - MoneroRpcConnection providing server configuration (optional)<br>
   * </p>
   * 
   * @param config configures the wallet to open
   * @return the wallet instance
   */
  public static MoneroWalletJni openWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must specify config to open wallet");
    if (config.getPath() == null) throw new MoneroError("Must specify path to open wallet");
    if (config.getPassword() == null) throw new MoneroError("Must specify password to decrypt wallet");
    if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
    if (config.getMnemonic() != null) throw new MoneroError("Cannot specify mnemonic when opening wallet");
    if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when opening wallet");
    if (config.getPrimaryAddress() != null) throw new MoneroError("Cannot specify primary address when opening wallet");
    if (config.getPrivateViewKey() != null) throw new MoneroError("Cannot specify private view key when opening wallet");
    if (config.getPrivateSpendKey() != null) throw new MoneroError("Cannot specify private spend key when opening wallet");
    if (config.getRestoreHeight() != null) throw new MoneroError("Cannot specify restore height when opening wallet");
    if (config.getLanguage() != null) throw new MoneroError("Cannot specify language when opening wallet");
    if (Boolean.TRUE.equals(config.getSaveCurrent())) throw new MoneroError("Cannot save current wallet when opening JNI wallet");
    
    // open wallet
    return openWallet(config.getPath(), config.getPassword(), config.getNetworkType(), config.getServer());
  }
  
  /**
   * <p>Create a wallet using JNI bindings to wallet2.h.</p>
   * 
   * <p>Examples:</p>
   * 
   * <code>
   * // create stagenet wallet with randomly generated mnemonic<br>
   * MoneroWallet wallet1 = MoneroWalletJni.createWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("/mywallets/wallet1")<br>
   * &nbsp;&nbsp; .setPassword("supersecretpassword")<br>
   * &nbsp;&nbsp; .setNetworkType(MoneroNetworkType.STAGENET)<br>
   * &nbsp;&nbsp; .setServerUri("http://localhost:38081") // leave blank for offline wallet<br>
   * &nbsp;&nbsp; .setServerUsername("superuser")<br>
   * &nbsp;&nbsp; .setServerPassword("abctesting123"));<br><br>
   * 
   * // restore mainnet wallet from mnemonic<br>
   * MoneroWallet wallet2 = MoneroWalletJni.createWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("/mywallets/wallet2")  // leave blank for in-memory wallet<br>
   * &nbsp;&nbsp; .setPassword("abctesting123")<br>
   * &nbsp;&nbsp; .setNetworkType("mainnet")<br>
   * &nbsp;&nbsp; .setServerUri("http://localhost:18081")<br>
   * &nbsp;&nbsp; .setServerUsername("superuser")<br>
   * &nbsp;&nbsp; .setServerPassword("abctesting123")<br>
   * &nbsp;&nbsp; .setMnemonic("biggest duets beware eskimos coexist igloo...")<br>
   * &nbsp;&nbsp; .setRestoreHeight(573800l));<br>
   * </code>
   * 
   * <p>
   * All supported configuration:<br>
   * &nbsp;&nbsp; path - path of the wallet to create (optional, in-memory wallet if not given)<br>
   * &nbsp;&nbsp; password - password of the wallet to create<br>
   * &nbsp;&nbsp; networkType - network type of the wallet to create (one of MoneroNetworkType.MAINNET|TESTNET|STAGENET)<br>
   * &nbsp;&nbsp; mnemonic - mnemonic of the wallet to create (optional, random wallet created if neither mnemonic nor keys given)<br>
   * &nbsp;&nbsp; seedOffset - the offset used to derive a new seed from the given mnemonic to recover a secret wallet from the mnemonic phrase<br>
   * &nbsp;&nbsp; primaryAddress - primary address of the wallet to create (only provide if restoring from keys)<br>
   * &nbsp;&nbsp; privateViewKey - private view key of the wallet to create (optional)<br>
   * &nbsp;&nbsp; privateSpendKey - private spend key of the wallet to create (optional)<br>
   * &nbsp;&nbsp; restoreHeight - block height to start scanning from (defaults to 0 unless generating random wallet)<br>
   * &nbsp;&nbsp; language - language of the wallet's mnemonic phrase (defaults to "English" or auto-detected)<br>
   * &nbsp;&nbsp; serverUri - uri of the wallet's daemon (optional)<br>
   * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; server - MoneroRpcConnection providing server configuration (optional)<br>
   * </p>
   * 
   * @param config configures the wallet to create
   * @return the wallet instance
   */
  public static MoneroWalletJni createWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must specify config to open wallet");
    if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
    if (config.getMnemonic() != null && (config.getPrimaryAddress() != null || config.getPrivateViewKey() != null || config.getPrivateSpendKey() != null)) {
      throw new MoneroError("Wallet may be initialized with a mnemonic or keys but not both");
    }
    if (Boolean.TRUE.equals(config.getSaveCurrent() != null)) throw new MoneroError("Cannot save current wallet when creating JNI wallet");
    
    // create wallet
    if (config.getMnemonic() != null) {
      if (config.getLanguage() != null) throw new MoneroError("Cannot specify language when creating wallet from mnemonic");
      return createWalletFromMnemonic(config.getPath(), config.getPassword(), config.getNetworkType(), config.getMnemonic(), config.getServer(), config.getRestoreHeight(), config.getSeedOffset());
    } else if (config.getPrimaryAddress() != null) {
      if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when creating wallet from keys");
      return createWalletFromKeys(config.getPath(), config.getPassword(), config.getNetworkType(), config.getPrimaryAddress(), config.getPrivateViewKey(), config.getPrivateSpendKey(), config.getServer(), config.getRestoreHeight(), config.getLanguage());
    } else {
      if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when creating random wallet");
      if (config.getRestoreHeight() != null) throw new MoneroError("Cannot specify restore height when creating random wallet");
      return createWalletRandom(config.getPath(), config.getPassword(), config.getNetworkType(), config.getServer(), config.getLanguage());
    }
  }
  
  /**
   * Create a new wallet with a randomly generated seed.
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection configuration to a daemon (default = an unconnected wallet)
   * @param language is the wallet and mnemonic's language (default = "English")
   * @return the wallet created with a randomly generated mnemonic
   */
  private static MoneroWalletJni createWalletRandom(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, String language) {
    if (path != null && !path.isEmpty() && MoneroWalletJni.walletExists(path)) throw new MoneroError("Wallet already exists: " + path);
    if (networkType == null) throw new MoneroError("Must provide a network type");
    if (language == null) language = DEFAULT_LANGUAGE;
    long jniWalletHandle;
    if (daemonConnection == null) jniWalletHandle = createWalletRandomJni(path, password, networkType.ordinal(), null, null, null, language);
    else jniWalletHandle = createWalletRandomJni(path, password, networkType.ordinal(), daemonConnection.getUri(), daemonConnection.getUsername(), daemonConnection.getPassword(), language);
    return new MoneroWalletJni(jniWalletHandle);
  }
  
  /**
   * Create a wallet from an existing mnemonic phrase.
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   * @param networkType is the wallet's network type
   * @param mnemonic is the mnemonic of the wallet to construct
   * @param daemonConnection is connection configuration to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore from (default = 0)
   * @param seedOffset is the offset used to derive a new seed from the given mnemonic to recover a secret wallet from the mnemonic phrase
   * @return the wallet created from a mnemonic
   */
  private static MoneroWalletJni createWalletFromMnemonic(String path, String password, MoneroNetworkType networkType, String mnemonic, MoneroRpcConnection daemonConnection, Long restoreHeight, String seedOffset) {
    if (path != null && !path.isEmpty() && MoneroWalletJni.walletExists(path)) throw new MoneroError("Wallet already exists: " + path);
    if (networkType == null) throw new MoneroError("Must provide a network type");
    if (restoreHeight == null) restoreHeight = 0l;
    long jniWalletHandle = createWalletFromMnemonicJni(path, password, networkType.ordinal(), mnemonic, restoreHeight, seedOffset);
    MoneroWalletJni wallet = new MoneroWalletJni(jniWalletHandle);
    wallet.setDaemonConnection(daemonConnection);
    return wallet;
  }
  
  /**
   * Create a wallet from an address, view key, and spend key.
   * 
   * @param path is the path to create the wallet
   * @param password is the password encrypt the wallet
   * @param networkType is the wallet's network type
   * @param address is the address of the wallet to construct
   * @param viewKey is the view key of the wallet to construct
   * @param spendKey is the spend key of the wallet to construct
   * @param daemonConnection is connection configuration to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   * @param language is the wallet and mnemonic's language (default = "English")
   * @return the wallet created from keys
   */
  private static MoneroWalletJni createWalletFromKeys(String path, String password, MoneroNetworkType networkType, String address, String viewKey, String spendKey, MoneroRpcConnection daemonConnection, Long restoreHeight, String language) {
    if (path != null && !path.isEmpty() && MoneroWalletJni.walletExists(path)) throw new MoneroError("Wallet already exists: " + path);
    if (restoreHeight == null) restoreHeight = 0l;
    if (networkType == null) throw new MoneroError("Must provide a network type");
    if (language == null) language = DEFAULT_LANGUAGE;
    try {
      long jniWalletHandle = createWalletFromKeysJni(path, password, networkType.ordinal(), address, viewKey, spendKey, restoreHeight, language);
      MoneroWalletJni wallet = new MoneroWalletJni(jniWalletHandle);
      wallet.setDaemonConnection(daemonConnection);
      return wallet;
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  /**
   * Get a list of available languages for the wallet's mnemonic phrase.
   * 
   * @return the available languages for the wallet's mnemonic phrase
   */
  public static List<String> getMnemonicLanguages() {
    return Arrays.asList(getMnemonicLanguagesJni());
  }
  
  // ------------ WALLET METHODS SPECIFIC TO JNI IMPLEMENTATION ---------------
  
  /**
   * Get the maximum height of the peers the wallet's daemon is connected to.
   *
   * @return the maximum height of the peers the wallet's daemon is connected to
   */
  public long getDaemonMaxPeerHeight() {
    assertNotClosed();
    try {
      return getDaemonMaxPeerHeightJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  /**
   * Indicates if the wallet's daemon is synced with the network.
   * 
   * @return true if the daemon is synced with the network, false otherwise
   */
  public boolean isDaemonSynced() {
    assertNotClosed();
    try {
      return isDaemonSyncedJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  /**
   * Indicates if the wallet is synced with the daemon.
   * 
   * @return true if the wallet is synced with the daemon, false otherwise
   */
  public boolean isSynced() {
    assertNotClosed();
    try {
      return isSyncedJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  /**
   * Get the wallet's network type (mainnet, testnet, or stagenet).
   * 
   * @return the wallet's network type
   */
  public MoneroNetworkType getNetworkType() {
    assertNotClosed();
    return MoneroNetworkType.values()[getNetworkTypeJni()];
  }
  
  /**
   * Get the height of the first block that the wallet scans.
   * 
   * @return the height of the first block that the wallet scans
   */
  public long getSyncHeight() {
    assertNotClosed();
    return getSyncHeightJni();
  }
  
  /**
   * Set the height of the first block that the wallet scans.
   * 
   * @param syncHeight is the height of the first block that the wallet scans
   */
  public void setSyncHeight(long syncHeight) {
    assertNotClosed();
    setSyncHeightJni(syncHeight);
  }
  
  /**
   * Register a listener to receive wallet notifications.
   * 
   * @param listener is the listener to receive wallet notifications
   */
  public void addListener(MoneroWalletListenerI listener) {
    assertNotClosed();
    listeners.add(listener);
    setIsListening(true);
  }
  
  /**
   * Unregister a listener to receive wallet notifications.
   * 
   * @param listener is the listener to unregister
   */
  public void removeListener(MoneroWalletListenerI listener) {
    assertNotClosed();
    if (!listeners.contains(listener)) throw new MoneroError("Listener is not registered to wallet");
    listeners.remove(listener);
    if (listeners.isEmpty()) setIsListening(false);
  }
  
  /**
   * Get the listeners registered with the wallet.
   * 
   * @return the registered listeners
   */
  public Set<MoneroWalletListenerI> getListeners() {
    assertNotClosed();
    return new HashSet<MoneroWalletListenerI>(listeners);
  }

  /**
   * Move the wallet from its current path to the given path.
   * 
   * @param path is the new wallet's path
   * @param password is the new wallet's password
   */
  public void moveTo(String path, String password) {
    assertNotClosed();
    moveToJni(path, password);
  }
  
  // -------------------------- COMMON WALLET METHODS -------------------------
  
  public boolean isViewOnly() {
    assertNotClosed();
    return isViewOnlyJni();
  }
  
  public void setDaemonConnection(MoneroRpcConnection daemonConnection) {
    assertNotClosed();
    if (daemonConnection == null) setDaemonConnectionJni("", "", "");
    else {
      try {
        setDaemonConnectionJni(daemonConnection.getUri() == null ? "" : daemonConnection.getUri().toString(), daemonConnection.getUsername(), daemonConnection.getPassword());
      } catch (Exception e) {
        throw new MoneroError(e.getMessage());
      }
    }
  }
  
  public MoneroRpcConnection getDaemonConnection() {
    assertNotClosed();
    try {
      String[] vals = getDaemonConnectionJni();
      return vals == null ? null : new MoneroRpcConnection(vals[0], vals[1], vals[2]);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  public boolean isConnected() {
    assertNotClosed();
    try {
      return isConnectedJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public MoneroVersion getVersion() {
    assertNotClosed();
    try {
      String versionJson = getVersionJni();
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, versionJson, MoneroVersion.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public String getPath() {
    assertNotClosed();
    String path = getPathJni();
    return path.isEmpty() ? null : path;
  }

  @Override
  public String getMnemonic() {
    assertNotClosed();
    String mnemonic = getMnemonicJni();
    if ("".equals(mnemonic)) return null;
    return mnemonic;
  }
  
  @Override
  public String getMnemonicLanguage() {
    assertNotClosed();
    String mnemonicLanguage = getMnemonicLanguageJni();
    if ("".equals(mnemonicLanguage)) return null;
    return mnemonicLanguage;
  }

  @Override
  public String getPrivateViewKey() {
    assertNotClosed();
    return getPrivateViewKeyJni();
  }
  
  @Override
  public String getPrivateSpendKey() {
    assertNotClosed();
    String privateSpendKey = getPrivateSpendKeyJni();
    if ("".equals(privateSpendKey)) return null;
    return privateSpendKey;
  }
  
  @Override
  public String getPublicViewKey() {
    assertNotClosed();
    return getPublicViewKeyJni();
  }
  
  @Override
  public String getPublicSpendKey() {
    assertNotClosed();
    return getPublicSpendKeyJni();
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    assertNotClosed();
    try {
      String integratedAddressJson = getIntegratedAddressJni("", paymentId);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, integratedAddressJson, MoneroIntegratedAddress.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    assertNotClosed();
    try {
      String integratedAddressJson = decodeIntegratedAddressJni(integratedAddress);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, integratedAddressJson, MoneroIntegratedAddress.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public long getHeight() {
    assertNotClosed();
    return getHeightJni();
  }

  @Override
  public long getDaemonHeight() {
    assertNotClosed();
    try {
      return getDaemonHeightJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public long getHeightByDate(int year, int month, int day) {
    assertNotClosed();
    try {
      return getHeightByDateJni(year, month ,day);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener) {
    assertNotClosed();
    if (startHeight == null) startHeight = Math.max(getHeight(), getSyncHeight());
    
    // register listener if given
    if (listener != null) addListener(listener);
    
    // sync wallet and handle exception
    try {
      Object[] results = syncJni(startHeight);
      return new MoneroSyncResult((long) results[0], (boolean) results[1]);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    } finally {
      if (listener != null) removeListener(listener); // unregister listener
    }
  }
  
  @Override
  public void startSyncing() {
    assertNotClosed();
    try {
      startSyncingJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  public void stopSyncing() {
    assertNotClosed();
    try {
      stopSyncingJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public void rescanSpent() {
    assertNotClosed();
    try {
      rescanSpentJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public void rescanBlockchain() {
    assertNotClosed();
    try {
      rescanBlockchainJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    assertNotClosed();
    String accountsJson = getAccountsJni(includeSubaddresses, tag);
    List<MoneroAccount> accounts = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountsJson, AccountsContainer.class).accounts;
    for (MoneroAccount account : accounts) sanitizeAccount(account);
    return accounts;
  }
  
  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    assertNotClosed();
    String accountJson = getAccountJni(accountIdx, includeSubaddresses);
    MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
    sanitizeAccount(account);
    return account;
  }

  @Override
  public MoneroAccount createAccount(String label) {
    assertNotClosed();
    String accountJson = createAccountJni(label);
    MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
    sanitizeAccount(account);
    return account;
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    assertNotClosed();
    String subaddresses_json = getSubaddressesJni(accountIdx, GenUtils.listToIntArray(subaddressIndices));
    List<MoneroSubaddress> subaddresses = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddresses_json, SubaddressesContainer.class).subaddresses;
    for (MoneroSubaddress subaddress : subaddresses) sanitizeSubaddress(subaddress);
    return subaddresses;
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    assertNotClosed();
    String subaddressJson = createSubaddressJni(accountIdx, label);
    MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
    sanitizeSubaddress(subaddress);
    return subaddress;
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    assertNotClosed();
    return getAddressJni(accountIdx, subaddressIdx);
  }

  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    assertNotClosed();
    try {
      String subaddressJson = getAddressIndexJni(address);
      MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
      return sanitizeSubaddress(subaddress);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public BigInteger getBalance() {
    assertNotClosed();
    try {
      return new BigInteger(getBalanceWalletJni());
    } catch (MoneroError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public BigInteger getBalance(int accountIdx) {
    assertNotClosed();
    try {
      return new BigInteger(getBalanceAccountJni(accountIdx));
    } catch (MoneroError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    assertNotClosed();
    try {
      return new BigInteger(getBalanceSubaddressJni(accountIdx, subaddressIdx));
    } catch (MoneroError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public BigInteger getUnlockedBalance() {
    assertNotClosed();
    try {
      return new BigInteger(getUnlockedBalanceWalletJni());
    } catch (MoneroError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    assertNotClosed();
    try {
      return new BigInteger(getUnlockedBalanceAccountJni(accountIdx));
    } catch (MoneroError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    assertNotClosed();
    try {
      return new BigInteger(getUnlockedBalanceSubaddressJni(accountIdx, subaddressIdx));
    } catch (MoneroError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public List<MoneroTxWallet> getTxs(MoneroTxQuery query, Collection<String> missingTxHashes) {
    assertNotClosed();
    
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
    return deserializeTxs(query, blocksJson, missingTxHashes);
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
    assertNotClosed();
    
    // copy and normalize query up to block
    if (query == null) query = new MoneroTransferQuery();
    else {
      if (query.getTxQuery() == null) query = query.copy();
      else {
        MoneroTxQuery txQuery = query.getTxQuery().copy();
        if (query.getTxQuery().getTransferQuery() == query) query = txQuery.getTransferQuery();
        else {
          GenUtils.assertNull("Transfer query's tx query must be circular reference or null", query.getTxQuery().getTransferQuery());
          query = query.copy();
          query.setTxQuery(txQuery);
        }
      }
    }
    if (query.getTxQuery() == null) query.setTxQuery(new MoneroTxQuery());
    query.getTxQuery().setTransferQuery(query);
    if (query.getTxQuery().getBlock() == null) query.getTxQuery().setBlock(new MoneroBlock().setTxs(query.getTxQuery()));
    
    // serialize query from block and fetch transfers from jni
    String blocksJson;
    try {
      blocksJson = getTransfersJni(JsonUtils.serialize(query.getTxQuery().getBlock()));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
    
    // deserialize and return transfers
    return deserializeTransfers(query, blocksJson);
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
  
  @Override
  public String getOutputsHex() {
    assertNotClosed();
    String outputsHex = getOutputsHexJni();
    return outputsHex.isEmpty() ? null : outputsHex;
  }

  @Override
  public int importOutputsHex(String outputsHex) {
    assertNotClosed();
    return importOutputsHexJni(outputsHex);
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    assertNotClosed();
    String keyImagesJson = getKeyImagesJni();
    List<MoneroKeyImage> keyImages = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, keyImagesJson, KeyImagesContainer.class).keyImages;
    return keyImages;
  }

  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    assertNotClosed();
    
    // wrap and serialize key images in container for jni
    KeyImagesContainer keyImageContainer = new KeyImagesContainer(keyImages);
    String importResultJson = importKeyImagesJni(JsonUtils.serialize(keyImageContainer));
    
    // deserialize response
    return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, importResultJson, MoneroKeyImageImportResult.class);
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletJni.getNewKeyImagesFromLastImport() not implemented");
  }
  
  @Override
  public List<String> relayTxs(Collection<String> txMetadatas) {
    assertNotClosed();
    String[] txMetadatasArr = txMetadatas.toArray(new String[txMetadatas.size()]);  // convert to array for jni
    try {
      return Arrays.asList(relayTxsJni(txMetadatasArr));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public List<MoneroTxWallet> createTxs(MoneroTxConfig config) {
    assertNotClosed();
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
  }
  
  @Override
  public MoneroTxWallet sweepOutput(MoneroTxConfig config) {
    assertNotClosed();
    try {
      String txSetJson = sweepOutputJni(JsonUtils.serialize(config));
      MoneroTxSet txSet = JsonUtils.deserialize(txSetJson, MoneroTxSet.class);
      return txSet.getTxs().get(0);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public List<MoneroTxWallet> sweepUnlocked(MoneroTxConfig config) {
    assertNotClosed();
    
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
  }

  @Override
  public List<MoneroTxWallet> sweepDust(boolean relay) {
    assertNotClosed();
    String txSetJson;
    try { txSetJson = sweepDustJni(relay); }
    catch (Exception e) { throw new MoneroError(e.getMessage()); }
    MoneroTxSet txSet = JsonUtils.deserialize(txSetJson, MoneroTxSet.class);
    return txSet.getTxs();
  }
  
  @Override
  public MoneroTxSet parseTxSet(MoneroTxSet txSet) {
    assertNotClosed();
    String parsedTxSetJson;
    try {
      parsedTxSetJson = parseTxSetJni(JsonUtils.serialize(txSet));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
    return JsonUtils.deserialize(parsedTxSetJson, MoneroTxSet.class);
  }
  
  @Override
  public String signTxs(String unsignedTxHex) {
    assertNotClosed();
    try {
      return signTxsJni(unsignedTxHex);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public List<String> submitTxs(String signedTxHex) {
    assertNotClosed();
    try {
      return Arrays.asList(submitTxsJni(signedTxHex));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroCheckTx checkTxKey(String txHash, String txKey, String address) {
    assertNotClosed();
    try {
      String checkStr = checkTxKeyJni(txHash, txKey, address);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckTx.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public String getTxProof(String txHash, String address, String message) {
    assertNotClosed();
    try {
      return getTxProofJni(txHash, address, message);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroCheckTx checkTxProof(String txHash, String address, String message, String signature) {
    assertNotClosed();
    try {
      String checkStr = checkTxProofJni(txHash, address, message, signature);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckTx.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public String getSpendProof(String txHash, String message) {
    assertNotClosed();
    try {
      return getSpendProofJni(txHash, message);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public boolean checkSpendProof(String txHash, String message, String signature) {
    assertNotClosed();
    try {
      return checkSpendProofJni(txHash, message, signature);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public String getReserveProofWallet(String message) {
    try {
      return getReserveProofWalletJni(message);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    assertNotClosed();
    try {
      return getReserveProofAccountJni(accountIdx, amount.toString(), message);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage(), -1);
    }
  }

  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    assertNotClosed();
    try {
      String checkStr = checkReserveProofJni(address, message, signature);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, checkStr, MoneroCheckReserve.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage(), -1);
    }
  }

  @Override
  public String signMessage(String msg, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx) {
    assertNotClosed();
    return signMessageJni(msg, signatureType.ordinal(), accountIdx, subaddressIdx);
  }

  @Override
  public MoneroMessageSignatureResult verifyMessage(String msg, String address, String signature) {
    assertNotClosed();
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
  }

  @Override
  public String getTxKey(String txHash) {
    assertNotClosed();
    try {
      return getTxKeyJni(txHash);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public List<String> getTxNotes(List<String> txHashes) {
    assertNotClosed();
    return Arrays.asList(getTxNotesJni(txHashes.toArray(new String[txHashes.size()])));  // convert to array for jni
  }

  @Override
  public void setTxNotes(List<String> txHashes, List<String> notes) {
    assertNotClosed();
    setTxNotesJni(txHashes.toArray(new String[txHashes.size()]), notes.toArray(new String[notes.size()]));
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
    assertNotClosed();
    if (entryIndices == null) entryIndices = new ArrayList<Integer>();
    String entriesJson = getAddressBookEntriesJni(GenUtils.listToIntArray(entryIndices));
    List<MoneroAddressBookEntry> entries = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, entriesJson, AddressBookEntriesContainer.class).entries;
    if (entries == null) entries = new ArrayList<MoneroAddressBookEntry>();
    return entries;
  }

  @Override
  public int addAddressBookEntry(String address, String description) {
    assertNotClosed();
    return addAddressBookEntryJni(address, description);
  }

  @Override
  public void editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description) {
    assertNotClosed();
    editAddressBookEntryJni(index, setAddress, address, setDescription, description);
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    assertNotClosed();
    deleteAddressBookEntryJni(entryIdx);
  }

  @Override
  public void tagAccounts(String tag, Collection<Integer> accountIndices) {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletJni.tagAccounts() not implemented");
  }

  @Override
  public void untagAccounts(Collection<Integer> accountIndices) {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletJni.untagAccounts() not implemented");
  }

  @Override
  public List<MoneroAccountTag> getAccountTags() {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletJni.getAccountTags() not implemented");
  }

  @Override
  public void setAccountTagLabel(String tag, String label) {
    assertNotClosed();
    throw new RuntimeException("MoneroWalletJni.setAccountTagLabel() not implemented");
  }

  @Override
  public String createPaymentUri(MoneroTxConfig request) {
    assertNotClosed();
    try {
      return createPaymentUriJni(JsonUtils.serialize(request));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroTxConfig parsePaymentUri(String uri) {
    assertNotClosed();
    try {
      String sendRequestJson = parsePaymentUriJni(uri);
      return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, sendRequestJson, MoneroTxConfig.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public String getAttribute(String key) {
    assertNotClosed();
    return getAttributeJni(key);
  }

  @Override
  public void setAttribute(String key, String val) {
    assertNotClosed();
    setAttributeJni(key, val);
  }

  @Override
  public void startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    assertNotClosed();
    try {
      startMiningJni(numThreads == null ? 0l : (long) numThreads, Boolean.TRUE.equals(backgroundMining), Boolean.TRUE.equals(ignoreBattery));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public void stopMining() {
    assertNotClosed();
    try {
      stopMiningJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  
  @Override
  public boolean isMultisigImportNeeded() {
    assertNotClosed();
    return isMultisigImportNeededJni();
  }
  
  @Override
  public MoneroMultisigInfo getMultisigInfo() {
    try {
      String multisigInfoJson = getMultisigInfoJni();
      return JsonUtils.deserialize(multisigInfoJson, MoneroMultisigInfo.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public String prepareMultisig() {
    return prepareMultisigJni();
  }

  @Override
  public MoneroMultisigInitResult makeMultisig(List<String> multisigHexes, int threshold, String password) {
    try {
      String initMultisigResultJson = makeMultisigJni(multisigHexes.toArray(new String[multisigHexes.size()]), threshold, password);
      return JsonUtils.deserialize(initMultisigResultJson, MoneroMultisigInitResult.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password) {
    try {
      String initMultisigResultJson = exchangeMultisigKeysJni(multisigHexes.toArray(new String[multisigHexes.size()]), password);
      return JsonUtils.deserialize(initMultisigResultJson, MoneroMultisigInitResult.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public String getMultisigHex() {
    try {
      return getMultisigHexJni();
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public int importMultisigHex(List<String> multisigHexes) {
    try {
      return importMultisigHexJni(multisigHexes.toArray(new String[multisigHexes.size()]));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex) {
    try {
      String signMultisigResultJson = signMultisigTxHexJni(multisigTxHex);
      return JsonUtils.deserialize(signMultisigResultJson, MoneroMultisigSignResult.class);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }

  @Override
  public List<String> submitMultisigTxHex(String signedMultisigTxHex) {
    try {
      return Arrays.asList(submitMultisigTxHexJni(signedMultisigTxHex));
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public void save() {
    assertNotClosed();
    saveJni();
  }
  
  @Override
  public void close(boolean save) {
    if (isClosed) return; // closing a closed wallet has no effect
    isClosed = true;
    setIsListening(false);
    try {
      closeJni(save);
    } catch (Exception e) {
      throw new MoneroError(e.getMessage());
    }
  }
  
  @Override
  public boolean isClosed() {
    return isClosed;
  }
  
  // ------------------------------ NATIVE METHODS ----------------------------
  
  private native static boolean walletExistsJni(String path);
  
  private native static long openWalletJni(String path, String password, int networkType);
  
  private native static long createWalletRandomJni(String path, String password, int networkType, String daemonUrl, String daemonUsername, String daemonPassword, String language);
  
  private native static long createWalletFromMnemonicJni(String path, String password, int networkType, String mnemonic, long restoreHeight, String seedOffset);
  
  private native static long createWalletFromKeysJni(String path, String password, int networkType, String address, String viewKey, String spendKey, long restoreHeight, String language);
  
  private native long getHeightJni();
  
  private native long getSyncHeightJni();
  
  private native void setSyncHeightJni(long height);
  
  private native long getDaemonHeightJni();
  
  private native long getDaemonMaxPeerHeightJni();
  
  private native long getHeightByDateJni(int year, int month, int day);
  
  private native boolean isViewOnlyJni();
  
  private native void setDaemonConnectionJni(String uri, String username, String password);
  
  private native String[] getDaemonConnectionJni(); // returns [uri, username, password]
  
  private native boolean isConnectedJni();
  
  private native boolean isDaemonSyncedJni();
  
  private native boolean isSyncedJni();
  
  private native int getNetworkTypeJni();
  
  private native String getVersionJni();
  
  private native String getPathJni();
  
  private native String getMnemonicJni();
  
  private native String getMnemonicLanguageJni();
  
  private static native String[] getMnemonicLanguagesJni();
  
  private native String getPublicViewKeyJni();
  
  private native String getPrivateViewKeyJni();
  
  private native String getPublicSpendKeyJni();
  
  private native String getPrivateSpendKeyJni();
  
  private native String getAddressJni(int accountIdx, int subaddressIdx);
  
  private native String getAddressIndexJni(String address);
  
  private native String getIntegratedAddressJni(String standardAddress, String paymentId);
  
  private native String decodeIntegratedAddressJni(String integratedAddress);
  
  private native long setListenerJni(WalletJniListener listener);
  
  private native Object[] syncJni(long startHeight);
  
  private native void startSyncingJni();
  
  private native void stopSyncingJni();
  
  private native void rescanSpentJni();
  
  private native void rescanBlockchainJni();
  
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
  
  private native String getTxsJni(String txQueryJson);
  
  private native String getTransfersJni(String transferQueryJson);
  
  private native String getOutputsJni(String outputQueryJson);
  
  private native String getOutputsHexJni();
  
  private native int importOutputsHexJni(String outputsHex);
  
  private native String getKeyImagesJni();
  
  private native String importKeyImagesJni(String keyImagesJson);
  
  private native String[] relayTxsJni(String[] txMetadatas);
  
  private native String createTxsJni(String txConfigJson);
  
  private native String sweepUnlockedJni(String txConfigJson);
  
  private native String sweepOutputJni(String txConfigJson);
  
  private native String sweepDustJni(boolean doNotRelay);
  
  private native String parseTxSetJni(String txSetJson);
  
  private native String signTxsJni(String unsignedTxHex);
  
  private native String[] submitTxsJni(String signedTxHex);
  
  private native String[] getTxNotesJni(String[] txHashes);
  
  private native void setTxNotesJni(String[] txHashes, String[] notes);
  
  private native String signMessageJni(String msg, int signatureType, int accountIdx, int subaddressIdx);
  
  private native String verifyMessageJni(String msg, String address, String signature);
  
  private native String getTxKeyJni(String txHash);
  
  private native String checkTxKeyJni(String txHash, String txKey, String address);
  
  private native String getTxProofJni(String txHash, String address, String message);
  
  private native String checkTxProofJni(String txHash, String address, String message, String signature);
  
  private native String getSpendProofJni(String txHash, String message);
  
  private native boolean checkSpendProofJni(String txHash, String message, String signature);
  
  private native String getReserveProofWalletJni(String message);
  
  private native String getReserveProofAccountJni(int accountIdx, String amount, String message);
  
  private native String checkReserveProofJni(String address, String message, String signature);
  
  private native String getAddressBookEntriesJni(int[] indices);
  
  private native int addAddressBookEntryJni(String address, String description);
  
  private native void editAddressBookEntryJni(int index, boolean setAddress, String address, boolean setDescription, String description);
  
  private native void deleteAddressBookEntryJni(int entryIdx);
  
  private native String createPaymentUriJni(String sendRequestJson);
  
  private native String parsePaymentUriJni(String uri);
  
  private native String getAttributeJni(String key);
  
  private native void setAttributeJni(String key, String val);

  private native void startMiningJni(long numThreads, boolean backgroundMining, boolean ignoreBattery);
  
  private native void stopMiningJni();
  
  private native boolean isMultisigImportNeededJni();
  
  private native String getMultisigInfoJni();
  
  private native String prepareMultisigJni();
  
  private native String makeMultisigJni(String[] multisigHexes, int threshold, String password);
  
  private native String exchangeMultisigKeysJni(String[] multisigHexes, String password);
  
  private native String getMultisigHexJni();
  
  private native int importMultisigHexJni(String[] multisigHexes);
  
  private native String signMultisigTxHexJni(String multisigTxHex);
  
  private native String[] submitMultisigTxHexJni(String signedMultisigTxHex);
  
  private native void saveJni();
  
  private native void moveToJni(String path, String password);
  
  private native void closeJni(boolean save);
  
  // ------------------------------- LISTENERS --------------------------------
  
  /**
   * Receives notifications directly from jni c++.
   */
  @SuppressWarnings("unused") // called directly from jni c++
  private class WalletJniListener {
    
    private MoneroWalletJni wallet; // wallet to notify listeners
    
    public WalletJniListener(MoneroWalletJni wallet) {  // TODO: make this MoneroWallet when all methods moved to top-level
      this.wallet = wallet;
    }
    
    public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
      for (MoneroWalletListenerI listener : wallet.getListeners()) {
        listener.onSyncProgress(height, startHeight, endHeight, percentDone, message);
      }
    }
    
    public void onNewBlock(long height) {
      for (MoneroWalletListenerI listener : wallet.getListeners()) listener.onNewBlock(height);
    }
    
    public void onBalancesChanged(String newBalanceStr, String newUnlockedBalanceStr) {
      for (MoneroWalletListenerI listener : wallet.getListeners()) listener.onBalancesChanged(new BigInteger(newBalanceStr), new BigInteger(newUnlockedBalanceStr));
    }
    
    public void onOutputReceived(long height, String txHash, String amountStr, int accountIdx, int subaddressIdx, int version, long unlockHeight, boolean isLocked) {
      
      // build output to announce
      MoneroOutputWallet output = new MoneroOutputWallet();
      output.setAmount(new BigInteger(amountStr));
      output.setAccountIndex(accountIdx);
      output.setSubaddressIndex(subaddressIdx);
      MoneroTxWallet tx = new MoneroTxWallet();
      tx.setHash(txHash);
      tx.setVersion(version);
      tx.setUnlockHeight(unlockHeight);
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
      for (MoneroWalletListenerI listener : wallet.getListeners()) listener.onOutputReceived((MoneroOutputWallet) tx.getOutputs().get(0));
    }
    
    public void onOutputSpent(long height, String txHash, String amountStr, int accountIdx, int subaddressIdx, int version) {
      
      // build spent output
      MoneroOutputWallet output = new MoneroOutputWallet();
      output.setAmount(new BigInteger(amountStr));
      output.setAccountIndex(accountIdx);
      output.setSubaddressIndex(subaddressIdx);
      MoneroTxWallet tx = new MoneroTxWallet();
      tx.setHash(txHash);
      tx.setVersion(version);
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
      for (MoneroWalletListenerI listener : wallet.getListeners()) listener.onOutputSpent((MoneroOutputWallet) tx.getInputs().get(0));
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
  
  private static class AccountsContainer {
    public List<MoneroAccount> accounts;
  };
  
  private static class SubaddressesContainer {
    public List<MoneroSubaddress> subaddresses;
  };
  
  private static class BlocksWalletContainer {
    public List<MoneroBlockWallet> blocks;
    public List<String> missingTxHashes;
  }
  
  private static class DeserializedBlocksContainer {
    public List<MoneroBlock> blocks;
    public List<String> missingTxHashes;
  }
  
  private static class TxSetsContainer {
    public List<MoneroTxSet> txSets;
  }
  
  private static class KeyImagesContainer {
    public List<MoneroKeyImage> keyImages;
    @SuppressWarnings("unused") public KeyImagesContainer() { } // necessary for serialization
    public KeyImagesContainer(List<MoneroKeyImage> keyImages) { this.keyImages = keyImages; };
  }
  
  private static DeserializedBlocksContainer deserializeBlocks(String blocksJson) {
    DeserializedBlocksContainer deserializedBlocksContainer = new DeserializedBlocksContainer();
    deserializedBlocksContainer.blocks = new ArrayList<MoneroBlock>();
    deserializedBlocksContainer.missingTxHashes = new ArrayList<String>();
    BlocksWalletContainer blocksWalletContainer = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, BlocksWalletContainer.class);
    if (blocksWalletContainer.blocks != null) for (MoneroBlockWallet blockWallet : blocksWalletContainer.blocks) deserializedBlocksContainer.blocks.add(blockWallet.toBlock());
    if (blocksWalletContainer.missingTxHashes != null) for (String missingTxHash : blocksWalletContainer.missingTxHashes) deserializedBlocksContainer.missingTxHashes.add(missingTxHash);
    return deserializedBlocksContainer;
  }
  
  private static List<MoneroTxWallet> deserializeTxs(MoneroTxQuery query, String blocksJson, Collection<String> missingTxHashes) {
    
    // deserialize blocks
    DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
    if (missingTxHashes == null && !deserializedBlocks.missingTxHashes.isEmpty()) throw new MoneroError("Wallet missing requested tx hashes: " + deserializedBlocks.missingTxHashes);
    for (String missingTxHash : deserializedBlocks.missingTxHashes) missingTxHashes.add(missingTxHash);
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
  
  private static List<MoneroTransfer> deserializeTransfers(MoneroTransferQuery query, String blocksJson) {
    
    // deserialize blocks
    DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
    if (!deserializedBlocks.missingTxHashes.isEmpty()) throw new RuntimeException("Wallet missing requested tx hashes: " + deserializedBlocks.missingTxHashes);
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
  
  private static List<MoneroOutputWallet> deserializeOutputs(MoneroOutputQuery query, String blocksJson) {
    
    // deserialize blocks
    DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
    if (!deserializedBlocks.missingTxHashes.isEmpty()) throw new RuntimeException("Wallet missing requested tx hashes: " + deserializedBlocks.missingTxHashes);
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
  
  private static class AddressBookEntriesContainer {
    public List<MoneroAddressBookEntry> entries;
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  /**
   * Enables or disables listening in the c++ wallet.
   */
  private void setIsListening(boolean isEnabled) {
    jniListenerHandle = setListenerJni(isEnabled ? jniListener : null);
  }
  
  private void assertNotClosed() {
    if (isClosed) throw new MoneroError("Wallet is closed");
  }
  
  private static MoneroAccount sanitizeAccount(MoneroAccount account) {
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
