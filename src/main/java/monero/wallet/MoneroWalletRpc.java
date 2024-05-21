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

import common.utils.GenUtils;
import common.utils.JsonUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroRpcError;
import monero.common.MoneroUtils;
import monero.common.SslOptions;
import monero.common.TaskLooper;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroVersion;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroMessageSignatureResult;
import monero.wallet.model.MoneroMessageSignatureType;
import monero.wallet.model.MoneroMultisigInfo;
import monero.wallet.model.MoneroMultisigInitResult;
import monero.wallet.model.MoneroMultisigSignResult;
import monero.wallet.model.MoneroOutgoingTransfer;
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
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

/**
 * Implements a Monero wallet using monero-wallet-rpc.
 */
public class MoneroWalletRpc extends MoneroWalletDefault {

  // class variables
  private static final Logger LOGGER = Logger.getLogger(MoneroWalletRpc.class.getName());
  private static final TxHeightComparator TX_HEIGHT_COMPARATOR = new TxHeightComparator();
  private static final int ERROR_CODE_INVALID_PAYMENT_ID = -5; // invalid payment id error code
  private static final long DEFAULT_SYNC_PERIOD_IN_MS = 20000; // default period between syncs in ms (defined by DEFAULT_AUTO_REFRESH_PERIOD in wallet_rpc_server.cpp)
  
  // instance variables
  private String path;                                     // wallet's path identifier
  private MoneroRpcConnection rpc;                         // rpc connection to monero-wallet-rpc
  private MoneroRpcConnection daemonConnection;            // current daemon connection (unknown/null until explicitly set)
  private WalletPoller walletPoller;                       // listener which polls monero-wallet-rpc
  private WalletRpcZmqListener zmqListener;                // listener which processes zmq notifications from monero-wallet-rpc
  private Map<Integer, Map<Integer, String>> addressCache; // cache static addresses to reduce requests
  private Process process;                                 // process running monero-wallet-rpc if applicable
  private long syncPeriodInMs = DEFAULT_SYNC_PERIOD_IN_MS; // period between syncs in ms (default 20000)
  
  public MoneroWalletRpc(String uri) {
    this(new MoneroRpcConnection(uri));
  }
  
  public MoneroWalletRpc(String uri, String username, String password) {
    this(new MoneroRpcConnection(uri, username, password));
  }
  
  public MoneroWalletRpc(String uri, String username, String password, String zmqUri) {
    this(new MoneroRpcConnection(uri, username, password, zmqUri));
  }
  
  public MoneroWalletRpc(MoneroRpcConnection rpc) {
    super();
    this.rpc = rpc;
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }
  
  /**
   * Create an internal process running monero-wallet-rpc and connect to it.
   * 
   * Use `stopProcess()` to stop the newly created process.
   * 
   * @param cmd path then arguments to external monero-wallet-rpc executable
   * @throws IOException if input/output error with process
   */
  public MoneroWalletRpc(List<String> cmd) throws IOException {
    super();
    
    // start process
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.environment().put("LANG", "en_US.UTF-8"); // scrape output in english
    pb.redirectErrorStream(true);
    process = pb.start();
    
    // print output to console for debug
    boolean printOutput = LOGGER.isLoggable(Level.FINER);
    
    // read process output until success
    String line;
    String uri = null;
    StringBuilder sb = new StringBuilder();
    BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()));
    boolean success = false;
    while ((line = in.readLine()) != null) {
      LOGGER.log(Level.FINER, line);
      sb.append(line).append('\n'); // capture output in case of error
      
      // extract uri from e.g. "I Binding on 127.0.0.1 (IPv4):38085"
      String uriLineContains = "Binding on ";
      int uriLineContainsIdx = line.indexOf(uriLineContains);
      if (uriLineContainsIdx >= 0) {
        String host = line.substring(uriLineContainsIdx + uriLineContains.length(), line.lastIndexOf(' '));
        String port = line.substring(line.lastIndexOf(':') + 1);
        int sslIdx = cmd.indexOf("--rpc-ssl");
        boolean sslEnabled = sslIdx >= 0 ? "enabled".equalsIgnoreCase(cmd.get(sslIdx + 1)) : false;
        uri = (sslEnabled ? "https" : "http") + "://" + host + ":" + port;
      }
      
      // read success message
      if (line.contains("Starting wallet RPC server")) {
        if (printOutput) {
          new Thread(new Runnable() { // continue printing output in separate, non-blocking thread
            @Override
            public void run() {
              try {
                String line;
                BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()));
                while ((line = in.readLine()) != null) LOGGER.log(Level.FINER, line);
              } catch (IOException e) {
                //e.printStackTrace(); // exception expected on close
              }
            }
          }).start();
        }
        success = true;
        break;
      }
    }
    if (!printOutput) in.close();
    
    // throw error with process output if unsuccessful
    if (!success) throw new MoneroError("Failed to start monero-wallet-rpc server:\n\n" + sb.toString().trim());
    
    // get username, password, and zmq publish uri from params
    int userPassIdx = cmd.indexOf("--rpc-login");
    String userPass = userPassIdx >= 0 ? cmd.get(userPassIdx + 1) : null;
    String username = userPass == null ? null : userPass.substring(0, userPass.indexOf(':'));
    String password = userPass == null ? null : userPass.substring(userPass.indexOf(':') + 1);
    int zmqUriIdx = cmd.indexOf("--zmq-pub");
    String zmqUri = zmqUriIdx >= 0 ? cmd.get(zmqUriIdx + 1) : null;
    
    // initialize internal state
    rpc = new MoneroRpcConnection(uri, username, password, zmqUri);
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }
  
  // --------------------------- RPC WALLET METHODS ---------------------------
  
  /**
   * Get the internal process running monero-wallet-rpc.
   * 
   * @return the process running monero-wallet-rpc, null if not created from new process
   */
  public Process getProcess() {
    return process;
  }
  
  /**
   * Stop the internal process running monero-wallet-rpc, if applicable.
   * 
   * @return the error code from stopping the process
   */
  public int stopProcess() {
    return stopProcess(false);
  }

  /**
   * Stop the internal process running monero-wallet-rpc, if applicable.
   * 
   * @param force specifies if the process should be destroyed forcibly
   * @return the error code from stopping the process
   */
  public int stopProcess(boolean force) {
    if (process == null) throw new MoneroError("MoneroWalletRpc instance not created from new process");
    clear();
    if (force) process.destroyForcibly();
    else process.destroy();
    try { return process.waitFor(); }
    catch (Exception e) { throw new MoneroError(e); }
  }
  
  /**
   * Get the wallet's RPC connection.
   * 
   * @return the wallet's rpc connection
   */
  public MoneroRpcConnection getRpcConnection() {
    return rpc;
  }
  
  /**
   * Open an existing wallet on the monero-wallet-rpc server.
   * 
   * @param name is the name of the wallet file to open
   * @param password is the wallet's password
   * @return this wallet client
   */
  public MoneroWalletRpc openWallet(String name, String password) {
    return openWallet(new MoneroWalletConfig().setPath(name).setPassword(password));
  }
  
  /**
   * <p>Open an existing wallet on the monero-wallet-rpc server.</p>
   * 
   * <p>Example:<p>
   * 
   * <code>
   * MoneroWallet walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");<br>
   * walletRpc.openWallet("mywallet1", "supersecretpassword");<br>
   * walletRpc.openWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("mywallet")<br>
   * &nbsp;&nbsp; .setPassword("abc123")<br>
   * &nbsp;&nbsp; .setServerUri("http://localhost:38081"));<br>
   * </code>
   * 
   * <p>
   * All supported configuration:<br>
   * &nbsp;&nbsp; path - path of the wallet to create (optional, in-memory wallet if not given)<br>
   * &nbsp;&nbsp; password - password of the wallet to create<br>
   * &nbsp;&nbsp; serverUri - uri of a daemon to use (optional, monero-wallet-rpc usually started with daemon config)<br>
   * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; server - MoneroRpcConnection providing daemon configuration (optional)<br>
   * </p>
   * 
   * @param config configures the wallet to open
   * @return this wallet client
   */
  public MoneroWalletRpc openWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must provide configuration of wallet to open");
    if (config.getPath() == null || config.getPath().isEmpty()) throw new MoneroError("Filename is not initialized");
    // TODO: ensure other fields are uninitialized?
    
    // open wallet on rpc server
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", config.getPath());
    params.put("password", config.getPassword() == null ? "" : config.getPassword());
    rpc.sendJsonRequest("open_wallet", params);
    clear();
    path = config.getPath();

    // set connection manager or server
    if (config.getConnectionManager() != null) {
      if (config.getServer() != null) throw new MoneroError("Wallet can be opened with a server or connection manager but not both");
      setConnectionManager(config.getConnectionManager());
    } else if (config.getServer() != null) {
      setDaemonConnection(config.getServer());
    }
    
    return this;
  }
  
  /**
   * <p>Create and open a wallet on the monero-wallet-rpc server.</p>
   * 
   * <p>Example:<p>
   * 
   * <code>
   * MoneroWallet walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");<br>
   * walletRpc.createWallet(new MoneroWalletConfig()<br>
   * &nbsp;&nbsp; .setPath("mywallet")<br>
   * &nbsp;&nbsp; .setPassword("supersecretpassword")<br>
   * &nbsp;&nbsp; .setSeed("coexist igloo pamphlet lagoon...")<br>
   * &nbsp;&nbsp; .setRestoreHeight(1543218l));<br>
   * </code>
   * 
   * <p>
   * All supported configuration:<br>
   * &nbsp;&nbsp; path - path of the wallet to create (optional, in-memory wallet if not given)<br>
   * &nbsp;&nbsp; password - password of the wallet to create<br>
   * &nbsp;&nbsp; seed - seed of the wallet to create (optional, random wallet created if neither seed nor keys given)<br>
   * &nbsp;&nbsp; seedOffset - the offset used to derive a new seed from the given seed to recover a secret wallet from the seed<br>
   * &nbsp;&nbsp; isMultisig - restore multisig wallet from seed<br>
   * &nbsp;&nbsp; primaryAddress - primary address of the wallet to create (only provide if restoring from keys)<br>
   * &nbsp;&nbsp; privateViewKey - private view key of the wallet to create (optional)<br>
   * &nbsp;&nbsp; privateSpendKey - private spend key of the wallet to create (optional)<br>
   * &nbsp;&nbsp; restoreHeight - block height to start scanning from (defaults to 0 unless generating random wallet)<br>
   * &nbsp;&nbsp; language - language of the wallet's seed (defaults to "English" or auto-detected)<br>
   * &nbsp;&nbsp; server - MoneroRpcConnection to a monero daemon (optional)<br>
   * &nbsp;&nbsp; serverUri - uri of the daemon to use (optional, monero-wallet-rpc usually started with daemon config)<br>
   * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
   * &nbsp;&nbsp; connectionManager - manage connections to monerod (optional)<br>
   * <p>
   * 
   * @param config configures the wallet to create
   * @return this wallet client
   */
  public MoneroWalletRpc createWallet(MoneroWalletConfig config) {
    
    // validate config
    if (config == null) throw new MoneroError("Must specify config to create wallet");
    if (config.getNetworkType() != null) throw new MoneroError("Cannot specify network type when creating RPC wallet");
    if (config.getSeed() != null && (config.getPrimaryAddress() != null || config.getPrivateViewKey() != null || config.getPrivateSpendKey() != null)) {
      throw new MoneroError("Wallet can be initialized with a seed or keys but not both");
    }
    if (config.getAccountLookahead() != null || config.getSubaddressLookahead() != null) throw new MoneroError("monero-wallet-rpc does not support creating wallets with subaddress lookahead over rpc");

    // set server from connection manager if provided
    if (config.getConnectionManager() != null) {
      if (config.getServer() != null) throw new MoneroError("Wallet can be created with a server or connection manager but not both");
      config.setServer(config.getConnectionManager().getConnection());
    }
    
    // create wallet
    if (config.getSeed() != null) createWalletFromSeed(config);
    else if (config.getPrivateSpendKey() != null || config.getPrimaryAddress() != null) createWalletFromKeys(config);
    else createWalletRandom(config);
    
    // set connection manager or server
    if (config.getConnectionManager() != null) {
      setConnectionManager(config.getConnectionManager());
    } else if (config.getServer() != null) {
      setDaemonConnection(config.getServer());
    }

    return this;
  }
  
  private MoneroWalletRpc createWalletRandom(MoneroWalletConfig config) {

    // validate and normalize config
    config = config.copy();
    if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when creating random wallet");
    if (config.getRestoreHeight() != null) throw new MoneroError("Cannot specify restore height when creating random wallet");
    if (Boolean.FALSE.equals(config.getSaveCurrent())) throw new MoneroError("Current wallet is saved automatically when creating random wallet");
    if (config.getPath() == null || config.getPath().isEmpty()) throw new MoneroError("Wallet name is not initialized");
    if (config.getLanguage() == null || config.getLanguage().isEmpty()) config.setLanguage(DEFAULT_LANGUAGE);

    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", config.getPath());
    params.put("password", config.getPassword());
    params.put("language", config.getLanguage());
    try { rpc.sendJsonRequest("create_wallet", params); }
    catch (MoneroRpcError e) { handleCreateWalletError(config.getPath(), e); }
    clear();
    path = config.getPath();
    return this;
  }
  
  private MoneroWalletRpc createWalletFromSeed(MoneroWalletConfig config) {
    config = config.copy();
    if (config.getLanguage() == null || config.getLanguage().isEmpty()) config.setLanguage(DEFAULT_LANGUAGE);
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", config.getPath());
    params.put("password", config.getPassword());
    params.put("seed", config.getSeed());
    params.put("seed_offset", config.getSeedOffset());
    params.put("restore_height", config.getRestoreHeight());
    params.put("language", config.getLanguage());
    params.put("autosave_current", config.getSaveCurrent());
    params.put("enable_multisig_experimental", config.isMultisig());
    try { rpc.sendJsonRequest("restore_deterministic_wallet", params); }
    catch (MoneroRpcError e) { handleCreateWalletError(config.getPath(), e); }
    clear();
    path = config.getPath();
    return this;
  }
  
  private MoneroWalletRpc createWalletFromKeys(MoneroWalletConfig config) {
    config = config.copy();
    if (config.getSeedOffset() != null) throw new MoneroError("Cannot specify seed offset when creating wallet from keys");
    if (config.getRestoreHeight() == null) config.setRestoreHeight(0l);
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", config.getPath());
    params.put("password", config.getPassword());
    params.put("address", config.getPrimaryAddress());
    params.put("viewkey", config.getPrivateViewKey());
    params.put("spendkey", config.getPrivateSpendKey());
    params.put("restore_height", config.getRestoreHeight());
    params.put("autosave_current", config.getSaveCurrent());
    try { rpc.sendJsonRequest("generate_from_keys", params); }
    catch (MoneroRpcError e) { handleCreateWalletError(config.getPath(), e); }
    clear();
    path = config.getPath();
    return this;
  }
  
  private void handleCreateWalletError(String name, MoneroRpcError e) {
    if (e.getMessage().equals("Cannot create wallet. Already exists.")) throw new MoneroRpcError("Wallet already exists: " + name, e.getCode(), e.getRpcMethod(), e.getRpcParams());
    if (e.getMessage().equals("Electrum-style word list failed verification")) throw new MoneroRpcError("Invalid mnemonic", e.getCode(), e.getRpcMethod(), e.getRpcParams());
    throw e;
  }
  
  /**
   * Get a list of available languages for the wallet's seed.
   * 
   * @return the available languages for the wallet's seed.
   */
  @SuppressWarnings("unchecked")
  public List<String> getSeedLanguages() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_languages");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (List<String>) result.get("languages");
  }
  
  /**
   * Save and close the current wallet and stop the RPC server.
   */
  public void stop() {
    clear();
    rpc.sendJsonRequest("stop_wallet");
  }
  
  // -------------------------- COMMON WALLET METHODS -------------------------
  
  @Override
  public void addListener(MoneroWalletListenerI listener) {
    super.addListener(listener);
    refreshListening();
  }
  
  @Override
  public void removeListener(MoneroWalletListenerI listener) {
    super.removeListener(listener);
    refreshListening();
  }
  
  @Override
  public boolean isViewOnly() {
    try {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("key_type", "mnemonic");
      rpc.sendJsonRequest("query_key", params);
      return false; // key retrieval succeeds if not view-only
    } catch (MoneroError e) {
      if (Integer.valueOf(-29).equals(e.getCode())) return true;  // wallet is view-only
      if (Integer.valueOf(-1).equals(e.getCode())) return false;  // wallet is offline but not view-only
      throw e;
    }
  }
  
  @Override
  public void setDaemonConnection(MoneroRpcConnection connection) {
    setDaemonConnection(connection, null, null);
  }
  
  public void setDaemonConnection(MoneroRpcConnection connection, Boolean isTrusted, SslOptions sslOptions) {
    if (sslOptions == null) sslOptions = new SslOptions();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", connection == null ? "placeholder" : connection.getUri());
    params.put("username", connection == null ? "" : connection.getUsername());
    params.put("password", connection == null ? "" : connection.getPassword());
    params.put("trusted", isTrusted);
    params.put("ssl_support", "autodetect");
    params.put("ssl_private_key_path", sslOptions.getPrivateKeyPath());
    params.put("ssl_certificate_path", sslOptions.getCertificatePath());
    params.put("ssl_ca_file", sslOptions.getCertificateAuthorityFile());
    params.put("ssl_allowed_fingerprints", sslOptions.getAllowedFingerprints());
    params.put("ssl_allow_any_cert", sslOptions.getAllowAnyCert());
    rpc.sendJsonRequest("set_daemon", params);
    this.daemonConnection = connection == null || connection.getUri() == null || connection.getUri().isEmpty() ? null : new MoneroRpcConnection(connection);
  }
  
  @Override
  public void setProxyUri(String uri) {
    throw new MoneroError("MoneroWalletRpc.setProxyUri() not supported. Start monero-wallet-rpc with --proxy instead.");
  }
  
  @Override
  public MoneroRpcConnection getDaemonConnection() {
    return daemonConnection;
  }
  
  @Override
  public boolean isConnectedToDaemon() {
    try {
      checkReserveProof(getPrimaryAddress(), "", ""); // TODO (monero-project): provide better way to know if wallet rpc is connected to daemon
      throw new RuntimeException("check reserve expected to fail");
    } catch (MoneroError e) {
      return !e.getMessage().contains("Failed to connect to daemon");
    }
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public MoneroVersion getVersion() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_version");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return new MoneroVersion(((BigInteger) result.get("version")).intValue(), (Boolean) result.get("release"));
  }
  
  @Override
  public String getPath() {
    return path;
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getSeed() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_type", "mnemonic");
    Map<String, Object> resp = rpc.sendJsonRequest("query_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("key");
  }

  @Override
  public String getSeedLanguage() {
    throw new MoneroError("MoneroWalletRpc.getSeedLanguage() not supported");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getPrivateViewKey() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_type", "view_key");
    Map<String, Object> resp = rpc.sendJsonRequest("query_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("key");
  }
  

  @Override
  public String getPublicViewKey() {
    throw new MoneroError("MoneroWalletRpc.getPublicViewKey() not supported");
  }

  @Override
  public String getPublicSpendKey() {
    throw new MoneroError("MoneroWalletRpc.getPublicSpendKey() not supported");
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public String getPrivateSpendKey() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_type", "spend_key");
    Map<String, Object> resp = rpc.sendJsonRequest("query_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("key");
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    Map<Integer, String> subaddressMap = addressCache.get(accountIdx);
    if (subaddressMap == null) {
      getSubaddresses(accountIdx, null, true);      // cache's all addresses at this account
      return getAddress(accountIdx, subaddressIdx); // uses cache
    }
    String address = subaddressMap.get(subaddressIdx);
    if (address == null) {
      getSubaddresses(accountIdx, null, true);      // cache's all addresses at this account
      return addressCache.get(accountIdx).get(subaddressIdx);
    }
    return address;
  }

  // TODO: use cache
  @SuppressWarnings("unchecked")
  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    
    // fetch result and normalize error if address does not belong to the wallet
    Map<String, Object> result;
    try {
      Map<String, Object> params =  new HashMap<String, Object>();
      params.put("address", address);
      Map<String, Object> resp = rpc.sendJsonRequest("get_address_index", params);
      result = (Map<String, Object>) resp.get("result");
    } catch (MoneroRpcError e) {
      System.out.println(e.getMessage());
      if (Integer.valueOf(-2).equals(e.getCode())) throw new MoneroError(e.getMessage(), e.getCode());
      throw e;
    }
    
    // convert rpc response
    Map<String, BigInteger> rpcIndices = (Map<String, BigInteger>) result.get("index");
    MoneroSubaddress subaddress = new MoneroSubaddress(address);
    subaddress.setAccountIndex(rpcIndices.get("major").intValue());
    subaddress.setIndex(rpcIndices.get("minor").intValue());
    return subaddress;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String standardAddress, String paymentId) {
    try {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("standard_address", standardAddress);
      params.put("payment_id", paymentId);
      Map<String, Object> resp = rpc.sendJsonRequest("make_integrated_address", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      String integratedAddressStr = (String) result.get("integrated_address");
      return decodeIntegratedAddress(integratedAddressStr);
    } catch (MoneroRpcError e) {
      if (e.getMessage().contains("Invalid payment ID")) throw new MoneroError("Invalid payment ID: " + paymentId, ERROR_CODE_INVALID_PAYMENT_ID);
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("integrated_address", integratedAddress);
    Map<String, Object> resp = rpc.sendJsonRequest("split_integrated_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return new MoneroIntegratedAddress((String) result.get("standard_address"), (String) result.get("payment_id"), integratedAddress);
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public long getHeight() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_height");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return ((BigInteger) result.get("height")).longValue();
  }

  @Override
  public long getDaemonHeight() {
    throw new MoneroError("monero-wallet-rpc does not support getting the chain height");
  }
  
  @Override
  public long getHeightByDate(int year, int month, int day) {
    throw new MoneroError("monero-wallet-rpc does not support getting a height by date");
  }

  private Object SYNC_LOCK = new Object();
  @SuppressWarnings("unchecked")
  @Override
  public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener) {
    if (listener != null) throw new MoneroError("Monero Wallet RPC does not support reporting sync progress");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("start_height", startHeight);
    synchronized(SYNC_LOCK) {  // TODO (monero-project): monero-wallet-rpc hangs at 100% cpu utilization if refresh called concurrently
      try {
        Map<String, Object> resp = rpc.sendJsonRequest("refresh", params);
        poll();
        Map<String, Object> result = (Map<String, Object>) resp.get("result");
        return new MoneroSyncResult(((BigInteger) result.get("blocks_fetched")).longValue(), (Boolean) result.get("received_money"));
      } catch (MoneroError err) {
        if (err.getMessage().equals("no connection to daemon")) throw new MoneroError("Wallet is not connected to daemon");
        throw err;
      }
    }
  }
  
  @Override
  public void startSyncing(Long syncPeriodInMs) {
    
    // convert ms to seconds for rpc parameter
    long syncPeriodInSeconds = (syncPeriodInMs == null ? DEFAULT_SYNC_PERIOD_IN_MS : syncPeriodInMs) / 1000;
    
    // send rpc request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("enable", true);
    params.put("period", syncPeriodInSeconds);
    rpc.sendJsonRequest("auto_refresh", params);
    
    // update sync period for poller
    this.syncPeriodInMs = syncPeriodInSeconds * 1000;
    if (walletPoller != null) walletPoller.setPeriodInMs(this.syncPeriodInMs);
    
    // poll if listening
    poll();
  }
  
  @Override
  public void stopSyncing() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("enable", false);
    rpc.sendJsonRequest("auto_refresh", params);
  }
  
  @Override
  public void scanTxs(Collection<String> txHashes) {
    if (txHashes == null || txHashes.isEmpty()) throw new MoneroError("No tx hashes given to scan");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txHashes);
    rpc.sendJsonRequest("scan_tx", params);
    poll(); // notify of changes
  }
  
  @Override
  public void rescanSpent() {
    rpc.sendJsonRequest("rescan_spent");
  }
  
  @Override
  public void rescanBlockchain() {
    rpc.sendJsonRequest("rescan_blockchain");
  }

  @Override
  public BigInteger getBalance(Integer accountIdx, Integer subaddressIdx) {
    return getBalances(accountIdx, subaddressIdx)[0];
  }

  @Override
  public BigInteger getUnlockedBalance(Integer accountIdx, Integer subaddressIdx) {
    return getBalances(accountIdx, subaddressIdx)[1];
  }
  
  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    return getAccounts(includeSubaddresses, tag, false);
  }

  @SuppressWarnings("unchecked")
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag, boolean skipBalances) {
    
    // fetch accounts from rpc
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    Map<String, Object> resp = rpc.sendJsonRequest("get_accounts", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // build account objects and fetch subaddresses per account using get_address
    // TODO monero-wallet-rpc: get_address should support all_accounts so not called once per account
    List<MoneroAccount> accounts = new ArrayList<MoneroAccount>();
    for (Map<String, Object> rpcAccount : (List<Map<String, Object>>) result.get("subaddress_accounts")) {
      MoneroAccount account = convertRpcAccount(rpcAccount);
      if (includeSubaddresses) account.setSubaddresses(getSubaddresses(account.getIndex(), null, true));
      accounts.add(account);
    }
    
    // fetch and merge fields from get_balance across all accounts
    if (includeSubaddresses && !skipBalances) {
      
      // these fields are not initialized if subaddress is unused and therefore not returned from `get_balance`
      for (MoneroAccount account : accounts) {
        for (MoneroSubaddress subaddress : account.getSubaddresses()) {
          subaddress.setBalance(BigInteger.valueOf(0));
          subaddress.setUnlockedBalance(BigInteger.valueOf(0));
          subaddress.setNumUnspentOutputs(0l);
          subaddress.setNumBlocksToUnlock(0l);
        }
      }
      
      // fetch and merge info from get_balance
      params.clear();
      params.put("all_accounts", true);
      resp = rpc.sendJsonRequest("get_balance", params);
      result = (Map<String, Object>) resp.get("result");
      if (result.containsKey("per_subaddress")) {
        for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("per_subaddress")) {
          MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
          
          // merge info
          MoneroAccount account = accounts.get(subaddress.getAccountIndex());
          GenUtils.assertEquals("RPC accounts are out of order", account.getIndex(), subaddress.getAccountIndex());  // would need to switch lookup to loop
          MoneroSubaddress tgtSubaddress = account.getSubaddresses().get(subaddress.getIndex());
          GenUtils.assertEquals("RPC subaddresses are out of order", tgtSubaddress.getIndex(), subaddress.getIndex());
          if (subaddress.getBalance() != null) tgtSubaddress.setBalance(subaddress.getBalance());
          if (subaddress.getUnlockedBalance() != null) tgtSubaddress.setUnlockedBalance(subaddress.getUnlockedBalance());
          if (subaddress.getNumUnspentOutputs() != null) tgtSubaddress.setNumUnspentOutputs(subaddress.getNumUnspentOutputs());
          if (subaddress.getNumBlocksToUnlock() != null) tgtSubaddress.setNumBlocksToUnlock(subaddress.getNumBlocksToUnlock());
        }
      }
    }
    
    // return accounts
    return accounts;
  }

  // TODO: getAccountByIndex(), getAccountByTag()
  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    return getAccount(accountIdx, includeSubaddresses, false);
  }
  
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses, boolean skipBalances) {
    if (accountIdx < 0) throw new MoneroError("Account index must be greater than or equal to 0");
    for (MoneroAccount account : getAccounts()) {
      if (account.getIndex() == accountIdx) {
        if (includeSubaddresses) account.setSubaddresses(getSubaddresses(accountIdx, null, skipBalances));
        return account;
      }
    }
    throw new MoneroError("Account with index " + accountIdx + " does not exist");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroAccount createAccount(String label) {
    label = label == null || label.isEmpty() ? null : label;
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("label", label);
    Map<String, Object> resp = rpc.sendJsonRequest("create_account", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return new MoneroAccount(((BigInteger) result.get("account_index")).intValue(), (String) result.get("address"), BigInteger.valueOf(0), BigInteger.valueOf(0), null);
  }
  
  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    return getSubaddresses(accountIdx, subaddressIndices, false);
  }

  @SuppressWarnings("unchecked")
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices, boolean skipBalances) {
    
    // fetch subaddresses
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    if (subaddressIndices != null && !subaddressIndices.isEmpty()) params.put("address_index", subaddressIndices);
    Map<String, Object> resp = rpc.sendJsonRequest("get_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // initialize subaddresses
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("addresses")) {
      MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
      subaddress.setAccountIndex(accountIdx);
      subaddresses.add(subaddress);
    }
    
    // fetch and initialize subaddress balances
    if (!skipBalances) {
      
      // these fields are not initialized if subaddress is unused and therefore not returned from `get_balance`
      for (MoneroSubaddress subaddress : subaddresses) {
        subaddress.setBalance(BigInteger.valueOf(0));
        subaddress.setUnlockedBalance(BigInteger.valueOf(0));
        subaddress.setNumUnspentOutputs(0l);
        subaddress.setNumBlocksToUnlock(0l);
      }

      // fetch and initialize balances
      resp = rpc.sendJsonRequest("get_balance", params);
      result = (Map<String, Object>) resp.get("result");
      if (result.containsKey("per_subaddress")) {
        for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("per_subaddress")) {
          MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
          
          // transfer info to existing subaddress object
          for (MoneroSubaddress tgtSubaddress : subaddresses) {
            if (!tgtSubaddress.getIndex().equals(subaddress.getIndex())) continue; // skip to subaddress with same index
            if (subaddress.getBalance() != null) tgtSubaddress.setBalance(subaddress.getBalance());
            if (subaddress.getUnlockedBalance() != null) tgtSubaddress.setUnlockedBalance(subaddress.getUnlockedBalance());
            if (subaddress.getNumUnspentOutputs() != null) tgtSubaddress.setNumUnspentOutputs(subaddress.getNumUnspentOutputs());
            if (subaddress.getNumBlocksToUnlock() != null) tgtSubaddress.setNumBlocksToUnlock(subaddress.getNumBlocksToUnlock());
          }
        }
      }
    }
    
    // cache addresses
    Map<Integer, String> subaddressMap = addressCache.get(accountIdx);
    if (subaddressMap == null) {
      subaddressMap = new HashMap<Integer, String>();
      addressCache.put(accountIdx, subaddressMap);
    }
    for (MoneroSubaddress subaddress : subaddresses) {
      subaddressMap.put(subaddress.getIndex(), subaddress.getAddress());
    }
    
    // return results
    return subaddresses;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    params.put("label", label);
    Map<String, Object> resp = rpc.sendJsonRequest("create_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // build subaddress object
    MoneroSubaddress subaddress = new MoneroSubaddress();
    subaddress.setAccountIndex(accountIdx);
    subaddress.setIndex(((BigInteger) result.get("address_index")).intValue());
    subaddress.setAddress((String) result.get("address"));
    subaddress.setLabel(label);
    subaddress.setBalance(BigInteger.valueOf(0));
    subaddress.setUnlockedBalance(BigInteger.valueOf(0));
    subaddress.setNumUnspentOutputs(0l);
    subaddress.setIsUsed(false);
    subaddress.setNumBlocksToUnlock(0l);
    return subaddress;
  }

  @Override
  public void setSubaddressLabel(int accountIdx, int subaddressIdx, String label) {
    Map<String, Object> params = new HashMap<String, Object>();
    Map<String, Integer> idx = new HashMap<String, Integer>();
    idx.put("major", accountIdx);
    idx.put("minor", subaddressIdx);
    params.put("index", idx);
    params.put("label", label);
    rpc.sendJsonRequest("label_address", params);
  }

  @Override
  public List<MoneroTxWallet> getTxs(MoneroTxQuery query) {
    
    // copy and normalize query
    query = query == null ? new MoneroTxQuery() : query.copy();
    if (query.getInputQuery() != null) query.getInputQuery().setTxQuery(query);
    if (query.getOutputQuery() != null) query.getOutputQuery().setTxQuery(query);
    
    // temporarily disable transfer and output queries in order to collect all tx information
    MoneroTransferQuery transferQuery = query.getTransferQuery();
    MoneroOutputQuery inputQuery = query.getInputQuery();
    MoneroOutputQuery outputQuery = query.getOutputQuery();
    query.setTransferQuery(null);
    query.setInputQuery(null);
    query.setOutputQuery(null);
    
    // fetch all transfers that meet tx query
    List<MoneroTransfer> transfers = getTransfersAux(new MoneroTransferQuery().setTxQuery(decontextualize(query.copy())));
    
    // collect unique txs from transfers while retaining order
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    Set<MoneroTxWallet> txsSet = new HashSet<MoneroTxWallet>();
    for (MoneroTransfer transfer : transfers) {
      if (!txsSet.contains(transfer.getTx())) {
        txs.add(transfer.getTx());
        txsSet.add(transfer.getTx());
      }
    }
    
    // cache types into maps for merging and lookup
    Map<String, MoneroTxWallet> txMap = new HashMap<String, MoneroTxWallet>();
    Map<Long, MoneroBlock> blockMap = new HashMap<Long, MoneroBlock>();
    for (MoneroTxWallet tx : txs) {
      mergeTx(tx, txMap, blockMap);
    }
    
    // fetch and merge outputs if queried
    if (Boolean.TRUE.equals(query.getIncludeOutputs()) || outputQuery != null) {
        
      // fetch outputs
      MoneroOutputQuery outputQueryAux = (outputQuery != null ? outputQuery.copy() : new MoneroOutputQuery()).setTxQuery(decontextualize(query.copy()));
      List<MoneroOutputWallet> outputs = getOutputsAux(outputQueryAux);
      
      // merge output txs one time while retaining order
      Set<MoneroTxWallet> outputTxs = new HashSet<MoneroTxWallet>();
      for (MoneroOutputWallet output : outputs) {
        if (!outputTxs.contains(output.getTx())) {
          mergeTx(output.getTx(), txMap, blockMap);
          outputTxs.add(output.getTx());
        }
      }
    }
    
    // restore transfer and output queries
    query.setTransferQuery(transferQuery);
    query.setInputQuery(inputQuery);
    query.setOutputQuery(outputQuery);
    
    // filter txs that don't meet transfer and output queries
    List<MoneroTxWallet> txsQueried = new ArrayList<MoneroTxWallet>();
    for (MoneroTxWallet tx : txs) {
      if (query.meetsCriteria(tx)) txsQueried.add(tx);
      else if (tx.getBlock() != null) tx.getBlock().getTxs().remove(tx);
    }
    txs = txsQueried;
    
    // special case: re-fetch txs if inconsistency caused by needing to make multiple rpc calls
    for (MoneroTxWallet tx : txs) {
      if (tx.isConfirmed() && tx.getBlock() == null || !tx.isConfirmed() && tx.getBlock() != null) {
        LOGGER.warning("Inconsistency detected building txs from multiple rpc calls, re-fetching");
        return getTxs(query);
      }
    }
    
    // order txs if tx hashes given
    if (query.getHashes() != null && !query.getHashes().isEmpty()) {
      Map<String, MoneroTxWallet> txsById = new HashMap<String, MoneroTxWallet>();  // store txs in temporary map for sorting
      for (MoneroTxWallet tx : txs) txsById.put(tx.getHash(), tx);
      List<MoneroTxWallet> orderedTxs = new ArrayList<MoneroTxWallet>();
      for (String txHash : query.getHashes()) if (txsById.get(txHash) != null) orderedTxs.add(txsById.get(txHash));
      txs = orderedTxs;
    }
    return txs;
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
    
    // copy and normalize query up to block
    query = normalizeTransferQuery(query);
    
    // get transfers directly if query does not require tx context (other transfers, outputs)
    if (!isContextual(query)) return getTransfersAux(query);
    
    // otherwise get txs with full models to fulfill query
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    query.getTxQuery().setTransferQuery(query);
    for (MoneroTxWallet tx : getTxs(query.getTxQuery())) transfers.addAll(tx.filterTransfers(query));
    return transfers;
  }
  
  @Override
  public List<MoneroOutputWallet> getOutputs(MoneroOutputQuery query) {
    
    // get outputs directly if query does not require tx context (other outputs, transfers)
    if (!isContextual(query)) return getOutputsAux(query);
    
    // otherwise get txs with full models to fulfill query
    List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
    for (MoneroTxWallet tx : getTxs(query.getTxQuery())) outputs.addAll(tx.filterOutputsWallet(query));
    return outputs;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public String exportOutputs(boolean all) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("all", all);
    Map<String, Object> resp = rpc.sendJsonRequest("export_outputs", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("outputs_data_hex");
  }

  @SuppressWarnings("unchecked")
  @Override
  public int importOutputs(String outputsHex) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("outputs_data_hex", outputsHex);
    Map<String, Object> resp = rpc.sendJsonRequest("import_outputs", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return ((BigInteger) result.get("num_imported")).intValue();
  }

  @Override
  public List<MoneroKeyImage> exportKeyImages(boolean all) {
    return rpcExportKeyImages(all);
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    
    // convert key images to rpc parameter
    List<Map<String, Object>> rpcKeyImages = new ArrayList<Map<String, Object>>();
    for (MoneroKeyImage keyImage : keyImages) {
      Map<String, Object> rpcKeyImage = new HashMap<String, Object>();
      rpcKeyImage.put("key_image", keyImage.getHex());
      rpcKeyImage.put("signature", keyImage.getSignature());
      rpcKeyImages.add(rpcKeyImage);
    }
    
    // send rpc request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("signed_key_images", rpcKeyImages);
    Map<String, Object> resp = rpc.sendJsonRequest("import_key_images", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // build and return result
    MoneroKeyImageImportResult importResult = new MoneroKeyImageImportResult();
    importResult.setHeight(((BigInteger) result.get("height")).longValue());
    importResult.setSpentAmount((BigInteger) result.get("spent"));
    importResult.setUnspentAmount((BigInteger) result.get("unspent"));
    return importResult;
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    return rpcExportKeyImages(false);
  }
  
  @Override
  public void freezeOutput(String keyImage) {
    if (keyImage == null) throw new MoneroError("Must specify key image to freeze");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_image", keyImage);
    rpc.sendJsonRequest("freeze", params);
  }
  
  @Override
  public void thawOutput(String keyImage) {
    if (keyImage == null) throw new MoneroError("Must specify key image to thaw");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_image", keyImage);
    rpc.sendJsonRequest("thaw", params);
  }
  
  @Override
  public boolean isOutputFrozen(String keyImage) {
    if (keyImage == null) throw new MoneroError("Must specify key image to check if frozen");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_image", keyImage);
    Map<String, Object> resp = rpc.sendJsonRequest("frozen", params);
    @SuppressWarnings("unchecked")
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return Boolean.TRUE.equals(result.get("frozen"));
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public List<MoneroTxWallet> createTxs(MoneroTxConfig config) {
    
    // validate, copy, and normalize request
    if (config == null) throw new MoneroError("Send request cannot be null");
    GenUtils.assertNotNull(config.getDestinations());
    GenUtils.assertNull(config.getSweepEachSubaddress());
    GenUtils.assertNull(config.getBelowAmount());
    if (config.getCanSplit() == null) {
      config = config.copy();
      config.setCanSplit(true);
    }
    if (Boolean.TRUE.equals(config.getRelay()) && isMultisig()) throw new MoneroError("Cannot relay multisig transaction until co-signed");
    
    // determine account and subaddresses to send from
    Integer accountIdx = config.getAccountIndex();
    if (accountIdx == null) throw new MoneroError("Must specify the account index to send from");
    List<Integer> subaddressIndices = config.getSubaddressIndices() == null ? null : new ArrayList<Integer>(config.getSubaddressIndices()); // fetch all or copy given indices
    
    // build request parameters
    Map<String, Object> params = new HashMap<String, Object>();
    List<Map<String, Object>> destinationMaps = new ArrayList<Map<String, Object>>();
    params.put("destinations", destinationMaps);
    for (MoneroDestination destination : config.getDestinations()) {
      GenUtils.assertNotNull("Destination address is not defined", destination.getAddress());
      GenUtils.assertNotNull("Destination amount is not defined", destination.getAmount());
      Map<String, Object> destinationMap = new HashMap<String, Object>();
      destinationMap.put("address", destination.getAddress());
      destinationMap.put("amount", destination.getAmount().toString());
      destinationMaps.add(destinationMap);
    }
    if (config.getSubtractFeeFrom() != null) params.put("subtract_fee_from_outputs", config.getSubtractFeeFrom());
    params.put("account_index", accountIdx);
    params.put("subaddr_indices", subaddressIndices);
    params.put("payment_id", config.getPaymentId());
    params.put("unlock_time", config.getUnlockTime());
    params.put("do_not_relay", !Boolean.TRUE.equals(config.getRelay()));
    params.put("priority", config.getPriority() == null ? null : config.getPriority().ordinal());
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    if (config.getCanSplit()) params.put("get_tx_keys", true); // param to get tx key(s) depends if split
    else params.put("get_tx_key", true);
    
    // cannot apply subtractFeeFrom with `transfer_split` call
    if (config.getCanSplit() && config.getSubtractFeeFrom() != null && config.getSubtractFeeFrom().size() > 0) {
      throw new MoneroError("subtractfeefrom transfers cannot be split over multiple transactions yet");
    }
    
    // send request
    Map<String, Object> result = null;
    try {
      Map<String, Object> resp = rpc.sendJsonRequest(config.getCanSplit() ? "transfer_split" : "transfer", params);
      result = (Map<String, Object>) resp.get("result");
    } catch (MoneroRpcError err) {
      if (err.getMessage().indexOf("WALLET_RPC_ERROR_CODE_WRONG_ADDRESS") > -1) throw new MoneroError("Invalid destination address");
      throw err;
    }
    
    // pre-initialize txs iff present. multisig and view-only wallets will have tx set without transactions
    List<MoneroTxWallet> txs = null;
    int numTxs = config.getCanSplit() ? (result.containsKey("fee_list") ? ((List<String>) result.get("fee_list")).size() : 0) : (result.containsKey("fee") ? 1 : 0);
    if (numTxs > 0) txs = new ArrayList<MoneroTxWallet>();
    boolean copyDestinations = numTxs == 1;
    for (int i = 0; i < numTxs; i++) {
      MoneroTxWallet tx = new MoneroTxWallet();
      initSentTxWallet(config, tx, copyDestinations);
      tx.getOutgoingTransfer().setAccountIndex(accountIdx);
      if (subaddressIndices != null && subaddressIndices.size() == 1) tx.getOutgoingTransfer().setSubaddressIndices(subaddressIndices);
      txs.add(tx);
    }
    
    // notify of changes
    if (Boolean.TRUE.equals(config.getRelay())) poll();
    
    // initialize tx set from rpc response with pre-initialized txs
    if (config.getCanSplit()) return convertRpcSentTxsToTxSet(result, txs, config).getTxs();
    else return convertRpcTxToTxSet(result, txs == null ? null : txs.get(0), true, config).getTxs();
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroTxWallet sweepOutput(MoneroTxConfig config) {
    
    // validate request
    GenUtils.assertNull(config.getSweepEachSubaddress());
    GenUtils.assertNull(config.getBelowAmount());
    GenUtils.assertNull("Splitting is not applicable when sweeping output", config.getCanSplit());
    if (config.getDestinations() == null || config.getDestinations().size() != 1 || config.getDestinations().get(0).getAddress() == null || config.getDestinations().get(0).getAddress().isEmpty()) throw new MoneroError("Must provide exactly one destination address to sweep output to");
    if (config.getSubtractFeeFrom() != null && config.getSubtractFeeFrom().size() > 0) throw new MoneroError("Sweep transactions do not support subtracting fees from destinations");
    
    // build request parameters
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", config.getDestinations().get(0).getAddress());
    params.put("account_index", config.getAccountIndex());
    params.put("subaddr_indices", config.getSubaddressIndices());
    params.put("key_image", config.getKeyImage());
    params.put("unlock_time", config.getUnlockTime());
    params.put("do_not_relay", !Boolean.TRUE.equals(config.getRelay()));
    params.put("priority", config.getPriority() == null ? null : config.getPriority().ordinal());
    params.put("payment_id", config.getPaymentId());
    params.put("get_tx_key", true);
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    
    // send request
    Map<String, Object> resp = rpc.sendJsonRequest("sweep_single", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // notify of changes
    if (Boolean.TRUE.equals(config.getRelay())) poll();

    // build and return tx
    MoneroTxWallet tx = initSentTxWallet(config, null, true);
    convertRpcTxToTxSet(result, tx, true, config);
    tx.getOutgoingTransfer().getDestinations().get(0).setAmount(tx.getOutgoingTransfer().getAmount()); // initialize destination amount
    return tx;
  }
  
  @Override
  public List<MoneroTxWallet> sweepUnlocked(MoneroTxConfig config) {
    
    // validate request
    if (config == null) throw new MoneroError("Sweep request cannot be null");
    if (config.getDestinations() == null || config.getDestinations().size() != 1) throw new MoneroError("Must specify exactly one destination to sweep to");
    if (config.getDestinations().get(0).getAddress() == null) throw new MoneroError("Must specify destination address to sweep to");
    if (config.getDestinations().get(0).getAmount() != null) throw new MoneroError("Cannot specify amount in sweep request");
    if (config.getKeyImage() != null) throw new MoneroError("Key image defined; use sweepOutput() to sweep an output by its key image");
    if (config.getSubaddressIndices() != null && config.getSubaddressIndices().isEmpty()) config.setSubaddressIndices((List<Integer>) null);
    if (config.getAccountIndex() == null && config.getSubaddressIndices() != null) throw new MoneroError("Must specify account index if subaddress indices are specified");
    if (config.getSubtractFeeFrom() != null && config.getSubtractFeeFrom().size() > 0) throw new MoneroError("Sweep transactions do not support subtracting fees from destinations");
    
    // determine account and subaddress indices to sweep; default to all with unlocked balance if not specified
    LinkedHashMap<Integer, List<Integer>> indices = new LinkedHashMap<Integer, List<Integer>>();  // java type preserves insertion order
    if (config.getAccountIndex() != null) {
      if (config.getSubaddressIndices() != null) {
        indices.put(config.getAccountIndex(), config.getSubaddressIndices());
      } else {
        List<Integer> subaddressIndices = new ArrayList<Integer>();
        indices.put(config.getAccountIndex(), subaddressIndices);
        for (MoneroSubaddress subaddress : getSubaddresses(config.getAccountIndex())) { // TODO: wallet rpc sweep_all now supports req.subaddr_indices_all
          if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) subaddressIndices.add(subaddress.getIndex());
        }
      }
    } else {
      List<MoneroAccount> accounts = getAccounts(true);
      for (MoneroAccount account : accounts) {
        if (account.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) {
          List<Integer> subaddressIndices = new ArrayList<Integer>();
          indices.put(account.getIndex(), subaddressIndices);
          for (MoneroSubaddress subaddress : account.getSubaddresses()) {
            if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) subaddressIndices.add(subaddress.getIndex());
          }
        }
      }
    }
    
    // sweep from each account and collect resulting tx sets
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    for (Integer accountIdx : indices.keySet()) {
      
      // copy and modify the original request
      MoneroTxConfig copy = config.copy();
      copy.setAccountIndex(accountIdx);
      copy.setSweepEachSubaddress(false);
      
      // sweep all subaddresses together // TODO monero-project: can this reveal outputs belong to same wallet?
      if (!Boolean.TRUE.equals(copy.getSweepEachSubaddress())) {
        copy.setSubaddressIndices(indices.get(accountIdx));
        txs.addAll(rpcSweepAccount(copy));
      }
      
      // otherwise sweep each subaddress individually
      else {
        for (int subaddressIdx : indices.get(accountIdx)) {
          copy.setSubaddressIndices(subaddressIdx);
          txs.addAll(rpcSweepAccount(copy));
        }
      }
    }
    
    // notify of changes
    if (Boolean.TRUE.equals(config.getRelay())) poll();
    return txs;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTxWallet> sweepDust(boolean relay) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("do_not_relay", !relay);
    Map<String, Object> resp = rpc.sendJsonRequest("sweep_dust", params);
    if (relay) poll();
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    MoneroTxSet txSet = convertRpcSentTxsToTxSet(result, null, null);
    if (txSet.getTxs() == null) return new ArrayList<MoneroTxWallet>();
    for (MoneroTxWallet tx : txSet.getTxs()) {
      tx.setIsRelayed(relay);
      tx.setInTxPool(relay);
    }
    return txSet.getTxs();
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public List<String> relayTxs(Collection<String> txMetadatas) {
    if (txMetadatas == null || txMetadatas.isEmpty()) throw new MoneroError("Must provide an array of tx metadata to relay");
    List<String> txHashes = new ArrayList<String>();
    for (String txMetadata : txMetadatas) {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("hex", txMetadata);
      Map<String, Object> resp = rpc.sendJsonRequest("relay_tx", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      txHashes.add((String) result.get("tx_hash"));
    }
    poll(); // notify of changes
    return txHashes;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public MoneroTxSet describeTxSet(MoneroTxSet txSet) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("unsigned_txset", txSet.getUnsignedTxHex());
    params.put("multisig_txset", txSet.getMultisigTxHex());
    Map<String, Object> resp = rpc.sendJsonRequest("describe_transfer", params);
    return convertRpcDescribeTransfer((Map<String, Object>) resp.get("result"));
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public MoneroTxSet signTxs(String unsignedTxHex) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("unsigned_txset", unsignedTxHex);
    Map<String, Object> resp = rpc.sendJsonRequest("sign_transfer", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return convertRpcSentTxsToTxSet(result, null, null);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> submitTxs(String signedTxHex) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tx_data_hex", signedTxHex);
    Map<String, Object> resp = rpc.sendJsonRequest("submit_transfer", params);
    poll();
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (List<String>) result.get("tx_hash_list");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String signMessage(String msg, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("data", msg);
    params.put("signature_type", signatureType == MoneroMessageSignatureType.SIGN_WITH_SPEND_KEY ? "spend" : "view");
    params.put("account_index", accountIdx);
    params.put("address_index", subaddressIdx);
    Map<String, Object> resp = rpc.sendJsonRequest("sign", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroMessageSignatureResult verifyMessage(String msg, String address, String signature) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("data", msg);
    params.put("address", address);
    params.put("signature", signature);
    try {
      Map<String, Object> resp = rpc.sendJsonRequest("verify", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      boolean isGood = (boolean) result.get("good");
      return new MoneroMessageSignatureResult(
          isGood,
          !isGood ? null : (Boolean) result.get("old"),
          !isGood || !result.containsKey("signature_type") ? null : "view".equals(result.get("signature_type")) ? MoneroMessageSignatureType.SIGN_WITH_VIEW_KEY : MoneroMessageSignatureType.SIGN_WITH_SPEND_KEY,
          !isGood ? null : ((BigInteger) result.get("version")).intValue());
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-2).equals(e.getCode())) return new MoneroMessageSignatureResult(false, null, null, null);
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getTxKey(String txHash) {
    try {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("txid", txHash);
      Map<String, Object> resp = rpc.sendJsonRequest("get_tx_key", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      return (String) result.get("tx_key");
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-8).equals(e.getCode()) && e.getMessage().indexOf("TX ID has invalid format") != -1) e = new MoneroRpcError("TX hash has invalid format", e.getCode(), e.getRpcMethod(), e.getRpcParams());  // normalize error message
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCheckTx checkTxKey(String txHash, String txKey, String address) {
    try {
      
      // send request
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("txid", txHash);
      params.put("tx_key", txKey);
      params.put("address", address);
      Map<String, Object> resp = rpc.sendJsonRequest("check_tx_key", params);
      
      // interpret result
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      MoneroCheckTx check = new MoneroCheckTx();
      check.setIsGood(true);
      check.setNumConfirmations(((BigInteger) result.get("confirmations")).longValue());
      check.setInTxPool((Boolean) result.get("in_pool"));
      check.setReceivedAmount((BigInteger) result.get("received"));
      return check;
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-8).equals(e.getCode()) && e.getMessage().indexOf("TX ID has invalid format") != -1) e = new MoneroRpcError("TX hash has invalid format", e.getCode(), e.getRpcMethod(), e.getRpcParams());  // normalize error message
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getTxProof(String txHash, String address, String message) {
    try {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("txid", txHash);
      params.put("address", address);
      params.put("message", message);
      Map<String, Object> resp = rpc.sendJsonRequest("get_tx_proof", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      return (String) result.get("signature");
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-8).equals(e.getCode()) && e.getMessage().indexOf("TX ID has invalid format") != -1) e = new MoneroRpcError("TX hash has invalid format", e.getCode(), e.getRpcMethod(), e.getRpcParams());  // normalize error message
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCheckTx checkTxProof(String txHash, String address, String message, String signature) {
    
    try {
      
      // send request
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("txid", txHash);
      params.put("address", address);
      params.put("message", message);
      params.put("signature", signature);
      Map<String, Object> resp = rpc.sendJsonRequest("check_tx_proof", params);
      
      // interpret response
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      boolean isGood = (boolean) result.get("good");
      MoneroCheckTx check = new MoneroCheckTx();
      check.setIsGood(isGood);
      if (isGood) {
        check.setNumConfirmations(((BigInteger) result.get("confirmations")).longValue());
        check.setInTxPool((boolean) result.get("in_pool"));
        check.setReceivedAmount((BigInteger) result.get("received"));
      }
      return check;
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-1).equals(e.getCode()) && e.getMessage().equals("basic_string")) e = new MoneroRpcError("Must provide signature to check tx proof", -1, null, null);
      if (Integer.valueOf(-8).equals(e.getCode()) && e.getMessage().indexOf("TX ID has invalid format") != -1) e = new MoneroRpcError("TX hash has invalid format", e.getCode(), e.getRpcMethod(), e.getRpcParams());
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getSpendProof(String txHash, String message) {
    try {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("txid", txHash);
      params.put("message", message);
      Map<String, Object> resp = rpc.sendJsonRequest("get_spend_proof", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      return (String) result.get("signature");
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-8).equals(e.getCode()) && e.getMessage().indexOf("TX ID has invalid format") != -1) e = new MoneroRpcError("TX hash has invalid format", e.getCode(), e.getRpcMethod(), e.getRpcParams());  // normalize error message
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean checkSpendProof(String txHash, String message, String signature) {
    try {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("txid", txHash);
      params.put("message", message);
      params.put("signature", signature);
      Map<String, Object> resp = rpc.sendJsonRequest("check_spend_proof", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      return (boolean) result.get("good");
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-8).equals(e.getCode()) && e.getMessage().indexOf("TX ID has invalid format") != -1) e = new MoneroRpcError("TX hash has invalid format", e.getCode(), e.getRpcMethod(), e.getRpcParams());  // normalize error message
      throw e;
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getReserveProofWallet(String message) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("all", true);
    params.put("message", message);
    Map<String, Object> resp = rpc.sendJsonRequest("get_reserve_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    params.put("amount", amount.toString());
    params.put("message", message);
    Map<String, Object> resp = rpc.sendJsonRequest("get_reserve_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", address);
    params.put("message", message);
    params.put("signature", signature);
    Map<String, Object> resp = rpc.sendJsonRequest("check_reserve_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // interpret results
    boolean isGood = (boolean) result.get("good");
    MoneroCheckReserve check = new MoneroCheckReserve();
    check.setIsGood(isGood);
    if (isGood) {
      check.setTotalAmount((BigInteger) result.get("total"));
      check.setUnconfirmedSpentAmount((BigInteger) result.get("spent"));
    }
    return check;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public List<String> getTxNotes(List<String> txHashes) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txHashes);
    Map<String, Object> resp = rpc.sendJsonRequest("get_tx_notes", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (List<String>) result.get("notes");
  }

  @Override
  public void setTxNotes(List<String> txHashes, List<String> notes) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txHashes);
    params.put("notes", notes);
    rpc.sendJsonRequest("set_tx_notes", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("entries", entryIndices);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_address_book", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<MoneroAddressBookEntry> entries = new ArrayList<MoneroAddressBookEntry>();
    if (!resultMap.containsKey("entries")) return entries;
    for (Map<String, Object> entryMap : (List<Map<String, Object>>) resultMap.get("entries")) {
      MoneroAddressBookEntry entry = new MoneroAddressBookEntry(
              ((BigInteger) entryMap.get("index")).intValue(),
              (String) entryMap.get("address"),
              (String) entryMap.get("description"),
              (String) entryMap.get("payment_id")
      );
      entries.add(entry);
    }
    return entries;
  }

  @SuppressWarnings("unchecked")
  @Override
  public int addAddressBookEntry(String address, String description) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", address);
    params.put("description", description);
    Map<String, Object> respMap = rpc.sendJsonRequest("add_address_book", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return ((BigInteger) resultMap.get("index")).intValue();
  }
  
  @Override
  public void editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("index", index);
    params.put("set_address", setAddress);
    params.put("address", address);
    params.put("set_description", setDescription);
    params.put("description", description);
    rpc.sendJsonRequest("edit_address_book", params);
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("index", entryIdx);
    rpc.sendJsonRequest("delete_address_book", params);
  }
  
  @Override
  public void tagAccounts(String tag, Collection<Integer> accountIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    params.put("accounts", accountIndices);
    rpc.sendJsonRequest("tag_accounts", params);
  }

  @Override
  public void untagAccounts(Collection<Integer> accountIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("accounts", accountIndices);
    rpc.sendJsonRequest("untag_accounts", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAccountTag> getAccountTags() {
    List<MoneroAccountTag> tags = new ArrayList<MoneroAccountTag>();
    Map<String, Object> respMap = rpc.sendJsonRequest("get_account_tags");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> accountTagMaps = (List<Map<String, Object>>) resultMap.get("account_tags");
    if (accountTagMaps != null) {
      for (Map<String, Object> accountTagMap : accountTagMaps) {
        MoneroAccountTag tag = new MoneroAccountTag();
        tags.add(tag);
        tag.setTag((String) accountTagMap.get("tag"));
        tag.setLabel((String) accountTagMap.get("label"));
        List<BigInteger> accountIndicesBI = (List<BigInteger>) accountTagMap.get("accounts");
        List<Integer> accountIndices = new ArrayList<Integer>();
        for (BigInteger idx : accountIndicesBI) accountIndices.add(idx.intValue());
        tag.setAccountIndices(accountIndices);
      }
    }
    return tags;
  }

  @Override
  public void setAccountTagLabel(String tag, String label) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    params.put("description", label);
    rpc.sendJsonRequest("set_account_tag_description", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getPaymentUri(MoneroTxConfig config) {
    GenUtils.assertNotNull("Must provide send request to create a payment URI", config);
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", config.getDestinations().get(0).getAddress());
    params.put("amount", config.getDestinations().get(0).getAmount() != null ? config.getDestinations().get(0).getAmount().toString() : null);
    params.put("payment_id", config.getPaymentId());
    params.put("recipient_name", config.getRecipientName());
    params.put("tx_description", config.getNote());
    Map<String, Object> resp = rpc.sendJsonRequest("make_uri", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("uri");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroTxConfig parsePaymentUri(String uri) {
    GenUtils.assertNotNull("Must provide URI to parse", uri);
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("uri", uri);
    Map<String, Object> resp = rpc.sendJsonRequest("parse_uri", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    Map<String, Object> rpcUri = (Map<String, Object>) result.get("uri");
    MoneroTxConfig config = new MoneroTxConfig().setAddress((String) rpcUri.get("address")).setAmount((BigInteger) rpcUri.get("amount"));
    config.setPaymentId((String) rpcUri.get("payment_id"));
    config.setRecipientName((String) rpcUri.get("recipient_name"));
    config.setNote((String) rpcUri.get("tx_description"));
    if ("".equals(config.getDestinations().get(0).getAddress())) config.getDestinations().get(0).setAddress(null);
    if ("".equals(config.getPaymentId())) config.setPaymentId(null);
    if ("".equals(config.getRecipientName())) config.setRecipientName(null);
    if ("".equals(config.getNote())) config.setNote(null);
    return config;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public String getAttribute(String key) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key", key);
    try {
      Map<String, Object> resp = rpc.sendJsonRequest("get_attribute", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      String value = (String) result.get("value");
      return value.isEmpty() ? null : value;
    } catch (MoneroRpcError e) {
      if (Integer.valueOf(-45).equals(e.getCode())) return null;  // -45: attribute not found
      throw e;
    }
  }

  @Override
  public void setAttribute(String key, String val) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key", key);
    params.put("value", val);
    rpc.sendJsonRequest("set_attribute", params);
  }

  @Override
  public void startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("threads_count", numThreads);
    params.put("backgroundMining", backgroundMining);
    params.put("ignoreBattery", ignoreBattery);
    rpc.sendJsonRequest("start_mining", params);
  }

  @Override
  public void stopMining() {
    rpc.sendJsonRequest("stop_mining");
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public boolean isMultisigImportNeeded() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_balance");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return Boolean.TRUE.equals(result.get("multisig_import_needed"));
  }

  @Override
  @SuppressWarnings("unchecked")
  public MoneroMultisigInfo getMultisigInfo() {
    Map<String, Object> resp = rpc.sendJsonRequest("is_multisig");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    MoneroMultisigInfo info = new MoneroMultisigInfo();
    info.setIsMultisig((boolean) result.get("multisig"));
    info.setIsReady((boolean) result.get("ready"));
    info.setThreshold(((BigInteger) result.get("threshold")).intValue());
    info.setNumParticipants(((BigInteger) result.get("total")).intValue());
    return info;
  }

  @Override
  @SuppressWarnings("unchecked")
  public String prepareMultisig() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("enable_multisig_experimental", true);
    Map<String, Object> resp = rpc.sendJsonRequest("prepare_multisig", params);
    addressCache.clear();
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("multisig_info");
  }

  @Override
  @SuppressWarnings("unchecked")
  public String makeMultisig(List<String> multisigHexes, int threshold, String password) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("multisig_info", multisigHexes);
    params.put("threshold", threshold);
    params.put("password", password);
    Map<String, Object> resp = rpc.sendJsonRequest("make_multisig", params);
    addressCache.clear();
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("multisig_info");
  }

  @Override
  @SuppressWarnings("unchecked")
  public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("multisig_info", multisigHexes);
    params.put("password", password);
    Map<String, Object> resp = rpc.sendJsonRequest("exchange_multisig_keys", params);
    addressCache.clear();
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    MoneroMultisigInitResult msResult = new MoneroMultisigInitResult();
    msResult.setAddress((String) result.get("address"));
    msResult.setMultisigHex((String) result.get("multisig_info"));
    if (msResult.getAddress().isEmpty()) msResult.setAddress(null);
    if (msResult.getMultisigHex().isEmpty()) msResult.setMultisigHex(null);
    return msResult;
  }

  @Override
  @SuppressWarnings("unchecked")
  public String exportMultisigHex() {
    Map<String, Object> resp = rpc.sendJsonRequest("export_multisig_info");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("info");
  }

  @Override
  @SuppressWarnings("unchecked")
  public int importMultisigHex(List<String> multisigHexes) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("info", multisigHexes);
    Map<String, Object> resp = rpc.sendJsonRequest("import_multisig_info", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return ((BigInteger) result.get("n_outputs")).intValue();
  }

  @Override
  @SuppressWarnings("unchecked")
  public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tx_data_hex", multisigTxHex);
    Map<String, Object> resp = rpc.sendJsonRequest("sign_multisig", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    MoneroMultisigSignResult signResult = new MoneroMultisigSignResult();
    signResult.setSignedMultisigTxHex((String) result.get("tx_data_hex"));
    signResult.setTxHashes((List<String>) result.get("tx_hash_list"));
    return signResult;
  }

  @Override
  @SuppressWarnings("unchecked")
  public List<String> submitMultisigTxHex(String signedMultisigTxHex) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tx_data_hex", signedMultisigTxHex);
    Map<String, Object> resp = rpc.sendJsonRequest("submit_multisig", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (List<String>) result.get("tx_hash_list");
  }
  
  @Override
  public void changePassword(String oldPassword, String newPassword) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("old_password", oldPassword);
    params.put("new_password", newPassword);
    rpc.sendJsonRequest("change_wallet_password", params);
  }
  
  @Override
  public void save() {
    rpc.sendJsonRequest("store");
  }
  
  @Override
  public void close(boolean save) {
    super.close(save);
    clear();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("autosave_current", save);
    rpc.sendJsonRequest("close_wallet", params);
  }
  
  @Override
  public boolean isClosed() {
    try {
      this.getPrimaryAddress();
    } catch (Exception e) {
      return e instanceof MoneroRpcError && Integer.valueOf(-8).equals(((MoneroRpcError) e).getCode()) && ((MoneroRpcError) e).getMessage().indexOf("No wallet file") > -1;
    }
    return false;
  }
  
  // ------------------------------ PRIVATE -----------------------------------
  
  private void clear() {
    listeners.clear();
    refreshListening();
    addressCache.clear();
    path = null;
  }
  
  private Map<Integer, List<Integer>> getAccountIndices(boolean getSubaddressIndices) {
    Map<Integer, List<Integer>> indices = new HashMap<Integer, List<Integer>>();
    for (MoneroAccount account : getAccounts()) {
      indices.put(account.getIndex(), getSubaddressIndices ? getSubaddressIndices(account.getIndex()) : null);
    }
    return indices;
  }
  
  @SuppressWarnings("unchecked")
  private List<Integer> getSubaddressIndices(int accountIdx) {
    List<Integer> subaddressIndices = new ArrayList<Integer>();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    Map<String, Object> resp = rpc.sendJsonRequest("get_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    for (Map<String, Object> address : (List<Map<String, Object>>) result.get("addresses")) {
      subaddressIndices.add(((BigInteger) address.get("address_index")).intValue());
    }
    return subaddressIndices;
  }
  
  /**
   * Common method to get key images.
   * 
   * @param all specifies to get all xor only new images from last import
   * @return {MoneroKeyImage[]} are the key images
   */
  @SuppressWarnings("unchecked")
  private List<MoneroKeyImage> rpcExportKeyImages(boolean all) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("all", all);
    Map<String, Object> resp = rpc.sendJsonRequest("export_key_images", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    List<MoneroKeyImage> images = new ArrayList<MoneroKeyImage>();
    if (!result.containsKey("signed_key_images")) return images;
    for (Map<String, Object> rpcImage : (List<Map<String, Object>>) result.get("signed_key_images")) {
      images.add(new MoneroKeyImage((String) rpcImage.get("key_image"), (String) rpcImage.get("signature")));
    }
    return images;
  }
  
  @SuppressWarnings("unchecked")
  private BigInteger[] getBalances(Integer accountIdx, Integer subaddressIdx) {
    if (accountIdx == null) {
      GenUtils.assertNull("Must provide account index with subaddress index", subaddressIdx);
      BigInteger balance = BigInteger.valueOf(0);
      BigInteger unlockedBalance = BigInteger.valueOf(0);
      for (MoneroAccount account : getAccounts()) {
        balance = balance.add(account.getBalance());
        unlockedBalance = unlockedBalance.add(account.getUnlockedBalance());
      }
      return new BigInteger[] { balance, unlockedBalance };
    } else {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("account_index", accountIdx);
      params.put("address_indices", subaddressIdx == null ? null : new Integer[] { subaddressIdx });
      Map<String, Object> resp = rpc.sendJsonRequest("get_balance", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      if (subaddressIdx == null) return new BigInteger[] { (BigInteger) result.get("balance"), (BigInteger) result.get("unlocked_balance") };
      else {
        List<Map<String, Object>> rpcBalancesPerSubaddress = (List<Map<String, Object>>) result.get("per_subaddress");
        return new BigInteger[] { (BigInteger) rpcBalancesPerSubaddress.get(0).get("balance"), (BigInteger) rpcBalancesPerSubaddress.get(0).get("unlocked_balance") };
      }
    }
  }
  
  @SuppressWarnings("unchecked")
  private List<MoneroTransfer> getTransfersAux(MoneroTransferQuery query) {
    
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
    MoneroTxQuery txQuery = query.getTxQuery();
    
    // build params for get_transfers rpc call
    Map<String, Object> params = new HashMap<String, Object>();
    boolean canBeConfirmed = !Boolean.FALSE.equals(txQuery.isConfirmed()) && !Boolean.TRUE.equals(txQuery.inTxPool()) && !Boolean.TRUE.equals(txQuery.isFailed()) && !Boolean.FALSE.equals(txQuery.isRelayed());
    boolean canBeInTxPool = !Boolean.TRUE.equals(txQuery.isConfirmed()) && !Boolean.FALSE.equals(txQuery.inTxPool()) && !Boolean.TRUE.equals(txQuery.isFailed()) && txQuery.getHeight() == null && txQuery.getMaxHeight() == null && !Boolean.FALSE.equals(txQuery.isLocked());
    boolean canBeIncoming = !Boolean.FALSE.equals(query.isIncoming()) && !Boolean.TRUE.equals(query.isOutgoing()) && !Boolean.TRUE.equals(query.hasDestinations());
    boolean canBeOutgoing = !Boolean.FALSE.equals(query.isOutgoing()) && !Boolean.TRUE.equals(query.isIncoming());

    // check if fetching pool txs contradicted by configuration
    if (Boolean.TRUE.equals(txQuery.inTxPool()) && !canBeInTxPool) {
      throw new MoneroError("Cannot fetch pool transactions because it contradicts configuration");
    }

    params.put("in", canBeIncoming && canBeConfirmed);
    params.put("out", canBeOutgoing && canBeConfirmed);
    params.put("pool", canBeIncoming && canBeInTxPool);
    params.put("pending", canBeOutgoing && canBeInTxPool);
    params.put("failed", !Boolean.FALSE.equals(txQuery.isFailed()) && !Boolean.TRUE.equals(txQuery.isConfirmed()) && !Boolean.TRUE.equals(txQuery.inTxPool()));
    if (txQuery.getMinHeight() != null) {
      if (txQuery.getMinHeight() > 0) params.put("min_height", txQuery.getMinHeight() - 1); // TODO monero-project: wallet2::get_payments() min_height is exclusive, so manually offset to match intended range (issues #5751, #5598)
      else params.put("min_height", txQuery.getMinHeight());
    }
    if (txQuery.getMaxHeight() != null) params.put("max_height", txQuery.getMaxHeight());
    params.put("filter_by_height", txQuery.getMinHeight() != null || txQuery.getMaxHeight() != null);
    if (query.getAccountIndex() == null) {
      GenUtils.assertTrue("Filter specifies a subaddress index but not an account index", query.getSubaddressIndex() == null && query.getSubaddressIndices() == null);
      params.put("all_accounts", true);
    } else {
      params.put("account_index", query.getAccountIndex());
      
      // set subaddress indices param
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      if (query.getSubaddressIndex() != null) subaddressIndices.add(query.getSubaddressIndex());
      if (query.getSubaddressIndices() != null) {
        for (int subaddressIdx : query.getSubaddressIndices()) subaddressIndices.add(subaddressIdx);
      }
      if (!subaddressIndices.isEmpty()) params.put("subaddr_indices", new ArrayList<Integer>(subaddressIndices));
    }
    
    // cache unique txs and blocks
    Map<String, MoneroTxWallet> txMap = new HashMap<String, MoneroTxWallet>();
    Map<Long, MoneroBlock> blockMap = new HashMap<Long, MoneroBlock>();
    
    // build txs using `get_transfers`
    Map<String, Object> resp = rpc.sendJsonRequest("get_transfers", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    for (String key : result.keySet()) {
      for (Map<String, Object> rpcTx :((List<Map<String, Object>>) result.get(key))) {
        MoneroTxWallet tx = convertRpcTxWithTransfer(rpcTx, null, null, null);
        if (tx.isConfirmed()) GenUtils.assertTrue(tx.getBlock().getTxs().contains(tx));
//        if (tx.getId().equals("38436c710dfbebfb24a14cddfd430d422e7282bbe94da5e080643a1bd2880b44")) {
//          System.out.println(rpcTx);
//          System.out.println(tx.getOutgoingAmount().compareTo(BigInteger.valueOf(0)) == 0);
//        }
        
        // replace transfer amount with destination sum
        // TODO monero-wallet-rpc: confirmed tx from/to same account has amount 0 but cached transfers
        if (tx.getOutgoingTransfer() != null && Boolean.TRUE.equals(tx.isRelayed()) && !Boolean.TRUE.equals(tx.isFailed()) &&
            tx.getOutgoingTransfer().getDestinations() != null && tx.getOutgoingAmount().compareTo(BigInteger.valueOf(0)) == 0) {
          MoneroOutgoingTransfer outgoingTransfer = tx.getOutgoingTransfer();
          BigInteger transferTotal = BigInteger.valueOf(0);
          for (MoneroDestination destination : outgoingTransfer.getDestinations()) transferTotal = transferTotal.add(destination.getAmount());
          tx.getOutgoingTransfer().setAmount(transferTotal);
        }
        
        // merge tx
        mergeTx(tx, txMap, blockMap);
      }
    }
    
    // sort txs by block height
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>(txMap.values());
    Collections.sort(txs, new TxHeightComparator());
    
    // filter and return transfers
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    for (MoneroTxWallet tx : txs) {

      // tx is not incoming/outgoing unless already set
      if (tx.isIncoming() == null) tx.setIsIncoming(false);
      if (tx.isOutgoing() == null) tx.setIsOutgoing(false);
      
      // sort incoming transfers
      if (tx.getIncomingTransfers() != null) Collections.sort(tx.getIncomingTransfers(), new IncomingTransferComparator());
      
      // collect queried transfers, erase if excluded
      transfers.addAll(tx.filterTransfers(query));
      
      // remove excluded txs from block
      if (tx.getBlock() != null && tx.getOutgoingTransfer() == null && tx.getIncomingTransfers() == null ) {
        tx.getBlock().getTxs().remove(tx);
      }
    }
    return transfers;
  }
  
  @SuppressWarnings("unchecked")
  private List<MoneroOutputWallet> getOutputsAux(MoneroOutputQuery query) {
    
    // copy and normalize query up to block
    if (query == null) query = new MoneroOutputQuery();
    else {
      if (query.getTxQuery() == null) query = query.copy();
      else {
        MoneroTxQuery txQuery = query.getTxQuery().copy();
        if (query.getTxQuery().getOutputQuery() == query) query = txQuery.getOutputQuery();
        else {
          GenUtils.assertNull("Output request's tx request must be circular reference or null", query.getTxQuery().getOutputQuery());
          query = query.copy();
          query.setTxQuery(txQuery);
        }
      }
    }
    if (query.getTxQuery() == null) query.setTxQuery(new MoneroTxQuery());
    
    // determine account and subaddress indices to be queried
    Map<Integer, List<Integer>> indices = new HashMap<Integer, List<Integer>>();
    if (query.getAccountIndex() != null) {
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      if (query.getSubaddressIndex() != null) subaddressIndices.add(query.getSubaddressIndex());
      if (query.getSubaddressIndices() != null) for (int subaddressIdx : query.getSubaddressIndices()) subaddressIndices.add(subaddressIdx);
      indices.put(query.getAccountIndex(), subaddressIndices.isEmpty() ? null : new ArrayList<Integer>(subaddressIndices));  // null will fetch from all subaddresses
    } else {
      GenUtils.assertEquals("Request specifies a subaddress index but not an account index", null, query.getSubaddressIndex());
      GenUtils.assertTrue("Request specifies subaddress indices but not an account index", query.getSubaddressIndices() == null || query.getSubaddressIndices().size() == 0);
      indices = getAccountIndices(false);  // fetch all account indices without subaddresses
    }
    
    // cache unique txs and blocks
    Map<String, MoneroTxWallet> txMap = new HashMap<String, MoneroTxWallet>();
    Map<Long, MoneroBlock> blockMap = new HashMap<Long, MoneroBlock>();
    
    // collect txs with outputs for each indicated account using `incoming_transfers` rpc call
    Map<String, Object> params = new HashMap<String, Object>();
    String transferType;
    if (Boolean.TRUE.equals(query.isSpent())) transferType = "unavailable";
    else if (Boolean.FALSE.equals(query.isSpent())) transferType = "available";
    else transferType = "all";
    params.put("transfer_type", transferType);
    params.put("verbose", true);
    for (int accountIdx : indices.keySet()) {
    
      // send request
      params.put("account_index", accountIdx);
      params.put("subaddr_indices", indices.get(accountIdx));
      Map<String, Object> resp = rpc.sendJsonRequest("incoming_transfers", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      
      // convert response to txs with outputs and merge
      if (!result.containsKey("transfers")) continue;
      for (Map<String, Object> rpcOutput : (List<Map<String, Object>>) result.get("transfers")) {
        MoneroTxWallet tx = convertRpcTxWithOutput(rpcOutput);
        mergeTx(tx, txMap, blockMap);
      }
    }
    
    // sort txs by block height
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>(txMap.values());
    Collections.sort(txs, new TxHeightComparator());
    
    // collect queried outputs
    List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
    for (MoneroTxWallet tx : txs) {
      
      // sort outputs
      if (tx.getOutputs() != null) Collections.sort(tx.getOutputs(), new OutputComparator());
      
      // collect queried outputs, erase if excluded
      outputs.addAll(tx.filterOutputsWallet(query));
      
      // remove excluded txs from block
      if (tx.getOutputs() == null && tx.getBlock() != null) tx.getBlock().getTxs().remove(tx);
    }
    return outputs;
  }
  
  @SuppressWarnings("unchecked")
  private List<MoneroTxWallet> rpcSweepAccount(MoneroTxConfig config) {
    
    // validate request
    if (config == null) throw new MoneroError("Sweep request cannot be null");
    if (config.getAccountIndex() == null) throw new MoneroError("Must specify an account index to sweep from");
    if (config.getDestinations() == null || config.getDestinations().size() != 1) throw new MoneroError("Must specify exactly one destination to sweep to");
    if (config.getDestinations().get(0).getAddress() == null) throw new MoneroError("Must specify destination address to sweep to");
    if (config.getDestinations().get(0).getAmount() != null) throw new MoneroError("Cannot specify amount in sweep request");
    if (config.getKeyImage() != null) throw new MoneroError("Key image defined; use sweepOutput() to sweep an output by its key image");
    if (config.getSubaddressIndices() != null && config.getSubaddressIndices().size() == 0) throw new MoneroError("Empty list given for subaddresses indices to sweep");
    if (Boolean.TRUE.equals(config.getSweepEachSubaddress())) throw new MoneroError("Cannot sweep each subaddress with RPC `sweep_all`");
    if (config.getSubtractFeeFrom() != null && config.getSubtractFeeFrom().size() > 0) throw new MoneroError("Sweeping output does not support subtracting fees from destinations");
    
    // sweep from all subaddresses if not otherwise defined
    if (config.getSubaddressIndices() == null) {
      config.setSubaddressIndices(new ArrayList<Integer>());
      for (MoneroSubaddress subaddress : getSubaddresses(config.getAccountIndex())) {
        config.getSubaddressIndices().add(subaddress.getIndex());
      }
    }
    if (config.getSubaddressIndices().size() == 0) throw new MoneroError("No subaddresses to sweep from");
    
    // common request params
    boolean relay = Boolean.TRUE.equals(config.getRelay());
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", config.getAccountIndex());
    params.put("subaddr_indices", config.getSubaddressIndices());
    params.put("address", config.getDestinations().get(0).getAddress());
    params.put("priority", config.getPriority() == null ? null : config.getPriority().ordinal());
    params.put("unlock_time", config.getUnlockTime());
    params.put("payment_id", config.getPaymentId());
    params.put("do_not_relay", !relay);
    params.put("below_amount", config.getBelowAmount());
    params.put("get_tx_keys", true);
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    
    // invoke wallet rpc `sweep_all`
    Map<String, Object> resp = rpc.sendJsonRequest("sweep_all", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // initialize txs from response
    MoneroTxSet txSet = convertRpcSentTxsToTxSet(result, null, config);
    
    // initialize remaining known fields
    for (MoneroTxWallet tx : txSet.getTxs()) {
      tx.setIsLocked(true);
      tx.setIsConfirmed(false);
      tx.setNumConfirmations(0l);
      tx.setRelay(relay);
      tx.setInTxPool(relay);
      tx.setIsRelayed(relay);
      tx.setIsMinerTx(false);
      tx.setIsFailed(false);
      tx.setRingSize(MoneroUtils.RING_SIZE);
      MoneroOutgoingTransfer transfer = tx.getOutgoingTransfer();
      transfer.setAccountIndex(config.getAccountIndex());
      if (config.getSubaddressIndices().size() == 1) transfer.setSubaddressIndices(new ArrayList<Integer>(config.getSubaddressIndices()));
      MoneroDestination destination = new MoneroDestination(config.getDestinations().get(0).getAddress(), transfer.getAmount());
      transfer.setDestinations(Arrays.asList(destination));
      tx.setPaymentId(config.getPaymentId());
      if (tx.getUnlockTime() == null) tx.setUnlockTime(config.getUnlockTime() == null ? BigInteger.valueOf(0) : config.getUnlockTime());
      if (tx.getRelay()) {
        if (tx.getLastRelayedTimestamp() == null) tx.setLastRelayedTimestamp(System.currentTimeMillis());  // TODO (monero-wallet-rpc): provide timestamp on response; unconfirmed timestamps vary
        if (tx.isDoubleSpendSeen() == null) tx.setIsDoubleSpendSeen(false);
      }
    }
    return txSet.getTxs();
  }
  
  // -------------------------------- LISTENER --------------------------------
  
  private void refreshListening() {
    if (rpc.getZmqUri() == null) {
      if (walletPoller == null && listeners.size() > 0) walletPoller = new WalletPoller(this);
      if (walletPoller != null) walletPoller.setIsPolling(listeners.size() > 0);
    } else {
      if (zmqListener == null && listeners.size() > 0) zmqListener = new WalletRpcZmqListener();
      if (zmqListener != null) zmqListener.setIsPolling(listeners.size() > 0);
    }
  }
  
  /**
   * Poll if listening.
   */
  private void poll() {
    if (walletPoller != null && walletPoller.isPolling) walletPoller.poll();
  }
  
  /**
   * Polls monero-wallet-rpc to provide listener notifications.
   * 
   * TODO: refactor base listener class with zmq listener
   */
  private class WalletPoller {
    
    private MoneroWalletDefault wallet;
    private boolean isPolling;
    private TaskLooper looper;
    private int numPolling = 0;
    private Long prevHeight;
    private BigInteger[] prevBalances;
    private List<MoneroTxWallet> prevLockedTxs = new ArrayList<MoneroTxWallet>();
    private Set<String> prevUnconfirmedNotifications = new HashSet<String>(); // tx hashes of previous notifications
    private Set<String> prevConfirmedNotifications = new HashSet<String>(); // tx hashes of previously confirmed but not yet unlocked notifications
    
    public WalletPoller(MoneroWalletDefault wallet) {
      this.wallet = wallet;
      looper = new TaskLooper(new Runnable() {
        @Override
        public void run() {
          poll();
        }
      });
    }
    
    public void setIsPolling(boolean isPolling) {
      this.isPolling = isPolling;
      if (isPolling) looper.start(syncPeriodInMs);
      else looper.stop();
    }
    
    public void setPeriodInMs(long periodInMs) {
      looper.setPeriodInMs(periodInMs);
    }
    
    public void poll() {

      // skip if next poll is queued
      if (numPolling > 1) return;
      numPolling++;

      // synchronize polls
      synchronized (this) {
        try {

          // skip if wallet is closed
          if (wallet.isClosed()) {
            numPolling--;
            return;
          }

          // take initial snapshot
          if (prevBalances == null) {
            prevHeight = getHeight();
            prevLockedTxs = getTxs(new MoneroTxQuery().setIsLocked(true));
            prevBalances = getBalances(null, null);
            numPolling--;
            return;
          }
          
          // announce height changes
          long height = getHeight();
          if (prevHeight != height) {
            for (long i = prevHeight; i < height; i++) onNewBlock(i);
            prevHeight = height;
          }
          
          // get locked txs for comparison to previous
          long minHeight = Math.max(0, height - 70); // only monitor recent txs
          List<MoneroTxWallet> lockedTxs = getTxs(new MoneroTxQuery().setIsLocked(true).setMinHeight(minHeight).setIncludeOutputs(true));
          
          // collect hashes of txs no longer locked
          List<String> noLongerLockedHashes = new ArrayList<String>();
          for (MoneroTxWallet prevLockedTx : prevLockedTxs) {
            if (getTx(lockedTxs, prevLockedTx.getHash()) == null) {
              noLongerLockedHashes.add(prevLockedTx.getHash());
            }
          }
          
          // save locked txs for next comparison
          prevLockedTxs = lockedTxs;
          
          // fetch txs which are no longer locked
          List<MoneroTxWallet> unlockedTxs = noLongerLockedHashes.isEmpty() ? new ArrayList<MoneroTxWallet>() : getTxs(new MoneroTxQuery().setIsLocked(false).setMinHeight(minHeight).setHashes(noLongerLockedHashes).setIncludeOutputs(true));
          
          // announce new unconfirmed and confirmed txs
          for (MoneroTxWallet lockedTx : lockedTxs) {
            boolean unannounced = lockedTx.isConfirmed() ? prevConfirmedNotifications.add(lockedTx.getHash()) : prevUnconfirmedNotifications.add(lockedTx.getHash());
            if (unannounced) notifyOutputs(lockedTx);
          }
          
          // announce new unlocked outputs
          for (MoneroTxWallet unlockedTx : unlockedTxs) {
            prevUnconfirmedNotifications.remove(unlockedTx.getHash()); // stop tracking tx notifications
            prevConfirmedNotifications.remove(unlockedTx.getHash());
            notifyOutputs(unlockedTx);
          }
          
          // announce balance changes
          checkForChangedBalances();
          numPolling--;
        } catch (Exception e) {
          numPolling--;
          if (isPolling) System.err.println("Failed to background poll wallet '" + path + "': " + e.getMessage());
        }
      }
    }
    
    private void notifyOutputs(MoneroTxWallet tx) {
      
      // notify spent outputs // TODO (monero-project): monero-wallet-rpc does not allow scrape of tx inputs so providing one input with outgoing amount
      if (tx.getOutgoingTransfer() != null) {
        GenUtils.assertNull(tx.getInputs());
        MoneroOutputWallet output = new MoneroOutputWallet()
            .setAmount(tx.getOutgoingTransfer().getAmount().add(tx.getFee()))
            .setAccountIndex(tx.getOutgoingTransfer().getAccountIndex())
            .setSubaddressIndex(tx.getOutgoingTransfer().getSubaddressIndices().size() == 1 ? tx.getOutgoingTransfer().getSubaddressIndices().get(0) : null) // initialize if transfer sourced from single subaddress
            .setTx(tx);
        tx.setInputsWallet(Arrays.asList(output));
        announceOutputSpent(output);
      }
      
      // notify received outputs
      if (tx.getIncomingTransfers() != null) {
        if (tx.getOutputs() != null && !tx.getOutputs().isEmpty()) { // TODO (monero-project): outputs only returned for confirmed txs
          for (MoneroOutputWallet output : tx.getOutputsWallet()) {
            announceOutputReceived(output);
          }
        } else { // TODO (monero-project): monero-wallet-rpc does not allow scrape of unconfirmed received outputs so using incoming transfer values
          List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
          for (MoneroIncomingTransfer transfer : tx.getIncomingTransfers()) {
            outputs.add(new MoneroOutputWallet()
                .setAccountIndex(transfer.getAccountIndex())
                .setSubaddressIndex(transfer.getSubaddressIndex())
                .setAmount(transfer.getAmount())
                .setTx(tx));
          }
          tx.setOutputsWallet(outputs);
            for (MoneroOutputWallet output : tx.getOutputsWallet()) {
              announceOutputReceived(output);
            }
        }
      }
    }
    
    private void onNewBlock(long height) {
      announceNewBlock(height);
    }
    
    private MoneroTxWallet getTx(List<MoneroTxWallet> txs, String txHash) {
      for (MoneroTxWallet tx : txs) if (txHash.equals(tx.getHash())) return tx;
      return null;
    }
    
    // TODO: factor to common wallet rpc listener
    private boolean checkForChangedBalances() {
      BigInteger[] balances = getBalances(null, null);
      if (!balances[0].equals(prevBalances[0]) || !balances[1].equals(prevBalances[1])) {
        prevBalances = balances;
        announceBalancesChanged(balances[0], balances[1]);
        return true;
      }
      return false;
    }
  }
  
  /**
   * Receives ZMQ notifications directly from monero-wallet-rpc.
   */
  private class WalletRpcZmqListener {
    
    private boolean isPolling;
    private Thread pollThread;
    private ExecutorService processNotificationPool;
    private ZContext context;
    private ZMQ.Socket subscriber;
    private BigInteger prevBalance;
    private BigInteger prevUnlockedBalance;
    private List<String> prevLockedTxHashes = new ArrayList<String>();
    
    public WalletRpcZmqListener() {
      prevBalance = getBalance();
      prevUnlockedBalance = getUnlockedBalance();
    }
    
    public void setIsPolling(boolean isPolling) {
      if (isPolling) start();
      else stop();
    }
    
    private void start() {
      if (isPolling) return;
      isPolling = true;
      
      // cache locked txs for later comparison
      checkForChangedUnlockedTxs();
      
      // create pool to process notifications in serial without blocking polling
      processNotificationPool = Executors.newFixedThreadPool(1);
      
      // create thread which polls zmq publications
      pollThread = new Thread(new Runnable() {
        @Override public void run() {
          
          // create subscriber
          context = new ZContext();
          subscriber = context.createSocket(SocketType.SUB);
          
          // subscribe to topic
          subscriber.connect(getRpcConnection().getZmqUri());
          subscriber.subscribe("json-minimal-chain_main".getBytes()); // TODO (monero-project): these notifications not received if subscribed to json-full
          subscriber.subscribe("json-full-money_received".getBytes());
          subscriber.subscribe("json-full-money_spent".getBytes());
          subscriber.subscribe("json-full-unconfirmed_money_received".getBytes());
          
          // TODO: refine topics
          //subscriber.subscribe("json-full".getBytes());
          //subscriber.subscribe("json-minimal".getBytes());
          
          // TODO: the content of these are yet to to be decided, see how much full is used
          //json-minimal-money_received
          //json-minimal-money_spent
          //json-minimal-unconfirmed_money_received
          
          // poll for zmq publications
          ZMQ.Poller poller = context.createPoller(1);
          poller.register(subscriber, ZMQ.Poller.POLLIN);
          while (!Thread.currentThread().isInterrupted() && isPolling) {
            try {
              poller.poll();
              if (poller.pollin(0)) {
                String notification = subscriber.recvStr();
                processNotificationPool.submit(new Runnable() {
                  @Override
                  public void run() {
                    processZmqNotification(notification);
                  }
                });
              }
            } catch (Exception e) {
              if (!Thread.currentThread().isInterrupted() && isPolling) {
                throw e;
              }
            }
          }
          
          stop(); // stop if disconnects
        }
      });
      pollThread.start();
    }
    
    private void stop() {
      if (!isPolling) return;
      isPolling = false;
      subscriber.close();
      context.close();
      prevLockedTxHashes.clear();
      processNotificationPool.shutdown();
      pollThread.interrupt();
    }
    
    @SuppressWarnings("unchecked")
    private void processZmqNotification(String content) {
      
      System.out.println("Processing zmq notification: " + content);
      
      // parse json content to map
      // TODO: more efficent way?
      int bodyIdx = content.indexOf(":");
      String topic = content.substring(0, bodyIdx);
      if (content.substring(bodyIdx + 1).isEmpty()) {
        System.out.println("WARNING: empty body to parse zmq notification");
        return;
      }
      
      // process topics
      if (topic.equals("json-minimal-chain_main")) {
        Map<String, Object> contentMap = JsonUtils.toMap(MoneroRpcConnection.MAPPER, content.substring(bodyIdx + 1)); // TODO: keep mapper in MoneroRpcConnection?
        long height = ((BigInteger) contentMap.get("first_height")).longValue();
        announceNewBlock(height);
        
        // notify if balances change
        boolean balancesChanged = checkForChangedBalances();
        
        // notify when txs unlock after wallet is synced
        if (balancesChanged) checkForChangedUnlockedTxs();  // TODO: only check for unlocked txs when isSynced()
      } else {
        
        // parse json to maps
        Map<String, Object> contentMap = JsonUtils.toMap(MoneroRpcConnection.MAPPER, content.substring(bodyIdx + 1));
        Map<String, Object> txMap = (Map<String, Object>) (contentMap.containsKey("tx_in") ? contentMap.get("tx_in") : contentMap.get("tx"));
        
        // build output
        MoneroOutputWallet output = new MoneroOutputWallet();
        output.setAmount((BigInteger) contentMap.get("amount"));
        output.setAccountIndex(((BigInteger) contentMap.get("subaddr_index_major")).intValue());  // TODO: deserialize as {major: 0, minor: 0}
        output.setSubaddressIndex(((BigInteger) contentMap.get("subaddr_index_minor")).intValue());
        MoneroTxWallet tx = new MoneroTxWallet();
        tx.setHash((String) contentMap.get("txid"));
        tx.setVersion(((BigInteger) txMap.get("version")).intValue());
        tx.setUnlockTime(((BigInteger) txMap.get("unlock_time")));
        output.setTx(tx);
        tx.setOutputs(Arrays.asList(output));
        long height = ((BigInteger) contentMap.get("height")).longValue();
        tx.setIsLocked(true);
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
        if (topic.equals("json-full-money_received")) {
          tx.setIsIncoming(true);
          prevLockedTxHashes.add(tx.getHash()); // watch for unlock
          announceOutputReceived(output);
        } else if (topic.equals("json-full-money_spent")) {
          tx.setIsIncoming(false);
          prevLockedTxHashes.add(tx.getHash()); // watch for unlock
          announceOutputSpent(output);
        } else if (topic.equals("json-full-unconfirmed_money_received")) {
          tx.setIsIncoming(true);
          announceOutputReceived(output);
          checkForChangedBalances();
        } else {
          LOGGER.warning("Received unsupported notification: " + content);
        }
      }
    }
    
    private boolean checkForChangedBalances() {
      BigInteger balance = getBalance();
      BigInteger unlockedBalance = getUnlockedBalance();
      if (!balance.equals(prevBalance) || !unlockedBalance.equals(prevUnlockedBalance)) {
        prevBalance =  balance;
        prevUnlockedBalance = unlockedBalance;
        announceBalancesChanged(balance, unlockedBalance);
        return true;
      }
      return false;
    }
    
    private void checkForChangedUnlockedTxs() {
      
      // get locked txs
      List<MoneroTxWallet> lockedTxs = getTxs(new MoneroTxQuery().setIsLocked(true).setIsConfirmed(true));
      
      // collect hashes of txs no longer locked
      List<String> txHashesNoLongerLocked = new ArrayList<String>();
      for (String prevLockedTxHash : prevLockedTxHashes) {
        boolean found = false;
        for (MoneroTxWallet lockedTx : lockedTxs) {
          if (lockedTx.getHash().equals(prevLockedTxHash)) {
            found = true;
            break;
          }
        }
        if (!found) txHashesNoLongerLocked.add(prevLockedTxHash);
      }
      
      // fetch txs that are no longer locked
      List<MoneroTxWallet> txsNoLongerLocked = new ArrayList<MoneroTxWallet>();
      if (!txHashesNoLongerLocked.isEmpty()) {
        MoneroTxQuery query = new MoneroTxQuery()
            .setHashes(txHashesNoLongerLocked)
            .setIsLocked(false)
            .setIsConfirmed(true)
            .setIncludeOutputs(true);
        txsNoLongerLocked = getTxs(query);
      }
      
      // notify listeners of newly unlocked tx outputs
      for (MoneroTxWallet unlockedTx : txsNoLongerLocked) {
        for (MoneroOutputWallet output : unlockedTx.getOutputsWallet()) {
          announceOutputReceived(output);
        }
      }
      
      // re-assign currently locked tx hashes // TODO: needs mutex for thread safety?
      prevLockedTxHashes.clear();
      for (MoneroTxWallet lockedTx : lockedTxs) prevLockedTxHashes.add(lockedTx.getHash());
    }
  }
  
  // ---------------------------- PRIVATE STATIC ------------------------------
  
  /**
   * Remove criteria which requires looking up other transfers/outputs to
   * fulfill query.
   * 
   * @param query the query to decontextualize
   * @return a reference to the query for convenience
   */
  private static MoneroTxQuery decontextualize(MoneroTxQuery query) {
    query.setIsIncoming(null);
    query.setIsOutgoing(null);
    query.setTransferQuery(null);
    query.setInputQuery(null);
    query.setOutputQuery(null);
    return query;
  }
  
  private static boolean isContextual(MoneroTransferQuery query) {
    if (query == null) return false;
    if (query.getTxQuery() == null) return false;
    if (query.getTxQuery().isIncoming() != null) return true;       // requires context of all transfers
    if (query.getTxQuery().isOutgoing() != null) return true;
    if (query.getTxQuery().getInputQuery() != null) return true;    // requires context of inputs
    if (query.getTxQuery().getOutputQuery() != null) return true;   // requires context of outputs
    return false;
  }
  
  private static boolean isContextual(MoneroOutputQuery query) {
    if (query == null) return false;
    if (query.getTxQuery() == null) return false;
    if (query.getTxQuery().isIncoming() != null) return true;       // requires context of all transfers
    if (query.getTxQuery().isOutgoing() != null) return true;
    if (query.getTxQuery().getTransferQuery() != null) return true; // requires context of transfers
    return false;
  }
  
  private static MoneroAccount convertRpcAccount(Map<String, Object> rpcAccount) {
    MoneroAccount account = new MoneroAccount();
    for (String key : rpcAccount.keySet()) {
      Object val = rpcAccount.get(key);
      if (key.equals("account_index")) account.setIndex(((BigInteger) val).intValue());
      else if (key.equals("balance")) account.setBalance((BigInteger) val);
      else if (key.equals("unlocked_balance")) account.setUnlockedBalance((BigInteger) val);
      else if (key.equals("base_address")) account.setPrimaryAddress((String) val);
      else if (key.equals("tag")) account.setTag((String) val);
      else if (key.equals("label")) { } // label belongs to first subaddress
      else LOGGER.warning("ignoring unexpected account field: " + key + ": " + val);
    }
    if ("".equals(account.getTag())) account.setTag(null);
    return account;
  }
  
  private static MoneroSubaddress convertRpcSubaddress(Map<String, Object> rpcSubaddress) {
    MoneroSubaddress subaddress = new MoneroSubaddress();
    for (String key : rpcSubaddress.keySet()) {
      Object val = rpcSubaddress.get(key);
      if (key.equals("account_index")) subaddress.setAccountIndex(((BigInteger) val).intValue());
      else if (key.equals("address_index")) subaddress.setIndex(((BigInteger) val).intValue());
      else if (key.equals("address")) subaddress.setAddress((String) val);
      else if (key.equals("balance")) subaddress.setBalance((BigInteger) val);
      else if (key.equals("unlocked_balance")) subaddress.setUnlockedBalance((BigInteger) val);
      else if (key.equals("num_unspent_outputs")) subaddress.setNumUnspentOutputs(((BigInteger) val).longValue());
      else if (key.equals("label")) { if (!"".equals(val)) subaddress.setLabel((String) val); }
      else if (key.equals("used")) subaddress.setIsUsed((Boolean) val);
      else if (key.equals("blocks_to_unlock")) subaddress.setNumBlocksToUnlock(((BigInteger) val).longValue());
      else if (key.equals("time_to_unlock")) {} // ignoring
      else LOGGER.warning("ignoring unexpected subaddress field: " + key + ": " + val);
    }
    return subaddress;
  }
  
  /**
   * Initializes a sent transaction.
   * 
   * TODO: remove copyDestinations after >18.3.1 when subtractFeeFrom fully supported
   * 
   * @param config is the send configuration
   * @param tx is an existing transaction to initialize (optional)
   * @param copyDestinations copies config destinations if true
   * @return tx is the initialized send tx
   */
  private static MoneroTxWallet initSentTxWallet(MoneroTxConfig config, MoneroTxWallet tx, boolean copyDestinations) {
    if (tx == null) tx = new MoneroTxWallet();
    boolean relay = Boolean.TRUE.equals(config.getRelay());
    tx.setIsOutgoing(true);
    tx.setIsConfirmed(false);
    tx.setNumConfirmations(0l);
    tx.setInTxPool(relay);
    tx.setRelay(relay);
    tx.setIsRelayed(relay);
    tx.setIsMinerTx(false);
    tx.setIsFailed(false);
    tx.setIsLocked(true);
    tx.setRingSize(MoneroUtils.RING_SIZE);
    MoneroOutgoingTransfer transfer = new MoneroOutgoingTransfer().setTx(tx);
    if (config.getSubaddressIndices() != null && config.getSubaddressIndices().size() == 1) transfer.setSubaddressIndices(new ArrayList<Integer>(config.getSubaddressIndices())); // we know src subaddress indices iff request specifies 1
    if (copyDestinations) {
      List<MoneroDestination> destCopies = new ArrayList<MoneroDestination>();
      for (MoneroDestination dest : config.getDestinations()) destCopies.add(dest.copy());
      transfer.setDestinations(destCopies);
    }
    tx.setOutgoingTransfer(transfer);
    tx.setPaymentId(config.getPaymentId());
    if (tx.getUnlockTime() == null) tx.setUnlockTime(config.getUnlockTime() == null ? BigInteger.valueOf(0) : config.getUnlockTime());
    if (tx.getRelay()) {
      if (tx.getLastRelayedTimestamp() == null) tx.setLastRelayedTimestamp(System.currentTimeMillis());  // TODO (monero-wallet-rpc): provide timestamp on response; unconfirmed timestamps vary
      if (tx.isDoubleSpendSeen() == null) tx.setIsDoubleSpendSeen(false);
    }
    return tx;
  }
  
  /**
   * Initializes a tx set from a RPC map excluding txs.
   * 
   * @param rpcMap is the map to initialize the tx set from
   * @return MoneroTxSet is the initialized tx set
   */
  private static MoneroTxSet convertRpcTxSet(Map<String, Object> rpcMap) {
    MoneroTxSet txSet = new MoneroTxSet();
    txSet.setMultisigTxHex((String) rpcMap.get("multisig_txset"));
    txSet.setUnsignedTxHex((String) rpcMap.get("unsigned_txset"));
    txSet.setSignedTxHex((String) rpcMap.get("signed_txset"));
    if (txSet.getMultisigTxHex() != null && txSet.getMultisigTxHex().isEmpty()) txSet.setMultisigTxHex(null);
    if (txSet.getUnsignedTxHex() != null && txSet.getUnsignedTxHex().isEmpty()) txSet.setUnsignedTxHex(null);
    if (txSet.getSignedTxHex() != null && txSet.getSignedTxHex().isEmpty()) txSet.setSignedTxHex(null);
    return txSet;
  }
  
  /**
   * Initializes a MoneroTxSet from a list of rpc txs.
   * 
   * @param rpcTxs are sent rpc txs to initialize the set from
   * @param txs are existing txs to further initialize (optional)
   * @return the converted tx set
   */
  @SuppressWarnings("unchecked")
  private static MoneroTxSet convertRpcSentTxsToTxSet(Map<String, Object> rpcTxs, List<MoneroTxWallet> txs, MoneroTxConfig config) {
    
    // build shared tx set
    MoneroTxSet txSet = convertRpcTxSet(rpcTxs);
    
    // get number of txs
    String numTxsKey = rpcTxs.containsKey("fee_list") ? "fee_list" : rpcTxs.containsKey("tx_hash_list") ? "tx_hash_list" : null;
    int numTxs = numTxsKey == null ? 0 : ((List<?>) rpcTxs.get(numTxsKey)).size();
    
    // done if rpc response contains no txs
    if (numTxs == 0) {
      GenUtils.assertNull(txs);
      return txSet;
    }
    
    // initialize txs if none given
    if (txs != null) txSet.setTxs(txs);
    else {
      txs = new ArrayList<MoneroTxWallet>();
      for (int i = 0; i < numTxs; i++) txs.add(new MoneroTxWallet());
    }
    for (MoneroTxWallet tx : txs) {
      tx.setTxSet(txSet);
      tx.setIsOutgoing(true);
    }
    txSet.setTxs(txs);
    
    // initialize txs from rpc lists
    for (String key : rpcTxs.keySet()) {
      Object val = rpcTxs.get(key);
      if (key.equals("tx_hash_list")) {
        List<String> hashes = (List<String>) val;
        for (int i = 0; i < hashes.size(); i++) txs.get(i).setHash(hashes.get(i));
      } else if (key.equals("tx_key_list")) {
        List<String> keys = (List<String>) val;
        for (int i = 0; i < keys.size(); i++) txs.get(i).setKey(keys.get(i));
      } else if (key.equals("tx_blob_list")) {
        List<String> blobs = (List<String>) val;
        for (int i = 0; i < blobs.size(); i++) txs.get(i).setFullHex(blobs.get(i));
      } else if (key.equals("tx_metadata_list")) {
        List<String> metadatas = (List<String>) val;
        for (int i = 0; i < metadatas.size(); i++) txs.get(i).setMetadata(metadatas.get(i));
      } else if (key.equals("fee_list")) {
        List<BigInteger> fees = (List<BigInteger>) val;
        for (int i = 0; i < fees.size(); i++) txs.get(i).setFee(fees.get(i));
      } else if (key.equals("amount_list")) {
        List<BigInteger> amounts = (List<BigInteger>) val;
        for (int i = 0; i < amounts.size(); i++) {
          if (txs.get(i).getOutgoingTransfer() == null) txs.get(i).setOutgoingTransfer(new MoneroOutgoingTransfer().setTx(txs.get(i)));
          txs.get(i).getOutgoingTransfer().setAmount(amounts.get(i));
        }
      } else if (key.equals("weight_list")) {
        List<BigInteger> weights = (List<BigInteger>) val;
        for (int i = 0; i < weights.size(); i++) txs.get(i).setWeight(weights.get(i).longValue());
      } else if (key.equals("multisig_txset") || key.equals("unsigned_txset") || key.equals("signed_txset")) {
        // handled elsewhere
      } else if (key.equals("spent_key_images_list")) {
        List<Map<String, Object>> inputKeyImagesList = (List<Map<String, Object>>) val;
        for (int i = 0; i < inputKeyImagesList.size(); i++) {
          GenUtils.assertTrue(txs.get(i).getInputs() == null);
          txs.get(i).setInputsWallet(new ArrayList<MoneroOutputWallet>());
          for (String inputKeyImage : (List<String>) inputKeyImagesList.get(i).get("key_images")) {
            txs.get(i).getInputs().add(new MoneroOutputWallet().setKeyImage(new MoneroKeyImage().setHex(inputKeyImage)).setTx(txs.get(i)));
          }
        }
      } else if (key.equals("amounts_by_dest_list")) {
        List<Map<String, Object>> amountsByDestList = (List<Map<String, Object>>) val;
        int destinationIdx = 0;
        for (int txIdx = 0; txIdx < amountsByDestList.size(); txIdx++) {
          List<BigInteger> amountsByDest = (List<BigInteger>) amountsByDestList.get(txIdx).get("amounts");
          if (txs.get(txIdx).getOutgoingTransfer() == null) txs.get(txIdx).setOutgoingTransfer(new MoneroOutgoingTransfer().setTx(txs.get(txIdx)));
          txs.get(txIdx).getOutgoingTransfer().setDestinations(new ArrayList<>());
          for (BigInteger amount : amountsByDest) {
            if (config.getDestinations().size() == 1) txs.get(txIdx).getOutgoingTransfer().getDestinations().add(new MoneroDestination(config.getDestinations().get(0).getAddress(), amount)); // sweeping can create multiple withone address
            else txs.get(txIdx).getOutgoingTransfer().getDestinations().add(new MoneroDestination(config.getDestinations().get(destinationIdx++).getAddress(), amount));
          }
        }
      } else {
        LOGGER.warning("ignoring unexpected transaction list field: " + key + ": " + val);
      }
    }
    
    return txSet;
  }
  
  /**
   * Converts a rpc tx with a transfer to a tx set with a tx and transfer.
   * 
   * @param rpcTx is the rpc tx to build from
   * @param tx is an existing tx to continue initializing (optional)
   * @param isOutgoing specifies if the tx is outgoing if true, incoming if false, or decodes from type if undefined
   * @returns the initialized tx set with a tx
   */
  private static MoneroTxSet convertRpcTxToTxSet(Map<String, Object> rpcTx, MoneroTxWallet tx, Boolean isOutgoing, MoneroTxConfig config) {
    MoneroTxSet txSet = convertRpcTxSet(rpcTx);
    txSet.setTxs(Arrays.asList(convertRpcTxWithTransfer(rpcTx, tx, isOutgoing, config).setTxSet(txSet)));
    return txSet;
  }
  
  /**
   * Builds a MoneroTxWallet from a RPC tx.
   * 
   * @param rpcTx is the rpc tx to build from
   * @param tx is an existing tx to continue initializing (optional)
   * @param isOutgoing specifies if the tx is outgoing if true, incoming if false, or decodes from type if undefined
   * @returns the initialized tx with a transfer
   */
  @SuppressWarnings("unchecked")
  private static MoneroTxWallet convertRpcTxWithTransfer(Map<String, Object> rpcTx, MoneroTxWallet tx, Boolean isOutgoing, MoneroTxConfig config) {  // TODO: change everything to safe set
    
    // initialize tx to return
    if (tx == null) tx = new MoneroTxWallet();
    
    // initialize tx state from rpc type
    if (rpcTx.containsKey("type")) isOutgoing = decodeRpcType((String) rpcTx.get("type"), tx);
    else GenUtils.assertNotNull("Must indicate if tx is outgoing (true) xor incoming (false) since unknown", isOutgoing);
    
    // TODO: safe set
    // initialize remaining fields  TODO: seems this should be part of common function with DaemonRpc._convertRpcTx
    MoneroBlockHeader header = null;
    MoneroTransfer transfer = null;
    for (String key : rpcTx.keySet()) {
      Object val = rpcTx.get(key);
      if (key.equals("txid")) tx.setHash((String) val);
      else if (key.equals("tx_hash")) tx.setHash((String) val);
      else if (key.equals("fee")) tx.setFee((BigInteger) val);
      else if (key.equals("note")) { if (!"".equals(val)) tx.setNote((String) val); }
      else if (key.equals("tx_key")) tx.setKey((String) val);
      else if (key.equals("type")) { } // type already handled
      else if (key.equals("tx_size")) tx.setSize(((BigInteger) val).longValue());
      else if (key.equals("unlock_time")) tx.setUnlockTime(((BigInteger) val));
      else if (key.equals("weight")) tx.setWeight(((BigInteger) val).longValue());
      else if (key.equals("locked")) tx.setIsLocked((Boolean) val);
      else if (key.equals("tx_blob")) tx.setFullHex((String) val);
      else if (key.equals("tx_metadata")) tx.setMetadata((String) val);
      else if (key.equals("double_spend_seen")) tx.setIsDoubleSpendSeen((Boolean) val);
      else if (key.equals("block_height") || key.equals("height")) {
        if (tx.isConfirmed()) {
          if (header == null) header = new MoneroBlockHeader();
          header.setHeight(((BigInteger) val).longValue());
        }
      }
      else if (key.equals("timestamp")) {
        if (tx.isConfirmed()) {
          if (header == null) header = new MoneroBlockHeader();
          header.setTimestamp(((BigInteger) val).longValue());
        } else {
          // timestamp of unconfirmed tx is current request time
        }
      }
      else if (key.equals("confirmations")) tx.setNumConfirmations(((BigInteger) val).longValue());
      else if (key.equals("suggested_confirmations_threshold")) {
        if (transfer == null) transfer = (isOutgoing ? new MoneroOutgoingTransfer() : new MoneroIncomingTransfer()).setTx(tx);
        if (!isOutgoing) ((MoneroIncomingTransfer) transfer).setNumSuggestedConfirmations(((BigInteger) val).longValue());
      }
      else if (key.equals("amount")) {
        if (transfer == null) transfer = (isOutgoing ? new MoneroOutgoingTransfer() : new MoneroIncomingTransfer()).setTx(tx);
        transfer.setAmount((BigInteger) val);
      } else if (key.equals("amounts")) {}  // ignoring, amounts sum to amount
      else if (key.equals("address")) {
        if (!isOutgoing) {
          if (transfer == null) transfer = new MoneroIncomingTransfer().setTx(tx);
          ((MoneroIncomingTransfer) transfer).setAddress((String) val);
        }
      }
      else if (key.equals("payment_id")) {
        if (!"".equals(val) && !MoneroTxWallet.DEFAULT_PAYMENT_ID.equals(val)) tx.setPaymentId((String) val);  // default is undefined
      }
      else if (key.equals("subaddr_index")) GenUtils.assertTrue(rpcTx.containsKey("subaddr_indices")); // handled by subaddr_indices
      else if (key.equals("subaddr_indices")) {
        if (transfer == null) transfer = (isOutgoing ? new MoneroOutgoingTransfer() : new MoneroIncomingTransfer()).setTx(tx);
        List<Map<String, BigInteger>> rpcIndices = (List<Map<String, BigInteger>>) val;
        transfer.setAccountIndex(rpcIndices.get(0).get("major").intValue());
        if (isOutgoing) {
          List<Integer> subaddressIndices = new ArrayList<Integer>();
          for (Map<String, BigInteger> rpcIndex : rpcIndices) subaddressIndices.add(rpcIndex.get("minor").intValue());
          ((MoneroOutgoingTransfer) transfer).setSubaddressIndices(subaddressIndices);
        } else {
          GenUtils.assertEquals(1, rpcIndices.size());
          ((MoneroIncomingTransfer) transfer).setSubaddressIndex(rpcIndices.get(0).get("minor").intValue());
        }
      }
      else if (key.equals("destinations") || key.equals("recipients")) {
        GenUtils.assertTrue(isOutgoing);
        List<MoneroDestination> destinations = new ArrayList<MoneroDestination>();
        for (Map<String, Object> rpcDestination : (List<Map<String, Object>>) val) {
          MoneroDestination destination = new MoneroDestination();
          destinations.add(destination);
          for (String destinationKey : rpcDestination.keySet()) {
            if (destinationKey.equals("address")) destination.setAddress((String) rpcDestination.get(destinationKey));
            else if (destinationKey.equals("amount")) destination.setAmount((BigInteger) rpcDestination.get(destinationKey));
            else throw new MoneroError("Unrecognized transaction destination field: " + destinationKey);
          }
        }
        if (transfer == null) transfer = new MoneroOutgoingTransfer().setTx(tx);
        ((MoneroOutgoingTransfer) transfer).setDestinations(destinations);
      }
      else if (key.equals("multisig_txset") && val != null) {}  // handled elsewhere; this method only builds a tx wallet
      else if (key.equals("unsigned_txset") && val != null) {}  // handled elsewhere; this method only builds a tx wallet
      else if (key.equals("amount_in")) tx.setInputSum((BigInteger) val);
      else if (key.equals("amount_out")) tx.setOutputSum((BigInteger) val);
      else if (key.equals("change_address")) tx.setChangeAddress("".equals(val) ? null : (String) val);
      else if (key.equals("change_amount")) tx.setChangeAmount((BigInteger) val);
      else if (key.equals("dummy_outputs")) tx.setNumDummyOutputs(((BigInteger) val).intValue());
      else if (key.equals("extra")) tx.setExtraHex((String) val);
      else if (key.equals("ring_size")) tx.setRingSize(((BigInteger) val).intValue());
      else if (key.equals("spent_key_images")) {
        List<String> inputKeyImages = (List<String>) ((Map<String, Object>) val).get("key_images");
        GenUtils.assertTrue(tx.getInputs() == null);
        tx.setInputs(new ArrayList<MoneroOutput>());
        for (String inputKeyImage : inputKeyImages) {
          tx.getInputs().add(new MoneroOutputWallet().setKeyImage(new MoneroKeyImage().setHex(inputKeyImage)).setTx(tx));
        }
      }
      else if (key.equals("amounts_by_dest")) {
        GenUtils.assertTrue(isOutgoing);
        List<BigInteger> amountsByDest = (List<BigInteger>) ((Map<String, Object>) val).get("amounts");
        GenUtils.assertEquals(config.getDestinations().size(), amountsByDest.size());
        if (transfer == null) transfer = new MoneroOutgoingTransfer().setTx(tx);
        ((MoneroOutgoingTransfer) transfer).setDestinations(new ArrayList<>());
        for (int i = 0; i < config.getDestinations().size(); i++) {
          ((MoneroOutgoingTransfer) transfer).getDestinations().add(new MoneroDestination(config.getDestinations().get(i).getAddress(), amountsByDest.get(i)));
        }
      }
      else LOGGER.warning("ignoring unexpected transaction field with transfer: " + key + ": " + val);
    }
    
    // link block and tx
    if (header != null) tx.setBlock(new MoneroBlock(header).setTxs(tx));
    
    // initialize final fields
    if (transfer != null) {
      if (tx.isConfirmed() == null) tx.setIsConfirmed(false);
      if (!transfer.getTx().isConfirmed()) tx.setNumConfirmations(0l);
      if (isOutgoing) {
        tx.setIsOutgoing(true);
        if (tx.getOutgoingTransfer() != null) {
          if (((MoneroOutgoingTransfer) transfer).getDestinations() != null) tx.getOutgoingTransfer().setDestinations(null); // overwrite to avoid reconcile error TODO: remove after >18.3.1 when amounts_by_dest supported
          tx.getOutgoingTransfer().merge(transfer);
        }
        else tx.setOutgoingTransfer((MoneroOutgoingTransfer) transfer);
      } else {
        tx.setIsIncoming(true);
        tx.setIncomingTransfers(new ArrayList<MoneroIncomingTransfer>(Arrays.asList((MoneroIncomingTransfer) transfer)));
      }
    }
    
    // return initialized transaction
    return tx;
  }
  
  @SuppressWarnings("unchecked")
  private static MoneroTxWallet convertRpcTxWithOutput(Map<String, Object> rpcOutput) {
    
    // initialize tx
    MoneroTxWallet tx = new MoneroTxWallet();
    tx.setIsConfirmed(true);
    tx.setIsRelayed(true);
    tx.setIsFailed(false);
    
    // initialize output
    MoneroOutputWallet output = new MoneroOutputWallet().setTx(tx);
    for (String key : rpcOutput.keySet()) {
      Object val = rpcOutput.get(key);
      if (key.equals("amount")) output.setAmount((BigInteger) val);
      else if (key.equals("spent")) output.setIsSpent((Boolean) val);
      else if (key.equals("key_image")) { if (!"".equals(val)) output.setKeyImage(new MoneroKeyImage((String) val)); }
      else if (key.equals("global_index")) output.setIndex(((BigInteger) val).longValue());
      else if (key.equals("tx_hash")) tx.setHash((String) val);
      else if (key.equals("unlocked")) tx.setIsLocked(!(Boolean) val);
      else if (key.equals("frozen")) output.setIsFrozen((Boolean) val);
      else if (key.equals("pubkey")) output.setStealthPublicKey((String) val);
      else if (key.equals("subaddr_index")) {
        Map<String, BigInteger> rpcIndices = (Map<String, BigInteger>) val;
        output.setAccountIndex(rpcIndices.get("major").intValue());
        output.setSubaddressIndex(rpcIndices.get("minor").intValue());
      }
      else if (key.equals("block_height")) {
        long height = ((BigInteger) val).longValue();
        tx.setBlock(new MoneroBlock().setHeight(height).setTxs(tx));
      }
      else LOGGER.warning("ignoring unexpected transaction field with output: " + key + ": " + val);
    }
    
    // initialize tx with output
    List<MoneroOutput> outputs = new ArrayList<MoneroOutput>();
    outputs.add(output);
    tx.setOutputs(outputs);
    return tx;
  }
  
  @SuppressWarnings("unchecked")
  private static MoneroTxSet convertRpcDescribeTransfer(Map<String, Object> rpcDescribeTransferResult) {
    MoneroTxSet txSet = new MoneroTxSet();
    for (String key : rpcDescribeTransferResult.keySet()) {
      Object val = rpcDescribeTransferResult.get(key);
      if (key.equals("desc")) {
        txSet.setTxs(new ArrayList<MoneroTxWallet>());
        for (Map<String, Object> txMap : (List<Map<String, Object>>) val) {
          MoneroTxWallet tx = convertRpcTxWithTransfer(txMap, null, true, null);
          tx.setTxSet(txSet);
          txSet.getTxs().add(tx);
        }
      }
      else if (key.equals("summary")) { } // TODO: support tx set summary fields?
      else LOGGER.warning("ignoring unexpected describe transfer field: " + key + ": " + val);
    }
    return txSet;
  }
  
  /**
   * Decodes a "type" from monero-wallet-rpc to initialize type and state
   * fields in the given transaction.
   * 
   * TODO: these should be safe set
   * 
   * @param rpcType is the type to decode
   * @param tx is the transaction to decode known fields to
   * @return {boolean} true if the rpc type indicates outgoing xor incoming
   */
  private static boolean decodeRpcType(String rpcType, MoneroTxWallet tx) {
    boolean isOutgoing;
    if (rpcType.equals("in")) {
      isOutgoing = false;
      tx.setIsConfirmed(true);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setRelay(true);
      tx.setIsFailed(false);
      tx.setIsMinerTx(false);
    } else if (rpcType.equals("out")) {
      isOutgoing = true;
      tx.setIsConfirmed(true);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setRelay(true);
      tx.setIsFailed(false);
      tx.setIsMinerTx(false);
    } else if (rpcType.equals("pool")) {
      isOutgoing = false;
      tx.setIsConfirmed(false);
      tx.setInTxPool(true);
      tx.setIsRelayed(true);
      tx.setRelay(true);
      tx.setIsFailed(false);
      tx.setIsMinerTx(false);  // TODO: but could it be?
    } else if (rpcType.equals("pending")) {
      isOutgoing = true;
      tx.setIsConfirmed(false);
      tx.setInTxPool(true);
      tx.setIsRelayed(true);
      tx.setRelay(true);
      tx.setIsFailed(false);
      tx.setIsMinerTx(false);
    } else if (rpcType.equals("block")) {
      isOutgoing = false;
      tx.setIsConfirmed(true);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setRelay(true);
      tx.setIsFailed(false);
      tx.setIsMinerTx(true);
    } else if (rpcType.equals("failed")) {
      isOutgoing = true;
      tx.setIsConfirmed(false);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setRelay(true);
      tx.setIsFailed(true);
      tx.setIsMinerTx(false);
    } else {
      throw new MoneroError("Unrecognized transfer type: " + rpcType);
    }
    return isOutgoing;
  }
  
  /**
   * Merges a transaction into a unique set of transactions.
   *
   * @param tx is the transaction to merge into the existing txs
   * @param txMap maps tx hashes to txs
   * @param blockMap maps block heights to blocks
   */
  private static void mergeTx(MoneroTxWallet tx, Map<String, MoneroTxWallet> txMap, Map<Long, MoneroBlock> blockMap) {
    GenUtils.assertNotNull(tx.getHash());

    // merge tx
    MoneroTxWallet aTx = txMap.get(tx.getHash());
    if (aTx == null) txMap.put(tx.getHash(), tx); // cache new tx
    else aTx.merge(tx); // merge with existing tx

    // merge tx's block if confirmed
    if (tx.getHeight() != null) {
      MoneroBlock aBlock = blockMap.get(tx.getHeight());
      if (aBlock == null) blockMap.put(tx.getHeight(), tx.getBlock()); // cache new block
      else aBlock.merge(tx.getBlock()); // merge with existing block
    }
  }
  
  /**
   * Compares two transactions by their height.
   */
  private static class TxHeightComparator implements Comparator<MoneroTx> {
    @Override
    public int compare(MoneroTx tx1, MoneroTx tx2) {
      if (tx1.getHeight() == null && tx2.getHeight() == null) return 0; // both unconfirmed
      else if (tx1.getHeight() == null) return 1;   // tx1 is unconfirmed
      else if (tx2.getHeight() == null) return -1;  // tx2 is unconfirmed
      int diff = tx1.getHeight().compareTo(tx2.getHeight());
      if (diff != 0) return diff;
      return tx1.getBlock().getTxs().indexOf(tx1) - tx2.getBlock().getTxs().indexOf(tx2); // txs are in the same block so retain their original order
    }
  }
  
  /**
   * Compares two transfers by ascending account and subaddress indices.
   */
  public static class IncomingTransferComparator implements Comparator<MoneroIncomingTransfer> {
    @Override
    public int compare(MoneroIncomingTransfer t1, MoneroIncomingTransfer t2) {
      
      // compare by height
      int heightComparison = TX_HEIGHT_COMPARATOR.compare(t1.getTx(), t2.getTx());
      if (heightComparison != 0) return heightComparison;
      
      // compare by account and subaddress index
      if (t1.getAccountIndex() < t2.getAccountIndex()) return -1;
      else if (t1.getAccountIndex() == t2.getAccountIndex()) return t1.getSubaddressIndex().compareTo(t2.getSubaddressIndex());
      return 1;
    }
  }
  
  /**
   * Compares two outputs by ascending account and subaddress indices.
   */
  public static class OutputComparator implements Comparator<MoneroOutput> {
    
    @Override
    public int compare(MoneroOutput o1, MoneroOutput o2) {
      MoneroOutputWallet ow1 = (MoneroOutputWallet) o1;
      MoneroOutputWallet ow2 = (MoneroOutputWallet) o2;
      
      // compare by height
      int heightComparison = TX_HEIGHT_COMPARATOR.compare(ow1.getTx(), ow2.getTx());
      if (heightComparison != 0) return heightComparison;
      
      // compare by account index, subaddress index, output index, then key image hex
      int compare = ow1.getAccountIndex().compareTo(ow2.getAccountIndex());
      if (compare != 0) return compare;
      compare = ow1.getSubaddressIndex().compareTo(ow2.getSubaddressIndex());
      if (compare != 0) return compare;
      compare = ow1.getIndex().compareTo(ow2.getIndex());
      if (compare != 0) return compare;
      return ow1.getKeyImage().getHex().compareTo(ow2.getKeyImage().getHex());
    }
  }
}
