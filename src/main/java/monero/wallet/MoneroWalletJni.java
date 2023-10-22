package monero.wallet;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.type.TypeReference;
import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.*;
import monero.wallet.model.*;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.math.BigInteger;
import java.util.*;
import java.util.logging.Logger;

public abstract class MoneroWalletJni extends MoneroWalletDefault {

    // ----------------------------- PRIVATE SETUP ------------------------------

    // load monero-project C++ as a dynamic library
    static {
        MoneroUtils.loadNativeLibrary();
    }

    // class variables
    protected static final Logger LOGGER = Logger.getLogger(MoneroWalletJni.class.getName());

    protected static final long DEFAULT_SYNC_PERIOD_IN_MS = 10000; // default period betweeen syncs in ms

    // instance variables
    protected long jniWalletHandle;                 // memory address of the wallet in c++; this variable is read directly by name in c++
    protected long jniListenerHandle;               // memory address of the wallet listener in c++; this variable is read directly by name in c++
    protected MoneroWalletJni.WalletJniListener jniListener;        // receives notifications from jni c++
    private String password;
    protected boolean isClosed;                     // whether or not wallet is closed

    /**
     * Private constructor with a handle to the memory address of the wallet in c++.
     *
     * @param jniWalletHandle memory address of the wallet in c++
     * @param password password of the wallet instance
     */
    protected MoneroWalletJni(long jniWalletHandle, String password) {
        super();
        this.jniWalletHandle = jniWalletHandle;
        this.jniListener = new MoneroWalletJni.WalletJniListener();
        this.password = password;
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
        throw new NotImplementedException();
    }
    public static MoneroWalletJni openWallet(String path, String password, MoneroNetworkType networkType) { return openWallet(path, password, networkType, (MoneroRpcConnection) null); }
    public static MoneroWalletJni openWallet(String path, String password, MoneroNetworkType networkType, String daemonUri) { return openWallet(path, password, networkType, daemonUri == null ? null : new MoneroRpcConnection(daemonUri)); }

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
    public static MoneroWalletJni openWalletData(String password, MoneroNetworkType networkType, byte[] keysData, byte[] cacheData, MoneroRpcConnection daemonConnection) {
        throw new NotImplementedException();
    }

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
     * &nbsp;&nbsp; server - MoneroRpcConnection to a monero daemon (optional)<br>
     * </p>
     *
     * @param config configures the wallet to open
     * @return the wallet instance
     */
    public static MoneroWalletJni openWallet(MoneroWalletConfig config) {
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

        // read wallet data from disk if not provided
        if (config.getKeysData() == null) {
            return openWallet(config.getPath(), config.getPassword(), config.getNetworkType(), config.getServer());
        } else {
            return openWalletData(config.getPassword(), config.getNetworkType(), config.getKeysData(), config.getCacheData(), config.getServer());
        }
    }

    /**
     * <p>Create a wallet using JNI bindings to wallet2.h.</p>
     *
     * <p>Examples:</p>
     *
     * <code>
     * // create stagenet wallet with randomly generated seed<br>
     * MoneroWallet wallet1 = MoneroWalletJni.createWallet(new MoneroWalletConfig()<br>
     * &nbsp;&nbsp; .setPath("/mywallets/wallet1")<br>
     * &nbsp;&nbsp; .setPassword("supersecretpassword")<br>
     * &nbsp;&nbsp; .setNetworkType(MoneroNetworkType.STAGENET)<br>
     * &nbsp;&nbsp; .setServerUri("http://localhost:38081") // leave blank for offline wallet<br>
     * &nbsp;&nbsp; .setServerUsername("superuser")<br>
     * &nbsp;&nbsp; .setServerPassword("abctesting123"));<br><br>
     *
     * // restore mainnet wallet from seed<br>
     * MoneroWallet wallet2 = MoneroWalletJni.createWallet(new MoneroWalletConfig()<br>
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
    public static MoneroWalletJni createWallet(MoneroWalletConfig config) {

        // validate config
        if (config == null) throw new MoneroError("Must specify config to open wallet");
        if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
        if (config.getPath() != null && !config.getPath().isEmpty() && MoneroWalletJni.walletExists(config.getPath())) throw new MoneroError("Wallet already exists: " + config.getPath());
        if (config.getSeed() != null && (config.getPrimaryAddress() != null || config.getPrivateViewKey() != null || config.getPrivateSpendKey() != null)) {
            throw new MoneroError("Wallet may be initialized with a seed or keys but not both");
        }
        if (Boolean.TRUE.equals(config.getSaveCurrent() != null)) throw new MoneroError("Cannot save current wallet when creating full wallet");

        // set server from connection manager if provided
        if (config.getConnectionManager() != null) {
            if (config.getServer() != null) throw new MoneroError("Wallet can be initialized with a server or connection manager but not both");
            config.setServer(config.getConnectionManager().getConnection());
        }

        // create wallet
        MoneroWalletJni wallet;
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
        wallet.setConnectionManager(config.getConnectionManager());
        return wallet;
    }

    protected static MoneroWalletJni createWalletFromSeed(MoneroWalletConfig config) {
        throw new NotImplementedException();
    }

    protected static MoneroWalletJni createWalletFromKeys(MoneroWalletConfig config) {
        throw new NotImplementedException();
    }

    protected static MoneroWalletJni createWalletRandom(MoneroWalletConfig config) {
        throw new NotImplementedException();
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
    public long getRestoreHeight() {
        assertNotClosed();
        return getRestoreHeightJni();
    }

    /**
     * Set the height of the first block that the wallet scans.
     *
     * @param syncHeight is the height of the first block that the wallet scans
     */
    public void setRestoreHeight(long syncHeight) {
        assertNotClosed();
        setRestoreHeightJni(syncHeight);
    }

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

    @Override
    public void addListener(MoneroWalletListenerI listener) {
        assertNotClosed();
        super.addListener(listener);
        refreshListening();
    }

    @Override
    public void removeListener(MoneroWalletListenerI listener) {
        assertNotClosed();
        super.removeListener(listener);
        refreshListening();
    }

    @Override
    public Set<MoneroWalletListenerI> getListeners() {
        assertNotClosed();
        return super.getListeners();
    }

    @Override
    public boolean isViewOnly() {
        assertNotClosed();
        return isViewOnlyJni();
    }

    @Override
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

    @Override
    public void setProxyUri(String uri) {
        assertNotClosed();
        if (uri == null) uri = "";
        try {
            setProxyJni(uri);
        } catch (Exception e) {
            e.printStackTrace();
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public MoneroRpcConnection getDaemonConnection() {
        assertNotClosed();
        try {
            String[] vals = getDaemonConnectionJni();
            return vals == null ? null : new MoneroRpcConnection(vals[0], vals[1], vals[2]);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public boolean isConnectedToDaemon() {
        assertNotClosed();
        try {
            return isConnectedToDaemonJni();
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
    public String getSeed() {
        assertNotClosed();
        String seed = getSeedJni();
        if ("".equals(seed)) return null;
        return seed;
    }

    @Override
    public String getSeedLanguage() {
        assertNotClosed();
        String seedLanguage = getSeedLanguageJni();
        if ("".equals(seedLanguage)) return null;
        return seedLanguage;
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
    public MoneroIntegratedAddress getIntegratedAddress(String standardAddress, String paymentId) {
        assertNotClosed();
        try {
            String integratedAddressJson = getIntegratedAddressJni(standardAddress, paymentId);
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
            if (listener != null) removeListener(listener); // unregister listener
        }
    }

    @Override
    public void startSyncing(Long syncPeriodInMs) {
        assertNotClosed();
        try {
            startSyncingJni(syncPeriodInMs == null ? DEFAULT_SYNC_PERIOD_IN_MS : syncPeriodInMs);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public void stopSyncing() {
        assertNotClosed();
        try {
            stopSyncingJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public void scanTxs(Collection<String> txHashes) {
        assertNotClosed();
        String[] txMetadatasArr = txHashes.toArray(new String[txHashes.size()]); // convert to array for jni
        try {
            scanTxsJni(txMetadatasArr);
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
    public void setSubaddressLabel(int accountIdx, int subaddressIdx, String label) {
        assertNotClosed();
        if (label == null) label = "";
        setSubaddressLabelJni(accountIdx, subaddressIdx, label);
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
    public String exportOutputs(boolean all) {
        assertNotClosed();
        String outputsHex = exportOutputsJni(all);
        return outputsHex.isEmpty() ? null : outputsHex;
    }

    @Override
    public int importOutputs(String outputsHex) {
        assertNotClosed();
        return importOutputsJni(outputsHex);
    }

    @Override
    public List<MoneroKeyImage> exportKeyImages(boolean all) {
        assertNotClosed();
        String keyImagesJson = exportKeyImagesJni(all);
        List<MoneroKeyImage> keyImages = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, keyImagesJson, MoneroWalletJni.KeyImagesContainer.class).keyImages;
        return keyImages;
    }

    @Override
    public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
        assertNotClosed();

        // wrap and serialize key images in container for jni
        MoneroWalletJni.KeyImagesContainer keyImageContainer = new MoneroWalletJni.KeyImagesContainer(keyImages);
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
    public void freezeOutput(String keyImage) {
        assertNotClosed();
        try {
            freezeOutputJni(keyImage);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public void thawOutput(String keyImage) {
        assertNotClosed();
        try {
            thawOutputJni(keyImage);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public boolean isOutputFrozen(String keyImage) {
        assertNotClosed();
        try {
            return isOutputFrozenJni(keyImage);
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
        List<MoneroTxSet> txSets = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, txSetsJson, MoneroWalletJni.TxSetsContainer.class).txSets;

        // return txs
        List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
        for (MoneroTxSet txSet : txSets) txs.addAll(txSet.getTxs());
        return txs;
    }

    @Override
    public List<MoneroTxWallet> sweepDust(boolean relay) {
        assertNotClosed();
        try {
            String txSetJson = sweepDustJni(relay);
            MoneroTxSet txSet = JsonUtils.deserialize(txSetJson, MoneroTxSet.class);
            if (txSet.getTxs() == null) txSet.setTxs(new ArrayList<MoneroTxWallet>());
            return txSet.getTxs();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public MoneroTxSet describeTxSet(MoneroTxSet txSet) {
        assertNotClosed();
        txSet = new MoneroTxSet()
                .setUnsignedTxHex(txSet.getUnsignedTxHex())
                .setSignedTxHex(txSet.getSignedTxHex())
                .setMultisigTxHex(txSet.getMultisigTxHex());
        String describedTxSetJson;
        try {
            describedTxSetJson = describeTxSetJni(JsonUtils.serialize(txSet));
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
        return JsonUtils.deserialize(describedTxSetJson, MoneroTxSet.class);
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
        List<MoneroAddressBookEntry> entries = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, entriesJson, MoneroWalletJni.AddressBookEntriesContainer.class).entries;
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
    public String getPaymentUri(MoneroTxConfig request) {
        assertNotClosed();
        try {
            return getPaymentUriJni(JsonUtils.serialize(request));
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


    // ----------------------------- MULTISIG ------------------------------

    @Override
    public boolean isMultisigImportNeeded() {
        assertNotClosed();
        return isMultisigImportNeededJni();
    }

    @Override
    public MoneroMultisigInfo getMultisigInfo() {
        assertNotClosed();
        try {
            String multisigInfoJson = getMultisigInfoJni();
            return JsonUtils.deserialize(multisigInfoJson, MoneroMultisigInfo.class);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public String prepareMultisig() {
        assertNotClosed();
        return prepareMultisigJni();
    }

    @Override
    public String makeMultisig(List<String> multisigHexes, int threshold, String password) {
        assertNotClosed();
        try {
            return makeMultisigJni(multisigHexes.toArray(new String[multisigHexes.size()]), threshold, password);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password) {
        assertNotClosed();
        try {
            String initMultisigResultJson = exchangeMultisigKeysJni(multisigHexes.toArray(new String[multisigHexes.size()]), password);
            return JsonUtils.deserialize(initMultisigResultJson, MoneroMultisigInitResult.class);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public String exportMultisigHex() {
        assertNotClosed();
        try {
            return exportMultisigHexJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public int importMultisigHex(List<String> multisigHexes) {
        assertNotClosed();
        try {
            return importMultisigHexJni(multisigHexes.toArray(new String[multisigHexes.size()]));
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    @Override
    public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex) {
        assertNotClosed();
        try {
            String signMultisigResultJson = signMultisigTxHexJni(multisigTxHex);
            return JsonUtils.deserialize(signMultisigResultJson, MoneroMultisigSignResult.class);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }


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
    public void save() {
        assertNotClosed();
        saveJni();
    }

    @Override
    public void close(boolean save) {
        super.close(save);
        if (isClosed) return; // closing a closed wallet has no effect
        isClosed = true;
        password = null;
        refreshListening();
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


    // -------------------------------- LISTENER --------------------------------

    /**
     * Receives notifications directly from jni c++.
     */
    @SuppressWarnings("unused") // called directly from jni c++
    protected class WalletJniListener {

        public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
            for (MoneroWalletListenerI listener : getListeners()) listener.onSyncProgress(height, startHeight, endHeight, percentDone, message);
        }

        public void onNewBlock(long height) {
            for (MoneroWalletListenerI listener : getListeners()) listener.onNewBlock(height);
        }

        public void onBalancesChanged(String newBalanceStr, String newUnlockedBalanceStr) {
            for (MoneroWalletListenerI listener : getListeners()) listener.onBalancesChanged(new BigInteger(newBalanceStr), new BigInteger(newUnlockedBalanceStr));
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
            for (MoneroWalletListenerI listener : getListeners()) listener.onOutputReceived((MoneroOutputWallet) tx.getOutputs().get(0));
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
            for (MoneroWalletListenerI listener : getListeners()) listener.onOutputSpent((MoneroOutputWallet) tx.getInputs().get(0));
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

    protected static class KeyImagesContainer {
        public List<MoneroKeyImage> keyImages;
        @SuppressWarnings("unused") public KeyImagesContainer() { } // necessary for serialization
        public KeyImagesContainer(List<MoneroKeyImage> keyImages) { this.keyImages = keyImages; };
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
     * Enables or disables listening in the c++ wallet.
     */
    protected void refreshListening() {
        boolean isEnabled = listeners.size() > 0;
        if (jniListenerHandle == 0 && !isEnabled || jniListenerHandle > 0 && isEnabled) return; // no difference
        jniListenerHandle = setListenerJni(isEnabled ? jniListener : null);
    }

    protected void assertNotClosed() {
        if (isClosed) throw new MoneroError("Wallet is closed");
    }

    protected static MoneroBlock sanitizeBlock(MoneroBlock block) {
        for (MoneroTx tx : block.getTxs()) sanitizeTxWallet((MoneroTxWallet) tx);
        return block;
    }

    protected static MoneroTxWallet sanitizeTxWallet(MoneroTxWallet tx) {
        return tx;
    }

    // ------------------------------ NATIVE METHODS ----------------------------

    protected native static boolean walletExistsJni(String path);

    protected native static long openWalletJni(String path, String password, int networkType);

    protected native static long openWalletDataJni(String password, int networkType, byte[] keysData, byte[] cacheData);

    protected native static long createWalletJni(String walletConfigJson);

    protected native long getHeightJni();

    protected native long getRestoreHeightJni();

    protected native void setRestoreHeightJni(long height);

    protected native long getDaemonHeightJni();

    protected native long getDaemonMaxPeerHeightJni();

    protected native long getHeightByDateJni(int year, int month, int day);

    protected native boolean isViewOnlyJni();

    protected native void setDaemonConnectionJni(String uri, String username, String password);

    protected native void setProxyJni(String uri);

    protected native String[] getDaemonConnectionJni(); // returns [uri, username, password]

    protected native boolean isConnectedToDaemonJni();

    protected native boolean isDaemonSyncedJni();

    protected native boolean isSyncedJni();

    protected native int getNetworkTypeJni();

    protected native String getVersionJni();

    protected native String getPathJni();

    protected native String getSeedJni();

    protected native String getSeedLanguageJni();

    protected static native String[] getSeedLanguagesJni();

    protected native String getPublicViewKeyJni();

    protected native String getPrivateViewKeyJni();

    protected native String getPublicSpendKeyJni();

    protected native String getPrivateSpendKeyJni();

    protected native String getAddressJni(int accountIdx, int subaddressIdx);

    protected native String getAddressIndexJni(String address);

    protected native String getIntegratedAddressJni(String standardAddress, String paymentId);

    protected native String decodeIntegratedAddressJni(String integratedAddress);

    protected native long setListenerJni(WalletJniListener listener);

    protected native Object[] syncJni(long startHeight);

    protected native void startSyncingJni(long syncPeriodInMs);

    protected native void stopSyncingJni();

    protected native void scanTxsJni(String[] txHashes);

    protected native void rescanSpentJni();

    protected native void rescanBlockchainJni();

    protected native String getBalanceWalletJni();

    protected native String getBalanceAccountJni(int accountIdx);

    protected native String getBalanceSubaddressJni(int accountIdx, int subaddressIdx);

    protected native String getUnlockedBalanceWalletJni();

    protected native String getUnlockedBalanceAccountJni(int accountIdx);

    protected native String getUnlockedBalanceSubaddressJni(int accountIdx, int subaddressIdx);

    protected native String getAccountsJni(boolean includeSubaddresses, String tag);

    protected native String getAccountJni(int accountIdx, boolean includeSubaddresses);

    protected native String createAccountJni(String label);

    protected native String getSubaddressesJni(int accountIdx, int[] subaddressIndices);

    protected native String createSubaddressJni(int accountIdx, String label);

    protected native void setSubaddressLabelJni(int accountIdx, int subaddressIdx, String label);

    protected native String getTxsJni(String txQueryJson);

    protected native String getTransfersJni(String transferQueryJson);

    protected native String getOutputsJni(String outputQueryJson);

    protected native String exportOutputsJni(boolean all);

    protected native int importOutputsJni(String outputsHex);

    protected native String exportKeyImagesJni(boolean all);

    protected native String importKeyImagesJni(String keyImagesJson);

    protected native String[] relayTxsJni(String[] txMetadatas);

    protected native void freezeOutputJni(String KeyImage);

    protected native void thawOutputJni(String keyImage);

    protected native boolean isOutputFrozenJni(String keyImage);

    protected native String createTxsJni(String txConfigJson);

    protected native String sweepUnlockedJni(String txConfigJson);

    protected native String sweepOutputJni(String txConfigJson);

    protected native String sweepDustJni(boolean doNotRelay);

    protected native String describeTxSetJni(String txSetJson);

    protected native String signTxsJni(String unsignedTxHex);

    protected native String[] submitTxsJni(String signedTxHex);

    protected native String[] getTxNotesJni(String[] txHashes);

    protected native void setTxNotesJni(String[] txHashes, String[] notes);

    protected native String signMessageJni(String msg, int signatureType, int accountIdx, int subaddressIdx);

    protected native String verifyMessageJni(String msg, String address, String signature);

    protected native String getTxKeyJni(String txHash);

    protected native String checkTxKeyJni(String txHash, String txKey, String address);

    protected native String getTxProofJni(String txHash, String address, String message);

    protected native String checkTxProofJni(String txHash, String address, String message, String signature);

    protected native String getSpendProofJni(String txHash, String message);

    protected native boolean checkSpendProofJni(String txHash, String message, String signature);

    protected native String getReserveProofWalletJni(String message);

    protected native String getReserveProofAccountJni(int accountIdx, String amount, String message);

    protected native String checkReserveProofJni(String address, String message, String signature);

    protected native String getAddressBookEntriesJni(int[] indices);

    protected native int addAddressBookEntryJni(String address, String description);

    protected native void editAddressBookEntryJni(int index, boolean setAddress, String address, boolean setDescription, String description);

    protected native void deleteAddressBookEntryJni(int entryIdx);

    protected native String getPaymentUriJni(String sendRequestJson);

    protected native String parsePaymentUriJni(String uri);

    protected native String getAttributeJni(String key);

    protected native void setAttributeJni(String key, String val);

    protected native void startMiningJni(long numThreads, boolean backgroundMining, boolean ignoreBattery);

    protected native void stopMiningJni();

    protected native boolean isMultisigImportNeededJni();

    protected native String getMultisigInfoJni();

    protected native String prepareMultisigJni();

    protected native String makeMultisigJni(String[] multisigHexes, int threshold, String password);

    protected native String exchangeMultisigKeysJni(String[] multisigHexes, String password);

    protected native String exportMultisigHexJni();

    protected native int importMultisigHexJni(String[] multisigHexes);

    protected native String signMultisigTxHexJni(String multisigTxHex);

    protected native String[] submitMultisigTxHexJni(String signedMultisigTxHex);

    protected native byte[] getKeysFileBufferJni(String password, boolean viewOnly);

    protected native byte[] getCacheFileBufferJni();

    protected native void changePasswordJni(String oldPassword, String newPassword);

    protected native void moveToJni(String path, String password);

    protected native void saveJni();

    protected native void closeJni(boolean save);

}
