package monero.wallet;

/*
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

import common.utils.GenUtils;
import common.utils.JsonUtils;
import java.math.BigInteger;
import java.util.*;
import java.util.logging.Logger;
import monero.common.MoneroError;
import monero.common.MoneroLwsConnection;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.model.*;

/**
 * Implements a Monero wallet using fully client-side JNI bindings to monero-project's wallet2 in C++.
 */
public class MoneroWalletLws extends MoneroWalletJni {

    // ----------------------------- PRIVATE SETUP ------------------------------

    // class variables
    protected static final Logger LOGGER = Logger.getLogger(MoneroWalletLws.class.getName());

    private MoneroLwsConnection lwsConnection;

    /**
     * Private constructor with a handle to the memory address of the wallet in c++.
     *
     * @param jniWalletHandle memory address of the wallet in c++
     * @param password        password of the wallet instance
     */
    protected MoneroWalletLws(long jniWalletHandle, String password) {
        super(jniWalletHandle, password);
    }
    // --------------------- LWS METHODS UTILITIES ------------------------

    private void rescanAddresses(Long height, List<String> addresses)
    {
        int size = addresses.size();
        lwsConnection.rescan(height, addresses.toArray(new String[size]));
    }

    // --------------------- WALLET MANAGEMENT UTILITIES ------------------------

    /**
     * Open an existing wallet using JNI bindings to wallet2.h.
     *
     * @param path is the path to the wallet file to open
     * @param password is the password of the wallet file to open
     * @param networkType is the wallet's network type
     * @param daemonConnection is connection configuration to a daemon (default = an unconnected wallet)
     * @return the opened wallet
     */
    public static MoneroWalletLws openWallet(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection) { return openWallet(path, password, networkType, daemonConnection, null); }
    public static MoneroWalletLws openWallet(String path, String password, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, MoneroLwsConnection lwsConnection) {
        if (!walletExistsJni(path)) throw new MoneroError("Wallet does not exist at path: " + path);
        if (networkType == null) throw new MoneroError("Must provide a network type");
        long jniWalletHandle = openWalletJni(path, password, networkType.ordinal());
        MoneroWalletLws wallet = new MoneroWalletLws(jniWalletHandle, password);
        if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
        if (lwsConnection != null) wallet.setLwsConnection(lwsConnection);
        return wallet;
    }
    public static MoneroWalletLws openWallet(String path, String password, MoneroNetworkType networkType) { return openWallet(path, password, networkType, (MoneroRpcConnection) null, null); }

    public static MoneroWalletLws openWallet(String path, String password, MoneroNetworkType networkType, String daemonUri) { return openWallet(path, password, networkType, daemonUri, null);}
    public static MoneroWalletLws openWallet(String path, String password, MoneroNetworkType networkType, String daemonUri, String lwsUri) { return openWallet(path, password, networkType, daemonUri == null ? null : new MoneroRpcConnection(daemonUri), lwsUri == null ? null : new MoneroLwsConnection(lwsUri)); }

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
    public static MoneroWalletLws openWalletData(String password, MoneroNetworkType networkType, byte[] keysData, byte[] cacheData, MoneroRpcConnection daemonConnection) {
        if (networkType == null) throw new MoneroError("Must provide a network type");
        long jniWalletHandle = openWalletDataJni(password, networkType.ordinal(), keysData, cacheData);
        MoneroWalletLws wallet = new MoneroWalletLws(jniWalletHandle, password);
        if (daemonConnection != null) wallet.setDaemonConnection(daemonConnection);
        return wallet;
    }

    /**
     * <p>Open an existing wallet using JNI bindings to wallet2.h.</p>
     *
     * <p>Example:</p>
     *
     * <code>
     * MoneroWallet wallet = MoneroWalletLws.openWallet(new MoneroWalletConfig()<br>
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
    public static MoneroWalletLws openWallet(MoneroWalletConfig config) {

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
     * MoneroWallet wallet1 = MoneroWalletLws.createWallet(new MoneroWalletConfig()<br>
     * &nbsp;&nbsp; .setPath("/mywallets/wallet1")<br>
     * &nbsp;&nbsp; .setPassword("supersecretpassword")<br>
     * &nbsp;&nbsp; .setNetworkType(MoneroNetworkType.STAGENET)<br>
     * &nbsp;&nbsp; .setServerUri("http://localhost:38081") // leave blank for offline wallet<br>
     * &nbsp;&nbsp; .setServerUsername("superuser")<br>
     * &nbsp;&nbsp; .setServerPassword("abctesting123"));<br><br>
     *
     * // restore mainnet wallet from seed<br>
     * MoneroWallet wallet2 = MoneroWalletLws.createWallet(new MoneroWalletConfig()<br>
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
    public static MoneroWalletLws createWallet(MoneroWalletConfig config) {

        // validate config
        if (config == null) throw new MoneroError("Must specify config to open wallet");
        if (config.getNetworkType() == null) throw new MoneroError("Must specify a network type: 'mainnet', 'testnet' or 'stagenet'");
        if (config.getPath() != null && !config.getPath().isEmpty() && MoneroWalletLws.walletExists(config.getPath())) throw new MoneroError("Wallet already exists: " + config.getPath());
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
        MoneroWalletLws wallet;
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

    protected static MoneroWalletLws createWalletFromSeed(MoneroWalletConfig config) {
        if (config.getRestoreHeight() == null) config.setRestoreHeight(0L);
        long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
        return new MoneroWalletLws(jniWalletHandle, config.getPassword());
    }

    protected static MoneroWalletLws createWalletFromKeys(MoneroWalletConfig config) {
        if (config.getRestoreHeight() == null) config.setRestoreHeight(0L);
        if (config.getLanguage() == null) config.setLanguage(DEFAULT_LANGUAGE);
        try {
            long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
            return new MoneroWalletLws(jniWalletHandle, config.getPassword());
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    protected static MoneroWalletLws createWalletRandom(MoneroWalletConfig config) {
        if (config.getLanguage() == null) config.setLanguage(DEFAULT_LANGUAGE);
        long jniWalletHandle = createWalletJni(serializeWalletConfig(config));
        return new MoneroWalletLws(jniWalletHandle, config.getPassword());
    }

    public void setLwsConnection(MoneroLwsConnection lwsConnection)
    {
        this.lwsConnection = lwsConnection;
    }

    public MoneroLwsConnection getLwsConnection()
    {
        return this.lwsConnection;
    }

    @Override
    public long getHeight() {
        // TO DO get height by get_address_info

        return 0L;
    }

    @Override
    public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener) {
        // TO DO if admin rescan from start height

        return super.sync(startHeight, listener);
    }

    private void startSyncingAccount(MoneroAccount account)
    {
        List<MoneroSubaddress> subaddresses = account.getSubaddresses();
        int size = subaddresses.size() + 1;
        String[] addresses = new String[size];
        addresses[0] = account.getPrimaryAddress();
        int i = 1;
        for(MoneroSubaddress subaddress : subaddresses)
        {
            addresses[i] = subaddress.getAddress();
            i++;
        }

        lwsConnection.modifyAccountStatus("active", addresses);
    }

    private void startSyncingAccounts()
    {
        stopSyncingAccounts(getAccounts());
    }

    private void startSyncingAccounts(List<MoneroAccount> accounts)
    {
        for(MoneroAccount account : accounts)
        {
            stopSyncingAccount(account);
        }
    }

    @Override
    public void startSyncing(Long syncPeriodInMs) {
        startSyncingAccounts();
        super.startSyncing(syncPeriodInMs);
    }

    private void stopSyncingAccount(MoneroAccount account)
    {
        List<MoneroSubaddress> subaddresses = account.getSubaddresses();
        int size = subaddresses.size() + 1;
        String[] addresses = new String[size];
        addresses[0] = account.getPrimaryAddress();
        int i = 1;
        for(MoneroSubaddress subaddress : subaddresses)
        {
            addresses[i] = subaddress.getAddress();
            i++;
        }

        lwsConnection.modifyAccountStatus("inactive", addresses);
    }

    private void stopSyncingAccounts()
    {
        stopSyncingAccounts(getAccounts());
    }

    private void stopSyncingAccounts(List<MoneroAccount> accounts)
    {
        for(MoneroAccount account : accounts)
        {
            stopSyncingAccount(account);
        }
    }

    @Override
    public void stopSyncing() {
        // TO DO if admin modify account status to inactive
        stopSyncingAccounts();

        super.stopSyncing();
    }

    @Override
    public void rescanSpent() {
        // TO DO if admin rescan addresses
        super.rescanSpent();
    }

    private void rescanAccount(MoneroAccount account)
    {
        rescanAccount(account, 1L);
    }

    private void rescanAccount(MoneroAccount account, Long height)
    {
        List<MoneroSubaddress> subaddresses = account.getSubaddresses();
        int size = subaddresses.size() + 1;
        String[] addresses = new String[size];
        addresses[0] = account.getPrimaryAddress();
        int i = 1;
        for(MoneroSubaddress subaddress : subaddresses)
        {
            addresses[i] = subaddress.getAddress();
            i++;
        }

        lwsConnection.rescan(height, addresses);
    }

    private void rescanAccounts()
    {
        rescanAccounts(1L);
    }

    private void rescanAccounts(Long height)
    {
        rescanAccounts(getAccounts(), height);
    }

    private void rescanAccounts(List<MoneroAccount> accounts)
    {
        rescanAccounts(accounts, 1L);
    }

    private void rescanAccounts(List<MoneroAccount> accounts, Long height) {
        for(MoneroAccount account: accounts) {
            rescanAccount(account, height);
        }
    }

    @Override
    public void rescanBlockchain() {
        rescanAccounts();

        super.rescanBlockchain();
    }

    // LWS
    @Override
    public MoneroAccount createAccount(String label) {
        MoneroAccount account = super.createAccount(label);

        if (lwsConnection.isAuthenticated())
        {
            lwsConnection.addAccount(account.getPrimaryAddress(), getPrivateViewKey());
        }
        else
        {
            lwsConnection.login(account.getPrimaryAddress(), getPrivateViewKey(), true, false);
        }

        return account;
    }

    @Override
    public MoneroSubaddress createSubaddress(int accountIdx, String label)
    {
        MoneroSubaddress subaddress = super.createSubaddress(accountIdx, label);

        if (lwsConnection.isAuthenticated())
        {
            lwsConnection.addAccount(subaddress.getAddress(), getPrivateViewKey());
        }

        else if (lwsConnection.isConnected())
        {
            lwsConnection.login(subaddress.getAddress(), getPrivateViewKey(), true);
        }

        return subaddress;
    }

    @Override
    public BigInteger getBalance(Integer accountIdx, Integer subaddressIdx) {
        return getBalance(accountIdx, subaddressIdx, false);
    }

    @Override
    public BigInteger getUnlockedBalance(Integer accountIdx, Integer subaddressIdx) {
        return getBalance(accountIdx, subaddressIdx, true);
    }

    private BigInteger getBalance(Integer accountIdx, Integer subaddressIdx, boolean unlockedBalance) {
        assertNotClosed();
        try {
            String address = getPrimaryAddress();

            if (accountIdx == null) {
                if (subaddressIdx != null) throw new MoneroError("Must provide account index with subaddress index");
            } else {
                if (subaddressIdx == null) return new BigInteger(getBalanceAccountJni(accountIdx));
                else address = getAddress(accountIdx, subaddressIdx);
            }

            Map<String, Object> addressInfo = lwsConnection.getAddressInfo(address, getPrivateViewKey());

            BigInteger totalReceived = (BigInteger) addressInfo.get("total_received");
            BigInteger totalSent = (BigInteger) addressInfo.get("total_sent");
            if (!unlockedBalance)
                return totalReceived.subtract(totalSent);

            BigInteger lockedFunds = (BigInteger) addressInfo.get("locked_funds");

            return totalReceived.subtract(totalSent).subtract(lockedFunds);

        } catch (MoneroError e) {
            throw new MoneroError(e.getMessage());
        }
    }

    // LWS
    @Override
    public List<MoneroTxWallet> getTxs(MoneroTxQuery query) {
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
        return deserializeTxs(query, blocksJson);
    }

    // LWS
    @Override
    public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
        assertNotClosed();

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
    }

    // LWS
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

    // LWS
    @Override
    public List<String> relayTxs(Collection<String> txMetadatas) {
        assertNotClosed();
        String[] txMetadatasArr = txMetadatas.toArray(new String[txMetadatas.size()]); // convert to array for jni
        List<String> result = new ArrayList<>();

        try {
            for(String txMetadata: txMetadatasArr)
            {
                Map<String, Object> submitResult = lwsConnection.submitRawTx(txMetadata);

                String status = (String) submitResult.get("status");

                if (!Objects.equals(status, "success"))
                {
                    throw new MoneroError("Error while relaying tx: " + txMetadata);
                }

                result.add("");
            }

            return result;
            //return Arrays.asList(relayTxsJni(txMetadatasArr));
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    // LWS
    @Override
    public List<String> submitTxs(String signedTxHex) {
        assertNotClosed();
        try {
            return Arrays.asList(submitTxsJni(signedTxHex));
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    // LWS
    @Override
    public List<String> submitMultisigTxHex(String signedMultisigTxHex) {
        assertNotClosed();
        try {
            return Arrays.asList(submitMultisigTxHexJni(signedMultisigTxHex));
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

}
