package monero.wallet;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroLWSConnection;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroVersion;
import monero.wallet.model.*;
import org.bouncycastle.crypto.agreement.srp.SRP6Client;
import org.omg.CORBA.PRIVATE_MEMBER;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;


public class MoneroWalletLWS extends MoneroWalletDefault
{

    // ----------------------------- PRIVATE SETUP ------------------------------

    private static final long DEFAULT_SYNC_PERIOD_IN_MS = 10000; // default period between syncs in ms


    private boolean isClosed;
    private MoneroLWSConnection LWSConnection;

    private boolean isLoggedIn;
    /**
     * Indicates if the wallet is view-only, meaning it does not have the private
     * spend key and can therefore only observe incoming outputs.
     *
     * @return {bool} true if the wallet is view-only, false otherwise
     */
    @Override
    public boolean isViewOnly() {
        assertNotClosed();
        return isViewOnlyJni();
    }

    /**
     * Set the wallet's daemon connection
     *
     * @param daemonConnection manages daemon connection information
     */
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

    public void setLWSConnection(MoneroLWSConnection LWSConnection)
    {
        this.LWSConnection = LWSConnection;
    }

    /**
     * Get the wallet's daemon connection.
     *
     * @return the wallet's daemon connection
     */
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

    public MoneroLWSConnection getLWSConnection() {
        return LWSConnection;
    }

    /**
     * Set the Tor proxy to the daemon.
     *
     * @param uri the Tor proxy URI
     */
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

    /**
     * Indicates if the wallet is connected a daemon.
     *
     * @return true if the wallet is connected to a daemon, false otherwise
     */
    @Override
    public boolean isConnectedToDaemon() {
        assertNotClosed();
        try {
            return isConnectedToDaemonJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    public boolean isConnectedToLWServer()
    {
        if (LWSConnection == null)
        {
            return false;
        }

        return LWSConnection.isConnected();
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

    public long getLWSHeight()
    {
        if (!isLoggedIn)
        {
            return 0L;
        }

        Map<String, Object> response = LWSConnection.getAddressInfo(getPrimaryAddress(), getPrivateViewKey());

        return (long)response.get("blockchain_height");
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


    /**
     * Scan transactions by their hash/id.
     *
     * @param txHashes tx hashes to scan
     */

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
            if (LWSConnection.isAuthenticated())
            {
                List<MoneroAccount> accounts = getAccounts(true);
                String[] addresses = new String[accounts.size()];
                int i = 0;

                for(MoneroAccount account: accounts)
                {
                    addresses[i] = account.getPrimaryAddress();
                    i++;
                }

                LWSConnection.rescan(0L, addresses);
            }

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

            if (LWSConnection.isAuthenticated())
            {
                List<MoneroAccount> accounts = getAccounts(true);
                String[] addresses = new String[accounts.size()];
                int i = 0;

                for(MoneroAccount account: accounts)
                {
                    addresses[i] = account.getPrimaryAddress();
                    i++;
                }

                LWSConnection.rescan(0L, addresses);
            }

        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Get a subaddress's balance.
     *
     * @param accountIdx    index of the account to get the balance of (default all accounts if null)
     * @param subaddressIdx index of the subaddress to get the balance of (default all subaddresses if null)
     * @return the requested balance
     */
    @Override
    public BigInteger getBalance(Integer accountIdx, Integer subaddressIdx) {
        String address = getAddress(accountIdx, subaddressIdx);
        String privateViewKey = getPrivateViewKey();

        Map<String, Object> response = LWSConnection.getAddressInfo(address, privateViewKey);

        BigInteger totalReceived = (BigInteger) response.get("total_received");
        BigInteger totalSent = (BigInteger) response.get("total_sent");
        BigInteger lockedFunds = (BigInteger) response.get("locked_funds");

        return totalReceived.subtract(totalSent).subtract(lockedFunds);
    }

    /**
     * Get a subaddress's unlocked balance.
     *
     * @param accountIdx    index of the subaddress to get the unlocked balance of (default all accounts if null)
     * @param subaddressIdx index of the subaddress to get the unlocked balance of (default all subaddresses if null)
     * @return the requested unlocked balance
     */
    @Override
    public BigInteger getUnlockedBalance(Integer accountIdx, Integer subaddressIdx) {
        return getBalance(accountIdx, subaddressIdx);
    }

    /**
     * Get accounts with a given tag.
     *
     * @param includeSubaddresses specifies if subaddresses should be included
     * @param tag                 is the tag for filtering accounts, all accounts if null
     * @return all accounts with the given tag
     */
    @Override
    public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
        assertNotClosed();
        String accountsJson = getAccountsJni(includeSubaddresses, tag);
        List<MoneroAccount> accounts = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountsJson, MoneroWalletLWS.AccountsContainer.class).accounts;
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

    /**
     * Create a new account with a label for the first subaddress.
     *
     * @param label specifies the label for account's first subaddress (optional)
     * @return the created account
     */
    @Override
    public MoneroAccount createAccount(String label) {
        assertNotClosed();
        String accountJson = createAccountJni(label);
        MoneroAccount account = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, accountJson, MoneroAccount.class);
        sanitizeAccount(account);

        if (LWSConnection.isConnected())
        {
            LWSConnection.login(account.getPrimaryAddress(), getPrivateViewKey());
        }

        return account;
    }


    /**
     * Get subaddresses in an account.
     *
     * @param accountIdx        specifies the account to get subaddresses within
     * @param subaddressIndices are specific subaddresses to get (optional)
     * @return the retrieved subaddresses
     */
    @Override
    public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
        assertNotClosed();
        String subaddresses_json = getSubaddressesJni(accountIdx, GenUtils.listToIntArray(subaddressIndices));
        List<MoneroSubaddress> subaddresses = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddresses_json, MoneroWalletFull.SubaddressesContainer.class).subaddresses;
        for (MoneroSubaddress subaddress : subaddresses) sanitizeSubaddress(subaddress);
        return subaddresses;
    }

    @Override
    public MoneroSubaddress createSubaddress(int accountIdx, String label) {
        assertNotClosed();
        String subaddressJson = createSubaddressJni(accountIdx, label);
        MoneroSubaddress subaddress = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, subaddressJson, MoneroSubaddress.class);
        if (LWSConnection.isConnected())
        {
            LWSConnection.login(subaddress.getAddress(), getPrivateViewKey());
        }        sanitizeSubaddress(subaddress);
        return subaddress;
    }

    @Override
    public void setSubaddressLabel(int accountIdx, int subaddressIdx, String label) {
        assertNotClosed();
        if (label == null) label = "";
        setSubaddressLabelJni(accountIdx, subaddressIdx, label);
    }


    /**
     * <p>Get wallet transactions that meet the criteria defined in a query object.</p>
     *
     * <p>Transactions must meet every criteria defined in the query in order to
     * be returned.  All criteria are optional and no filtering is applied when
     * not defined.</p>
     *
     * <p>
     * All supported query criteria:<br>
     * &nbsp;&nbsp; isConfirmed - path of the wallet to open<br>
     * &nbsp;&nbsp; password - password of the wallet to open<br>
     * &nbsp;&nbsp; networkType - network type of the wallet to open (one of MoneroNetworkType.MAINNET|TESTNET|STAGENET)<br>
     * &nbsp;&nbsp; serverUri - uri of the wallet's daemon (optional)<br>
     * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
     * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
     * &nbsp;&nbsp; server - MoneroRpcConnection to a monero daemon (optional)<br>
     * &nbsp;&nbsp; isConfirmed - get txs that are confirmed or not (optional)<br>
     * &nbsp;&nbsp; inTxPool - get txs that are in the tx pool or not (optional)<br>
     * &nbsp;&nbsp; isRelayed - get txs that are relayed or not (optional)<br>
     * &nbsp;&nbsp; isFailed - get txs that are failed or not (optional)<br>
     * &nbsp;&nbsp; isMinerTx - get miner txs or not (optional)<br>
     * &nbsp;&nbsp; hash - get a tx with the hash (optional)<br>
     * &nbsp;&nbsp; hashes - get txs with the hashes (optional)<br>
     * &nbsp;&nbsp; paymentId - get transactions with the payment id (optional)<br>
     * &nbsp;&nbsp; paymentIds - get transactions with the payment ids (optional)<br>
     * &nbsp;&nbsp; hasPaymentId - get transactions with a payment id or not (optional)<br>
     * &nbsp;&nbsp; minHeight - get txs with height greater than or equal to the given height (optional)<br>
     * &nbsp;&nbsp; maxHeight - get txs with height less than or equal to the given height (optional)<br>
     * &nbsp;&nbsp; isOutgoing - get txs with an outgoing transfer or not (optional)<br>
     * &nbsp;&nbsp; isIncoming - get txs with an incoming transfer or not (optional)<br>
     * &nbsp;&nbsp; transferQuery - get txs that have a transfer that meets this query (optional)<br>
     * &nbsp;&nbsp; includeOutputs - specifies that tx outputs should be returned with tx results (optional)<br>
     * </p>
     *
     * @param query specifies properties of the transactions to get
     * @return wallet transactions that meet the query
     */
    @Override
    public List<MoneroTxWallet> getTxs(MoneroTxQuery query) {
        List<MoneroTxWallet> result = new ArrayList<>();
        String privateViewKey = getPrivateViewKey();

        List<MoneroAccount> accounts = getAccounts(true);

        for(MoneroAccount account: accounts)
        {
            List<MoneroSubaddress> subaddresses = account.getSubaddresses();

            for(MoneroSubaddress subaddress: subaddresses)
            {
                Map<String, Object> response = LWSConnection.getAddressTxs(subaddress.getAddress(), privateViewKey);
                Map<String, Object>[] txs = (Map<String, Object>[]) response.get("transactions");
                Long blockchainHeight = (Long)response.get("blockchain_height");
                for(Map<String, Object> tx: txs)
                {
                    MoneroTxWallet txWallet = new MoneroTxWallet();
                    txWallet.setHash((String)tx.get("hash"));
                    MoneroBlock block = new MoneroBlock();
                    block.setHeight((Long)tx.get("height"));
                    txWallet.setBlock(block);
                    txWallet.setUnlockTime((BigInteger) tx.get("unlock_time"));

                    Map<String, Object>[] spentOutputs = (Map<String, Object>[]) tx.get("spent_outputs");
                    List<MoneroOutput> outputs = new ArrayList<>();

                    for(Map<String, Object> spentOutput: spentOutputs)
                    {
                        MoneroOutput output = new MoneroOutput();
                        output.setAmount((BigInteger) spentOutput.get("amount"));
                        MoneroKeyImage keyImage = new MoneroKeyImage();
                        keyImage.setHex((String)spentOutput.get("key_image"));
                        output.setKeyImage(keyImage);
                        output.setIndex((Long)spentOutput.get("out_index"));
                        output.setStealthPublicKey((String)spentOutput.get("tx_pub_key"));

                        outputs.add(output);
                    }

                    txWallet.setOutputs(outputs);
                    txWallet.setIsConfirmed(!(boolean)tx.get("mempool"));

                    if (txWallet.isConfirmed()) {
                        txWallet.setNumConfirmations(blockchainHeight - (Long) tx.get("height"));
                    }

                    else {
                        txWallet.setNumConfirmations(0L);
                    }

                    txWallet.setReceivedTimestamp((Long) tx.get("timestamp"));
                }
            }
        }

        Map<String, Object> addressTxs = LWSConnection.getAddressTxs(getPrimaryAddress(), privateViewKey);

        return result;
    }

    /**
     * <p>Get tranfsers that meet the criteria defined in a query object.</p>
     *
     * <p>Transfers must meet every criteria defined in the query in order to be
     * returned.  All criteria are optional and no filtering is applied when not
     * defined.</p>
     * <p>
     * All supported query criteria:<br>
     * &nbsp;&nbsp; isOutgoing - get transfers that are outgoing or not (optional)<br>
     * &nbsp;&nbsp; isIncoming - get transfers that are incoming or not (optional)<br>
     * &nbsp;&nbsp; address - wallet's address that a transfer either originated from (if outgoing) or is destined for (if incoming) (optional)<br>
     * &nbsp;&nbsp; accountIndex - get transfers that either originated from (if outgoing) or are destined for (if incoming) a specific account index (optional)<br>
     * &nbsp;&nbsp; subaddressIndex - get transfers that either originated from (if outgoing) or are destined for (if incoming) a specific subaddress index (optional)<br>
     * &nbsp;&nbsp; subaddressIndices - get transfers that either originated from (if outgoing) or are destined for (if incoming) specific subaddress indices (optional)<br>
     * &nbsp;&nbsp; amount - amount being transferred (optional)<br>
     * &nbsp;&nbsp; destinations - individual destinations of an outgoing transfer, which is local wallet data and NOT recoverable from the blockchain (optional)<br>
     * &nbsp;&nbsp; hasDestinations - get transfers that have destinations or not (optional)<br>
     * &nbsp;&nbsp; txQuery - get transfers whose transaction meets this query (optional)<br>
     *
     * @param query specifies attributes of transfers to get
     * @return wallet transfers that meet the query
     */
    @Override
    public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
        return null;
    }

    /**
     * <p>Get outputs which meet the criteria defined in a query object.</p>
     *
     * <p>Outputs must meet every criteria defined in the query in order to be
     * returned.  All criteria are optional and no filtering is applied when not
     * defined.</p>
     *
     * <p>
     * All supported query criteria:<br>
     * &nbsp;&nbsp; accountIndex - get outputs associated with a specific account index (optional)<br>
     * &nbsp;&nbsp; subaddressIndex - get outputs associated with a specific subaddress index (optional)<br>
     * &nbsp;&nbsp; subaddressIndices - get outputs associated with specific subaddress indices (optional)<br>
     * &nbsp;&nbsp; amount - get outputs with a specific amount (optional)<br>
     * &nbsp;&nbsp; minAmount - get outputs greater than or equal to a minimum amount (optional)<br>
     * &nbsp;&nbsp; maxAmount - get outputs less than or equal to a maximum amount (optional)<br>
     * &nbsp;&nbsp; isSpent - get outputs that are spent or not (optional)<br>
     * &nbsp;&nbsp; keyImage - get outputs that match the fields defined in the given key image (optional)<br>
     * &nbsp;&nbsp; txQuery - get outputs whose transaction meets this filter (optional)<br>
     * </p>
     *
     * @param query specifies attributes of outputs to get
     * @return the queried outputs
     */
    @Override
    public List<MoneroOutputWallet> getOutputs(MoneroOutputQuery query) {
        return null;
    }

    /**
     * Export outputs in hex format.
     *
     * @param all exports all outputs if true, else exports the outputs since the last export
     * @return outputs in hex format
     */
    @Override
    public String exportOutputs(boolean all) {
        return null;
    }

    /**
     * Import outputs in hex format.
     *
     * @param outputsHex are outputs in hex format
     * @return the number of outputs imported
     */
    @Override
    public int importOutputs(String outputsHex) {
        return 0;
    }

    /**
     * Export signed key images.
     *
     * @param all exports all key images if true, else exports the key images since the last export
     * @return signed key images
     */
    @Override
    public List<MoneroKeyImage> exportKeyImages(boolean all) {
        return null;
    }

    /**
     * Import signed key images and verify their spent status.
     *
     * @param keyImages are key images to import and verify (requires hex and signature)
     * @return results of the import
     */
    @Override
    public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
        return null;
    }

    /**
     * Get new key images from the last imported outputs.
     *
     * @return the key images from the last imported outputs
     */
    @Override
    public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
        return null;
    }

    /**
     * Freeze an output.
     *
     * @param keyImage key image of the output to freeze
     */
    @Override
    public void freezeOutput(String keyImage) {

    }

    /**
     * Thaw a frozen output.
     *
     * @param keyImage key image of the output to thaw
     */
    @Override
    public void thawOutput(String keyImage) {

    }

    /**
     * Check if an output is frozen.
     *
     * @param keyImage key image of the output to check if frozen
     * @return true if the output is frozen, false otherwise
     */
    @Override
    public boolean isOutputFrozen(String keyImage) {
        return false;
    }

    /**
     * Create one or more transactions to transfer funds from this wallet.
     *
     * <p>
     * All supported configuration:<br>
     * &nbsp;&nbsp; address - single destination address (required unless `destinations` provided)<br>
     * &nbsp;&nbsp; amount - single destination amount (required unless `destinations` provided)<br>
     * &nbsp;&nbsp; accountIndex - source account index to transfer funds from (required)<br>
     * &nbsp;&nbsp; subaddressIndex - source subaddress index to transfer funds from (optional)<br>
     * &nbsp;&nbsp; subaddressIndices - source subaddress indices to transfer funds from (optional)<br>
     * &nbsp;&nbsp; relay - relay the transactions to peers to commit to the blockchain (default false)<br>
     * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
     * &nbsp;&nbsp; destinations - addresses and amounts in a multi-destination tx (required unless `address` and `amount` provided)<br>
     * &nbsp;&nbsp; paymentId - transaction payment ID (optional)<br>
     * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transactions to unlock (default 0)<br>
     * &nbsp;&nbsp; canSplit - allow funds to be transferred using multiple transactions (default true)<br>
     * </p>
     *
     * @param config configures the transactions to create
     * @return the created transactions
     */
    @Override
    public List<MoneroTxWallet> createTxs(MoneroTxConfig config) {
        return null;
    }

    /**
     * Sweep an output with a given key image.
     *
     * <p>
     * All supported configuration:<br>
     * &nbsp;&nbsp; address - single destination address (required)<br>
     * &nbsp;&nbsp; keyImage - key image to sweep (required)<br>
     * &nbsp;&nbsp; relay - relay the transaction to peers to commit to the blockchain (default false)<br>
     * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transaction to unlock (default 0)<br>
     * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
     * </p>
     *
     * @param config configures the sweep transaction
     * @return the created transaction
     */
    @Override
    public MoneroTxWallet sweepOutput(MoneroTxConfig config) {
        return null;
    }

    /**
     * Sweep all unlocked funds according to the given config.
     *
     * <p>
     * All supported configuration:<br>
     * &nbsp;&nbsp; address - single destination address (required)<br>
     * &nbsp;&nbsp; accountIndex - source account index to sweep from (optional, defaults to all accounts)<br>
     * &nbsp;&nbsp; subaddressIndex - source subaddress index to sweep from (optional, defaults to all subaddresses)<br>
     * &nbsp;&nbsp; subaddressIndices - source subaddress indices to sweep from (optional)<br>
     * &nbsp;&nbsp; relay - relay the transactions to peers to commit to the blockchain (default false)<br>
     * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
     * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transactions to unlock (default 0)<br>
     * &nbsp;&nbsp; sweepEachSubaddress - sweep each subaddress individually if true (default false)<br>
     * </p>
     *
     * @param config is the sweep configuration
     * @return the created transactions
     */
    @Override
    public List<MoneroTxWallet> sweepUnlocked(MoneroTxConfig config) {
        return null;
    }

    /**
     * Sweep all unmixable dust outputs back to the wallet to make them easier to spend and mix.
     * <p>
     * NOTE: Dust only exists pre RCT, so this method will throw "no dust to sweep" on new wallets.
     *
     * @param relay specifies if the resulting transaction should be relayed (defaults to false i.e. not relayed)
     * @return the created transactions
     */
    @Override
    public List<MoneroTxWallet> sweepDust(boolean relay) {
        return null;
    }

    /**
     * Relay previously created transactions.
     *
     * @param txMetadatas are transaction metadata previously created without relaying
     * @return the hashes of the relayed txs
     */
    @Override
    public List<String> relayTxs(Collection<String> txMetadatas) {
        return null;
    }

    /**
     * Describe a tx set containing unsigned or multisig tx hex to a new tx set containing structured transactions.
     *
     * @param txSet is a tx set containing unsigned or multisig tx hex
     * @return the tx set containing structured transactions
     */
    @Override
    public MoneroTxSet describeTxSet(MoneroTxSet txSet) {
        return null;
    }

    /**
     * Sign unsigned transactions from a view-only wallet.
     *
     * @param unsignedTxHex is unsigned transaction hex from when the transactions were created
     * @return the signed transaction hex
     */
    @Override
    public String signTxs(String unsignedTxHex) {
        return null;
    }

    /**
     * Submit signed transactions from a view-only wallet.
     *
     * @param signedTxHex is signed transaction hex from signTxs()
     * @return the resulting transaction hashes
     */
    @Override
    public List<String> submitTxs(String signedTxHex) {
        return null;
    }

    /**
     * Sign a message.
     *
     * @param message       the message to sign
     * @param signatureType sign with spend key or view key
     * @param accountIdx    the account index of the message signature (default 0)
     * @param subaddressIdx the subaddress index of the message signature (default 0)
     * @return the signature
     */
    @Override
    public String signMessage(String message, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx) {
        return null;
    }

    /**
     * Verify a signature on a message.
     *
     * @param message   is the signed message
     * @param address   is the signing address
     * @param signature is the signature
     * @return the message signature verification result
     */
    @Override
    public MoneroMessageSignatureResult verifyMessage(String message, String address, String signature) {
        return null;
    }

    /**
     * Get a transaction's secret key from its hash.
     *
     * @param txHash is the transaction's hash
     * @return is the transaction's secret key
     */
    @Override
    public String getTxKey(String txHash) {
        return null;
    }

    /**
     * Check a transaction in the blockchain with its secret key.
     *
     * @param txHash  specifies the transaction to check
     * @param txKey   is the transaction's secret key
     * @param address is the destination public address of the transaction
     * @return the result of the check
     */
    @Override
    public MoneroCheckTx checkTxKey(String txHash, String txKey, String address) {
        return null;
    }

    /**
     * Get a transaction signature to prove it.
     *
     * @param txHash  specifies the transaction to prove
     * @param address is the destination public address of the transaction
     * @param message is a message to include with the signature to further authenticate the proof (optional)
     * @return the transaction signature
     */
    @Override
    public String getTxProof(String txHash, String address, String message) {
        return null;
    }

    /**
     * Prove a transaction by checking its signature.
     *
     * @param txHash    specifies the transaction to prove
     * @param address   is the destination public address of the transaction
     * @param message   is a message included with the signature to further authenticate the proof (optional)
     * @param signature is the transaction signature to confirm
     * @return the result of the check
     */
    @Override
    public MoneroCheckTx checkTxProof(String txHash, String address, String message, String signature) {
        return null;
    }

    /**
     * Generate a signature to prove a spend. Unlike proving a transaction, it does not require the destination public address.
     *
     * @param txHash  specifies the transaction to prove
     * @param message is a message to include with the signature to further authenticate the proof (optional)
     * @return the transaction signature
     */
    @Override
    public String getSpendProof(String txHash, String message) {
        return null;
    }

    /**
     * Prove a spend using a signature. Unlike proving a transaction, it does not require the destination public address.
     *
     * @param txHash    specifies the transaction to prove
     * @param message   is a message included with the signature to further authenticate the proof (optional)
     * @param signature is the transaction signature to confirm
     * @return true if the signature is good, false otherwise
     */
    @Override
    public boolean checkSpendProof(String txHash, String message, String signature) {
        return false;
    }

    /**
     * Generate a signature to prove the entire balance of the wallet.
     *
     * @param message is a message included with the signature to further authenticate the proof (optional)
     * @return the reserve proof signature
     */
    @Override
    public String getReserveProofWallet(String message) {
        return null;
    }

    /**
     * Generate a signature to prove an available amount in an account.
     *
     * @param accountIdx specifies the account to prove ownership of the amount
     * @param amount     is the minimum amount to prove as available in the account
     * @param message    is a message to include with the signature to further authenticate the proof (optional)
     * @return the reserve proof signature
     */
    @Override
    public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
        return null;
    }

    /**
     * Proves a wallet has a disposable reserve using a signature.
     *
     * @param address   is the public wallet address
     * @param message   is a message included with the signature to further authenticate the proof (optional)
     * @param signature is the reserve proof signature to check
     * @return the result of checking the signature proof
     */
    @Override
    public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
        return null;
    }

    /**
     * Get notes for multiple transactions.
     *
     * @param txHashes identify the transactions to get notes for
     * @return notes for the transactions
     */
    @Override
    public List<String> getTxNotes(List<String> txHashes) {
        return null;
    }

    /**
     * Set notes for multiple transactions.
     *
     * @param txHashes specify the transactions to set notes for
     * @param notes    are the notes to set for the transactions
     */
    @Override
    public void setTxNotes(List<String> txHashes, List<String> notes) {

    }

    /**
     * Get address book entries.
     *
     * @param entryIndices are indices of the entries to get (optional)
     * @return the address book entries
     */
    @Override
    public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
        return null;
    }

    /**
     * Add an address book entry.
     *
     * @param address     is the entry address
     * @param description is the entry description (optional)
     * @return the index of the added entry
     */
    @Override
    public int addAddressBookEntry(String address, String description) {
        return 0;
    }

    /**
     * Edit an address book entry.
     *
     * @param index          is the index of the address book entry to edit
     * @param setAddress     specifies if the address should be updated
     * @param address        is the updated address
     * @param setDescription specifies if the description should be updated
     * @param description    is the updated description
     */
    @Override
    public void editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description) {

    }

    /**
     * Delete an address book entry.
     *
     * @param entryIdx is the index of the entry to delete
     */
    @Override
    public void deleteAddressBookEntry(int entryIdx) {

    }

    /**
     * Tag accounts.
     *
     * @param tag            is the tag to apply to the specified accounts
     * @param accountIndices are the indices of the accounts to tag
     */
    @Override
    public void tagAccounts(String tag, Collection<Integer> accountIndices) {

    }

    /**
     * Untag acconts.
     *
     * @param accountIndices are the indices of the accounts to untag
     */
    @Override
    public void untagAccounts(Collection<Integer> accountIndices) {

    }

    /**
     * Return all account tags.
     *
     * @return the wallet's account tags
     */
    @Override
    public List<MoneroAccountTag> getAccountTags() {
        return null;
    }

    /**
     * Sets a human-readable description for a tag.
     *
     * @param tag   is the tag to set a description for
     * @param label is the label to set for the tag
     */
    @Override
    public void setAccountTagLabel(String tag, String label) {

    }

    /**
     * Creates a payment URI from a send configuration.
     *
     * @param config specifies configuration for a potential tx
     * @return the payment uri
     */
    @Override
    public String getPaymentUri(MoneroTxConfig config) {
        return null;
    }

    /**
     * Parses a payment URI to a transaction configuration.
     *
     * @param uri is the payment uri to parse
     * @return the send configuration parsed from the uri
     */
    @Override
    public MoneroTxConfig parsePaymentUri(String uri) {
        return null;
    }

    /**
     * Get an attribute.
     *
     * @param key is the attribute to get the value of
     * @return the attribute's value
     */
    @Override
    public String getAttribute(String key) {
        return null;
    }

    /**
     * Set an arbitrary attribute.
     *
     * @param key is the attribute key
     * @param val is the attribute value
     */
    @Override
    public void setAttribute(String key, String val) {

    }

    /**
     * Start mining.
     *
     * @param numThreads       is the number of threads created for mining (optional)
     * @param backgroundMining specifies if mining should occur in the background (optional)
     * @param ignoreBattery    specifies if the battery should be ignored for mining (optional)
     */
    @Override
    public void startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery) {

    }

    /**
     * Stop mining.
     */
    @Override
    public void stopMining() {

    }

    /**
     * Indicates if importing multisig data is needed for returning a correct balance.
     *
     * @return true if importing multisig data is needed for returning a correct balance, false otherwise
     */
    @Override
    public boolean isMultisigImportNeeded() {
        return false;
    }

    /**
     * Get multisig info about this wallet.
     *
     * @return multisig info about this wallet
     */
    @Override
    public MoneroMultisigInfo getMultisigInfo() {
        return null;
    }

    /**
     * Get multisig info as hex to share with participants to begin creating a
     * multisig wallet.
     *
     * @return this wallet's multisig hex to share with participants
     */
    @Override
    public String prepareMultisig() {
        return null;
    }

    /**
     * Make this wallet multisig by importing multisig hex from participants.
     *
     * @param multisigHexes are multisig hex from each participant
     * @param threshold     is the number of signatures needed to sign transfers
     * @param password      is the wallet password
     * @return this wallet's multisig hex to share with participants
     */
    @Override
    public String makeMultisig(List<String> multisigHexes, int threshold, String password) {
        return null;
    }

    /**
     * Exchange multisig hex with participants in a M/N multisig wallet.
     * <p>
     * This process must be repeated with participants exactly N-M times.
     *
     * @param multisigHexes are multisig hex from each participant
     * @param password      is the wallet's password // TODO monero-project: redundant? wallet is created with password
     * @return the result which has the multisig's address xor this wallet's multisig hex to share with participants iff not done
     */
    @Override
    public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password) {
        return null;
    }

    /**
     * Export this wallet's multisig info as hex for other participants.
     *
     * @return this wallet's multisig info as hex for other participants
     */
    @Override
    public String exportMultisigHex() {
        return null;
    }

    /**
     * Import multisig info as hex from other participants.
     *
     * @param multisigHexes are multisig hex from each participant
     * @return the number of outputs signed with the given multisig hex
     */
    @Override
    public int importMultisigHex(List<String> multisigHexes) {
        return 0;
    }

    /**
     * Sign multisig transactions from a multisig wallet.
     *
     * @param multisigTxHex represents unsigned multisig transactions as hex
     * @return the result of signing the multisig transactions
     */
    @Override
    public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex) {
        return null;
    }

    /**
     * Submit signed multisig transactions from a multisig wallet.
     *
     * @param signedMultisigTxHex is signed multisig hex returned from signMultisigTxHex()
     * @return the resulting transaction hashes
     */
    @Override
    public List<String> submitMultisigTxHex(String signedMultisigTxHex) {
        return null;
    }

    /**
     * Change the wallet password.
     *
     * @param oldPassword is the wallet's old password
     * @param newPassword is the wallet's new password
     */
    @Override
    public void changePassword(String oldPassword, String newPassword) {

    }

    /**
     * Save the wallet at its current path.
     */
    @Override
    public void save() {

    }

    /**
     * Indicates if this wallet is closed or not.
     *
     * @return true if the wallet is closed, false otherwise
     */
    @Override
    public boolean isClosed() {
        return isClosed;
    }

    // ------------------------------ NATIVE METHODS ----------------------------

    private native boolean isViewOnlyJni();

    private native void setDaemonConnectionJni(String uri, String username, String password);

    private native String[] getDaemonConnectionJni();

    private native void setProxyJni(String uri);

    private native boolean isConnectedToDaemonJni();

    private native String getVersionJni();

    private native String getPathJni();

    private native String getSeedJni();

    private native String getSeedLanguageJni();

    private native String getPrivateViewKeyJni();

    private native String getPrivateSpendKeyJni();

    private native String getPublicViewKeyJni();

    private native String getPublicSpendKeyJni();

    private native String getIntegratedAddressJni(String standardAddress, String paymentId);

    private native String decodeIntegratedAddressJni(String integratedAddress);

    private native long getHeightJni();

    private native long getDaemonHeightJni();

    private native long getHeightByDateJni(int year, int month, int day);

    private native String getAddressJni(int accountIdx, int subaddressIdx);

    private native String getAddressIndexJni(String address);

    private native void startSyncingJni(Long syncPeriodInMs);

    private native void stopSyncingJni();

    private native void scanTxsJni(String[] txMetadatas);

    private native void rescanBlockchainJni();

    private native void rescanSpentJni();

    private native String getAccountsJni(boolean includeSubaddresses, String tag);

    private native String getAccountJni(int accountIdx, boolean includeSubaddresses);

    private native String createAccountJni(String label);

    // ---------------------------- PRIVATE HELPERS -----------------------------

    private void assertNotClosed() {
        if (isClosed) throw new MoneroError("Wallet is closed");
    }

    // ------------------------ RESPONSE DESERIALIZATION ------------------------

    private static class AccountsContainer {
        public List<MoneroAccount> accounts;
    };


}
