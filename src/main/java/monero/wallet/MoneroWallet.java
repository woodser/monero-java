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

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import monero.common.MoneroConnectionManager;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroKeyImage;
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
import monero.wallet.model.MoneroOutgoingTransfer;
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

/**
 * Monero wallet interface.
 */
public interface MoneroWallet {
  
  public static final String DEFAULT_LANGUAGE = "English";
  
  /**
   * Register a listener to receive wallet notifications.
   * 
   * @param listener is the listener to receive wallet notifications
   */
  public void addListener(MoneroWalletListenerI listener);
  
  /**
   * Unregister a listener to receive wallet notifications.
   * 
   * @param listener is the listener to unregister
   */
  public void removeListener(MoneroWalletListenerI listener);
  
  /**
   * Get the listeners registered with the wallet.
   * 
   * @return the registered listeners
   */
  public Set<MoneroWalletListenerI> getListeners();
  
  /**
   * Indicates if the wallet is view-only, meaning it does not have the private
   * spend key and can therefore only observe incoming outputs.
   * 
   * @return {bool} true if the wallet is view-only, false otherwise
   */
  public boolean isViewOnly();
  
  /**
   * Set the wallet's daemon connection.
   * 
   * @param uri is the uri of the daemon for the wallet to use
   */
  public void setDaemonConnection(String uri);
  
  /**
   * Set the wallet's daemon connection.
   * 
   * @param uri is the daemon's URI
   * @param username is the username to authenticate with the daemon (optional)
   * @param password is the password to authenticate with the daemon (optional)
   */
  public void setDaemonConnection(String uri, String username, String password);
  
  /**
   * Set the wallet's daemon connection
   * 
   * @param daemonConnection manages daemon connection information
   */
  public void setDaemonConnection(MoneroRpcConnection daemonConnection);
  
  /**
   * Get the wallet's daemon connection.
   * 
   * @return the wallet's daemon connection
   */
  public MoneroRpcConnection getDaemonConnection();

  /**
   * Set the wallet's daemon connection manager.
   * 
   * @param connectionManager manages connections to monerod
   */
  public void setConnectionManager(MoneroConnectionManager connectionManager);

  /**
   * Get the wallet's daemon connection manager.
   * 
   * @return the wallet's daemon connection manager
   */
  public MoneroConnectionManager getConnectionManager();
  
  /**
   * Indicates if the wallet is connected a daemon.
   * 
   * @return true if the wallet is connected to a daemon, false otherwise
   */
  public boolean isConnectedToDaemon();
  
  /**
   * Returns the wallet version.
   * 
   * @return the wallet version
   */
  public MoneroVersion getVersion();
  
  /**
   * Get the wallet's path.
   * 
   * @return the path the wallet can be opened with
   */
  public String getPath();
  
  /**
   * Get the wallet's mnemonic phrase or seed.
   * 
   * @return the wallet's mnemonic phrase or seed.
   */
  public String getSeed();
  
  /**
   * Get the language of the wallet's mnemonic phrase or seed.
   * 
   * @return the language of the wallet's mnemonic phrase or seed
   */
  public String getSeedLanguage();
  
  /**
   * Get the wallet's private view key.
   * 
   * @return the wallet's private view key
   */
  public String getPrivateViewKey();
  
  /**
   * Get the wallet's private spend key.
   * 
   * @return the wallet's private spend key
   */
  public String getPrivateSpendKey();
  
  /**
   * Get the wallet's public view key.
   * 
   * @return the wallet's public view key
   */
  public String getPublicViewKey();
  
  /**
   * Get the wallet's public spend key.
   * 
   * @return the wallet's public spend key
   */
  public String getPublicSpendKey();
    
  /**
   * Get the wallet's primary address.
   * 
   * @return the wallet's primary address
   */
  public String getPrimaryAddress();
  
  /**
   * Get the address of a specific subaddress.
   * 
   * @param accountIdx specifies the account index of the address's subaddress
   * @param subaddressIdx specifies the subaddress index within the account
   * @return the receive address of the specified subaddress
   */
  public String getAddress(int accountIdx, int subaddressIdx);
  
  /**
   * Get the account and subaddress index of the given address.
   * 
   * @param address is the address to get the account and subaddress index from
   * @return the account and subaddress indices
   */
  public MoneroSubaddress getAddressIndex(String address);
  
  /**
   * Get an integrated address based on this wallet's primary address and a
   * randomly generated payment ID.
   * 
   * @return the integrated address
   */
  public MoneroIntegratedAddress getIntegratedAddress();
  
  /**
   * Get an integrated address based on the given standard address and payment
   * ID. Uses the wallet's primary address if an address is not given.
   * Generates a random payment ID if a payment ID is not given.
   * 
   * @param standardAddress is the standard address to generate the integrated address from (wallet's primary address if null)
   * @param paymentId is the payment ID to generate an integrated address from (randomly generated if null)
   * @return the integrated address
   */
  public MoneroIntegratedAddress getIntegratedAddress(String standardAddress, String paymentId);
  
  /**
   * Decode an integrated address to get its standard address and payment id.
   * 
   * @param integratedAddress is an integrated address to decode
   * @return the decoded integrated address including standard address and payment id
   */
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress);
  
  /**
   * Get the block height that the wallet is synced to.
   * 
   * @return the block height that the wallet is synced to
   */
  public long getHeight();
  
  /**
   * Get the blockchain's height.
   * 
   * @return the blockchain's height
   */
  public long getDaemonHeight();
  
  /**
   * Get the blockchain's height by date as a conservative estimate for scanning.
   * 
   * @param year year of the height to get
   * @param month month of the height to get as a number between 1 and 12
   * @param day day of the height to get as a number between 1 and 31
   * @return the blockchain's approximate height at the given date
   */
  public long getHeightByDate(int year, int month, int day);

  /**
   * Synchronize the wallet with the daemon as a one-time synchronous process.
   * 
   * @return the sync result
   */
  public MoneroSyncResult sync();
  
  /**
   * Synchronize the wallet with the daemon as a one-time synchronous process.
   * 
   * @param listener listener to receive notifications during synchronization
   * @return the sync result
   */
  public MoneroSyncResult sync(MoneroWalletListenerI listener);
  
  /**
   * Synchronize the wallet with the daemon as a one-time synchronous process.
   * 
   * @param startHeight is the start height to sync from (defaults to the last synced block)
   * @return the sync result
   */
  public MoneroSyncResult sync(Long startHeight);
  
  /**
   * Synchronize the wallet with the daemon as a one-time synchronous process.
   * 
   * @param startHeight is the start height to sync from (defaults to the last synced block)
   * @param listener listener to receive notifications during synchronization
   * @return the sync result
   */
  public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener);
  
  /**
   * Start background synchronizing.
   */
  public void startSyncing();
  
  /**
   * Start background synchronizing with a maximum period between syncs.
   * 
   * @param syncPeriodInMs maximum period between syncs in milliseconds
   */
  public void startSyncing(Long syncPeriodInMs);
  
  /**
   * Stop synchronizing the wallet with the daemon.
   */
  public void stopSyncing();
  
  /**
   * Scan transactions by their hash/id.
   * 
   * @param txHashes tx hashes to scan
   */
  public void scanTxs(Collection<String> txHashes);
  
  /**
   * Rescan the blockchain for spent outputs.
   *
   * Note: this can only be called with a trusted daemon.
   *
   * Example use case: peer multisig hex is import when connected to an untrusted daemon,
   * so the wallet will not rescan spent outputs.  Then the wallet connects to a trusted
   * daemon.  This method should be manually invoked to rescan outputs.
   */
  public void rescanSpent();
  
  /**
   * Rescan the blockchain from scratch, losing any information which cannot be recovered from
   * the blockchain itself.
   * 
   * WARNING: This method discards local wallet data like destination addresses, tx secret keys,
   * tx notes, etc.
   */
  public void rescanBlockchain();
  
  /**
   * Get the wallet's balance.
   * 
   * @return the wallet's balance
   */
  public BigInteger getBalance();
  
  /**
   * Get an account's balance.
   * 
   * @param accountIdx index of the account to get the balance of (default all accounts if null)
   * @return the requested balance
   */
  public BigInteger getBalance(Integer accountIdx);
  
  /**
   * Get a subaddress's balance.
   * 
   * @param accountIdx index of the account to get the balance of (default all accounts if null)
   * @param subaddressIdx index of the subaddress to get the balance of (default all subaddresses if null)
   * @return the requested balance
   */
  public BigInteger getBalance(Integer accountIdx, Integer subaddressIdx);
  
  /**
   * Get the wallet's unlocked balance.
   * 
   * @return the wallet's unlocked balance
   */
  public BigInteger getUnlockedBalance();
  
  /**
   * Get an account's unlocked balance.
   * 
   * @param accountIdx index of the account to get the unlocked balance of (default all accounts if null)
   * @return the requested unlocked balance
   */
  public BigInteger getUnlockedBalance(Integer accountIdx);
  
  /**
   * Get a subaddress's unlocked balance.
   * 
   * @param accountIdx index of the subaddress to get the unlocked balance of (default all accounts if null)
   * @param subaddressIdx index of the subaddress to get the unlocked balance of (default all subaddresses if null)
   * @return the requested unlocked balance
   */
  public BigInteger getUnlockedBalance(Integer accountIdx, Integer subaddressIdx);
  
  /**
   * Get all accounts.
   * 
   * @return all accounts
   */
  public List<MoneroAccount> getAccounts();
  
  /**
   * Get all accounts.
   * 
   * @param includeSubaddresses specifies if subaddresses should be included
   * @return all accounts
   */
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses);
  
  /**
   * Get accounts with a given tag.
   * 
   * @param tag is the tag for filtering accounts, all accounts if null
   * @return all accounts with the given tag
   */
  public List<MoneroAccount> getAccounts(String tag);
  
  /**
   * Get accounts with a given tag.
   * 
   * @param includeSubaddresses specifies if subaddresses should be included
   * @param tag is the tag for filtering accounts, all accounts if null
   * @return all accounts with the given tag
   */
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag);
  
  /**
   * Get an account without subaddress information.
   * 
   * @param accountIdx specifies the account to get
   * @return the retrieved account
   */
  public MoneroAccount getAccount(int accountIdx);
  
  /**
   * Get an account.
   * 
   * @param accountIdx specifies the account to get
   * @param includeSubaddresses specifies if subaddresses should be included
   * @return the retrieved account
   */
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses);
  
  /**
   * Create a new account.
   * 
   * @return the created account
   */
  public MoneroAccount createAccount();

  /**
   * Create a new account with a label for the first subaddress.
   * 
   * @param label specifies the label for account's first subaddress (optional)
   * @return the created account
   */
  public MoneroAccount createAccount(String label);

  /**
   * Set an account label.
   * 
   * @param accountIdx index of the account to set the label for
   * @param label the label to set
   */
  public void setAccountLabel(int accountIdx, String label);
  
  /**
   * Get all subaddresses in an account.
   * 
   * @param accountIdx specifies the account to get subaddresses within
   * @return the retrieved subaddresses
   */
  public List<MoneroSubaddress> getSubaddresses(int accountIdx);
  
  /**
   * Get subaddresses in an account.
   * 
   * @param accountIdx specifies the account to get subaddresses within
   * @param subaddressIndices are specific subaddresses to get (optional)
   * @return the retrieved subaddresses
   */
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices);
  
  /**
   * Get a subaddress.
   * 
   * @param accountIdx specifies the index of the subaddress's account
   * @param subaddressIdx specifies index of the subaddress within the account
   * @return the retrieved subaddress
   */
  public MoneroSubaddress getSubaddress(int accountIdx, int subaddressIdx);
  
  /**
   * Create a subaddress within an account and without a label.
   * 
   * @param accountIdx specifies the index of the account to create the subaddress within
   * @return the created subaddress
   */
  public MoneroSubaddress createSubaddress(int accountIdx);
  
  /**
   * Create a subaddress within an account.
   * 
   * @param accountIdx specifies the index of the account to create the subaddress within
   * @param label specifies the the label for the subaddress (optional)
   * @return the created subaddress
   */
  public MoneroSubaddress createSubaddress(int accountIdx, String label);

  /**
   * Set a subaddress label.
   * 
   * @param accountIdx index of the account to set the label for
   * @param subaddressIdx index of the subaddress to set the label for
   * @param label the label to set
   */
  public void setSubaddressLabel(int accountIdx, int subaddressIdx, String label);

  /**
   * Get a wallet transaction by hash.
   * 
   * @param txHash is the hash of a transaction to get
   * @return the identified transaction or null if not found
   */
  public MoneroTxWallet getTx(String txHash);
  
  /**
   * Get all wallet transactions.  Wallet transactions contain one or more
   * transfers that are either incoming or outgoing to the wallet.
   * 
   * @return all wallet transactions
   */
  public List<MoneroTxWallet> getTxs();
  
  /**
   * Get wallet transactions by hash.
   * 
   * @param txHashes are hashes of transactions to get
   * @return the found transactions
   */
  public List<MoneroTxWallet> getTxs(String... txHashes);
  
  /**
   * Get wallet transactions by hash.
   * 
   * @param txHashes are hashes of transactions to get
   * @return the found transactions
   */
  public List<MoneroTxWallet> getTxs(List<String> txHashes);
  
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
  public List<MoneroTxWallet> getTxs(MoneroTxQuery query);
  
  /**
   * Get all incoming and outgoing transfers to and from this wallet.  An
   * outgoing transfer represents a total amount sent from one or more
   * subaddresses within an account to individual destination addresses, each
   * with their own amount.  An incoming transfer represents a total amount
   * received into a subaddress within an account.  Transfers belong to
   * transactions which are stored on the blockchain.
   * 
   * @return all wallet transfers
   */
  public List<MoneroTransfer> getTransfers();
  
  /**
   * Get incoming and outgoing transfers to and from an account.  An outgoing
   * transfer represents a total amount sent from one or more subaddresses
   * within an account to individual destination addresses, each with their
   * own amount.  An incoming transfer represents a total amount received into
   * a subaddress within an account.  Transfers belong to transactions which
   * are stored on the blockchain.
   * 
   * @param accountIdx is the index of the account to get transfers from
   * @return transfers to/from the account
   */
  public List<MoneroTransfer> getTransfers(int accountIdx);
  
  /**
   * Get incoming and outgoing transfers to and from a subaddress.  An outgoing
   * transfer represents a total amount sent from one or more subaddresses
   * within an account to individual destination addresses, each with their
   * own amount.  An incoming transfer represents a total amount received into
   * a subaddress within an account.  Transfers belong to transactions which
   * are stored on the blockchain.
   * 
   * @param accountIdx is the index of the account to get transfers from
   * @param subaddressIdx is the index of the subaddress to get transfers from
   * @return transfers to/from the subaddress
   */
  public List<MoneroTransfer> getTransfers(int accountIdx, int subaddressIdx);
  
  /**
   * <p>Get tranfsers that meet the criteria defined in a query object.</p>
   * 
   * <p>Transfers must meet every criteria defined in the query in order to be
   * returned.  All criteria are optional and no filtering is applied when not
   * defined.</p>
   * 
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
  public List<MoneroTransfer> getTransfers(MoneroTransferQuery query);
  
  /**
   * Get all of the wallet's incoming transfers.
   * 
   * @return the wallet's incoming transfers
   */
  public List<MoneroIncomingTransfer> getIncomingTransfers();
  
  /**
   * <p>Get incoming transfers that meet a query.</p>
   * 
   * <p>
   * All supported query criteria:<br>
   * &nbsp;&nbsp; address - get incoming transfers to a specific address in the wallet (optional)<br>
   * &nbsp;&nbsp; accountIndex - get incoming transfers to a specific account index (optional)<br>
   * &nbsp;&nbsp; subaddressIndex - get incoming transfers to a specific subaddress index (optional)<br>
   * &nbsp;&nbsp; subaddressIndices - get transfers destined for specific subaddress indices (optional)<br>
   * &nbsp;&nbsp; amount - amount being transferred (optional)<br>
   * &nbsp;&nbsp; txQuery - get transfers whose transaction meets this query (optional)<br>
   * </p>
   * 
   * @param query specifies which incoming transfers to get
   * @return incoming transfers that meet the query
   */
  public List<MoneroIncomingTransfer> getIncomingTransfers(MoneroTransferQuery query);
  
  /**
   * Get all of the wallet's outgoing transfers.
   * 
   * @return the wallet's outgoing transfers
   */
  public List<MoneroOutgoingTransfer> getOutgoingTransfers();
  
  /**
   * <p>Get outgoing transfers that meet a query.</p>
   * 
   * <p>
   * All supported query criteria:<br>
   * &nbsp;&nbsp; address - get outgoing transfers from a specific address in the wallet (optional)<br>
   * &nbsp;&nbsp; accountIndex - get outgoing transfers from a specific account index (optional)<br>
   * &nbsp;&nbsp; subaddressIndex - get outgoing transfers from a specific subaddress index (optional)<br>
   * &nbsp;&nbsp; subaddressIndices - get outgoing transfers from specific subaddress indices (optional)<br>
   * &nbsp;&nbsp; amount - amount being transferred (optional)<br>
   * &nbsp;&nbsp; destinations - individual destinations of an outgoing transfer, which is local wallet data and NOT recoverable from the blockchain (optional)<br>
   * &nbsp;&nbsp; hasDestinations - get transfers that have destinations or not (optional)<br>
   * &nbsp;&nbsp; txQuery - get transfers whose transaction meets this query (optional)<br>
   * </p>
   * 
   * @param query specifies which outgoing transfers to get
   * @return outgoing transfers that meet the query
   */
  public List<MoneroOutgoingTransfer> getOutgoingTransfers(MoneroTransferQuery query);
  
  /**
   * Get outputs created from previous transactions that belong to the wallet
   * (i.e. that the wallet can spend one time).  Outputs are part of
   * transactions which are stored in blocks on the blockchain.
   * 
   * @return all wallet outputs
   */
  public List<MoneroOutputWallet> getOutputs();
  
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
  public List<MoneroOutputWallet> getOutputs(MoneroOutputQuery query);
  
  /**
   * Export outputs since the last export.
   * 
   * @return outputs since the last export in hex format
   */
  public String exportOutputs();
  
  /**
   * Export outputs in hex format.
   *
   * @param all exports all outputs if true, else exports the outputs since the last export
   * @return outputs in hex format
   */
  public String exportOutputs(boolean all);
  
  /**
   * Import outputs in hex format.
   * 
   * @param outputsHex are outputs in hex format
   * @return the number of outputs imported
   */
  public int importOutputs(String outputsHex);
  
  /**
   * Export key images since the last export.
   * 
   * @return signed key images since the last export
   */
  List<MoneroKeyImage> exportKeyImages();
  
  /**
   * Export signed key images.
   * 
   * @param all exports all key images if true, else exports the key images since the last export
   * @return signed key images
   */
  public List<MoneroKeyImage> exportKeyImages(boolean all);
  
  /**
   * Import signed key images and verify their spent status.
   * 
   * @param keyImages are key images to import and verify (requires hex and signature)
   * @return results of the import
   */
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages);
  
  /**
   * Get new key images from the last imported outputs.
   * 
   * @return the key images from the last imported outputs
   */
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport();
  
  /**
   * Freeze an output.
   * 
   * @param keyImage key image of the output to freeze
   */
  public void freezeOutput(String keyImage);
  
  /**
   * Thaw a frozen output.
   * 
   * @param keyImage key image of the output to thaw
   */
  public void thawOutput(String keyImage);
  
  /**
   * Check if an output is frozen.
   * 
   * @param keyImage key image of the output to check if frozen
   * @return true if the output is frozen, false otherwise
   */
  public boolean isOutputFrozen(String keyImage);

  /**
   * Get the current default fee priority (unimportant, normal, elevated, etc).
   * 
   * @return the current fee priority
   */
  public MoneroTxPriority getDefaultFeePriority();
  
  /**
   * Create a transaction to transfer funds from this wallet.
   * 
   * <p>
   * All supported configuration:<br>
   * &nbsp;&nbsp; address - single destination address (required unless `destinations` provided)<br>
   * &nbsp;&nbsp; amount - single destination amount (required unless `destinations` provided)<br>
   * &nbsp;&nbsp; accountIndex - source account index to transfer funds from (required)<br>
   * &nbsp;&nbsp; subaddressIndex - source subaddress index to transfer funds from (optional)<br>
   * &nbsp;&nbsp; subaddressIndices - source subaddress indices to transfer funds from (optional)<br>
   * &nbsp;&nbsp; relay - relay the transaction to peers to commit to the blockchain (default false)<br>
   * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
   * &nbsp;&nbsp; destinations - addresses and amounts in a multi-destination tx (required unless `address` and `amount` provided)<br>
   * &nbsp;&nbsp; subtractFeeFrom - list of destination indices to split the transaction fee (optional)<br>
   * &nbsp;&nbsp; paymentId - transaction payment ID (optional)<br>
   * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transaction to unlock (default 0)<br>
   * </p>
   * 
   * @param config configures the transaction to create
   * @return the created transaction
   */
  public MoneroTxWallet createTx(MoneroTxConfig config);
  
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
  public List<MoneroTxWallet> createTxs(MoneroTxConfig config);
  
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
  public MoneroTxWallet sweepOutput(MoneroTxConfig config);

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
  public List<MoneroTxWallet> sweepUnlocked(MoneroTxConfig config);
  
  /**
   * Sweep all unmixable dust outputs back to the wallet to make them easier to spend and mix.
   * 
   * NOTE: Dust only exists pre RCT, so this method will throw "no dust to sweep" on new wallets.
   * 
   * @param relay specifies if the resulting transaction should be relayed (defaults to false i.e. not relayed)
   * @return the created transactions
   */
  public List<MoneroTxWallet> sweepDust(boolean relay);
  
  /**
   * Relay a previously created transaction.
   * 
   * @param txMetadata is transaction metadata previously created without relaying
   * @return the hash of the relayed tx
   */
  public String relayTx(String txMetadata);
  
  /**
   * Relay a previously created transaction.
   * 
   * @param tx is the transaction to relay
   * @return the hash of the relayed tx
   */
  public String relayTx(MoneroTxWallet tx);
  
  /**
   * Relay previously created transactions.
   * 
   * @param txMetadatas are transaction metadata previously created without relaying
   * @return the hashes of the relayed txs
   */
  public List<String> relayTxs(Collection<String> txMetadatas);
  
  /**
   * Relay previously created transactions.
   * 
   * @param txs are the transactions to relay
   * @return the hashes of the relayed txs
   */
  public List<String> relayTxs(List<MoneroTxWallet> txs);
  
  /**
   * Describe a tx set from unsigned tx hex.
   * 
   * @param unsignedTxHex unsigned tx hex
   * @return the tx set containing structured transactions
   */
  public MoneroTxSet describeUnsignedTxSet(String unsignedTxHex);
  
  /**
   * Describe a tx set from multisig tx hex.
   * 
   * @param multisigTxHex multisig tx hex
   * @return the tx set containing structured transactions
   */
  public MoneroTxSet describeMultisigTxSet(String multisigTxHex);
  
  /**
   * Describe a tx set containing unsigned or multisig tx hex to a new tx set containing structured transactions.
   * 
   * @param txSet is a tx set containing unsigned or multisig tx hex
   * @return the tx set containing structured transactions
   */
  public MoneroTxSet describeTxSet(MoneroTxSet txSet);
  
  /**
   * Sign unsigned transactions from a view-only wallet.
   * 
   * @param unsignedTxHex is unsigned transaction hex from when the transactions were created
   * @return the signed transaction set
   */
  public MoneroTxSet signTxs(String unsignedTxHex);
  
  /**
   * Submit signed transactions from a view-only wallet.
   * 
   * @param signedTxHex is signed transaction hex from signTxs()
   * @return the resulting transaction hashes
   */
  public List<String> submitTxs(String signedTxHex);
  
  /**
   * Sign a message.
   * 
   * @param message is the message to sign
   * @return the signature
   */
  public String signMessage(String message);
  
  /**
   * Sign a message.
   * 
   * @param message the message to sign
   * @param signatureType sign with spend key or view key
   * @param accountIdx the account index of the message signature (default 0)
   * @param subaddressIdx the subaddress index of the message signature (default 0)
   * @return the signature
   */
  public String signMessage(String message, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx);
  
  /**
   * Verify a signature on a message.
   * 
   * @param message is the signed message
   * @param address is the signing address
   * @param signature is the signature
   * @return the message signature verification result
   */
  public MoneroMessageSignatureResult verifyMessage(String message, String address, String signature);
  
  /**
   * Get a transaction's secret key from its hash.
   * 
   * @param txHash is the transaction's hash
   * @return is the transaction's secret key
   */
  public String getTxKey(String txHash);
  
  /**
   * Check a transaction in the blockchain with its secret key.
   * 
   * @param txHash specifies the transaction to check
   * @param txKey is the transaction's secret key
   * @param address is the destination public address of the transaction
   * @return the result of the check
   */
  public MoneroCheckTx checkTxKey(String txHash, String txKey, String address);
  
  /**
   * Get a transaction signature to prove it.
   * 
   * @param txHash specifies the transaction to prove
   * @param address is the destination public address of the transaction
   * @return the transaction signature
   */
  public String getTxProof(String txHash, String address);
  
  /**
   * Get a transaction signature to prove it.
   * 
   * @param txHash specifies the transaction to prove
   * @param address is the destination public address of the transaction
   * @param message is a message to include with the signature to further authenticate the proof (optional)
   * @return the transaction signature
   */
  public String getTxProof(String txHash, String address, String message);
  
  /**
   * Prove a transaction by checking its signature.
   * 
   * @param txHash specifies the transaction to prove
   * @param address is the destination public address of the transaction
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @param signature is the transaction signature to confirm
   * @return the result of the check
   */
  public MoneroCheckTx checkTxProof(String txHash, String address, String message, String signature);
  
  /**
   * Generate a signature to prove a spend. Unlike proving a transaction, it does not require the destination public address.
   * 
   * @param txHash specifies the transaction to prove
   * @return the transaction signature
   */
  public String getSpendProof(String txHash);
  
  /**
   * Generate a signature to prove a spend. Unlike proving a transaction, it does not require the destination public address.
   * 
   * @param txHash specifies the transaction to prove
   * @param message is a message to include with the signature to further authenticate the proof (optional)
   * @return the transaction signature
   */
  public String getSpendProof(String txHash, String message);
  
  /**
   * Prove a spend using a signature. Unlike proving a transaction, it does not require the destination public address.
   * 
   * @param txHash specifies the transaction to prove
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @param signature is the transaction signature to confirm
   * @return true if the signature is good, false otherwise
   */
  public boolean checkSpendProof(String txHash, String message, String signature);
  
  /**
   * Generate a signature to prove the entire balance of the wallet.
   * 
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @return the reserve proof signature
   */
  public String getReserveProofWallet(String message);
  
  /**
   * Generate a signature to prove an available amount in an account.
   * 
   * @param accountIdx specifies the account to prove ownership of the amount
   * @param amount is the minimum amount to prove as available in the account
   * @param message is a message to include with the signature to further authenticate the proof (optional)
   * @return the reserve proof signature
   */
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message);

  /**
   * Proves a wallet has a disposable reserve using a signature.
   * 
   * @param address is the public wallet address
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @param signature is the reserve proof signature to check
   * @return the result of checking the signature proof
   */
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature);
  
  /**
   * Get a transaction note.
   * 
   * @param txHash specifies the transaction to get the note of
   * @return the tx note
   */
  public String getTxNote(String txHash);
  
  /**
   * Get notes for multiple transactions.
   * 
   * @param txHashes identify the transactions to get notes for
   * @return notes for the transactions
   */
  public List<String> getTxNotes(List<String> txHashes);
  
  /**
   * Set a note for a specific transaction.
   * 
   * @param txHash specifies the transaction
   * @param note specifies the note
   */
  public void setTxNote(String txHash, String note);
  
  /**
   * Set notes for multiple transactions.
   * 
   * @param txHashes specify the transactions to set notes for
   * @param notes are the notes to set for the transactions
   */
  public void setTxNotes(List<String> txHashes, List<String> notes);
  
  /**
   * Get all address book entries.
   * 
   * @return the address book entries
   */
  public List<MoneroAddressBookEntry> getAddressBookEntries();
  
  /**
   * Get address book entries.
   * 
   * @param entryIndices are indices of the entries to get (optional)
   * @return the address book entries
   */
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices);
  
  /**
   * Add an address book entry.
   * 
   * @param address is the entry address
   * @param description is the entry description (optional)
   * @return the index of the added entry
   */
  public int addAddressBookEntry(String address, String description);
  
  /**
   * Edit an address book entry.
   * 
   * @param index is the index of the address book entry to edit
   * @param setAddress specifies if the address should be updated
   * @param address is the updated address
   * @param setDescription specifies if the description should be updated
   * @param description is the updated description
   */
  public void editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description);
  
  /**
   * Delete an address book entry.
   * 
   * @param entryIdx is the index of the entry to delete
   */
  public void deleteAddressBookEntry(int entryIdx);
  
  /**
   * Tag accounts.
   * 
   * @param tag is the tag to apply to the specified accounts
   * @param accountIndices are the indices of the accounts to tag
   */
  public void tagAccounts(String tag, Collection<Integer> accountIndices);

  /**
   * Untag acconts.
   * 
   * @param accountIndices are the indices of the accounts to untag
   */
  public void untagAccounts(Collection<Integer> accountIndices);

  /**
   * Return all account tags.
   * 
   * @return the wallet's account tags
   */
  public List<MoneroAccountTag> getAccountTags();

  /**
   * Sets a human-readable description for a tag.
   * 
   * @param tag is the tag to set a description for
   * @param label is the label to set for the tag
   */
  public void setAccountTagLabel(String tag, String label);
  
  /**
   * Creates a payment URI from a send configuration.
   * 
   * @param config specifies configuration for a potential tx
   * @return the payment uri
   */
  public String getPaymentUri(MoneroTxConfig config);
  
  /**
   * Parses a payment URI to a transaction configuration.
   * 
   * @param uri is the payment uri to parse
   * @return the send configuration parsed from the uri
   */
  public MoneroTxConfig parsePaymentUri(String uri);
  
  /**
   * Get an attribute.
   * 
   * @param key is the attribute to get the value of
   * @return the attribute's value
   */
  public String getAttribute(String key);
  
  /**
   * Set an arbitrary attribute.
   * 
   * @param key is the attribute key
   * @param val is the attribute value
   */
  public void setAttribute(String key, String val);
  
  /**
   * Start mining.
   * 
   * @param numThreads is the number of threads created for mining (optional)
   * @param backgroundMining specifies if mining should occur in the background (optional)
   * @param ignoreBattery specifies if the battery should be ignored for mining (optional)
   */
  public void startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery);
  
  /**
   * Stop mining.
   */
  public void stopMining();
  
  /**
   * Indicates if importing multisig data is needed for returning a correct balance.
   * 
   * @return true if importing multisig data is needed for returning a correct balance, false otherwise
   */
  public boolean isMultisigImportNeeded();
  
  /**
   * Indicates if this wallet is a multisig wallet.
   * 
   * @return true if this is a multisig wallet, false otherwise
   */
  public boolean isMultisig();
  
  /**
   * Get multisig info about this wallet.
   * 
   * @return multisig info about this wallet
   */
  public MoneroMultisigInfo getMultisigInfo();
  
  /**
   * Get multisig info as hex to share with participants to begin creating a
   * multisig wallet.
   * 
   * @return this wallet's multisig hex to share with participants
   */
  public String prepareMultisig();
  
  /**
   * Make this wallet multisig by importing multisig hex from participants.
   * 
   * @param multisigHexes are multisig hex from each participant
   * @param threshold is the number of signatures needed to sign transfers
   * @param password is the wallet password
   * @return this wallet's multisig hex to share with participants
   */
  public String makeMultisig(List<String> multisigHexes, int threshold, String password);
  
  /**
   * Exchange multisig hex with participants in a M/N multisig wallet.
   * 
   * This process must be repeated with participants exactly N-M times.
   * 
   * @param multisigHexes are multisig hex from each participant
   * @param password is the wallet's password // TODO monero-project: redundant? wallet is created with password
   * @return the result which has the multisig's address xor this wallet's multisig hex to share with participants iff not done
   */
  public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password);
  
  /**
   * Export this wallet's multisig info as hex for other participants.
   * 
   * @return this wallet's multisig info as hex for other participants
   */
  public String exportMultisigHex();
  
  /**
   * Import multisig info as hex from other participants.
   * 
   * @param multisigHexes are multisig hex from each participant
   * @return the number of outputs signed with the given multisig hex
   */
  public int importMultisigHex(String... multisigHexes);
  
  /**
   * Import multisig info as hex from other participants.
   * 
   * @param multisigHexes are multisig hex from each participant
   * @return the number of outputs signed with the given multisig hex
   */
  public int importMultisigHex(List<String> multisigHexes);
  
  /**
   * Sign multisig transactions from a multisig wallet.
   * 
   * @param multisigTxHex represents unsigned multisig transactions as hex
   * @return the result of signing the multisig transactions
   */
  public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex);
  
  /**
   * Submit signed multisig transactions from a multisig wallet.
   * 
   * @param signedMultisigTxHex is signed multisig hex returned from signMultisigTxHex()
   * @return the resulting transaction hashes
   */
  public List<String> submitMultisigTxHex(String signedMultisigTxHex);
  
  /**
   * Change the wallet password.
   * 
   * @param oldPassword is the wallet's old password
   * @param newPassword is the wallet's new password
   */
  public void changePassword(String oldPassword, String newPassword);

  /**
   * Save the wallet at its current path.
   */
  public void save();
  
  /**
   * Close the wallet (does not save).
   */
  public void close();
  
  /**
   * Optionally save then close the wallet.
   *
   * @param save specifies if the wallet should be saved before being closed (default false)
   */
  public void close(boolean save);
  
  /**
   * Indicates if this wallet is closed or not.
   * 
   * @return true if the wallet is closed, false otherwise
   */
  public boolean isClosed();
}