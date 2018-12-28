package monero.wallet;

import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImage;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxFilter;
import monero.wallet.model.MoneroUri;

/**
 * Monero wallet interface.
 */
public interface MoneroWallet {
  
  /**
   * Get the wallet's current block height.
   * 
   * @return int is the current block height of the wallet
   */
  public int getHeight();
  
  /**
   * Get the wallet's mnemonic seed.
   * 
   * @return String is the wallet's mnemonic seed
   */
  public String getMnemonicSeed();

  /**
   * Get the wallet's view key.
   * 
   * @return String is the wallet's view key
   */
  public String getViewKey();
  
  /**
   * Gets the wallet's primary address (account 0, subaddress 0).
   * 
   * @return String is the wallet's primary address
   */
  public String getPrimaryAddress();
  
  /**
   * Returns an integrated address based on this wallet's standard address and the given payment ID.
   * 
   * Generates a random payment ID if none is given.
   * 
   * @param paymentId is the payment id to generate an integrated address from (optional)
   * @return MoneroIntegratedAddress is the integrated address
   */
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
  
  /**
   * Get all accounts.
   * 
   * @return List<MoneroAccount> are all accounts within the wallet
   */
  public List<MoneroAccount> getAccounts();
  
  /**
   * Get all accounts.
   * 
   * @param includeSubaddresses specifies if subaddresses should be included
   * @return List<MoneroAccount> are all accounts
   */
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses);
  
  /**
   * Get accounts with a given tag.
   * 
   * @param tag is the tag for filtering accounts, all accounts if null
   * @return List<MoneroAccount> are all accounts for the wallet with the given tag
   */
  public List<MoneroAccount> getAccounts(String tag);
  
  /**
   * Get accounts with a given tag.
   * 
   * @param includeSubaddresses specifies if subaddresses should be included
   * @param tag is the tag for filtering accounts, all accounts if null
   * @return List<MoneroAccount> are all accounts for the wallet with the given tag
   */
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag);
  
  /**
   * Get an account.
   * 
   * @param accountIdx identifies the account
   * @return MoneroAccount is the account
   */
  public MoneroAccount getAccount(int accountIdx);
  
  /**
   * Get an account.
   * 
   * @param accountIdx identifies the account
   * @param includeSubaddresses specifies if subaddresses should be included
   * @return MoneroAccount is the account
   */
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses);
  
  /**
   * Create a new account without a label.
   * 
   * @return MoneroAccount is the created account
   */
  public MoneroAccount createAccount();
  
  /**
   * Create a new account with an optional label.
   * 
   * @param label specifies the label for the account (optional)
   * @return MoneroAccount is the created account
   */
  public MoneroAccount createAccount(String label);
  
  /**
   * Gets an account's subaddresses.
   * 
   * @param accountIdx identifies the account
   * @return List<MoneroSubaddress> are subaddresses within an account
   */
  public List<MoneroSubaddress> getSubaddresses(int accountIdx);
  
  /**
   * Gets specific subaddresses within an account.
   * 
   * @param accountIdx identifies the account
   * @param subaddressIndices identifies the subaddresses within the account (optional)
   * @return List<MoneroSubaddress> are the specified subaddresses
   */
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, Collection<Integer> subaddressIndices);
  
  /**
   * Gets a subaddress in an account.
   * 
   * @param accountIdx identifies the account
   * @param subaddressIdx identifies the subaddress
   * @return MoneroSubaddress is the subaddress in the account
   */
  public MoneroSubaddress getSubaddress(int accountIdx, int subaddressIdx);
  
  /**
   * Create a subaddress within an account without a label.
   * 
   * @param accountIdx identifies the account
   * @return MoneroSubaddress is the created subaddress
   */
  public MoneroSubaddress createSubaddress(int accountIdx);  
  
  /**
   * Create a subaddress within an account.
   * 
   * @param accountIdx identifies the account
   * @param label specifies the label for the subaddress (optional)
   * @return MoneroSubaddress is the created subaddress
   */
  public MoneroSubaddress createSubaddress(int accountIdx, String label);
  
  /**
   * Returns the receive address of the given subaddress.
   * 
   * @param accountIdx identifies the subaddress's account index
   * @param subaddressIdx identifies the subaddress's index within the account
   * @return String is the receive address of the given subaddress
   */
  public String getAddress(int accountIdx, int subaddressIdx);
  
  /**
   * Get the balance across all accounts.
   * 
   * @return BigInteger is the balance across all accounts
   */
  public BigInteger getBalance();
  
  /**
   * Get the unlocked balance across all accounts.
   * 
   * @return BigInteger is the unlocked balance across all accounts
   */
  public BigInteger getUnlockedBalance();
  
  /**
   * Indicates if importing multisig data is needed for returning a correct balance.
   * 
   * @return true if importing multisig data is needed for returning a correct balance, false otherwise
   */
  public boolean isMultisigImportNeeded();
  
  /**
   * Get all wallet transactions, each containing payments, outputs, and other metadata depending on the transaction type.
   * 
   * @return List<MoneroTx> are all of the wallet's transactions
   */
  public List<MoneroTx> getTxs();
  
  /**
   * Get all account transactions, each containing payments, outputs, and other metadata depending on the transaction type.
   * 
   * @return List<MoneroTx> are all of the account's transactions
   */
  public List<MoneroTx> getTxs(int accountIdx);
  
  /**
   * Get all subaddress transactions, each containing payments, outputs, and other metadata depending on the transaction type.
   * 
   * @return List<MoneroTx> are all of the subaddress's transactions
   */
  public List<MoneroTx> getTxs(int accountIdx, int subaddressIdx);
  
  /**
   * Get wallet transactions that meet the criteria specified in a filter.
   * 
   * @param filter filters wallet transactions
   * @return List<MoneroTx> are the transactions that meet the criteria specified in the filter
   */
  public List<MoneroTx> getTxs(MoneroTxFilter filter);
  
  /**
   * Send a payment.
   * 
   * @param address is the address to send to
   * @param amount is the amount to send
   * @return MoneroTx is the resulting transaction from sending a payment
   */
  public MoneroTx send(String address, BigInteger amount);

  /**
   * Send a payment.
   * 
   * @param address is the address to send to
   * @param paymentId is the payment id to send to (optional)
   * @param amount is the amount to send
   * @return MoneroTx is the resulting transaction from sending a payment
   */
  public MoneroTx send(String address, String paymentId, BigInteger amount);

  /**
   * Send payments.
   * 
   * @param config is the transaction configuration
   * @return MoneroTx is the resulting transaction from sending payments
   */
  public MoneroTx send(MoneroTxConfig config);

  /**
   * Send a payment which may be split across multiple transactions.
   * 
   * @param address is the address to send to
   * @param amount is the amount to send
   * @return List<MoneroTx> are the resulting transactions from sending a payment
   */
  public List<MoneroTx> sendSplit(String address, BigInteger amount);

  /**
   * Send a payment which may be split across multiple transactions.
   * 
   * @param address is the address to send to
   * @param paymentId is the payment id to send to (optional)
   * @param amount is the amount to send
   * @return List<MoneroTx> are the resulting transactions from sending a payment
   */
  public List<MoneroTx> sendSplit(String address, String paymentId, BigInteger amount);

  /**
   * Send payments which may be split across multiple transactions.
   * 
   * @param config is the transaction configuration
   * @return List<MoneroTx> are the resulting transactions from sending payments
   */
  public List<MoneroTx> sendSplit(MoneroTxConfig config);

  /**
   * Sweep the wallet's unlocked funds to an address.
   * 
   * @param address is the address to sweep the wallet's funds to
   * @return List<MoneroTx> are the resulting transactions
   */
  public List<MoneroTx> sweepWallet(String address);

  /**
   * Sweep an acount's unlocked funds to an address.
   * 
   * @param accountIdx is the index of the account
   * @param address is the address to sweep the account's funds to
   * @return List<MoneroTx> are the resulting transactions
   */
  public List<MoneroTx> sweepAccount(int accountIdx, String address);

  /**
   * Sweep a subaddress's unlocked funds to an address.
   * 
   * @param accountIdx is the index of the account
   * @param subaddressIdx is the index of the subaddress
   * @param address is the address to sweep the subaddress's funds to
   * @return List<MoneroTx> are the resulting transactions
   */
  public List<MoneroTx> sweepSubaddress(int accountIdx, int subaddressIdx, String address);

  /**
   * Sweep unlocked funds.
   * 
   * @param config specifies the sweep configuration
   * @param List<MoneroTx> are the resulting transactions
   */
  public List<MoneroTx> sweepAll(MoneroTxConfig config);

  /**
   * Send all dust outputs back to the wallet to make them easier to spend and mix.
   * 
   * @return List<MoneroTx> are the resulting transactions from sweeping dust
   */
  public List<MoneroTx> sweepDust();
  
  /**
   * Relays a transaction previously created without relaying.
   * 
   * @param tx is the transaction to relay
   * @return MoneroTx is the relayed transaction
   */
  public MoneroTx relayTx(MoneroTx tx);
  
  /**
   * Relays tranactions previously created without relaying.
   * 
   * @param txs are the transactions to relay
   * @return List<MoneroTx> are the relayed txs
   */
  public List<MoneroTx> relayTxs(List<MoneroTx> txs);
  
  /**
   * Sign transactions created on a read-only wallet (in cold-signing process).
   * 
   * @param unsignedTxSet are the unsigned transactions to sign
   * @return List<MoneroTx> is data for each transaction after signing
   */
  public List<MoneroTx> signTxs(String unsignedTxSet);
  
  /**
   * Submit previously signed transactions on a read-only wallet (in cold-signing process).
   * 
   * @param signedTxSet are the signed transactions to submit
   * @return List<MoneroTx> is data for each transaction after submitting
   */
  public List<MoneroTx> submitTxs(String signedTxSet);

  /**
   * Tags accounts.
   * 
   * @param tag is the tag to apply to the specified accounts
   * @param accountIndices are the indices of the accounts to tag
   */
  public void tagAccounts(String tag, Collection<Integer> accountIndices);

  /**
   * Untags acconts.
   * 
   * @param accountIndices are the indices of the accounts to untag
   */
  public void untagAccounts(Collection<Integer> accountIndices);

  /**
   * Returns all account tags.
   * 
   * @return List<MoneroAccountTag> are the wallet's account tags
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
   * Set a note for a specific transaction.
   * 
   * @param txId specifies the transaction
   * @param txNote specifies the note
   */
  public void setTxNote(String txId, String txNote);

  /**
   * Set notes for multiple transactions.
   * 
   * @param txIds specifies the transactions to set notes for
   * @param txNotes are the notes to set for the transactions
   */
  public void setTxNotes(List<String> txIds, List<String> txNotes);
  
  /**
   * Get a transaction note.
   * 
   * @param txId specifies the transaction to get the note of
   * @return String is the transaction note
   */
  public String getTxNote(String txId);
  
  /**
   * Get arbitrary string notes for transactions.
   * 
   * @param txIds identify the transactions to get notes for
   * @preturn List<String> are notes for the transactions
   */
  public List<String> getTxNotes(List<String> txIds);
  
  /**
   * Get a transaction's secret key from its id.
   * 
   * @param txId is the transaction's id
   * @return String is the transaction's secret key
   */
  public String getTxKey(String txId);
  
  /**
   * Check a transaction in the blockchain with its secret key.
   * 
   * @param txId specifies the transaction to check
   * @param txKey is the transaction's secret key
   * @param address is the destination public address of the transaction
   * @return MoneroCheckTx is the result of the check
   */
  public MoneroCheckTx checkTxKey(String txId, String txKey, String address);
  
  /**
   * Get a transaction signature to prove it.
   * 
   * @param txId specifies the transaction to prove
   * @param address is the destination public address of the transaction
   * @param message is a message to include with the signature to further authenticate the proof (optional)
   * @return String is the transaction signature
   */
  public String getTxProof(String txId, String address, String message);
  
  /**
   * Prove a transaction by checking its signature.
   * 
   * @param txId specifies the transaction to prove
   * @param address is the destination public address of the transaction
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @param signature is the transaction signature to confirm
   * @return MoneroCheckTx is the result of the check
   */
  public MoneroCheckTx checkTxProof(String txId, String address, String message, String signature);
  
  /**
   * Generate a signature to prove a spend. Unlike proving a transaction, it does not require the destination public address.
   * 
   * @param txId specifies the transaction to prove
   * @param message is a message to include with the signature to further authenticate the proof (optional)
   * @return String is the transaction signature
   */
  public String getSpendProof(String txId, String message);
  
  /**
   * Prove a spend using a signature. Unlike proving a transaction, it does not require the destination public address.
   * 
   * @param txId specifies the transaction to prove
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @param signature is the transaction signature to confirm
   * @return true if the signature is good, false otherwise
   */
  public boolean checkSpendProof(String txId, String message, String signature);
  
  /**
   * Generate a signature to prove the entire balance of the wallet.
   * 
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @return String is the reserve proof signature
   */
  public String getReserveProof(String message);
  
  /**
   * Generate a signature to prove an available amount in an account.
   * 
   * @param accountIdx specifies the account to prove contains an available amount
   * @param amount is the minimum amount to prove as available in the account
   * @param message is a message to include with the signature to further authenticate the proof (optional)
   * @return String is the reserve proof signature
   */
  public String getReserveProof(int accountIdx, BigInteger amount, String message);

  /**
   * Proves a wallet has a disposable reserve using a signature.
   * 
   * @param address is the public wallet address
   * @param message is a message included with the signature to further authenticate the proof (optional)
   * @param signature is the reserve proof signature to check
   * @return MoneroCheckReserve is the result of checking the signature proof
   */
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature);

  /**
   * Returns a signed set of key images.
   * 
   * @return Collection<MoneroKeyImage> are exported key images
   */
  public Collection<MoneroKeyImage> getKeyImages();
  
  /**
   * Import signed key images list and verify their spent status.
   * 
   * @param keyImages are key images to import
   * @return Map<String, BigInteger> contains "height", "spent", and "unspent"
   */
  public Map<String, BigInteger> importKeyImages(Collection<MoneroKeyImage> keyImages);
  
  /**
   * Returns all address book entries.
   * 
   * @return List<MoneroAddressBookEntry> are the wallet's address book entries
   */
  public List<MoneroAddressBookEntry> getAddressBookEntries();
  
  /**
   * Returns address book entries at the given indices.
   * 
   * @param entryIndices are entry indices to retrieve
   * @return List<MoneroAddressBookEntry> are the wallet's address book entries
   */
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices);
  
  /**
   * Adds an address book entry.
   * 
   * @param address is the entry's Monero address
   * @param description is the entry's description (optional)
   * @return int is the index of the new address book entry
   */
  public int addAddressBookEntry(String address, String description);
  
  /**
   * Adds an address book entry.
   * 
   * @param address is the entry's Monero address
   * @param paymentId is the entry's payment id (optional)
   * @param description is the entry's description (optional)
   * @return int is the index of the new address book entry
   */
  public int addAddressBookEntry(String address, String paymentId, String description);
  
  /**
   * Deletes an address book entry.
   * 
   * @param entryIdx is the index of the address book entry to delete
   */
  public void deleteAddressBookEntry(int entryIdx);
  
  /**
   * Get a list of available languages for wallet seeds.
   * 
   * @return List<String> is a list of available languages
   */
  public List<String> getLanguages();
  
  /**
   * Sign a string.
   * 
   * @param data is the string to sign
   * @return String is the signature
   */
  public String sign(String data);
  
  /**
   * Verify a signature on a string.
   * 
   * @param data is the signed string
   * @param address is the signing address
   * @param signature is the signature
   * @return true if the signature is good, false otherwise
   */
  public boolean verify(String data, String address, String signature);
  
  /**
   * Convert a MoneroUri to a standard URI.
   * 
   * @param moneroUri is the MoneroUri to convert to a standard URI
   * @return URI is the MoneroUri converted to a standard URI
   */
  public URI toUri(MoneroUri moneroUri);

  /**
   * Convert a standard URI to a Monero URI.
   * 
   * @param uri is the standard URI to convert
   * @return MoneroUri is the URI converted to a Monero URI
   */
  public MoneroUri toMoneroUri(URI uri);
  
  /**
   * Decodes an integrated address into its standard address and payment id components.
   * 
   * @param integratedAddress is a string representation of the integrated address
   * @return MoneroIntegratedAddress contains the integrated address, standard address, and payment id
   */
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress);
  
  /**
   * Create a new wallet.
   * 
   * @param filename is the name of the wallet file to create
   * @param password is the wallet password
   * @param language is the wallet language
   */
  public void createWallet(String filename, String password, String language);

  /**
   * Open a wallet.
   * 
   * @param filename is the name of the wallet file to open
   * @param password is the wallet password
   */
  public void openWallet(String filename, String password);

  /**
   * Stop the wallet.
   */
  public void stopWallet();

  /**
   * Save the current state of the blockchain.
   */
  public void saveBlockchain();
  
  /**
   * Rescan the blockchain.
   */
  public void rescanBlockchain();
  
  /**
   * Rescan the blockchain for spent outputs.
   */
  public void rescanSpent();

  /**
   * Start mining in the Monero daemon.
   * 
   * @param numThreads is the number of threads created for mining
   * @param backgroundMining specifies if mining should occur in the background
   * @param ignoreBattery specifies if the battery should be ignored for mining
   */
  public void startMining(int numThreads, boolean backgroundMining, boolean ignoreBattery);
  
  /**
   * Stop mining in the Monero daemon.
   */
  public void stopMining();
  
  /**
   * Set an arbitrary attribute.
   * 
   * @param key is the attribute key
   * @param value is the attribute value
   */
  public void setAttribute(String key, String value);
  
  /**
   * Get an attribute.
   * 
   * @param key is the attribute to get the value of
   * @return String is the attribute's value
   */
  public String getAttribute(String key);
}
