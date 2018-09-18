package wallet;

import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import wallet.model.MoneroAccount;
import wallet.model.MoneroAddressBookEntry;
import wallet.model.MoneroIntegratedAddress;
import wallet.model.MoneroKeyImage;
import wallet.model.MoneroSubaddress;
import wallet.model.MoneroTx;
import wallet.model.MoneroTxConfig;
import wallet.model.MoneroTxFilter;
import wallet.model.MoneroUri;

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
   * Get accounts with a given tag.
   * 
   * @param tag is the tag for filtering accounts (optional)
   * @return List<MoneroAccount> are all accounts for the wallet with the given tag
   */
  public List<MoneroAccount> getAccounts(String tag);
  
  /**
   * Get an account.
   * 
   * @param accountIdx identifies the account
   * @return MoneroAccount is the account
   */
  public MoneroAccount getAccount(int accountIdx);
  
  /**
   * Create a new account with an optional label.
   * 
   * @param label specifies the label for the account (optional)
   * @return MoneroAccount is the created account
   */
  public MoneroAccount createAccount(String label);
  
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
   * Create a subaddress within an account.
   * 
   * @param accountIdx identifies the account
   * @param label specifies the label for the subaddress (optional)
   * @return MoneroSubaddress is the created subaddress
   */
  public MoneroSubaddress createSubaddress(int accountIdx, String label);  
  
  /**
   * Returns the balance for a specific account.
   * 
   * @param accountIdx identifies the account
   * @return BigInteger is the account's balance
   */
  public BigInteger getBalance(int accountIdx);
  
  /**
   * Returns the balance for a specific subaddress.
   * 
   * @param accountIdx identifies the account within the wallet
   * @param subaddressIdx identifies the subbaddress within the account
   * @return BigInteger is the subaddresse's balance
   */
  public BigInteger getBalance(int accountIdx, int subaddressIdx);
  
  /**
   * Returns the unlocked balance for a specific account.
   * 
   * @param accountIdx identifies the account
   * @return BigInteger is the account's unlocked balance
   */
  public BigInteger getUnlockedBalance(int accountIdx);
  
  /**
   * Returns the unlocked balance for a specific subaddress.
   * 
   * @param accountIdx identifies the account within the wallet
   * @param subaddressIdx identifies the subbaddress within the account
   * @return BigInteger is the subaddresse's unlocked balance
   */
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx);
  
  /**
   * Indicates if importing multisig data is needed for returning a correct balance.
   * 
   * @return true if importing multisig data is needed for returning a correct balance, false otherwise
   */
  public boolean isMultisigImportNeeded();
  
  /**
   * Send a payment.
   * 
   * @param address is the address to send to
   * @param amount is the amount to send
   * @param mixin is the transaction mixin to use (optional)
   * @return MoneroTx is the resulting transaction from sending a payment
   */
  public MoneroTx send(String address, BigInteger amount, Integer mixin);
  
  /**
   * Send a payment.
   * 
   * @param address is the address to send to
   * @param paymentId is the payment id to send to (optional)
   * @param amount is the amount to send
   * @param mixin is the transaction mixin to use (optional)
   * @return MoneroTx is the resulting transaction from sending a payment
   */
  public MoneroTx send(String address, String paymentId, BigInteger amount, Integer mixin);
  
  /**
   * Send a payment.
   * 
   * @param config is the transaction configuration
   * @return MoneroTx is the resulting transaction from sending payment
   */
  public MoneroTx send(MoneroTxConfig config);
  
  /**
   * Send a payment which may be split across multiple transactions.
   * 
   * @param config is the transaction configuration
   * @return List<MoneroTx> are the resulting transactions from sending payment
   */
  public List<MoneroTx> sendSplit(MoneroTxConfig config);
  
  /**
   * Send all unlocked balance to an address.
   * 
   * @param config is the transaction configuration (account index defaults to 0 and subaddress indices defaults to [0] if not provided)
   * @param List<MoneroTx> are the resulting transactions from sweeping
   */
  public List<MoneroTx> sweepAll(MoneroTxConfig config);
  
  /**
   * Send all dust outputs back to the wallet to make them easier to spend and mix.
   * 
   * @return List<MoneroTx> are the resulting transactions from sweeping dust
   */
  public List<MoneroTx> sweepDust();
  
  /**
   * Get all wallet transactions, each containing payments, outputs, and other metadata depending on the transaction type.
   * 
   * @return List<MoneroTx> are all of the wallet's transactions
   */
  public List<MoneroTx> getTxs();
  
  /**
   * Get wallet transactions that meet the criteria specified in a filter.
   * 
   * @param filter filters wallet transactions
   * @return List<MoneroTx> are the transactions that meet the criteria specified in the filter
   */
  public List<MoneroTx> getTxs(MoneroTxFilter filter);
  
  /**
   * Set arbitrary string notes for transactions.
   * 
   * @param txIds identify the transactions to get notes for
   * @param txNotes are the notes to set for transactions
   */
  public void setTxNotes(List<String> txIds, List<String> txNotes);
  
  /**
   * Get arbitrary string notes for transactions.
   * 
   * @param txIds identify the transactions to get notes for
   * @preturn List<String> are notes for the transactions
   */
  public List<String> getTxNotes(List<String> txIds);

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
   * Stop the wallet.
   */
  public void stopWallet();
  
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
}
