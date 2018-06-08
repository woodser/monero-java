package api;

import java.net.URI;
import java.util.List;

/**
 * Monero wallet interface.
 */
public abstract class MoneroWallet {
  
  /**
   * Get the current height of the blockchain.
   * 
   * @return int is the height of the wallet's blockchain
   */
  public abstract int getHeight();
  
  /**
   * Get the wallet's mnemonic seed.
   * 
   * @return String is the wallet's mnemonic seed
   */
  public abstract String getMnemonicSeed();

  /**
   * Get the wallet's view key.
   * 
   * @return String is the wallet's view key
   */
  public abstract String getViewKey();
  
  /**
   * Create a new account with an optional label.
   * 
   * @param label specifies the label for the account (optional)
   * @return MoneroAccount is the created account
   */
  public abstract MoneroAccount createAccount(String label);
  
  /**
   * Get all accounts for a wallet.
   * 
   * @return List<MoneroAccount> are all accounts for the wallet
   */
  public List<MoneroAccount> getAccounts() {
    return getAccounts(null);
  }
  
  /**
   * Get all accounts for a wallet filtered by a tag.
   * 
   * @param tag is the tag for filtering accounts
   * @return List<MoneroAccount> are all accounts for the wallet with the given tag
   */
  public abstract List<MoneroAccount> getAccounts(String tag);
  
  /**
   * Split an integrated address into its standard address and payment id components.
   * 
   * @param integratedAddress is a string representation of the integrated address
   * @return MoneroIntegratedAddress contains the integrated address, standard address, and payment id
   */
  public abstract MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress);

  /**
   * Send all dust outputs back to the wallet to make them easier to spend and mix.
   * 
   * @return List<MoneroTransaction> are the resulting transactions from sweeping dust
   */
  public abstract List<MoneroTransaction> sweepDust();
  
  /**
   * Convert a MoneroUri to a standard URI.
   * 
   * @param moneroUri is the MoneroUri to convert to a standard URI
   * @return URI is the MoneroUri converted to a standard URI
   */
  public abstract URI toUri(MoneroUri moneroUri);

  /**
   * Convert a standard URI to a Monero URI.
   * 
   * @param uri is the standard URI to convert
   * @return MoneroUri is the URI converted to a Monero URI
   */
  public abstract MoneroUri toMoneroUri(URI uri);

  /**
   * Save the current state of the blockchain.
   */
  public abstract void saveBlockchain();
  
  /**
   * Rescan the blockchain.
   */
  public abstract void rescanBlockchain();
  
  /**
   * Rescan the blockchain for spent outputs.
   */
  public abstract void rescanSpent();

  /**
   * Stop the wallet.
   */
  public abstract void stopWallet();
  
  /**
   * Set arbitrary string notes for transactions.
   * 
   * @param txIds identify the transactions to get notes for
   * @param txNotes are the notes to set for transactions
   */
  public abstract void setTxNotes(List<String> txIds, List<String> txNotes);
  
  /**
   * Get arbitrary string notes for transactions.
   * 
   * @param txIds identify the transactions to get notes for
   */
  public abstract void getTxNotes(List<String> txIds);
  
  /**
   * Sign a string.
   * 
   * @param data is the string to sign
   * @return String is the signature
   */
  public abstract String sign(String data);
  
  /**
   * Verify a signature on a string.
   * 
   * @param data is the signed string
   * @param address is the signing address
   * @param signature is the signature
   * @return true if the signature is good, false otherwise
   */
  public abstract boolean verify(String data, String address, String signature);
  
  public abstract List<MoneroKeyImage> getKeyImages();
  
  public abstract void importKeyImages(List<MoneroKeyImage> keyImages);
  
  /**
   * Get a list of available languages for your wallet's seed.
   * 
   * @return List<String> is a list of available languages
   */
  public abstract List<String> getLanguages();
  
  /**
   * Create a new wallet. You need to have set the argument "–wallet-dir" when launching monero-wallet-rpc to make this work.
   * 
   * @param filename is the name of the wallet file to create
   * @param password is the wallet password
   * @param language is the wallet language
   */
  public abstract void createWallet(String filename, String password, String language);
  
  /**
   * Open a wallet. You need to have set the argument "–wallet-dir" when launching monero-wallet-rpc to make this work.
   * 
   * @param filename is the name of the wallet file to open
   * @param password is the wallet password
   */
  public abstract void openWallet(String filename, String password);
}
