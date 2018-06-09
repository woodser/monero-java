package service;

import java.util.List;

import api.MoneroAccount;
import api.MoneroKeyImage;
import api.MoneroTransaction;

public interface MoneroWallet {
  
  /**
   * Returns the wallet's current block height.
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
   * Create a new account with an optional label.
   * 
   * @param label specifies the label for the account (optional)
   * @return MoneroAccount is the created account
   */
  public MoneroAccount createAccount(String label);
  
  /**
   * Get all accounts for a wallet.
   * 
   * @return List<MoneroAccount> are all accounts for the wallet
   */
  public List<MoneroAccount> getAccounts();
  
  /**
   * Get all accounts for a wallet filtered by a tag.
   * 
   * @param tag is the tag for filtering accounts
   * @return List<MoneroAccount> are all accounts for the wallet with the given tag
   */
  public List<MoneroAccount> getAccounts(String tag);
  
  /**
   * Send all dust outputs back to the wallet to make them easier to spend and mix.
   * 
   * @return List<MoneroTransaction> are the resulting transactions from sweeping dust
   */
  public List<MoneroTransaction> sweepDust();  // TODO: stopped here, go through other MoneroWallet.java

  public List<MoneroKeyImage> getKeyImages();
  
  public void importKeyImages(List<MoneroKeyImage> keyImages);
}
