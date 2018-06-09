package service;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import model.MoneroTransaction;
import model.MoneroTransactionConfig;

/**
 * Monero account interface.
 */
public interface MoneroAccount {
  
  /**
   * Get the account index in the wallet.
   * 
   * @return int is the account's index in the wallet
   */
  public int getIndex();
  
  /**
   * Get the account label.
   * 
   * @return String is the account label
   */
  public String getLabel();
  
  /**
   * Set the account label.
   * 
   * @param label specifies the account label to set
   */
  public void setLabel(String label);

  /**
   * Gets the account's balance.
   * 
   * @return BigInteger is the account's balance
   */
  public BigInteger getBalance();

  /**
   * Gets the account's unlocked balance.
   * 
   * @return BigInteger is the account's unlocked balance
   */
  public BigInteger getUnlockedBalance();
  
  /**
   * Indicates if importing multisig data is needed for returning a correct balance.
   * 
   * @return true if importing multisig data is needed for returning a correct balance, false otherwise
   */
  public boolean isMultisigImportNeeded();
  
  /**
   * Create a new sub-address for an account.
   * 
   * @param label specifies the label for the sub-address
   * @return MoneroSubAddress is the created sub-address
   */
  public MoneroSubAddress createSubAddress(String label);
  
  /**
   * Gets the account's sub-addresses.
   * 
   * @param indices are indices of sub-addresses to get
   * @return List<MoneroSubAddress> are the account's sub-addresses at the given indices
   */
  public List<MoneroSubAddress> getSubAddresses(Collection<Integer> indices);
  
  /**
   * Send a payment.
   * 
   * @param config is the transaction configuration
   * @return MoneroTransaction is the resulting transaction from sending payment
   */
  public MoneroTransaction send(MoneroTransactionConfig config);
  
  /**
   * Send a payment which may be split across multiple transactions.
   * 
   * @param config is the transaction configuration
   * @return List<MoneroTransaction> are the resulting transactions from sending payment
   */
  public List<MoneroTransaction> sendSplit(MoneroTransactionConfig config);
  
  /**
   * Send all unlocked balance to an address.
   * 
   * @param config is the transcaction configuration
   * @param List<MoneroTransaction> are the resulting transactions from sweeping
   */
  public List<MoneroTransaction> sweepAll(MoneroTransactionConfig config);
}
