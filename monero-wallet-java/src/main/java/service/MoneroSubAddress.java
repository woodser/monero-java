package service;

import java.math.BigInteger;

import model.MoneroAddress;
import model.MoneroIntegratedAddress;

/**
 * Monero subaddress interface.
 */
public interface MoneroSubAddress {

  /**
   * Get the index of the subaddress in the account.
   * 
   * @return int is the index of the subaddress in the account
   */
  public int getIndex();
  
  /**
   * Set the subaddress label.
   * 
   * @param label specifies the subaddress label
   */
  public void setLabel(String label);
  
  /**
   * Get the subaddress label.
   * 
   * @return String is the subaddress label
   */
  public String getLabel();
  
  /**
   * Returns this subaddress's standard address.
   * 
   * @return MoneroAddress is this subaddress's standard address
   */
  public MoneroAddress getStandardAddress();
  
  /**
   * Returns the number of unspent outputs available for the subaddress.
   * 
   * @return int is the number of unspent outputs available for the subaddress
   */
  public int getNumUnspentOutputs();
  
  /**
   * Gets the subaddress's balance.
   * 
   * @return BigInteger is the subaddress's balance
   */
  public BigInteger getBalance();
  
  /**
   * Gets the subaddress's unlocked balance.
   * 
   * @return BigInteger is the subaddress's unlocked balance
   */
  public BigInteger getUnlockedBalance();
  
  /**
   * Indicates if the subaddress is used.
   * 
   * @return true if the subaddress is used, false otherwise
   */
  public boolean isUsed();
  
  // ----------------------- DELETE -----------------------

  /**
   * Returns an integrated address based on this subaddress's standard address and the given payment id.
   * 
   * @param paymentId is the payment id to generate an integrated address from
   * @return MoneroIntegratedAddress is the integrated address with standard address and payment id components
   */
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
}