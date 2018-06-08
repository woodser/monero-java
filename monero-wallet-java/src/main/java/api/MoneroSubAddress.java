package api;

import java.math.BigInteger;

/**
 * Monero subaddress interface.
 */
public interface MoneroSubAddress {

  /**
   * Get the index of the sub-address in the account.
   * 
   * @return int is the index of the sub-address in the account
   */
  public int getIndex();
  
  /**
   * Set the sub-address label.
   * 
   * @param label specifies the sub-address label
   */
  public void setLabel(String label);
  
  /**
   * Get the sub-address label.
   * 
   * @return String is the sub-address label
   */
  public String getLabel();
  
  /**
   * Returns this sub-address's standard address.
   * 
   * @return MoneroAddress is this sub-address's standard address
   */
  public MoneroAddress getStandardAddress();

  /**
   * Returns an integrated address based on this sub-address's standard address and the given payment id.
   * 
   * @param paymentId is the payment id to generate an integrated address from
   * @return MoneroIntegratedAddress is the integrated address with standard address and payment id components
   */
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
  
  public BigInteger getBalance();
  
  public BigInteger getUnlockedBalance();
 
  public int getNumUnspentOutputs();
  
  public boolean isUsed();
}