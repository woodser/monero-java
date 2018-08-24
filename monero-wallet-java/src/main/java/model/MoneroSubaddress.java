package model;

import java.math.BigInteger;

/**
 * Monero subaddress model.
 */
public class MoneroSubaddress {

  private int index;
  private String label;
  private MoneroAddress address;
  private BigInteger balance;
  private BigInteger unlockedBalance;
  private int numUnspentOutputs;
  private boolean isUsed;
  
  /**
   * Get the index of the subaddress in the account.
   * 
   * @return int is the index of the subaddress in the account
   */
  public int getIndex() {
    return index;
  }
  
  public void setIndex(int index) {
    this.index = index;
  }
  
  /**
   * Get the subaddress label.
   * 
   * @return String is the subaddress label
   */
  public String getLabel() {
    return label;
  }
  
  /**
   * Set the subaddress label.
   * 
   * @param label specifies the subaddress label
   */
  public void setLabel(String label) {
    this.label = label;
  }
  
  /**
   * Returns this subaddress's address.
   * 
   * @return MoneroAddress is this subaddress's address
   */
  public MoneroAddress getAddress() {
    return address;
  }
  
  public void setAddress(MoneroAddress address) {
    this.address = address;
  }
  
  /**
   * Gets the subaddress's balance.
   * 
   * @return BigInteger is the subaddress's balance
   */
  public BigInteger getBalance() {
    return balance;
  }
  
  public void setBalance(BigInteger balance) {
    this.balance = balance;
  }
  
  /**
   * Gets the subaddress's unlocked balance.
   * 
   * @return BigInteger is the subaddress's unlocked balance
   */
  public BigInteger getUnlockedBalance() {
    return unlockedBalance;
  }
  
  public void setUnlockedBalance(BigInteger unlockedBalance) {
    this.unlockedBalance = unlockedBalance;
  }
  
  /**
   * Returns the number of unspent outputs available for the subaddress.
   * 
   * @return int is the number of unspent outputs available for the subaddress
   */
  public int getNumUnspentOutputs() {
    return numUnspentOutputs;
  }
  
  public void setNumUnspentOutputs(int numUnspentOutputs) {
    this.numUnspentOutputs = numUnspentOutputs;
  }
  
  /**
   * Indicates if the subaddress is used.
   * 
   * @return true if the subaddress is used, false otherwise
   */
  public boolean isUsed() {
    return isUsed;
  }
  
  public void setUsed(boolean isUsed) {
    this.isUsed = isUsed;
  }
}
