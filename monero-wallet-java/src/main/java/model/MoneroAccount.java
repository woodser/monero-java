package model;

import java.math.BigInteger;
import java.util.List;

/**
 * Monero account model.
 */
public class MoneroAccount {

  private int index;
  private String label;
  private BigInteger balance;
  private BigInteger unlockedBalance;
  private boolean isMultisigImportNeeded;
  private List<MoneroSubaddress> subaddresses;
  
  /**
   * Get the account index in the wallet.
   * 
   * @return int is the account's index in the wallet
   */
  public int getIndex() {
    return index;
  }
  
  public void setIndex(int index) {
    this.index = index;
  }
  
  /**
   * Get the account label.
   * 
   * @return String is the account label
   */
  public String getLabel() {
    return label;
  }
  
  /**
   * Set the account label.
   * 
   * @param label specifies the account label to set
   */
  public void setLabel(String label) {
    this.label = label;
  }
  
  /**
   * Gets the account's balance.
   * 
   * @return BigInteger is the account's balance
   */
  public BigInteger getBalance() {
    return balance;
  }
  
  public void setBalance(BigInteger balance) {
    this.balance = balance;
  }
  
  /**
   * Gets the account's unlocked balance.
   * 
   * @return BigInteger is the account's unlocked balance
   */
  public BigInteger getUnlockedBalance() {
    return unlockedBalance;
  }
  
  public void setUnlockedBalance(BigInteger unlockedBalance) {
    this.unlockedBalance = unlockedBalance;
  }
  
  /**
   * Indicates if importing multisig data is needed for returning a correct balance.
   * 
   * @return true if importing multisig data is needed for returning a correct balance, false otherwise
   */
  public boolean isMultisigImportNeeded() {
    return isMultisigImportNeeded;
  }
  
  public void setMultisigImportNeeded(boolean isMultisigImportNeeded) {
    this.isMultisigImportNeeded = isMultisigImportNeeded;
  }
  
  public List<MoneroSubaddress> getSubaddresses() {
    return subaddresses;
  }
  
  public void setSubaddresses(List<MoneroSubaddress> subaddresses) {
    this.subaddresses = subaddresses;
  }
}
