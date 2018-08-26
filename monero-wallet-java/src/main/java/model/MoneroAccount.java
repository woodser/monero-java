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
  
  public MoneroAccount(int index, String label, BigInteger balance, BigInteger unlockedBalance, boolean isMultisigImportNeeded, List<MoneroSubaddress> subaddresses) {
    super();
    this.index = index;
    this.label = label;
    this.balance = balance;
    this.unlockedBalance = unlockedBalance;
    this.isMultisigImportNeeded = isMultisigImportNeeded;
    this.subaddresses = subaddresses;
  }
  
  public int getIndex() {
    return index;
  }
  
  public void setIndex(int index) {
    this.index = index;
  }
  
  public String getLabel() {
    return label;
  }
  
  public void setLabel(String label) {
    this.label = label;
  }
  
  public BigInteger getBalance() {
    return balance;
  }
  
  public void setBalance(BigInteger balance) {
    this.balance = balance;
  }
  
  public BigInteger getUnlockedBalance() {
    return unlockedBalance;
  }
  
  public void setUnlockedBalance(BigInteger unlockedBalance) {
    this.unlockedBalance = unlockedBalance;
  }
  
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
