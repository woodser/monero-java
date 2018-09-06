package model;

import java.math.BigInteger;
import java.util.List;

/**
 * Monero account model.
 */
public class MoneroAccount {

  private int index;
  private String primaryAddress;
  private String label;
  private BigInteger balance;
  private BigInteger unlockedBalance;
  private Boolean isMultisigImportNeeded;
  private List<MoneroSubaddress> subaddresses;
  
  public MoneroAccount(int index, String primaryAddress, String label, BigInteger balance, BigInteger unlockedBalance, Boolean isMultisigImportNeeded, List<MoneroSubaddress> subaddresses) {
    super();
    this.index = index;
    this.primaryAddress = primaryAddress;
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
  
  public String getPrimaryAddress() {
    return primaryAddress;
  }

  public void setPrimaryAddress(String primaryAddress) {
    this.primaryAddress = primaryAddress;
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
  
  public Boolean isMultisigImportNeeded() {
    return isMultisigImportNeeded;
  }
  
  public void setMultisigImportNeeded(Boolean isMultisigImportNeeded) {
    this.isMultisigImportNeeded = isMultisigImportNeeded;
  }
  
  public List<MoneroSubaddress> getSubaddresses() {
    return subaddresses;
  }
  
  public void setSubaddresses(List<MoneroSubaddress> subaddresses) {
    this.subaddresses = subaddresses;
  }
}
