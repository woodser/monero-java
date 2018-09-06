package model;

import java.math.BigInteger;

/**
 * Monero subaddress model.
 */
public class MoneroSubaddress {

  private int index;
  private String label;
  private String address;
  private BigInteger balance;
  private BigInteger unlockedBalance;
  private boolean isMultisigImportNeeded;
  private int numUnspentOutputs;
  private boolean isUsed;
  
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
  
  public String getAddress() {
    return address;
  }
  
  public void setAddress(String address) {
    this.address = address;
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

  public int getNumUnspentOutputs() {
    return numUnspentOutputs;
  }
  
  public void setNumUnspentOutputs(int numUnspentOutputs) {
    this.numUnspentOutputs = numUnspentOutputs;
  }
  
  public boolean isUsed() {
    return isUsed;
  }
  
  public void setUsed(boolean isUsed) {
    this.isUsed = isUsed;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((address == null) ? 0 : address.hashCode());
    result = prime * result + ((balance == null) ? 0 : balance.hashCode());
    result = prime * result + index;
    result = prime * result + (isMultisigImportNeeded ? 1231 : 1237);
    result = prime * result + (isUsed ? 1231 : 1237);
    result = prime * result + ((label == null) ? 0 : label.hashCode());
    result = prime * result + numUnspentOutputs;
    result = prime * result + ((unlockedBalance == null) ? 0 : unlockedBalance.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroSubaddress other = (MoneroSubaddress) obj;
    if (address == null) {
      if (other.address != null) return false;
    } else if (!address.equals(other.address)) return false;
    if (balance == null) {
      if (other.balance != null) return false;
    } else if (!balance.equals(other.balance)) return false;
    if (index != other.index) return false;
    if (isMultisigImportNeeded != other.isMultisigImportNeeded) return false;
    if (isUsed != other.isUsed) return false;
    if (label == null) {
      if (other.label != null) return false;
    } else if (!label.equals(other.label)) return false;
    if (numUnspentOutputs != other.numUnspentOutputs) return false;
    if (unlockedBalance == null) {
      if (other.unlockedBalance != null) return false;
    } else if (!unlockedBalance.equals(other.unlockedBalance)) return false;
    return true;
  }
}
