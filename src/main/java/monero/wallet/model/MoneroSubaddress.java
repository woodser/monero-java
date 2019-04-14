package monero.wallet.model;

import java.math.BigInteger;

/**
 * Monero subaddress model.
 */
public class MoneroSubaddress {

  private Integer accountIndex;
  private Integer index;
  private String address;
  private String label;
  private BigInteger balance;
  private BigInteger unlockedBalance;
  private Integer numUnspentOutputs;
  private Boolean isUsed;
  
  public MoneroSubaddress() {
    // nothing to construct
  }
  
  public MoneroSubaddress(String address) {
    this.address = address;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public void setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
  }
  
  public Integer getIndex() {
    return index;
  }
  
  public void setIndex(Integer index) {
    this.index = index;
  }
  
  public String getAddress() {
    return address;
  }
  
  public void setAddress(String address) {
    this.address = address;
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
  
  public Integer getNumUnspentOutputs() {
    return numUnspentOutputs;
  }
  
  public void setNumUnspentOutputs(Integer numUnspentOutputs) {
    this.numUnspentOutputs = numUnspentOutputs;
  }
  
  public Boolean getIsUsed() {
    return isUsed;
  }
  
  public void setIsUsed(Boolean isUsed) {
    this.isUsed = isUsed;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((address == null) ? 0 : address.hashCode());
    result = prime * result + ((balance == null) ? 0 : balance.hashCode());
    result = prime * result + ((index == null) ? 0 : index.hashCode());
    result = prime * result + ((isUsed == null) ? 0 : isUsed.hashCode());
    result = prime * result + ((label == null) ? 0 : label.hashCode());
    result = prime * result + ((numUnspentOutputs == null) ? 0 : numUnspentOutputs.hashCode());
    result = prime * result + ((unlockedBalance == null) ? 0 : unlockedBalance.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroSubaddress other = (MoneroSubaddress) obj;
    if (accountIndex == null) {
      if (other.accountIndex != null) return false;
    } else if (!accountIndex.equals(other.accountIndex)) return false;
    if (address == null) {
      if (other.address != null) return false;
    } else if (!address.equals(other.address)) return false;
    if (balance == null) {
      if (other.balance != null) return false;
    } else if (!balance.equals(other.balance)) return false;
    if (index == null) {
      if (other.index != null) return false;
    } else if (!index.equals(other.index)) return false;
    if (isUsed == null) {
      if (other.isUsed != null) return false;
    } else if (!isUsed.equals(other.isUsed)) return false;
    if (label == null) {
      if (other.label != null) return false;
    } else if (!label.equals(other.label)) return false;
    if (numUnspentOutputs == null) {
      if (other.numUnspentOutputs != null) return false;
    } else if (!numUnspentOutputs.equals(other.numUnspentOutputs)) return false;
    if (unlockedBalance == null) {
      if (other.unlockedBalance != null) return false;
    } else if (!unlockedBalance.equals(other.unlockedBalance)) return false;
    return true;
  }
}
