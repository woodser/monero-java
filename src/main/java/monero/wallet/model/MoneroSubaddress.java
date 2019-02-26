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
}
