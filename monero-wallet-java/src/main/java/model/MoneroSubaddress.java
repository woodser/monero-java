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
  
  public MoneroAddress getAddress() {
    return address;
  }
  
  public void setAddress(MoneroAddress address) {
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
}
