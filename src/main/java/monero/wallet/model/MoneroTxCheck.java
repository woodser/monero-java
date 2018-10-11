package monero.wallet.model;

import java.math.BigInteger;

/**
 * Results from checking a transaction key or proof.
 */
public class MoneroTxCheck {

  public Boolean isGood;
  public Boolean isInPool;
  public Integer numConfirmations;
  public BigInteger amountReceived;
  public BigInteger amountSpent;
  public BigInteger amountTotal;  // TODO: separate into different check objects?
  
  public Boolean getIsGood() {
    return isGood;
  }

  public void setIsGood(Boolean isGood) {
    this.isGood = isGood;
  }

  public Boolean getIsInPool() {
    return isInPool;
  }
  
  public void setIsInPool(Boolean isInPool) {
    this.isInPool = isInPool;
  }
  
  public Integer getNumConfirmations() {
    return numConfirmations;
  }
  
  public void setNumConfirmations(Integer numConfirmations) {
    this.numConfirmations = numConfirmations;
  }
  
  public BigInteger getAmountReceived() {
    return amountReceived;
  }
  
  public void setAmountReceived(BigInteger amountReceived) {
    this.amountReceived = amountReceived;
  }

  public BigInteger getAmountSpent() {
    return amountSpent;
  }

  public void setAmountSpent(BigInteger amountSpent) {
    this.amountSpent = amountSpent;
  }

  public BigInteger getAmountTotal() {
    return amountTotal;
  }

  public void setAmountTotal(BigInteger amountTotal) {
    this.amountTotal = amountTotal;
  }
}
