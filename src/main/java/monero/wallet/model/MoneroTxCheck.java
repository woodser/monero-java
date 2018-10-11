package monero.wallet.model;

import java.math.BigInteger;

/**
 * Results from checking a transaction key or proof.
 */
public class MoneroTxCheck {

  public Boolean isGood;
  public Boolean isInPool;
  public Integer numConfirmations;
  public BigInteger receivedAmount;
  public BigInteger spentAmount;
  public BigInteger totalAmount;  // TODO: separate into different check objects?
  
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
  
  public BigInteger getReceivedAmount() {
    return receivedAmount;
  }
  
  public void setReceivedAmount(BigInteger amountReceived) {
    this.receivedAmount = amountReceived;
  }

  public BigInteger getSpentAmount() {
    return spentAmount;
  }

  public void setSpentAmount(BigInteger spentAmount) {
    this.spentAmount = spentAmount;
  }

  public BigInteger getTotalAmount() {
    return totalAmount;
  }

  public void setTotalAmount(BigInteger totalAmount) {
    this.totalAmount = totalAmount;
  }
}
