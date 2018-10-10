package monero.wallet.model;

import java.math.BigInteger;

/**
 * Results from checking a transaction key or proof.
 */
public class MoneroTxCheck {

  public Boolean isInPool;
  public Integer numConfirmations;
  public BigInteger amountReceived;
  
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
}
