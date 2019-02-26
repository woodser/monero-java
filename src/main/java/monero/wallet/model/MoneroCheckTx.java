package monero.wallet.model;

import java.math.BigInteger;

/**
 * Results from checking a transaction key.
 */
public class MoneroCheckTx extends MoneroCheck {

  public Boolean inTxPool;
  public Integer numConfirmations;
  public BigInteger receivedAmount;
  
  public Boolean getInTxPool() {
    return inTxPool;
  }
  
  public void setInTxPool(Boolean inTxPool) {
    this.inTxPool = inTxPool;
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
  
  public void setReceivedAmount(BigInteger receivedAmount) {
    this.receivedAmount = receivedAmount;
  }
}
