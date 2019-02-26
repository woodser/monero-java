package monero.wallet.model;

import java.math.BigInteger;

/**
 * Results from checking a reserve proof.
 */
public class MoneroCheckReserve extends MoneroCheck {
  
  private BigInteger spentAmount;
  private BigInteger totalAmount;
  
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
