package monero.wallet.model;

import java.math.BigInteger;

/**
 * Results from checking a reserve proof.
 */
public class MoneroCheckReserve extends MoneroCheck {

  public BigInteger amountSpent;
  public BigInteger amountTotal;
  
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
