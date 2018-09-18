package monero.daemon.model;

import java.math.BigInteger;

/**
 * Model for the summation of coinbase emissions and fees.
 */
public class MoneroCoinbaseTxSum extends MoneroDaemonModel {

  private BigInteger totalEmission;
  private BigInteger totalFees;
  
  public BigInteger getTotalEmission() {
    return totalEmission;
  }
  
  public void setTotalEmission(BigInteger totalEmission) {
    this.totalEmission = totalEmission;
  }
  
  public BigInteger getTotalFees() {
    return totalFees;
  }
  
  public void setTotalFees(BigInteger totalFees) {
    this.totalFees = totalFees;
  }  
}