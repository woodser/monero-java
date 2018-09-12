package daemon.model;

import java.math.BigInteger;

/**
 * Model for the summation of coinbase emissions and fees.
 */
public class MoneroCoinbaseTxSum extends MoneroDaemonModel {

  private BigInteger emissionSum;
  private BigInteger feeSum;
  
  public BigInteger getEmissionSum() {
    return emissionSum;
  }
  
  public void setEmissionSum(BigInteger emissionSum) {
    this.emissionSum = emissionSum;
  }
  
  public BigInteger getFeeSum() {
    return feeSum;
  }
  
  public void setFeeSum(BigInteger feeSum) {
    this.feeSum = feeSum;
  }
}