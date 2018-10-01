package monero.daemon.model;

import java.math.BigInteger;

/**
 * Monero fee estimate.
 */
public class MoneroFeeEstimate extends MoneroDaemonModel {

  private BigInteger feeEstimate;

  public BigInteger getFeeEstimate() {
    return feeEstimate;
  }

  public void setFeeEstimate(BigInteger feeEstimate) {
    this.feeEstimate = feeEstimate;
  }
}
