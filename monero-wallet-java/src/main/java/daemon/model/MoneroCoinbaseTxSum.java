package daemon.model;

import java.math.BigInteger;

/**
 * Model for the summation of coinbase emissions and fees.
 */
public class MoneroCoinbaseTxSum {

  private BigInteger emissionSum;
  private BigInteger feeSum;
  private MoneroDaemonStatus responseInfo;
}