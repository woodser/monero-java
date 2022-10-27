package monero.daemon.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import common.utils.GenUtils;

/**
 * Models a Monero fee estimate.
 */
public class MoneroFeeEstimate {

  private BigInteger fee;
  private List<BigInteger> fees;
  private BigInteger quantizationMask;
  
  public MoneroFeeEstimate() {
    // nothing to construct
  }

  public MoneroFeeEstimate(BigInteger fee, List<BigInteger> fees, BigInteger quantizationMask) {
    this.fee = fee;
    this.fees = fees;
    this.quantizationMask = quantizationMask;
  }

  public MoneroFeeEstimate(MoneroFeeEstimate feeEstimate) {
    this.fee = feeEstimate.fee;
    this.fees = new ArrayList<BigInteger>(feeEstimate.fees);
    this.quantizationMask = feeEstimate.quantizationMask;
  }
  
  public BigInteger getFee() {
    return fee;
  }
  
  public MoneroFeeEstimate setFee(BigInteger fee) {
    this.fee = fee;
    return this;
  }
  
  public List<BigInteger> getFees() {
    return fees;
  }
  
  public MoneroFeeEstimate setFees(List<BigInteger> fees) {
    this.fees = fees;
    return this;
  }
  
  public BigInteger getQuantizationMask() {
    return quantizationMask;
  }
  
  public MoneroFeeEstimate setQuantizationMask(BigInteger quantizationMask) {
    this.quantizationMask = quantizationMask;
    return this;
  }
  
  public MoneroFeeEstimate copy() {
    return new MoneroFeeEstimate(this);
  }
  
  @Override
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.kvLine("Fee", getFee(), indent));
    sb.append(GenUtils.kvLine("Fees", getFees(), indent));
    sb.append(GenUtils.kvLine("Quantization mask", getQuantizationMask(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);  // strip newline
  }
}
