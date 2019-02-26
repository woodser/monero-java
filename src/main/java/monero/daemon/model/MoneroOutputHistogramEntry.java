package monero.daemon.model;

import java.math.BigInteger;

/**
 * Entry in a Monero output histogram (see get_output_histogram of Daemon RPC documentation).
 */
public class MoneroOutputHistogramEntry {
  
  private BigInteger amount;
  private Integer numInstances;
  private Integer numUnlockedInstances;
  private Integer numRecentInstances;
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public void setAmount(BigInteger amount) {
    this.amount = amount;
  }
  
  public Integer getNumInstances() {
    return numInstances;
  }
  
  public void setNumInstances(Integer numInstances) {
    this.numInstances = numInstances;
  }
  
  public Integer getNumUnlockedInstances() {
    return numUnlockedInstances;
  }
  
  public void setNumUnlockedInstances(Integer numUnlockedInstances) {
    this.numUnlockedInstances = numUnlockedInstances;
  }
  
  public Integer getNumRecentInstances() {
    return numRecentInstances;
  }
  
  public void setNumRecentInstances(Integer numRecentInstances) {
    this.numRecentInstances = numRecentInstances;
  }
}