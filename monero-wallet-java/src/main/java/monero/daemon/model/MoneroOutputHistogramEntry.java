package monero.daemon.model;

import java.math.BigInteger;

/**
 * Entry in a Monero output histogram (see get_output_histogram of Daemon RPC documentation).
 */
public class MoneroOutputHistogramEntry extends MoneroDaemonModel {

  private BigInteger amount;
  private Integer totalInstances;
  private Integer unlockedInstances;
  private Integer recentInstances;
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public void setAmount(BigInteger amount) {
    this.amount = amount;
  }

  public Integer getTotalInstances() {
    return totalInstances;
  }

  public void setTotalInstances(Integer totalInstances) {
    this.totalInstances = totalInstances;
  }

  public Integer getUnlockedInstances() {
    return unlockedInstances;
  }

  public void setUnlockedInstances(Integer unlockedInstances) {
    this.unlockedInstances = unlockedInstances;
  }

  public Integer getRecentInstances() {
    return recentInstances;
  }

  public void setRecentInstances(Integer recentInstances) {
    this.recentInstances = recentInstances;
  }
}