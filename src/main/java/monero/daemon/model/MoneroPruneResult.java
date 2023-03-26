package monero.daemon.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Result of pruning the blockchain.
 */
public class MoneroPruneResult {

  private Boolean isPruned;
  private Integer pruningSeed;
  
  public MoneroPruneResult() {
    // nothing to construct
  }
  
  @JsonProperty("isPruned")
  public Boolean isPruned() {
    return isPruned;
  }
  
  public void setIsPruned(Boolean isPruned) {
    this.isPruned = isPruned;
  }
  
  public Integer getPruningSeed() {
    return pruningSeed;
  }
  
  public void setPruningSeed(Integer pruningSeed) {
    this.pruningSeed = pruningSeed;
  }
}
