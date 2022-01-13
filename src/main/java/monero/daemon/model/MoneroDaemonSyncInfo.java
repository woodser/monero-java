package monero.daemon.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Models daemon synchronization information.
 */
public class MoneroDaemonSyncInfo {

  private Long height;
  private List<MoneroPeer> peers;
  private List<MoneroConnectionSpan> spans;
  private Long targetHeight;
  private Integer nextNeededPruningSeed;
  private String overview;
  private BigInteger credits;
  private String topBlockHash;
  
  public Long getHeight() {
    return height;
  }
  
  public void setHeight(Long height) {
    this.height = height;
  }
  
  public List<MoneroPeer> getPeers() {
    return peers;
  }
  
  public void setPeers(List<MoneroPeer> peers) {
    this.peers = peers;
  }
  
  public List<MoneroConnectionSpan> getSpans() {
    return spans;
  }
  
  public void setSpans(List<MoneroConnectionSpan> spans) {
    this.spans = spans;
  }
  
  public Long getTargetHeight() {
    return targetHeight;
  }
  
  public void setTargetHeight(Long targetHeight) {
    this.targetHeight = targetHeight;
  }
  
  public Integer getNextNeededPruningSeed() {
    return nextNeededPruningSeed;
  }
  
  public void setNextNeededPruningSeed(Integer nextNeededPruningSeed) {
    this.nextNeededPruningSeed = nextNeededPruningSeed;
  }
  
  public String getOverview() {
    return overview;
  }
  
  public void setOverview(String overview) {
    this.overview = overview;
  }

  public BigInteger getCredits() {
    return credits;
  }

  public void setCredits(BigInteger credits) {
    this.credits = credits;
  }

  public String getTopBlockHash() {
    return topBlockHash;
  }

  public void setTopBlockHash(String topBlockHash) {
    this.topBlockHash = topBlockHash;
  }
}
