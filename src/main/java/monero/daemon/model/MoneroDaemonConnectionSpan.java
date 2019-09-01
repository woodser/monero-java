package monero.daemon.model;

import java.math.BigInteger;

/**
 * Monero daemon connection span.
 */
public class MoneroDaemonConnectionSpan {

  private String connectionId;
  private Integer numBlocks;	// TODO: LONG
  private String remoteAddress;
  private Long rate;
  private Long speed;
  private Long size;
  private BigInteger startBlockHeight;  // TODO: Long
  
  public String getConnectionId() {
    return connectionId;
  }
  
  public void setConnectionId(String connectionId) {
    this.connectionId = connectionId;
  }
  
  public Integer getNumBlocks() {
    return numBlocks;
  }
  
  public void setNumBlocks(Integer numBlocks) {
    this.numBlocks = numBlocks;
  }
  
  public String getRemoteAddress() {
    return remoteAddress;
  }
  
  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }
  
  public Long getRate() {
    return rate;
  }
  
  public void setRate(Long rate) {
    this.rate = rate;
  }
  
  public Long getSpeed() {
    return speed;
  }
  
  public void setSpeed(Long speed) {
    this.speed = speed;
  }
  
  public Long getSize() {
    return size;
  }
  
  public void setSize(Long size) {
    this.size = size;
  }
  
  public BigInteger getStartBlockHeight() {
    return startBlockHeight;
  }
  
  public void setStartBlockHeight(BigInteger startBlockHeight) {
    this.startBlockHeight = startBlockHeight;
  }
}
