package daemon.model;

import java.math.BigInteger;

/**
 * Monero daemon connection span.
 */
public class MoneroDaemonConnectionSpan extends MoneroDaemonModel {

  private String connectionId;
  private Integer numBlocks;
  private String remoteAddress;
  private BigInteger rate;
  private BigInteger speed;
  private BigInteger size;
  private Integer startBlockHeight;
  
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
  
  public BigInteger getRate() {
    return rate;
  }
  
  public void setRate(BigInteger rate) {
    this.rate = rate;
  }
  
  public BigInteger getSpeed() {
    return speed;
  }
  
  public void setSpeed(BigInteger speed) {
    this.speed = speed;
  }
  
  public BigInteger getSize() {
    return size;
  }
  
  public void setSize(BigInteger size) {
    this.size = size;
  }
  
  public Integer getStartBlockHeight() {
    return startBlockHeight;
  }
  
  public void setStartBlockHeight(Integer startBlockHeight) {
    this.startBlockHeight = startBlockHeight;
  }
}
