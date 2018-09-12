package daemon.model;

/**
 * Monero daemon connection span.
 */
public class MoneroDaemonConnectionSpan extends MoneroDaemonModel {

  private String connectionId;
  private Integer numBlocks;
  private Integer connectionRate;
  private String remoteAddress;
  private Integer size;
  private Integer speed;
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

  public Integer getConnectionRate() {
    return connectionRate;
  }

  public void setConnectionRate(Integer connectionRate) {
    this.connectionRate = connectionRate;
  }

  public String getRemoteAddress() {
    return remoteAddress;
  }

  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }

  public Integer getSize() {
    return size;
  }

  public void setSize(Integer size) {
    this.size = size;
  }

  public Integer getSpeed() {
    return speed;
  }

  public void setSpeed(Integer speed) {
    this.speed = speed;
  }

  public Integer getStartBlockHeight() {
    return startBlockHeight;
  }

  public void setStartBlockHeight(Integer startBlockHeight) {
    this.startBlockHeight = startBlockHeight;
  }
}
