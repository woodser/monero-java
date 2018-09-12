package daemon.model;

import java.util.List;

/**
 * Models daemon synchronization information.
 */
public class MoneroDaemonSyncInfo extends MoneroDaemonModel {

  private Integer height;
  private List<MoneroDaemonConnection> peers;
  private List<MoneroDaemonConnectionSpan> spans; // TODO: incorporate span info into daemon connection?
  private Integer targetHeight;
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public List<MoneroDaemonConnection> getPeers() {
    return peers;
  }
  
  public void setPeers(List<MoneroDaemonConnection> peers) {
    this.peers = peers;
  }
  
  public List<MoneroDaemonConnectionSpan> getSpans() {
    return spans;
  }
  
  public void setSpans(List<MoneroDaemonConnectionSpan> spans) {
    this.spans = spans;
  }
  
  public Integer getTargetHeight() {
    return targetHeight;
  }
  
  public void setTargetHeight(Integer targetHeight) {
    this.targetHeight = targetHeight;
  }
}
