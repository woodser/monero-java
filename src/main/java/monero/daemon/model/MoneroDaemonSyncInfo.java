package monero.daemon.model;

import java.util.List;

/**
 * Models daemon synchronization information.
 */
public class MoneroDaemonSyncInfo {

  private Integer height;
  private List<MoneroDaemonConnection> connections;
  private List<MoneroDaemonConnectionSpan> spans;
  private Integer targetHeight;
  private Integer nextNeededPruningSeed;
  private String overview;
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public List<MoneroDaemonConnection> getConnections() {
    return connections;
  }
  
  public void setConnections(List<MoneroDaemonConnection> connections) {
    this.connections = connections;
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
}
