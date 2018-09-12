package daemon.model;

/**
 * Monero hard fork info.
 */
public class MoneroHardForkInfo extends MoneroDaemonModel {

  private Integer earliestHeight;
  private Boolean isEnabled;
  private Integer state;
  private Integer threshold;
  private Integer version;
  private Integer votes;
  private Integer voting;
  private Integer window;

  public Integer getEarliestHeight() {
    return earliestHeight;
  }

  public void setEarliestHeight(Integer earliestHeight) {
    this.earliestHeight = earliestHeight;
  }

  public Boolean getIsEnabled() {
    return isEnabled;
  }

  public void setIsEnabled(Boolean isEnabled) {
    this.isEnabled = isEnabled;
  }

  public Integer getState() {
    return state;
  }

  public void setState(Integer state) {
    this.state = state;
  }

  public Integer getThreshold() {
    return threshold;
  }

  public void setThreshold(Integer threshold) {
    this.threshold = threshold;
  }

  public Integer getVersion() {
    return version;
  }

  public void setVersion(Integer version) {
    this.version = version;
  }

  public Integer getVotes() {
    return votes;
  }

  public void setVotes(Integer votes) {
    this.votes = votes;
  }

  public Integer getVoting() {
    return voting;
  }

  public void setVoting(Integer voting) {
    this.voting = voting;
  }

  public Integer getWindow() {
    return window;
  }

  public void setWindow(Integer window) {
    this.window = window;
  }
}
