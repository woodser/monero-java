package daemon.model;

/**
 * Base daemon model including daemon status.
 */
public class MoneroDaemonModel {

  private MoneroDaemonStatus status;

  public MoneroDaemonStatus getStatus() {
    return status;
  }

  public void setStatus(MoneroDaemonStatus status) {
    this.status = status;
  }
}
