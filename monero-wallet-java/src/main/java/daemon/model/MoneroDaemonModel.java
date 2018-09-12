package daemon.model;

/**
 * Base daemon model including a reference to common daemon response information (status, isTrusted, etc).
 */
public class MoneroDaemonModel {
  
  private MoneroDaemonResponseInfo responseInfo;

  public MoneroDaemonResponseInfo getResponseInfo() {
    return responseInfo;
  }

  public void setResponseInfo(MoneroDaemonResponseInfo responseInfo) {
    this.responseInfo = responseInfo;
  }
}
