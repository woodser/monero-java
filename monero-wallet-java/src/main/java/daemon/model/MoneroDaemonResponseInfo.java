package daemon.model;

/**
 * Model for common daemon response attributes.
 */
public class MoneroDaemonResponseInfo {

  private String status;
  private Boolean isTrusted;
  
  public String getStatus() {
    return status;
  }
  
  public void setStatus(String status) {
    this.status = status;
  }
  
  public Boolean getIsTrusted() {
    return isTrusted;
  }
  
  public void setIsTrusted(Boolean isTrusted) {
    this.isTrusted = isTrusted;
  }
}
