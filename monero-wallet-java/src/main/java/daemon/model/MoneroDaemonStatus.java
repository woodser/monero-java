package daemon.model;

/**
 * Base daemon model including status.
 */
public class MoneroDaemonStatus {

  private String status;
  private Boolean isTrusted;
  
  public String getStatus() {
    return status;
  }
  
  public void setStatus(String status) {
    this.status = status;
  }
  
  public Boolean isTrusted() {
    return isTrusted;
  }
  
  public void setTrusted(Boolean isTrusted) {
    this.isTrusted = isTrusted;
  }
}
