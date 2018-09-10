package daemon.model;

/**
 * Monero daemon status when request is sent and response received.
 */
public class MoneroDaemonStatus {

  private String status;
  private Boolean isUntrusted;
  
  public String getStatus() {
    return status;
  }
  
  public void setStatus(String status) {
    this.status = status;
  }
  
  public Boolean getIsUntrusted() {
    return isUntrusted;
  }
  
  public void setIsUntrusted(Boolean isUntrusted) {
    this.isUntrusted = isUntrusted;
  }
}
