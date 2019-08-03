package monero.daemon.model;

/**
 * Monero banhammer.
 */
public class MoneroBan {
  
  private String host;  // e.g. 192.168.1.100
  private Integer ip;   // integer formatted IP
  private Boolean isBanned;
  private Long seconds;
  
  public String getHost() {
    return host;
  }
  
  public void setHost(String host) {
    this.host = host;
  }
  
  public Integer getIp() {
    return ip;
  }
  
  public void setIp(Integer ip) {
    this.ip = ip;
  }
  
  public Boolean isBanned() {
    return isBanned;
  }
  
  public void setIsBanned(Boolean isBanned) {
    this.isBanned = isBanned;
  }
  
  public Long getSeconds() {
    return seconds;
  }
  
  public void setSeconds(Long seconds) {
    this.seconds = seconds;
  }
}
