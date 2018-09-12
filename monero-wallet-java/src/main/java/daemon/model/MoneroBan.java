package daemon.model;

/**
 * Monero banhammer.
 */
public class MoneroBan extends MoneroDaemonModel {
  
  private String host;  // e.g. 192.168.1.100
  private Integer ip;   // integer formatted IP
  private Boolean isBanned;
  private Integer seconds;
  
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
  
  public Boolean getIsBanned() {
    return isBanned;
  }
  
  public void setIsBanned(Boolean isBanned) {
    this.isBanned = isBanned;
  }
  
  public Integer getSeconds() {
    return seconds;
  }
  
  public void setSeconds(Integer seconds) {
    this.seconds = seconds;
  }
}
