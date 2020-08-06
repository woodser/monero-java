package monero.daemon.model;

import com.fasterxml.jackson.annotation.JsonProperty;

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
  
  public MoneroBan setHost(String host) {
    this.host = host;
    return this;
  }
  
  public Integer getIp() {
    return ip;
  }
  
  public MoneroBan setIp(Integer ip) {
    this.ip = ip;
    return this;
  }
  
  @JsonProperty("isBanned")
  public Boolean isBanned() {
    return isBanned;
  }
  
  public MoneroBan setIsBanned(Boolean isBanned) {
    this.isBanned = isBanned;
    return this;
  }
  
  public Long getSeconds() {
    return seconds;
  }
  
  public MoneroBan setSeconds(Long seconds) {
    this.seconds = seconds;
    return this;
  }
}
