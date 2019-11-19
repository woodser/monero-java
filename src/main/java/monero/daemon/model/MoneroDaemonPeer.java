package monero.daemon.model;

import java.math.BigInteger;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Models a peer to the daemon.
 */
public class MoneroDaemonPeer {

  private String id;
  private String address;
  private String host;
  private Integer port;
  private Boolean isOnline;
  private Long lastSeenTimestamp;
  private Integer pruningSeed;
  private Integer rpcPort;
  private BigInteger rpcCreditsPerHash;
  
  public String getId() {
    return id;
  }
  
  public MoneroDaemonPeer setId(String id) {
    this.id = id;
    return this;
  }
  
  public String getAddress() {
    return address;
  }
  
  public MoneroDaemonPeer setAddress(String address) {
    this.address = address;
    return this;
  }
  
  public String getHost() {
    return host;
  }
  
  public MoneroDaemonPeer setHost(String host) {
    this.host = host;
    return this;
  }
  
  public Integer getPort() {
    return port;
  }
  
  public MoneroDaemonPeer setPort(Integer port) {
    this.port = port;
    return this;
  }
  
  @JsonProperty("isOnline")
  public Boolean isOnline() {
    return isOnline;
  }
  
  public MoneroDaemonPeer setIsOnline(Boolean isOnline) {
    this.isOnline = isOnline;
    return this;
  }
  
  public Long getLastSeenTimestamp() {
    return lastSeenTimestamp;
  }
  
  public MoneroDaemonPeer setLastSeenTimestamp(Long lastSeenTimestamp) {
    this.lastSeenTimestamp = lastSeenTimestamp;
    return this;
  }
  
  public Integer getPruningSeed() {
    return pruningSeed;
  }
  
  public MoneroDaemonPeer setPruningSeed(Integer pruningSeed) {
    this.pruningSeed = pruningSeed;
    return this;
  }
  
  public Integer getRpcPort() {
    return rpcPort;
  }
  
  public MoneroDaemonPeer setRpcPort(Integer rpcPort) {
    this.rpcPort = rpcPort;
    return this;
  }
  
  public BigInteger getRpcCreditsPerHash() {
    return this.rpcCreditsPerHash;
  }
  
  public MoneroDaemonPeer setRpcCreditsPerHash(BigInteger rpcCreditsPerHash) {
    this.rpcCreditsPerHash = rpcCreditsPerHash;
    return this;
  }
}
