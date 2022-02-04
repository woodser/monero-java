package monero.daemon.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import common.utils.GenUtils;
import java.math.BigInteger;

/**
 * Models a peer to the daemon.
 */
public class MoneroPeer {

  private String id;
  private String address;
  private String host;
  private Integer port;
  private Boolean isOnline;
  private Long lastSeenTimestamp;
  private Integer pruningSeed;
  private Integer rpcPort;
  private BigInteger rpcCreditsPerHash;
  private String hash;
  private Long avgDownload;
  private Long avgUpload;
  private Long currentDownload;
  private Long currentUpload;
  private Long height;
  private Boolean isIncoming;
  private Long liveTime;
  private Boolean isLocalIp;
  private Boolean isLocalHost;
  private Integer numReceives;
  private Integer numSends;
  private Long receiveIdleTime;
  private Long sendIdleTime;
  private String state;
  private Integer numSupportFlags;
  private ConnectionType type;
  
  public String getId() {
    return id;
  }
  
  public MoneroPeer setId(String id) {
    this.id = id;
    return this;
  }
  
  public String getAddress() {
    return address;
  }
  
  public MoneroPeer setAddress(String address) {
    this.address = address;
    return this;
  }
  
  public String getHost() {
    return host;
  }
  
  public MoneroPeer setHost(String host) {
    this.host = host;
    return this;
  }
  
  public Integer getPort() {
    return port;
  }
  
  public MoneroPeer setPort(Integer port) {
    this.port = port;
    return this;
  }
  
  @JsonProperty("isOnline")
  public Boolean isOnline() {
    return isOnline;
  }
  
  public MoneroPeer setIsOnline(Boolean isOnline) {
    this.isOnline = isOnline;
    return this;
  }
  
  public Long getLastSeenTimestamp() {
    return lastSeenTimestamp;
  }
  
  public MoneroPeer setLastSeenTimestamp(Long lastSeenTimestamp) {
    this.lastSeenTimestamp = lastSeenTimestamp;
    return this;
  }
  
  public Integer getPruningSeed() {
    return pruningSeed;
  }
  
  public MoneroPeer setPruningSeed(Integer pruningSeed) {
    this.pruningSeed = pruningSeed;
    return this;
  }
  
  public Integer getRpcPort() {
    return rpcPort;
  }
  
  public MoneroPeer setRpcPort(Integer rpcPort) {
    this.rpcPort = rpcPort;
    return this;
  }
  
  public BigInteger getRpcCreditsPerHash() {
    return this.rpcCreditsPerHash;
  }
  
  public MoneroPeer setRpcCreditsPerHash(BigInteger rpcCreditsPerHash) {
    this.rpcCreditsPerHash = rpcCreditsPerHash;
    return this;
  }
  
  public String getHash() {
    return hash;
  }
  
  public void setHash(String hash) {
    this.hash = hash;
  }
  
  public Long getAvgDownload() {
    return avgDownload;
  }
  
  public void setAvgDownload(Long avgDownload) {
    this.avgDownload = avgDownload;
  }
  
  public Long getAvgUpload() {
    return avgUpload;
  }
  
  public void setAvgUpload(Long avgUpload) {
    this.avgUpload = avgUpload;
  }
  
  public Long getCurrentDownload() {
    return currentDownload;
  }
  
  public void setCurrentDownload(Long currentDownload) {
    this.currentDownload = currentDownload;
  }
  
  public Long getCurrentUpload() {
    return currentUpload;
  }
  
  public void setCurrentUpload(Long currentUpload) {
    this.currentUpload = currentUpload;
  }
  
  public Long getHeight() {
    return height;
  }
  
  public void setHeight(Long height) {
    this.height = height;
  }
  
  @JsonProperty("isIncoming")
  public Boolean isIncoming() {
    return isIncoming;
  }
  
  public void setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
  }
  
  public Long getLiveTime() {
    return liveTime;
  }
  
  public void setLiveTime(Long liveTime) {
    this.liveTime = liveTime;
  }
  
  @JsonProperty("isLocalIp")
  public Boolean isLocalIp() {
    return isLocalIp;
  }
  
  public void setIsLocalIp(Boolean isLocalIp) {
    this.isLocalIp = isLocalIp;
  }
  
  public Boolean isLocalHost() {
    return isLocalHost;
  }
  
  public void setIsLocalHost(Boolean isLocalHost) {
    this.isLocalHost = isLocalHost;
  }
  
  public Integer getNumReceives() {
    return numReceives;
  }
  
  public void setNumReceives(Integer numReceives) {
    this.numReceives = numReceives;
  }
  
  public Integer getNumSends() {
    return numSends;
  }
  
  public void setNumSends(Integer numSends) {
    this.numSends = numSends;
  }
  
  public Long getReceiveIdleTime() {
    return receiveIdleTime;
  }
  
  public void setReceiveIdleTime(Long receiveIdleTime) {
    this.receiveIdleTime = receiveIdleTime;
  }
  
  public Long getSendIdleTime() {
    return sendIdleTime;
  }
  
  public void setSendIdleTime(Long sendIdleTime) {
    this.sendIdleTime = sendIdleTime;
  }
  
  public String getState() {
    return state;
  }
  
  public void setState(String state) {
    this.state = state;
  }
  
  public Integer getNumSupportFlags() {
    return numSupportFlags;
  }
  
  public void setNumSupportFlags(Integer numSupportFlags) {
    this.numSupportFlags = numSupportFlags;
  }

  public ConnectionType getType() {
    return type;
  }

  public void setType(ConnectionType type) {
    this.type = type;
  }
  
  @Override
  public String toString() {
    int indent = 0;
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.getIndent(indent) + "=== MoneroPeer ===\n");
    sb.append(GenUtils.kvLine("id", getId(), indent));
    sb.append(GenUtils.kvLine("address", getHeight(), indent));
    sb.append(GenUtils.kvLine("host", getHost(), indent));
    sb.append(GenUtils.kvLine("port", getPort(), indent));
    sb.append(GenUtils.kvLine("isOnline", isOnline(), indent));
    sb.append(GenUtils.kvLine("lastSeenTimestamp", getLastSeenTimestamp(), indent));
    sb.append(GenUtils.kvLine("pruningSeed", getPruningSeed(), indent));
    sb.append(GenUtils.kvLine("rpcPort", getRpcPort(), indent));
    sb.append(GenUtils.kvLine("rpcCreditsPerHash", getRpcCreditsPerHash(), indent));
    sb.append(GenUtils.kvLine("hash", getHash(), indent));
    sb.append(GenUtils.kvLine("avgDownload", getAvgDownload(), indent));
    sb.append(GenUtils.kvLine("avgUpload", getAvgUpload(), indent));
    sb.append(GenUtils.kvLine("currentDownload", getCurrentDownload(), indent));
    sb.append(GenUtils.kvLine("currentUpload", getCurrentUpload(), indent));
    sb.append(GenUtils.kvLine("height", getHeight(), indent));
    sb.append(GenUtils.kvLine("isIncoming", isIncoming(), indent));
    sb.append(GenUtils.kvLine("isLocalIp", isLocalIp(), indent));
    sb.append(GenUtils.kvLine("isLocalHost", isLocalHost(), indent));
    sb.append(GenUtils.kvLine("numReceives", getNumReceives(), indent));
    sb.append(GenUtils.kvLine("numSends", getNumSends(), indent));
    sb.append(GenUtils.kvLine("receiveIdleTime", getReceiveIdleTime(), indent));
    sb.append(GenUtils.kvLine("sendIdleTime", getSendIdleTime(), indent));
    sb.append(GenUtils.kvLine("state", getState(), indent));
    sb.append(GenUtils.kvLine("numSupportFlags", getNumSupportFlags(), indent));
    sb.append(GenUtils.kvLine("type", getType(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);
  }
}
