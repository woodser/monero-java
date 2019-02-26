package monero.daemon.model;

/**
 * Monero daemon connection.
 */
public class MoneroDaemonConnection {
  
  private MoneroDaemonPeer peer;
  private String id;
  private Integer avgDownload;
  private Integer avgUpload;
  private Integer currentDownload;
  private Integer currentUpload;
  private Integer height;
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
  
  public MoneroDaemonPeer getPeer() {
    return peer;
  }
  
  public void setPeer(MoneroDaemonPeer peer) {
    this.peer = peer;
  }
  
  public String getId() {
    return id;
  }
  
  public void setId(String id) {
    this.id = id;
  }
  
  public Integer getAvgDownload() {
    return avgDownload;
  }
  
  public void setAvgDownload(Integer avgDownload) {
    this.avgDownload = avgDownload;
  }
  
  public Integer getAvgUpload() {
    return avgUpload;
  }
  
  public void setAvgUpload(Integer avgUpload) {
    this.avgUpload = avgUpload;
  }
  
  public Integer getCurrentDownload() {
    return currentDownload;
  }
  
  public void setCurrentDownload(Integer currentDownload) {
    this.currentDownload = currentDownload;
  }
  
  public Integer getCurrentUpload() {
    return currentUpload;
  }
  
  public void setCurrentUpload(Integer currentUpload) {
    this.currentUpload = currentUpload;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public Boolean getIsIncoming() {
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
  
  public Boolean getIsLocalIp() {
    return isLocalIp;
  }
  
  public void setIsLocalIp(Boolean isLocalIp) {
    this.isLocalIp = isLocalIp;
  }
  
  public Boolean getIsLocalHost() {
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
}
