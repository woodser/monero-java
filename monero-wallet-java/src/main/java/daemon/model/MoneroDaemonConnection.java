package daemon.model;

/**
 * Monero daemon connection.
 */
public class MoneroDaemonConnection extends MoneroDaemonModel {

  private String id;
  private String address;
  private Integer avgDownload;
  private Integer avgUpload;
  private Integer currentDownload;
  private Integer currentUpload;
  private Integer height;
  private String host;
  private Boolean isIncoming;
  private String ip;
  private Integer liveTime;
  private Boolean isLocalIp;
  private Boolean isLocalhost;
  private String peerId;
  private String port;
  private Integer receiveCount;
  private Integer receiveIdleTime;
  private Integer sendCount;
  private Integer sendIdleTime;
  private String state;
  private Integer supportFlags;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
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

  public String getHost() {
    return host;
  }

  public void setHost(String host) {
    this.host = host;
  }

  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public void setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
  }

  public String getIp() {
    return ip;
  }

  public void setIp(String ip) {
    this.ip = ip;
  }

  public Integer getLiveTime() {
    return liveTime;
  }

  public void setLiveTime(Integer liveTime) {
    this.liveTime = liveTime;
  }

  public Boolean getIsLocalIp() {
    return isLocalIp;
  }

  public void setIsLocalIp(Boolean isLocalIp) {
    this.isLocalIp = isLocalIp;
  }

  public Boolean getIsLocalhost() {
    return isLocalhost;
  }

  public void setIsLocalhost(Boolean isLocalhost) {
    this.isLocalhost = isLocalhost;
  }

  public String getPeerId() {
    return peerId;
  }

  public void setPeerId(String peerId) {
    this.peerId = peerId;
  }

  public String getPort() {
    return port;
  }

  public void setPort(String port) {
    this.port = port;
  }

  public Integer getReceiveCount() {
    return receiveCount;
  }

  public void setReceiveCount(Integer receiveCount) {
    this.receiveCount = receiveCount;
  }

  public Integer getReceiveIdleTime() {
    return receiveIdleTime;
  }

  public void setReceiveIdleTime(Integer receiveIdleTime) {
    this.receiveIdleTime = receiveIdleTime;
  }

  public Integer getSendCount() {
    return sendCount;
  }

  public void setSendCount(Integer sendCount) {
    this.sendCount = sendCount;
  }

  public Integer getSendIdleTime() {
    return sendIdleTime;
  }

  public void setSendIdleTime(Integer sendIdleTime) {
    this.sendIdleTime = sendIdleTime;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  public Integer getSupportFlags() {
    return supportFlags;
  }

  public void setNumSupportFlags(Integer supportFlags) {
    this.supportFlags = supportFlags;
  }
}
