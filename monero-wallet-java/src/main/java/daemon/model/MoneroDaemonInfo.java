package daemon.model;

import daemon.MoneroDaemonDefault.MoneroNetworkType;

/**
 * Monero daemon info.
 */
public class MoneroDaemonInfo extends MoneroDaemonModel {
  
  private Integer altBlocksCount;
  private Integer blockSizeLimit;
  private Integer blockSizeMedian;
  private String bootstrapDaemonAddress;
  private Integer cumulativeDifficulty;
  private Integer difficulty;
  private Integer freeSpace;
  private Integer greyPeerlistSize;
  private Integer whitePeerlistSize;
  private Integer height;
  private Integer heightWithoutBootstrap;
  private MoneroNetworkType networkType;
  private Boolean isOffline;
  private Integer incomingConnectionsCount;
  private Integer outgoingConnectionsCount;
  private Integer rpcConnectionsCount;
  private Long startTime;
  private Integer target;
  private Integer targetHeight;
  private String topBlockHash;
  private Integer txCount;
  private Integer txPoolSize;
  private Boolean wasBootstrapEverUsed;
  private Integer version;
  
  public Integer getAltBlocksCount() {
    return altBlocksCount;
  }
  
  public void setAltBlocksCount(Integer altBlocksCount) {
    this.altBlocksCount = altBlocksCount;
  }
  
  public Integer getBlockSizeLimit() {
    return blockSizeLimit;
  }
  
  public void setBlockSizeLimit(Integer blockSizeLimit) {
    this.blockSizeLimit = blockSizeLimit;
  }
  
  public Integer getBlockSizeMedian() {
    return blockSizeMedian;
  }
  
  public void setBlockSizeMedian(Integer blockSizeMedian) {
    this.blockSizeMedian = blockSizeMedian;
  }
  
  public String getBootstrapDaemonAddress() {
    return bootstrapDaemonAddress;
  }
  
  public void setBootstrapDaemonAddress(String bootstrapDaemonAddress) {
    this.bootstrapDaemonAddress = bootstrapDaemonAddress;
  }
  
  public Integer getCumulativeDifficulty() {
    return cumulativeDifficulty;
  }
  
  public void setCumulativeDifficulty(Integer cumulativeDifficulty) {
    this.cumulativeDifficulty = cumulativeDifficulty;
  }
  
  public Integer getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(Integer difficulty) {
    this.difficulty = difficulty;
  }
  
  public Integer getFreeSpace() {
    return freeSpace;
  }
  
  public void setFreeSpace(Integer freeSpace) {
    this.freeSpace = freeSpace;
  }
  
  public Integer getGreyPeerlistSize() {
    return greyPeerlistSize;
  }
  
  public void setGreyPeerlistSize(Integer greyPeerlistSize) {
    this.greyPeerlistSize = greyPeerlistSize;
  }
  
  public Integer getWhitePeerlistSize() {
    return whitePeerlistSize;
  }
  
  public void setWhitePeerlistSize(Integer whitePeerlistSize) {
    this.whitePeerlistSize = whitePeerlistSize;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public Integer getHeightWithoutBootstrap() {
    return heightWithoutBootstrap;
  }
  
  public void setHeightWithoutBootstrap(Integer heightWithoutBootstrap) {
    this.heightWithoutBootstrap = heightWithoutBootstrap;
  }
  
  public MoneroNetworkType getNetworkType() {
    return networkType;
  }

  public void setNetworkType(MoneroNetworkType networkType) {
    this.networkType = networkType;
  }

  public Boolean getIsOffline() {
    return isOffline;
  }
  
  public void setIsOffline(Boolean isOffline) {
    this.isOffline = isOffline;
  }
  
  public Integer getIncomingConnectionsCount() {
    return incomingConnectionsCount;
  }
  
  public void setIncomingConnectionsCount(Integer incomingConnectionsCount) {
    this.incomingConnectionsCount = incomingConnectionsCount;
  }
  
  public Integer getOutgoingConnectionsCount() {
    return outgoingConnectionsCount;
  }
  
  public void setOutgoingConnectionsCount(Integer outgoingConnectionsCount) {
    this.outgoingConnectionsCount = outgoingConnectionsCount;
  }
  
  public Integer getRpcConnectionsCount() {
    return rpcConnectionsCount;
  }
  
  public void setRpcConnectionsCount(Integer rpcConnectionsCount) {
    this.rpcConnectionsCount = rpcConnectionsCount;
  }
  
  public Long getStartTime() {
    return startTime;
  }
  
  public void setStartTime(Long startTime) {
    this.startTime = startTime;
  }
  
  public Integer getTarget() {
    return target;
  }
  
  public void setTarget(Integer target) {
    this.target = target;
  }
  public Integer getTargetHeight() {
    return targetHeight;
  }
  
  public void setTargetHeight(Integer targetHeight) {
    this.targetHeight = targetHeight;
  }
  
  public String getTopBlockHash() {
    return topBlockHash;
  }
  
  public void setTopBlockHash(String topBlockHash) {
    this.topBlockHash = topBlockHash;
  }
  
  public Integer getTxCount() {
    return txCount;
  }
  
  public void setTxCount(Integer txCount) {
    this.txCount = txCount;
  }
  
  public Integer getTxPoolSize() {
    return txPoolSize;
  }
  
  public void setTxPoolSize(Integer txPoolSize) {
    this.txPoolSize = txPoolSize;
  }
  
  public Boolean getWasBootstrapEverUsed() {
    return wasBootstrapEverUsed;
  }
  
  public void setWasBootstrapEverUsed(Boolean wasBootstrapEverUsed) {
    this.wasBootstrapEverUsed = wasBootstrapEverUsed;
  }
  
  public Integer getVersion() {
    return version;
  }
  
  public void setVersion(Integer version) {
    this.version = version;
  }
}
