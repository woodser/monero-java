package monero.daemon.model;

import java.math.BigInteger;

/**
 * Monero daemon info.
 */
public class MoneroDaemonInfo {
  
  private String version;
  private Integer numAltBlocks;
  private Integer blockSizeLimit;
  private Integer blockSizeMedian;
  private Integer blockWeightMedian;
  private String bootstrapDaemonAddress;
  private BigInteger difficulty;
  private BigInteger cumulativeDifficulty;
  private BigInteger freeSpace;
  private Integer numOfflinePeers;
  private Integer numOnlinePeers;
  private Integer height;
  private Integer heightWithoutBootstrap;
  private MoneroNetworkType networkType;
  private Boolean isOffline;
  private Integer numIncomingConnections;
  private Integer numOutgoingConnections;
  private Integer numRpcConnections;
  private Long startTimestamp;
  private Integer target;
  private Integer targetHeight;
  private String topBlockId;
  private Integer numTxs;
  private Integer numTxsPool;
  private Boolean wasBootstrapEverUsed;
  private Integer databaseSize;
  private Boolean updateAvailable;
  
  public String getVersion() {
    return version;
  }
  
  public void setVersion(String version) {
    this.version = version;
  }
  
  public Integer getNumAltBlocks() {
    return numAltBlocks;
  }
  
  public void setNumAltBlocks(Integer numAltBlocks) {
    this.numAltBlocks = numAltBlocks;
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
  
  public Integer getBlockWeightMedian() {
    return blockWeightMedian;
  }
  
  public void setBlockWeightMedian(Integer blockWeightMedian) {
    this.blockWeightMedian = blockWeightMedian;
  }
  
  public String getBootstrapDaemonAddress() {
    return bootstrapDaemonAddress;
  }
  
  public void setBootstrapDaemonAddress(String bootstrapDaemonAddress) {
    this.bootstrapDaemonAddress = bootstrapDaemonAddress;
  }
  
  public BigInteger getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(BigInteger difficulty) {
    this.difficulty = difficulty;
  }
  
  public BigInteger getCumulativeDifficulty() {
    return cumulativeDifficulty;
  }
  
  public void setCumulativeDifficulty(BigInteger cumulativeDifficulty) {
    this.cumulativeDifficulty = cumulativeDifficulty;
  }
  
  public BigInteger getFreeSpace() {
    return freeSpace;
  }
  
  public void setFreeSpace(BigInteger freeSpace) {
    this.freeSpace = freeSpace;
  }
  
  public Integer getNumOfflinePeers() {
    return numOfflinePeers;
  }
  
  public void setNumOfflinePeers(Integer numOfflinePeers) {
    this.numOfflinePeers = numOfflinePeers;
  }
  
  public Integer getNumOnlinePeers() {
    return numOnlinePeers;
  }
  
  public void setNumOnlinePeers(Integer numOnlinePeers) {
    this.numOnlinePeers = numOnlinePeers;
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
  
  public Integer getNumIncomingConnections() {
    return numIncomingConnections;
  }
  
  public void setNumIncomingConnections(Integer numIncomingConnections) {
    this.numIncomingConnections = numIncomingConnections;
  }
  
  public Integer getNumOutgoingConnections() {
    return numOutgoingConnections;
  }
  
  public void setNumOutgoingConnections(Integer numOutgoingConnections) {
    this.numOutgoingConnections = numOutgoingConnections;
  }
  
  public Integer getNumRpcConnections() {
    return numRpcConnections;
  }
  
  public void setNumRpcConnections(Integer numRpcConnections) {
    this.numRpcConnections = numRpcConnections;
  }
  
  public Long getStartTimestamp() {
    return startTimestamp;
  }
  
  public void setStartTimestamp(Long startTimestamp) {
    this.startTimestamp = startTimestamp;
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
  
  public String getTopBlockId() {
    return topBlockId;
  }
  
  public void setTopBlockId(String topBlockId) {
    this.topBlockId = topBlockId;
  }
  
  public Integer getNumTxs() {
    return numTxs;
  }
  
  public void setNumTxs(Integer numTxs) {
    this.numTxs = numTxs;
  }
  
  public Integer getNumTxsPool() {
    return numTxsPool;
  }
  
  public void setNumTxsPool(Integer numTxsPool) {
    this.numTxsPool = numTxsPool;
  }
  
  public Boolean getWasBootstrapEverUsed() {
    return wasBootstrapEverUsed;
  }
  
  public void setWasBootstrapEverUsed(Boolean wasBootstrapEverUsed) {
    this.wasBootstrapEverUsed = wasBootstrapEverUsed;
  }
  
  public Integer getDatabaseSize() {
    return databaseSize;
  }
  
  public void setDatabaseSize(Integer databaseSize) {
    this.databaseSize = databaseSize;
  }
  
  public Boolean getUpdateAvailable() {
    return updateAvailable;
  }
  
  public void setUpdateAvailable(Boolean updateAvailable) {
    this.updateAvailable = updateAvailable;
  }
}
