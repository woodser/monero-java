package monero.daemon.model;

import java.math.BigInteger;

/**
 * Monero block header information.
 */
public class MoneroBlockHeader extends MoneroDaemonModel {
  
  private String id;
  private Integer height;
  private Long timestamp;
  private Long size;
  private Long weight;
  private Long longTermWeight;
  private Long depth;
  private Long difficulty;
  private Long cumulativeDifficulty;
  private Integer majorVersion;
  private Integer minorVersion;
  private Long nonce;
  private Integer numTxs;
  private Boolean orphanStatus;
  private String prevId;
  private BigInteger reward;
  private String powHash;
  
  public String getId() {
    return id;
  }
  
  public void setId(String id) {
    this.id = id;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public Long getTimestamp() {
    return timestamp;
  }
  
  public void setTimestamp(Long timestamp) {
    this.timestamp = timestamp;
  }
  
  public Long getSize() {
    return size;
  }
  
  public void setSize(Long size) {
    this.size = size;
  }
  
  public Long getWeight() {
    return weight;
  }
  
  public void setWeight(Long weight) {
    this.weight = weight;
  }
  
  public Long getLongTermWeight() {
    return longTermWeight;
  }
  
  public void setLongTermWeight(Long longTermWeight) {
    this.longTermWeight = longTermWeight;
  }
  
  public Long getDepth() {
    return depth;
  }
  
  public void setDepth(Long depth) {
    this.depth = depth;
  }
  
  public Long getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(Long difficulty) {
    this.difficulty = difficulty;
  }
  
  public Long getCumulativeDifficulty() {
    return cumulativeDifficulty;
  }
  
  public void setCumulativeDifficulty(Long cumulativeDifficulty) {
    this.cumulativeDifficulty = cumulativeDifficulty;
  }
  
  public Integer getMajorVersion() {
    return majorVersion;
  }
  
  public void setMajorVersion(Integer majorVersion) {
    this.majorVersion = majorVersion;
  }
  
  public Integer getMinorVersion() {
    return minorVersion;
  }
  
  public void setMinorVersion(Integer minorVersion) {
    this.minorVersion = minorVersion;
  }
  
  public Long getNonce() {
    return nonce;
  }
  
  public void setNonce(Long nonce) {
    this.nonce = nonce;
  }
  
  public Integer getNumTxs() {
    return numTxs;
  }
  
  public void setNumTxs(Integer numTxs) {
    this.numTxs = numTxs;
  }
  
  public Boolean getOrphanStatus() {
    return orphanStatus;
  }
  
  public void setOrphanStatus(Boolean orphanStatus) {
    this.orphanStatus = orphanStatus;
  }
  
  public String getPrevId() {
    return prevId;
  }
  
  public void setPrevId(String prevId) {
    this.prevId = prevId;
  }
  
  public BigInteger getReward() {
    return reward;
  }
  
  public void setReward(BigInteger reward) {
    this.reward = reward;
  }
  
  public String getPowHash() {
    return powHash;
  }
  
  public void setPowHash(String powHash) {
    this.powHash = powHash;
  }  
}
