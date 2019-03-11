package monero.daemon.model;

import java.math.BigInteger;

import monero.utils.MoneroUtils;

/**
 * Monero block header information.
 */
public class MoneroBlockHeader {
  
  private String id;
  private Integer height;
  private Long timestamp;
  private Long size;
  private Long weight;
  private Long longTermWeight;
  private Long depth;
  private BigInteger difficulty;
  private BigInteger cumulativeDifficulty;
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
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(MoneroUtils.kvLine("Id", getId(), indent));
    sb.append(MoneroUtils.kvLine("Height", getHeight(), indent));
    sb.append(MoneroUtils.kvLine("Timestamp", getNumTxs(), indent));
    sb.append(MoneroUtils.kvLine("Size", getSize(), indent));
    sb.append(MoneroUtils.kvLine("Weight", getWeight(), indent));
    sb.append(MoneroUtils.kvLine("Depth", getDepth(), indent));
    sb.append(MoneroUtils.kvLine("Difficulty", getDifficulty(), indent));
    sb.append(MoneroUtils.kvLine("Cumulative difficulty", getCumulativeDifficulty(), indent));
    sb.append(MoneroUtils.kvLine("Major version", getMajorVersion(), indent));
    sb.append(MoneroUtils.kvLine("Minor version", getMinorVersion(), indent));
    sb.append(MoneroUtils.kvLine("Nonce", getNonce(), indent));
    sb.append(MoneroUtils.kvLine("Num txs", getNumTxs(), indent));
    sb.append(MoneroUtils.kvLine("Orphan status", getOrphanStatus(), indent));
    sb.append(MoneroUtils.kvLine("Prev id", getPrevId(), indent));
    sb.append(MoneroUtils.kvLine("Reward", getReward(), indent));
    sb.append(MoneroUtils.kvLine("Pow hash", getPowHash(), indent));
    String str = sb.toString();
    return str.charAt(str.length() - 1) == '\n' ? str.substring(0, str.length() - 1) : str; // strip newline
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((cumulativeDifficulty == null) ? 0 : cumulativeDifficulty.hashCode());
    result = prime * result + ((depth == null) ? 0 : depth.hashCode());
    result = prime * result + ((difficulty == null) ? 0 : difficulty.hashCode());
    result = prime * result + ((height == null) ? 0 : height.hashCode());
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    result = prime * result + ((longTermWeight == null) ? 0 : longTermWeight.hashCode());
    result = prime * result + ((majorVersion == null) ? 0 : majorVersion.hashCode());
    result = prime * result + ((minorVersion == null) ? 0 : minorVersion.hashCode());
    result = prime * result + ((nonce == null) ? 0 : nonce.hashCode());
    result = prime * result + ((numTxs == null) ? 0 : numTxs.hashCode());
    result = prime * result + ((orphanStatus == null) ? 0 : orphanStatus.hashCode());
    result = prime * result + ((powHash == null) ? 0 : powHash.hashCode());
    result = prime * result + ((prevId == null) ? 0 : prevId.hashCode());
    result = prime * result + ((reward == null) ? 0 : reward.hashCode());
    result = prime * result + ((size == null) ? 0 : size.hashCode());
    result = prime * result + ((timestamp == null) ? 0 : timestamp.hashCode());
    result = prime * result + ((weight == null) ? 0 : weight.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroBlockHeader other = (MoneroBlockHeader) obj;
    if (cumulativeDifficulty == null) {
      if (other.cumulativeDifficulty != null) return false;
    } else if (!cumulativeDifficulty.equals(other.cumulativeDifficulty)) return false;
    if (depth == null) {
      if (other.depth != null) return false;
    } else if (!depth.equals(other.depth)) return false;
    if (difficulty == null) {
      if (other.difficulty != null) return false;
    } else if (!difficulty.equals(other.difficulty)) return false;
    if (height == null) {
      if (other.height != null) return false;
    } else if (!height.equals(other.height)) return false;
    if (id == null) {
      if (other.id != null) return false;
    } else if (!id.equals(other.id)) return false;
    if (longTermWeight == null) {
      if (other.longTermWeight != null) return false;
    } else if (!longTermWeight.equals(other.longTermWeight)) return false;
    if (majorVersion == null) {
      if (other.majorVersion != null) return false;
    } else if (!majorVersion.equals(other.majorVersion)) return false;
    if (minorVersion == null) {
      if (other.minorVersion != null) return false;
    } else if (!minorVersion.equals(other.minorVersion)) return false;
    if (nonce == null) {
      if (other.nonce != null) return false;
    } else if (!nonce.equals(other.nonce)) return false;
    if (numTxs == null) {
      if (other.numTxs != null) return false;
    } else if (!numTxs.equals(other.numTxs)) return false;
    if (orphanStatus == null) {
      if (other.orphanStatus != null) return false;
    } else if (!orphanStatus.equals(other.orphanStatus)) return false;
    if (powHash == null) {
      if (other.powHash != null) return false;
    } else if (!powHash.equals(other.powHash)) return false;
    if (prevId == null) {
      if (other.prevId != null) return false;
    } else if (!prevId.equals(other.prevId)) return false;
    if (reward == null) {
      if (other.reward != null) return false;
    } else if (!reward.equals(other.reward)) return false;
    if (size == null) {
      if (other.size != null) return false;
    } else if (!size.equals(other.size)) return false;
    if (timestamp == null) {
      if (other.timestamp != null) return false;
    } else if (!timestamp.equals(other.timestamp)) return false;
    if (weight == null) {
      if (other.weight != null) return false;
    } else if (!weight.equals(other.weight)) return false;
    return true;
  }
}
