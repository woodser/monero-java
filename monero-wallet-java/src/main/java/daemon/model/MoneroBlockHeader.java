package daemon.model;

import java.math.BigInteger;

/**
 * Monero block header information.
 */
public class MoneroBlockHeader extends MoneroDaemonModel {
  
  private Integer blockSize;
  private Integer depth;
  private BigInteger difficulty;
  private String hash;
  private Integer height;
  private Integer majorVersion;
  private Integer minorVersion;
  private BigInteger nonce;
  private Integer numTxs;
  private Boolean orphanStatus;
  private String prevHash;
  private BigInteger reward;
  private Long timestamp;
  
  public Integer getBlockSize() {
    return blockSize;
  }
  
  public void setBlockSize(Integer size) {
    this.blockSize = size;
  }
  
  public Integer getDepth() {
    return depth;
  }
  
  public void setDepth(Integer depth) {
    this.depth = depth;
  }
  
  public BigInteger getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(BigInteger difficulty) {
    this.difficulty = difficulty;
  }
  
  public String getHash() {
    return hash;
  }
  
  public void setHash(String hash) {
    this.hash = hash;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
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
  
  public BigInteger getNonce() {
    return nonce;
  }
  
  public void setNonce(BigInteger nonce) {
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
  
  public String getPrevHash() {
    return prevHash;
  }
  
  public void setPrevHash(String prevHash) {
    this.prevHash = prevHash;
  }
  
  public BigInteger getReward() {
    return reward;
  }
  
  public void setReward(BigInteger reward) {
    this.reward = reward;
  }
  
  public Long getTimestamp() {
    return timestamp;
  }
  
  public void setTimestamp(Long timestamp) {
    this.timestamp = timestamp;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((blockSize == null) ? 0 : blockSize.hashCode());
    result = prime * result + ((depth == null) ? 0 : depth.hashCode());
    result = prime * result + ((difficulty == null) ? 0 : difficulty.hashCode());
    result = prime * result + ((hash == null) ? 0 : hash.hashCode());
    result = prime * result + ((height == null) ? 0 : height.hashCode());
    result = prime * result + ((majorVersion == null) ? 0 : majorVersion.hashCode());
    result = prime * result + ((minorVersion == null) ? 0 : minorVersion.hashCode());
    result = prime * result + ((nonce == null) ? 0 : nonce.hashCode());
    result = prime * result + ((numTxs == null) ? 0 : numTxs.hashCode());
    result = prime * result + ((orphanStatus == null) ? 0 : orphanStatus.hashCode());
    result = prime * result + ((prevHash == null) ? 0 : prevHash.hashCode());
    result = prime * result + ((reward == null) ? 0 : reward.hashCode());
    result = prime * result + ((timestamp == null) ? 0 : timestamp.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroBlockHeader other = (MoneroBlockHeader) obj;
    if (blockSize == null) {
      if (other.blockSize != null) return false;
    } else if (!blockSize.equals(other.blockSize)) return false;
    if (depth == null) {
      if (other.depth != null) return false;
    } else if (!depth.equals(other.depth)) return false;
    if (difficulty == null) {
      if (other.difficulty != null) return false;
    } else if (!difficulty.equals(other.difficulty)) return false;
    if (hash == null) {
      if (other.hash != null) return false;
    } else if (!hash.equals(other.hash)) return false;
    if (height == null) {
      if (other.height != null) return false;
    } else if (!height.equals(other.height)) return false;
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
    if (prevHash == null) {
      if (other.prevHash != null) return false;
    } else if (!prevHash.equals(other.prevHash)) return false;
    if (reward == null) {
      if (other.reward != null) return false;
    } else if (!reward.equals(other.reward)) return false;
    if (timestamp == null) {
      if (other.timestamp != null) return false;
    } else if (!timestamp.equals(other.timestamp)) return false;
    return true;
  }
}
