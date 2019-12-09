package monero.daemon.model;

import java.math.BigInteger;

import common.utils.GenUtils;

/**
 * Models a Monero block header which contains information about the block.
 */
public class MoneroBlockHeader {
  
  private String hash;
  private Long height;
  private Long timestamp;
  private Long size;
  private Long weight;
  private Long longTermWeight;
  private Long depth;
  private BigInteger difficulty;
  private BigInteger cumulativeDifficulty;
  private Integer majorVersion;
  private Integer minorVersion;
  private Integer nonce;
  private String minerTxHash;
  private Integer numTxs;
  private Boolean orphanStatus;
  private String prevHash;
  private BigInteger reward;
  private String powHash;
  
  public MoneroBlockHeader() {
    super();
  }
  
  public MoneroBlockHeader(MoneroBlockHeader header) {
    this.hash = header.hash;
    this.height = header.height;
    this.timestamp = header.timestamp;
    this.size = header.size;
    this.weight = header.weight;
    this.longTermWeight = header.longTermWeight;
    this.depth = header.depth;
    this.difficulty = header.difficulty;
    this.cumulativeDifficulty = header.cumulativeDifficulty;
    this.majorVersion = header.majorVersion;
    this.minorVersion = header.minorVersion;
    this.nonce = header.nonce;
    this.numTxs = header.numTxs;
    this.orphanStatus = header.orphanStatus;
    this.prevHash = header.prevHash;
    this.reward = header.reward;
    this.powHash = header.powHash;
  }
  
  public String getHash() {
    return hash;
  }
  
  public MoneroBlockHeader setHash(String hash) {
    this.hash = hash;
    return this;
  }
  
  /**
   * Return the block's height which is the total number of blocks that have occurred before.
   * 
   * @return the block's height
   */
  public Long getHeight() {
    return height;
  }
  
  /**
   * Set the block's height which is the total number of blocks that have occurred before.
   * 
   * @param height is the block's height to set
   * @return a reference to this header for chaining
   */
  public MoneroBlockHeader setHeight(Long height) {
    this.height = height;
    return this;
  }
  
  public Long getTimestamp() {
    return timestamp;
  }
  
  public MoneroBlockHeader setTimestamp(Long timestamp) {
    this.timestamp = timestamp;
    return this;
  }
  
  public Long getSize() {
    return size;
  }
  
  public MoneroBlockHeader setSize(Long size) {
    this.size = size;
    return this;
  }
  
  public Long getWeight() {
    return weight;
  }
  
  public MoneroBlockHeader setWeight(Long weight) {
    this.weight = weight;
    return this;
  }
  
  public Long getLongTermWeight() {
    return longTermWeight;
  }
  
  public MoneroBlockHeader setLongTermWeight(Long longTermWeight) {
    this.longTermWeight = longTermWeight;
    return this;
  }
  
  public Long getDepth() {
    return depth;
  }
  
  public MoneroBlockHeader setDepth(Long depth) {
    this.depth = depth;
    return this;
  }
  
  public BigInteger getDifficulty() {
    return difficulty;
  }
  
  public MoneroBlockHeader setDifficulty(BigInteger difficulty) {
    this.difficulty = difficulty;
    return this;
  }
  
  public BigInteger getCumulativeDifficulty() {
    return cumulativeDifficulty;
  }
  
  public MoneroBlockHeader setCumulativeDifficulty(BigInteger cumulativeDifficulty) {
    this.cumulativeDifficulty = cumulativeDifficulty;
    return this;
  }
  
  public Integer getMajorVersion() {
    return majorVersion;
  }
  
  public MoneroBlockHeader setMajorVersion(Integer majorVersion) {
    this.majorVersion = majorVersion;
    return this;
  }
  
  public Integer getMinorVersion() {
    return minorVersion;
  }
  
  public MoneroBlockHeader setMinorVersion(Integer minorVersion) {
    this.minorVersion = minorVersion;
    return this;
  }
  
  public Integer getNonce() {
    return nonce;
  }
  
  public MoneroBlockHeader setNonce(Integer nonce) {
    this.nonce = nonce;
    return this;
  }
  
  public String getMinerTxHash() {
    return minerTxHash;
  }
  
  public MoneroBlockHeader setMinerTxHash(String minerTxHash) {
    this.minerTxHash = minerTxHash;
    return this;
  }
  
  public Integer getNumTxs() {
    return numTxs;
  }
  
  public MoneroBlockHeader setNumTxs(Integer numTxs) {
    this.numTxs = numTxs;
    return this;
  }
  
  public Boolean getOrphanStatus() {
    return orphanStatus;
  }
  
  public MoneroBlockHeader setOrphanStatus(Boolean orphanStatus) {
    this.orphanStatus = orphanStatus;
    return this;
  }
  
  public String getPrevHash() {
    return prevHash;
  }
  
  public MoneroBlockHeader setPrevHash(String prevHash) {
    this.prevHash = prevHash;
    return this;
  }
  
  public BigInteger getReward() {
    return reward;
  }
  
  public MoneroBlockHeader setReward(BigInteger reward) {
    this.reward = reward;
    return this;
  }
  
  public String getPowHash() {
    return powHash;
  }
  
  public MoneroBlockHeader setPowHash(String powHash) {
    this.powHash = powHash;
    return this;
  }
  
  public MoneroBlockHeader merge(MoneroBlockHeader header) {
    GenUtils.assertNotNull(header);
    if (this == header) return this;
    this.setHash(GenUtils.reconcile(this.getHash(), header.getHash()));
    this.setHeight(GenUtils.reconcile(this.getHeight(), header.getHeight(), null, null, true));  // height can increase
    this.setTimestamp(GenUtils.reconcile(this.getTimestamp(), header.getTimestamp(), null, null, true));  // block timestamp can increase
    this.setSize(GenUtils.reconcile(this.getSize(), header.getSize()));
    this.setWeight(GenUtils.reconcile(this.getWeight(), header.getWeight()));
    this.setDepth(GenUtils.reconcile(this.getDepth(), header.getDepth()));
    this.setDifficulty(GenUtils.reconcile(this.getDifficulty(), header.getDifficulty()));
    this.setCumulativeDifficulty(GenUtils.reconcile(this.getCumulativeDifficulty(), header.getCumulativeDifficulty()));
    this.setMajorVersion(GenUtils.reconcile(this.getMajorVersion(), header.getMajorVersion()));
    this.setMinorVersion(GenUtils.reconcile(this.getMinorVersion(), header.getMinorVersion()));
    this.setNonce(GenUtils.reconcile(this.getNonce(), header.getNonce()));
    this.setMinerTxHash(GenUtils.reconcile(this.getMinerTxHash(), header.getMinerTxHash()));
    this.setNumTxs(GenUtils.reconcile(this.getNumTxs(), header.getNumTxs()));
    this.setOrphanStatus(GenUtils.reconcile(this.getOrphanStatus(), header.getOrphanStatus()));
    this.setPrevHash(GenUtils.reconcile(this.getPrevHash(), header.getPrevHash()));
    this.setReward(GenUtils.reconcile(this.getReward(), header.getReward()));
    this.setPowHash(GenUtils.reconcile(this.getPowHash(), header.getPowHash()));
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.kvLine("Hash", getHash(), indent));
    sb.append(GenUtils.kvLine("Height", getHeight(), indent));
    sb.append(GenUtils.kvLine("Timestamp", getTimestamp(), indent));
    sb.append(GenUtils.kvLine("Size", getSize(), indent));
    sb.append(GenUtils.kvLine("Weight", getWeight(), indent));
    sb.append(GenUtils.kvLine("Depth", getDepth(), indent));
    sb.append(GenUtils.kvLine("Difficulty", getDifficulty(), indent));
    sb.append(GenUtils.kvLine("Cumulative difficulty", getCumulativeDifficulty(), indent));
    sb.append(GenUtils.kvLine("Major version", getMajorVersion(), indent));
    sb.append(GenUtils.kvLine("Minor version", getMinorVersion(), indent));
    sb.append(GenUtils.kvLine("Nonce", getNonce(), indent));
    sb.append(GenUtils.kvLine("Miner tx hash", getMinerTxHash(), indent));
    sb.append(GenUtils.kvLine("Num txs", getNumTxs(), indent));
    sb.append(GenUtils.kvLine("Orphan status", getOrphanStatus(), indent));
    sb.append(GenUtils.kvLine("Prev hash", getPrevHash(), indent));
    sb.append(GenUtils.kvLine("Reward", getReward(), indent));
    sb.append(GenUtils.kvLine("Pow hash", getPowHash(), indent));
    String str = sb.toString();
    if (str.isEmpty()) return "";
    return str.charAt(str.length() - 1) == '\n' ? str.substring(0, str.length() - 1) : str; // strip newline
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((minerTxHash == null) ? 0 : minerTxHash.hashCode());
    result = prime * result + ((cumulativeDifficulty == null) ? 0 : cumulativeDifficulty.hashCode());
    result = prime * result + ((depth == null) ? 0 : depth.hashCode());
    result = prime * result + ((difficulty == null) ? 0 : difficulty.hashCode());
    result = prime * result + ((height == null) ? 0 : height.hashCode());
    result = prime * result + ((hash == null) ? 0 : hash.hashCode());
    result = prime * result + ((longTermWeight == null) ? 0 : longTermWeight.hashCode());
    result = prime * result + ((majorVersion == null) ? 0 : majorVersion.hashCode());
    result = prime * result + ((minorVersion == null) ? 0 : minorVersion.hashCode());
    result = prime * result + ((nonce == null) ? 0 : nonce.hashCode());
    result = prime * result + ((numTxs == null) ? 0 : numTxs.hashCode());
    result = prime * result + ((orphanStatus == null) ? 0 : orphanStatus.hashCode());
    result = prime * result + ((powHash == null) ? 0 : powHash.hashCode());
    result = prime * result + ((prevHash == null) ? 0 : prevHash.hashCode());
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
    if (minerTxHash == null) {
      if (other.minerTxHash != null) return false;
    } else if (!minerTxHash.equals(other.minerTxHash)) return false;
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
    if (hash == null) {
      if (other.hash != null) return false;
    } else if (!hash.equals(other.hash)) return false;
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
    if (prevHash == null) {
      if (other.prevHash != null) return false;
    } else if (!prevHash.equals(other.prevHash)) return false;
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
