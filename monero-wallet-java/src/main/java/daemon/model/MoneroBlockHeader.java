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
}
