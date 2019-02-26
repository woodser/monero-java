package monero.daemon.model;

import java.math.BigInteger;

/**
 * Monero block template to mine.
 */
public class MoneroBlockTemplate {
  
  private String blockTemplateBlob;
  private String blockHashingBlob;
  private Long difficulty;
  private BigInteger expectedReward;
  private Integer height;
  private String prevId;
  private Integer reservedOffset;
  
  public String getBlockTemplateBlob() {
    return blockTemplateBlob;
  }
  
  public void setBlockTemplateBlob(String blockTemplateBlob) {
    this.blockTemplateBlob = blockTemplateBlob;
  }
  
  public String getBlockHashingBlob() {
    return blockHashingBlob;
  }
  
  public void setBlockHashingBlob(String blockHashingBlob) {
    this.blockHashingBlob = blockHashingBlob;
  }
  
  public Long getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(Long difficulty) {
    this.difficulty = difficulty;
  }
  
  public BigInteger getExpectedReward() {
    return expectedReward;
  }
  
  public void setExpectedReward(BigInteger expectedReward) {
    this.expectedReward = expectedReward;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public String getPrevId() {
    return prevId;
  }
  
  public void setPrevId(String prevId) {
    this.prevId = prevId;
  }
  
  public Integer getReservedOffset() {
    return reservedOffset;
  }
  
  public void setReservedOffset(Integer reservedOffset) {
    this.reservedOffset = reservedOffset;
  }
}
