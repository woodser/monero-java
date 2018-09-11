package daemon.model;

/**
 * Monero block template to mine.
 */
public class MoneroBlockTemplate extends MoneroDaemonModel {

  private String templateBlob;
  private String hashBlob;
  private Integer difficulty;
  private Integer expectedReward;
  private Integer height;
  private String prevHash;
  private Integer reservedOffset;
  
  public String getTemplateBlob() {
    return templateBlob;
  }
  
  public void setTemplateBlob(String templateBlob) {
    this.templateBlob = templateBlob;
  }
  
  public String getHashBlob() {
    return hashBlob;
  }
  
  public void setHashBlob(String hashBlob) {
    this.hashBlob = hashBlob;
  }
  
  public Integer getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(Integer difficulty) {
    this.difficulty = difficulty;
  }
  
  public Integer getExpectedReward() {
    return expectedReward;
  }
  
  public void setExpectedReward(Integer expectedReward) {
    this.expectedReward = expectedReward;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public String getPrevHash() {
    return prevHash;
  }
  
  public void setPrevHash(String prevHash) {
    this.prevHash = prevHash;
  }
  
  public Integer getReservedOffset() {
    return reservedOffset;
  }
  
  public void setReservedOffset(Integer reservedOffset) {
    this.reservedOffset = reservedOffset;
  }
}
