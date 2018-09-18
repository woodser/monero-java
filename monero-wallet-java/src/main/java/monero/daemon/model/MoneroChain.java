package monero.daemon.model;

/**
 * Models a chain seen by the network.
 */
public class MoneroChain extends MoneroDaemonModel {

  private String blockHash;
  private Integer difficulty;
  private Integer height;
  private Integer length;
  
  public String getBlockHash() {
    return blockHash;
  }
  
  public void setBlockHash(String blockHash) {
    this.blockHash = blockHash;
  }
  
  public Integer getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(Integer difficulty) {
    this.difficulty = difficulty;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public Integer getLength() {
    return length;
  }
  
  public void setLength(Integer length) {
    this.length = length;
  }
}
