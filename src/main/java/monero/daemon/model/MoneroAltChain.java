package monero.daemon.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Models an alternative chain seen by the node.
 */
public class MoneroAltChain {
  
  private List<String> blockHashes;
  private BigInteger difficulty;
  private Long height;
  private Long length;
  private String mainChainParentBlockHash;
  
  public List<String> getBlockHashes() {
    return blockHashes;
  }
  
  public void setBlockHashes(List<String> blockHashes) {
    this.blockHashes = blockHashes;
  }
  
  public BigInteger getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(BigInteger difficulty) {
    this.difficulty = difficulty;
  }
  
  public Long getHeight() {
    return height;
  }
  
  public void setHeight(Long height) {
    this.height = height;
  }
  
  public Long getLength() {
    return length;
  }
  
  public void setLength(Long length) {
    this.length = length;
  }
  
  public String getMainChainParentBlockHash() {
    return mainChainParentBlockHash;
  }
  
  public void setMainChainParentBlockHash(String mainChainParentBlockHash) {
    this.mainChainParentBlockHash = mainChainParentBlockHash;
  }
}
