package monero.daemon.model;

import java.util.List;

/**
 * Models an alternative chain seen by the node.
 */
public class MoneroAltChain {
  
  private List<String> blockIds;
  private Long difficulty;
  private Integer height;
  private Integer length;
  private String mainChainParentBlockId;
  
  public List<String> getBlockIds() {
    return blockIds;
  }
  
  public void setBlockIds(List<String> blockIds) {
    this.blockIds = blockIds;
  }
  
  public Long getDifficulty() {
    return difficulty;
  }
  
  public void setDifficulty(Long difficulty) {
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
  
  public String getMainChainParentBlockId() {
    return mainChainParentBlockId;
  }
  
  public void setMainChainParentBlockId(String mainChainParentBlockId) {
    this.mainChainParentBlockId = mainChainParentBlockId;
  }
}
