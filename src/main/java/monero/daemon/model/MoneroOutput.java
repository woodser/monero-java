package monero.daemon.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Represents a transaction output.
 */
public class MoneroOutput {

  private MoneroTx tx;
  private MoneroKeyImage keyImage;
  private BigInteger amount;
  private Integer index;
  private List<Integer> ringOutputIndices;
  private String stealthPublicKey;
  
  public MoneroTx getTx() {
    return tx;
  }
  
  public MoneroOutput setTx(MoneroTx tx) {
    this.tx = tx;
    return this;
  }
  
  public MoneroKeyImage getKeyImage() {
    return keyImage;
  }
  
  public MoneroOutput setKeyImage(MoneroKeyImage keyImage) {
    this.keyImage = keyImage;
    return this;
  }
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public MoneroOutput setAmount(BigInteger amount) {
    this.amount = amount;
    return this;
  }
  
  public Integer getIndex() {
    return index;
  }
  
  public MoneroOutput setIndex(Integer index) {
    this.index = index;
    return this;
  }
  
  public List<Integer> getRingOutputIndices() {
    return ringOutputIndices;
  }
  
  public MoneroOutput setRingOutputIndices(List<Integer> ringOutputIndices) {
    this.ringOutputIndices = ringOutputIndices;
    return this;
  }
  
  public String getStealthPublicKey() {
    return stealthPublicKey;
  }
  
  public MoneroOutput setStealthPublicKey(String stealthPublicKey) {
    this.stealthPublicKey = stealthPublicKey;
    return this;
  }
}
