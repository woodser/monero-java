package monero.daemon.model;

import java.math.BigInteger;
import java.util.List;

import monero.utils.MoneroUtils;

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
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    if (getKeyImage() != null) {
      sb.append(MoneroUtils.kvLine("Key image", "", indent));
      sb.append(getKeyImage().toString(indent + 1) + "\n");
    }
    sb.append(MoneroUtils.kvLine("Amount", getAmount(), indent));
    sb.append(MoneroUtils.kvLine("Index", getIndex(), indent));
    sb.append(MoneroUtils.kvLine("Ring output indices", getRingOutputIndices(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);  // strip newline
  }
}
