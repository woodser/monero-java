package monero.daemon.model;

import java.math.BigInteger;
import java.util.List;

import monero.wallet.model.MoneroKeyImage;
import monero.wallet.model.MoneroTx;

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
  
  public void setTx(MoneroTx tx) {
    this.tx = tx;
  }
  
  public MoneroKeyImage getKeyImage() {
    return keyImage;
  }
  
  public void setKeyImage(MoneroKeyImage keyImage) {
    this.keyImage = keyImage;
  }
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public void setAmount(BigInteger amount) {
    this.amount = amount;
  }
  
  public Integer getIndex() {
    return index;
  }
  
  public void setIndex(Integer index) {
    this.index = index;
  }
  
  public List<Integer> getRingOutputIndices() {
    return ringOutputIndices;
  }
  
  public void setRingOutputIndices(List<Integer> ringOutputIndices) {
    this.ringOutputIndices = ringOutputIndices;
  }
  
  public String getStealthPublicKey() {
    return stealthPublicKey;
  }
  
  public void setStealthPublicKey(String stealthPublicKey) {
    this.stealthPublicKey = stealthPublicKey;
  }
}
