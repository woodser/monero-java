package monero.wallet.model;

import java.math.BigInteger;

/**
 * Models results from importing key images.
 */
public class MoneroKeyImageImportResult {
  
  private Integer height;
  private BigInteger spentAmount;
  private BigInteger unspentAmount;
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public BigInteger getSpentAmount() {
    return spentAmount;
  }
  
  public void setSpentAmount(BigInteger spentAmount) {
    this.spentAmount = spentAmount;
  }
  
  public BigInteger getUnspentAmount() {
    return unspentAmount;
  }
  
  public void setUnspentAmount(BigInteger unspentAmount) {
    this.unspentAmount = unspentAmount;
  }
}
