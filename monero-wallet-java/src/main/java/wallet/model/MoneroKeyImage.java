package wallet.model;

import daemon.model.MoneroDaemonModel;

/**
 * Represents a Monero key image.
 */
public class MoneroKeyImage extends MoneroDaemonModel {

  private String keyImage;
  private String signature;
  private Integer spentStatus;
  
  public MoneroKeyImage(String keyImage, String signature) {
    this(keyImage, signature, null); 
  }
  
  public MoneroKeyImage(String keyImage, String signature, Integer spentStatus) {
    super();
    this.keyImage = keyImage;
    this.signature = signature;
    this.spentStatus = spentStatus;
  }

  public String getKeyImage() {
    return keyImage;
  }

  public void setKeyImage(String keyImage) {
    this.keyImage = keyImage;
  }

  public String getSignature() {
    return signature;
  }

  public void setSignature(String signature) {
    this.signature = signature;
  }

  public Integer getSpentStatus() {
    return spentStatus;
  }

  public void setSpentStatus(Integer spentStatus) {
    this.spentStatus = spentStatus;
  }
}
