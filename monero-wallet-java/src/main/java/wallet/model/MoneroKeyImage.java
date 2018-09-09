package wallet.model;

/**
 * Represents a Monero key image.
 */
public class MoneroKeyImage {

  private String keyImage;
  private String signature;
  
  public MoneroKeyImage(String keyImage, String signature) {
    super();
    this.keyImage = keyImage;
    this.signature = signature;
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
}
