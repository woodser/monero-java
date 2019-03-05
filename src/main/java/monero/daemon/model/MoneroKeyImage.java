package monero.daemon.model;

/**
 * Models a Monero key image.
 */
public class MoneroKeyImage {

  private String hex;
  private String signature;
  
  public MoneroKeyImage(String hex) {
    this(hex, null);
  }
  
  public MoneroKeyImage(String hex, String signature) {
    this.hex = hex;
    this.signature = signature;
  }
  
  public String getHex() {
    return hex;
  }
  
  public void setHex(String hex) {
    this.hex = hex;
  }
  
  public String getSignature() {
    return signature;
  }
  
  public void setSignature(String signature) {
    this.signature = signature;
  }
}
