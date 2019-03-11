package monero.daemon.model;

import monero.utils.MoneroUtils;

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
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(MoneroUtils.kvLine("Hex", getHex(), indent));
    sb.append(MoneroUtils.kvLine("Signature", getSignature(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);  // strip newline
  }
}
