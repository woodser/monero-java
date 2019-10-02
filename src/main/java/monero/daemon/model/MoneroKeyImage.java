package monero.daemon.model;

import common.utils.GenUtils;

/**
 * Models a Monero key image.
 */
public class MoneroKeyImage {

  private String hex;
  private String signature;
  
  public MoneroKeyImage() {
    // nothing to construct
  }
  
  public MoneroKeyImage(String hex) {
    this(hex, null);
  }
  
  public MoneroKeyImage(String hex, String signature) {
    this.hex = hex;
    this.signature = signature;
  }
  
  public MoneroKeyImage(MoneroKeyImage keyImage) {
    this.hex = keyImage.hex;
    this.signature = keyImage.signature;
  }
  
  public String getHex() {
    return hex;
  }
  
  public MoneroKeyImage setHex(String hex) {
    this.hex = hex;
    return this;
  }
  
  public String getSignature() {
    return signature;
  }
  
  public MoneroKeyImage setSignature(String signature) {
    this.signature = signature;
    return this;
  }
  
  public MoneroKeyImage copy() {
    return new MoneroKeyImage(this);
  }
  
  public MoneroKeyImage merge(MoneroKeyImage keyImage) {
    GenUtils.assertTrue(keyImage instanceof MoneroKeyImage);
    if (keyImage == this) return this;
    this.setHex(GenUtils.reconcile(this.getHex(), keyImage.getHex()));
    this.setSignature(GenUtils.reconcile(this.getSignature(), keyImage.getSignature()));
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.kvLine("Hex", getHex(), indent));
    sb.append(GenUtils.kvLine("Signature", getSignature(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);  // strip newline
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((hex == null) ? 0 : hex.hashCode());
    result = prime * result + ((signature == null) ? 0 : signature.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroKeyImage other = (MoneroKeyImage) obj;
    if (hex == null) {
      if (other.hex != null) return false;
    } else if (!hex.equals(other.hex)) return false;
    if (signature == null) {
      if (other.signature != null) return false;
    } else if (!signature.equals(other.signature)) return false;
    return true;
  }
}
