package monero.wallet.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.JsonUtils;

/**
 * Message signature verification result.
 */
public class MoneroMessageSignatureResult {

  private Boolean isGood;
  private Boolean isOld;
  private MoneroMessageSignatureType signatureType;
  private Integer version;
  
  public MoneroMessageSignatureResult(Boolean isGood, Boolean isOld, MoneroMessageSignatureType signatureType, Integer version) {
    super();
    this.isGood = isGood;
    this.isOld = isOld;
    this.signatureType = signatureType;
    this.version = version;
  }

  @JsonProperty("isGood")
  public Boolean isGood() {
    return isGood;
  }

  public void setIsGood(Boolean isGood) {
    this.isGood = isGood;
  }
  
  @JsonProperty("isOld")
  public Boolean isOld() {
    return isOld;
  }

  public void setIsOld(Boolean isOld) {
    this.isOld = isOld;
  }

  public MoneroMessageSignatureType getSignatureType() {
    return signatureType;
  }

  public void setSignatureType(MoneroMessageSignatureType signatureType) {
    this.signatureType = signatureType;
  }

  public Integer getVersion() {
    return version;
  }

  public void setVersion(Integer version) {
    this.version = version;
  }
  
  public String toString() {
    return JsonUtils.serialize(this);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((isGood == null) ? 0 : isGood.hashCode());
    result = prime * result + ((isOld == null) ? 0 : isOld.hashCode());
    result = prime * result + ((signatureType == null) ? 0 : signatureType.hashCode());
    result = prime * result + ((version == null) ? 0 : version.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroMessageSignatureResult other = (MoneroMessageSignatureResult) obj;
    if (isGood == null) {
      if (other.isGood != null) return false;
    } else if (!isGood.equals(other.isGood)) return false;
    if (isOld == null) {
      if (other.isOld != null) return false;
    } else if (!isOld.equals(other.isOld)) return false;
    if (signatureType != other.signatureType) return false;
    if (version == null) {
      if (other.version != null) return false;
    } else if (!version.equals(other.version)) return false;
    return true;
  }
}
