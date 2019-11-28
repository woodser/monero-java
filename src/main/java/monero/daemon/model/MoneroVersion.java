package monero.daemon.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Defines a Monero version.
 */
public class MoneroVersion {
  
  public MoneroVersion() {
    // necessary for deserialization
  }
  
  public MoneroVersion(Integer versionNumber, Boolean isRelease) {
    this.versionNumber = versionNumber;
    this.isRelease = isRelease;
  }

  private Integer versionNumber;
  private Boolean isRelease;
  
  public Integer getVersionNumber() {
    return versionNumber;
  }
  
  public void setVersionNumber(Integer version) {
    this.versionNumber = version;
  }
  
  @JsonProperty("isRelease")
  public Boolean getIsRelease() {
    return isRelease;
  }
  
  public void setIsRelease(Boolean isRelease) {
    this.isRelease = isRelease;
  }
}
