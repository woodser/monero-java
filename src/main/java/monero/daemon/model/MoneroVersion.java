package monero.daemon.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Defines a Monero version.
 */
public class MoneroVersion {
  
  public MoneroVersion() {
    // necessary for deserialization
  }
  
  public MoneroVersion(Integer number, Boolean isRelease) {
    this.number = number;
    this.isRelease = isRelease;
  }

  private Integer number;
  private Boolean isRelease;
  
  public Integer getNumber() {
    return number;
  }
  
  public void setNumber(Integer number) {
    this.number = number;
  }
  
  @JsonProperty("isRelease")
  public Boolean getIsRelease() {
    return isRelease;
  }
  
  public void setIsRelease(Boolean isRelease) {
    this.isRelease = isRelease;
  }
}
