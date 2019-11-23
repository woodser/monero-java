package monero.daemon.model;

/**
 * Defines a Monero version.
 */
public class MoneroVersion {
  
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
  
  public Boolean getIsRelease() {
    return isRelease;
  }
  
  public void setIsRelease(Boolean isRelease) {
    this.isRelease = isRelease;
  }
}
