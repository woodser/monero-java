package monero.daemon.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Monero daemon mining status.
 */
public class MoneroMiningStatus {
  
  private Boolean isActive;
  private Boolean isBackground;
  private String address;
  private Integer speed;
  private Integer numThreads;
  
  @JsonProperty("isActive")
  public Boolean isActive() {
    return isActive;
  }
  
  public void setIsActive(Boolean isActive) {
    this.isActive = isActive;
  }
  
  @JsonProperty("isBackground")
  public Boolean isBackground() {
    return isBackground;
  }
  
  public void setIsBackground(Boolean isBackground) {
    this.isBackground = isBackground;
  }
  
  public String getAddress() {
    return address;
  }
  
  public void setAddress(String address) {
    this.address = address;
  }
  
  public Integer getSpeed() {
    return speed;
  }
  
  public void setSpeed(Integer speed) {
    this.speed = speed;
  }
  
  public Integer getNumThreads() {
    return numThreads;
  }
  
  public void setNumThreads(Integer numThreads) {
    this.numThreads = numThreads;
  }
}
