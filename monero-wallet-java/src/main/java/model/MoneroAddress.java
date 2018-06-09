package model;

import service.MoneroUtils;

public class MoneroAddress {

  private String standardAddress;

  public MoneroAddress(String standardAddress) {
    super();
    MoneroUtils.validateStandardAddress(standardAddress);
    this.standardAddress = standardAddress;
  }

  public String getStandardAddress() {
    return standardAddress;
  }
  
  public String toString() {
    return standardAddress;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((standardAddress == null) ? 0 : standardAddress.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroAddress other = (MoneroAddress) obj;
    if (standardAddress == null) {
      if (other.standardAddress != null) return false;
    } else if (!standardAddress.equals(other.standardAddress)) return false;
    return true;
  }
}
