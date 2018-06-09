package model;

import service.MoneroException;
import utils.MoneroUtils;

public class MoneroAddress {

  private String standardAddress;

  public MoneroAddress(String standardAddress) {
    super();
    validateStandardAddress(standardAddress);
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
  
  // ------------------------------ STATIC UTILITIES --------------------------
  
  private static final int STANDARD_ADDRESS_LENGTH = 95;
  
  private static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    if (!standardAddress.startsWith("5") && !standardAddress.startsWith("4") && !standardAddress.startsWith("9") && !standardAddress.startsWith("A")) throw new MoneroException("Standard address does not start with 4, 5, 9 or A");
    MoneroUtils.validateBase58(standardAddress);
    if (standardAddress.length() != STANDARD_ADDRESS_LENGTH) throw new MoneroException("Standard address is " + standardAddress.length() + " characters but must be " + STANDARD_ADDRESS_LENGTH);
  }
  
  public static boolean isValidStandardAddress(String standardAddress) {
    try {
      validateStandardAddress(standardAddress);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
}
