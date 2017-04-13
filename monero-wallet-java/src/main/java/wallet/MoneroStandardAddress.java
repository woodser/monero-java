package wallet;

import utils.MoneroUtils;

/**
 * Represents a Monero standard address.
 * 
 * @author woodser
 */
public class MoneroStandardAddress {
  
  private String standardAddress;
  
  public MoneroStandardAddress(String standardAddress) {
    MoneroUtils.validateStandardAddress(standardAddress);
    this.standardAddress = standardAddress;
  }
  
  public String getStandardAddress() {
    return standardAddress;
  }

  public String toString() {
    return standardAddress;
  }
}
