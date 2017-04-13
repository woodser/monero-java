package wallet;

import utils.MoneroUtils;

/**
 * Represents a Monero address.
 * 
 * Every Monero address has a standard address, whether integrated or not.
 * 
 * @author woodser
 */
public class MoneroAddress {
  
  private String standardAddress;
  
  public MoneroAddress(String standardAddress) {
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
