package wallet;

/**
 * Represents a Monero address as a string.
 * 
 * The address string can be a standard address or integrated address.
 * 
 * @author woodser
 */
public class MoneroAddress {
  
  private String address;
  private String standardAddress;
  private String paymentId;
  private String integratedAddress;
  private MoneroAddressType addressType;
  
  public enum MoneroAddressType {
    STANDARD,
    INTEGRATED
  }
  
  public MoneroAddress(String address) {
    this.address = address;
  }
  
  public String getAddress(String standardAddress, String paymentId) {
    return address;
  }
  
  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public String getStandardAddress() {
    return standardAddress;
  }

  public String getPaymentId() {
    return paymentId;
  }

  public String getIntegratedAddress() {
    return integratedAddress;
  }
  
  public MoneroAddressType getAddressType() {
    return addressType;
  }

  public String toString() {
    return address;
  }
}
