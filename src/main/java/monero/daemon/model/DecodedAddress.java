package monero.daemon.model;

/**
 * Maintains metadata for a decoded address.
 */
public class DecodedAddress {
  
  private String address;
  private AddressType addressType;
  private MoneroNetworkType networkType;
  
  public DecodedAddress(String address, AddressType addressType, MoneroNetworkType networkType) {
    this.address = address;
    this.addressType = addressType;
    this.networkType = networkType;
  }
  
  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public AddressType getAddressType() {
    return addressType;
  }
  
  public void setAddressType(AddressType addressType) {
    this.addressType = addressType;
  }
  
  public MoneroNetworkType getNetworkType() {
    return networkType;
  }
  
  public void setNetworkType(MoneroNetworkType networkType) {
    this.networkType = networkType;
  }
}
