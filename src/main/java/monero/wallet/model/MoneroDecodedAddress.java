package monero.wallet.model;

import monero.daemon.model.MoneroNetworkType;

/**
 * Maintains metadata for a decoded address.
 */
public class MoneroDecodedAddress {
  
  private String address;
  private MoneroAddressType addressType;
  private MoneroNetworkType networkType;
  
  public MoneroDecodedAddress(String address, MoneroAddressType addressType, MoneroNetworkType networkType) {
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

  public MoneroAddressType getAddressType() {
    return addressType;
  }
  
  public void setAddressType(MoneroAddressType addressType) {
    this.addressType = addressType;
  }
  
  public MoneroNetworkType getNetworkType() {
    return networkType;
  }
  
  public void setNetworkType(MoneroNetworkType networkType) {
    this.networkType = networkType;
  }
}
