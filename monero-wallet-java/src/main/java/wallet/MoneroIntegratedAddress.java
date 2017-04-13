package wallet;

import utils.MoneroUtils;

/**
 * Represents a Monero integrated address.
 * 
 * An integrated address is derived from a standard address and a payment id.
 * 
 * @author woodser
 */
public class MoneroIntegratedAddress extends MoneroAddress {

  private String paymentId;
  private String integratedAddress;
  
  public MoneroIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    super(standardAddress);
    this.paymentId = paymentId;
    this.integratedAddress = integratedAddress;
    MoneroUtils.validateAddress(this);
  }
  
  public MoneroIntegratedAddress(String integratedAddress) {
    super(MoneroUtils.getIntegratedAddressComponents(integratedAddress).getFirst());
    this.paymentId = MoneroUtils.getIntegratedAddressComponents(integratedAddress).getSecond();
    this.integratedAddress = integratedAddress;
    MoneroUtils.validateAddress(this);
  }

  public String getPaymentId() {
    return paymentId;
  }

  public String getIntegratedAddress() {
    return integratedAddress;
  }
  
  public String toString() {
    return integratedAddress;
  }
}
