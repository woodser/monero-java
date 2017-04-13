package wallet;

import utils.MoneroUtils;

/**
 * Represents a Monero integrated address.
 * 
 * An integrated address is derived from a standard address and a payment id.
 * 
 * @author woodser
 */
public class MoneroIntegratedAddress {

  private String standardAddress;
  private String paymentId;
  private String integratedAddress;
  
  public MoneroIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    MoneroUtils.validateStandardAddress(standardAddress);
    MoneroUtils.validatePaymentId(paymentId);
    MoneroUtils.validateIntegratedAddress(integratedAddress);
    this.standardAddress = standardAddress;
    this.paymentId = paymentId;
    this.integratedAddress = integratedAddress;
  }
  
  public MoneroIntegratedAddress(String integratedAddress) {
    MoneroUtils.validateIntegratedAddress(integratedAddress);
    throw new MoneroException("Not yet implemented.");
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
  
  public String toString() {
    return integratedAddress;
  }
}
