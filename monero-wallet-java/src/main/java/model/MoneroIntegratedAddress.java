package model;

import utils.MoneroUtils;

public class MoneroIntegratedAddress extends MoneroAddress {

  private String paymentId;
  private String integratedAddress;
  
  public MoneroIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    super(standardAddress);
    validatePaymentId(paymentId);
    validateIntegratedAddress(integratedAddress);
    this.paymentId = paymentId;
    this.integratedAddress = integratedAddress;
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

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((integratedAddress == null) ? 0 : integratedAddress.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!super.equals(obj)) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroIntegratedAddress other = (MoneroIntegratedAddress) obj;
    if (integratedAddress == null) {
      if (other.integratedAddress != null) return false;
    } else if (!integratedAddress.equals(other.integratedAddress)) return false;
    if (paymentId == null) {
      if (other.paymentId != null) return false;
    } else if (!paymentId.equals(other.paymentId)) return false;
    return true;
  }
  
  // ------------------------------ STATIC UTILITIES --------------------------
  
  private static final int PAYMENT_ID_LENGTH = 16;
  private static final int INTEGRATED_ADDRESS_LENGTH = 106;
  
  public static boolean isValidIntegratedAddress(String integratedAddress) {
    try {
      validateIntegratedAddress(integratedAddress);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  private static boolean isValidPaymentId(String paymentId) {
    try {
      validatePaymentId(paymentId);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  private static void validatePaymentId(String paymentId) {
    if (paymentId == null) throw new MoneroException("Payment id is null");
    MoneroUtils.validateHex(paymentId);
    if (paymentId.length() != PAYMENT_ID_LENGTH) throw new MoneroException("Payment id is " + paymentId.length() + " characters but must be " + PAYMENT_ID_LENGTH);
  }
  
  private static void validateIntegratedAddress(String integratedAddress) {
    if (integratedAddress == null) throw new MoneroException("Integrated address is null");
    if (integratedAddress.length() != INTEGRATED_ADDRESS_LENGTH) throw new MoneroException("Integrated address is " + integratedAddress.length() + " characters but must be " + INTEGRATED_ADDRESS_LENGTH);
  }
  
  private static void validateIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    new MoneroAddress(standardAddress);
    if (paymentId != null) validatePaymentId(paymentId);
    validateIntegratedAddress(integratedAddress);
    // TODO: make sure standard address + payment id = integrated address
  }
}
