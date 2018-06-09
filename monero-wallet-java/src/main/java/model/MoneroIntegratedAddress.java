package model;

import utils.MoneroUtils;

public class MoneroIntegratedAddress extends MoneroAddress {

  private String paymentId;
  private String integratedAddress;
  
  public MoneroIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    super(standardAddress);
    MoneroUtils.validatePaymentId(paymentId);
    MoneroUtils.validateIntegratedAddress(integratedAddress);
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
}
