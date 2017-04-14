package wallet;

public class MoneroIntegratedAddress extends MoneroAddress {

  private String paymentId;
  private String integratedAddress;
  
  public MoneroIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    super(standardAddress);
    this.paymentId = paymentId;
    this.integratedAddress = integratedAddress;
  }

  public String getPaymentId() {
    return paymentId;
  }

  public String getIntegratedAddress() {
    return integratedAddress;
  }
}
