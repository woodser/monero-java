package utils;

import wallet.MoneroAddress;
import wallet.MoneroException;
import wallet.MoneroIntegratedAddress;

/**
 * Collection of utilities for working with Monero types.
 * 
 * TODO: validate addresses are hexidecimal
 * 
 * @author woodser
 */
public class MoneroUtils {
  
  private static final int MONERO_STANDARD_ADDRESS_LENGTH = 95;
  private static final int MONERO_PAYMENT_ID_LENGTH = 16;
  private static final int MONERO_INTEGRATED_ADDRESS_LENGTH = 106;

  public static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    validateHex(standardAddress);
    if (standardAddress.length() != MONERO_STANDARD_ADDRESS_LENGTH) throw new MoneroException("Standard address is " + standardAddress.length() + " characters but must be " + MONERO_STANDARD_ADDRESS_LENGTH);
  }
  
  public static void validatePaymentId(String paymentId) {
    if (paymentId == null) throw new MoneroException("Payment id is null");
    validateHex(paymentId);
    if (paymentId.length() != MONERO_PAYMENT_ID_LENGTH) throw new MoneroException("Payment id is " + paymentId.length() + " characters but must be " + MONERO_PAYMENT_ID_LENGTH);
  }
  
  public static void validateIntegratedAddress(String integratedAddress) {
    if (integratedAddress == null) throw new MoneroException("Integrated address is null");
    if (integratedAddress.length() != MONERO_INTEGRATED_ADDRESS_LENGTH) throw new MoneroException("Integrated address is " + integratedAddress.length() + " characters but must be " + MONERO_INTEGRATED_ADDRESS_LENGTH);
  }
  
  public static void validateIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    validateStandardAddress(standardAddress);
    if (paymentId != null) validatePaymentId(paymentId);
    validateIntegratedAddress(integratedAddress);
    // TODO: make sure standard address + payment id = integrated address
  }
  
  public static void validateAddress(MoneroAddress address) {
    if (address instanceof MoneroIntegratedAddress) {
      MoneroIntegratedAddress integratedAddress = (MoneroIntegratedAddress) address;
      validateIntegratedAddress(integratedAddress.getStandardAddress(), integratedAddress.getPaymentId(), integratedAddress.getIntegratedAddress());
    } else {
      validateStandardAddress(address.getStandardAddress());
    }
  }
  
  private static void validateHex(String str) {
    
  }
}
