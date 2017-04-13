package utils;

import types.Pair;
import wallet.MoneroAddress;
import wallet.MoneroException;
import wallet.MoneroIntegratedAddress;

/**
 * Collection of utilities for working with Monero types.
 * 
 * @author woodser
 */
public class MoneroUtils {
  
  private static final int MONERO_STANDARD_ADDRESS_LENGTH = 95;

  public static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    if (standardAddress.length() != MONERO_STANDARD_ADDRESS_LENGTH) throw new MoneroException("Standard address is " + standardAddress.length() + " characters but must be " + MONERO_STANDARD_ADDRESS_LENGTH);
  }
  
  public static void validatePaymentId(String paymentId) {
    throw new RuntimeException("Not yet implemented.");
  }
  
  public static void validateIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not yet implemented.");
  }
  
  public static void validateIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    validateStandardAddress(standardAddress);
    validatePaymentId(paymentId);
    validateIntegratedAddress(integratedAddress);
    // TODO: validate standard address + paymentId = integrated address
  }
  
  public static Pair<String, String> getIntegratedAddressComponents(String integratedAddress) {
    throw new RuntimeException("Not yet implemented.");
  }
  
  public static void validateAddress(MoneroAddress address) {
    if (address instanceof MoneroIntegratedAddress) {
      MoneroIntegratedAddress integratedAddress = (MoneroIntegratedAddress) address;
      validateIntegratedAddress(integratedAddress.getStandardAddress(), integratedAddress.getPaymentId(), integratedAddress.getIntegratedAddress());
    } else{
      validateStandardAddress(address.getStandardAddress());
    }
  }
}
