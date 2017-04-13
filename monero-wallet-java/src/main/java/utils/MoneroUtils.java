package utils;

import types.Pair;
import wallet.MoneroException;

public class MoneroUtils {
  
  private static final int MONERO_STANDARD_ADDRESS_LENGTH = 95;

  public static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    if (standardAddress.length() != MONERO_STANDARD_ADDRESS_LENGTH) throw new MoneroException("Standard address is " + standardAddress.length() + " characters but must be " + MONERO_STANDARD_ADDRESS_LENGTH);
  }
  
  public static void validateIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not yet implemented.");
  }
  
  public static void validatePaymentId(String paymentId) {
    throw new RuntimeException("Not yet implemented.");
  }
  
  public static Pair<String, String> getIntegratedAddressComponents(String integratedAddress) {
    throw new MoneroException("Not yet implemented.");
  }
}
