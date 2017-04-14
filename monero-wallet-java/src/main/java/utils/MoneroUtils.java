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
  
  private static final int STANDARD_ADDRESS_LENGTH = 95;
  private static final int PAYMENT_ID_LENGTH = 16;
  private static final int INTEGRATED_ADDRESS_LENGTH = 106;
  private static final int MNEMONIC_SEED_NUM_WORDS = 25;
  private static final int VIEW_KEY_LENGTH = 64;

  public static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    validateHex(standardAddress);
    if (standardAddress.length() != STANDARD_ADDRESS_LENGTH) throw new MoneroException("Standard address is " + standardAddress.length() + " characters but must be " + STANDARD_ADDRESS_LENGTH);
  }
  
  public static void validatePaymentId(String paymentId) {
    if (paymentId == null) throw new MoneroException("Payment id is null");
    validateHex(paymentId);
    if (paymentId.length() != PAYMENT_ID_LENGTH) throw new MoneroException("Payment id is " + paymentId.length() + " characters but must be " + PAYMENT_ID_LENGTH);
  }
  
  public static void validateIntegratedAddress(String integratedAddress) {
    if (integratedAddress == null) throw new MoneroException("Integrated address is null");
    if (integratedAddress.length() != INTEGRATED_ADDRESS_LENGTH) throw new MoneroException("Integrated address is " + integratedAddress.length() + " characters but must be " + INTEGRATED_ADDRESS_LENGTH);
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
  
  public static void validateMnemonicSeed(String mnemonicSeed) {
    if (mnemonicSeed == null) throw new MoneroException("Mnemonic seed is null");
    String[] words = mnemonicSeed.split(" ");
    if (words.length != MNEMONIC_SEED_NUM_WORDS) throw new MoneroException("Mnemonic seed must be " + MNEMONIC_SEED_NUM_WORDS + " words but was " + words.length + " words");
  }
  
  public static void validateViewKey(String viewKey) {
    if (viewKey == null) throw new MoneroException("View key is null");
    if (viewKey.length() != VIEW_KEY_LENGTH) throw new MoneroException("View key must be " + VIEW_KEY_LENGTH + " characters but was " + viewKey.length() + " characters");
  }
  
  private static void validateHex(String str) {
    
  }
}
