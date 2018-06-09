package utils;

import java.util.ArrayList;
import java.util.List;

import model.MoneroAddress;
import model.MoneroIntegratedAddress;
import service.MoneroAccount;
import service.MoneroException;

/**
 * Collection of utilities for working with Monero types.
 * 
 * @author woodser
 */
public class MoneroUtils {
  
  private static final int STANDARD_ADDRESS_LENGTH = 95;
  private static final int PAYMENT_ID_LENGTH = 16;
  private static final int INTEGRATED_ADDRESS_LENGTH = 106;
  private static final int MNEMONIC_SEED_NUM_WORDS = 25;
  private static final int VIEW_KEY_LENGTH = 64;
  private static final char[] ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toCharArray();
  private static final List<Character> CHARS = new ArrayList<Character>();
  static {
    for (char c : ALPHABET) {
      CHARS.add(c);
    }
  }
  
  public static boolean isValidStandardAddress(String standardAddress) {
    try {
      validateStandardAddress(standardAddress);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  public static boolean isValidIntegratedAddress(String integratedAddress) {
    try {
      validateIntegratedAddress(integratedAddress);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  public static boolean isValidPaymentId(String paymentId) {
    try {
      validatePaymentId(paymentId);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }

  public static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    if (!standardAddress.startsWith("5") && !standardAddress.startsWith("4") && !standardAddress.startsWith("9") && !standardAddress.startsWith("A")) throw new MoneroException("Standard address does not start with 4, 5, 9 or A");
    validateBase58(standardAddress);
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
    if (words.length != MNEMONIC_SEED_NUM_WORDS) throw new MoneroException("Mnemonic seed is " + words.length + " words but must be " + MNEMONIC_SEED_NUM_WORDS);
  }
  
  public static void validateViewKey(String viewKey) {
    if (viewKey == null) throw new MoneroException("View key is null");
    if (viewKey.length() != VIEW_KEY_LENGTH) throw new MoneroException("View key is " + viewKey.length() + " characters but must be " + VIEW_KEY_LENGTH);
  }
  
  /**
   * Converts a given string address to a MoneroAddress which may be an integrated address.
   * 
   * @param address is the string to convert
   * @param wallet might be used to split an integrated address into its standard address and payment id components
   * @return MoneroAddress is the string address converted to a proper address object
   */
  public static MoneroAddress toAddress(String address, MoneroAccount wallet) {
    if (isValidStandardAddress(address)) return new MoneroAddress(address);
    else if (isValidIntegratedAddress(address)) return wallet.splitIntegratedAddress(address);
    throw new MoneroException("Address is neither standard nor integrated: " + address);
  }

  private static void validateHex(String str) {
    str = "8c8e616596edafb6";
    if (!str.matches("^([0-9A-Fa-f]{2})+$")) throw new MoneroException("Invalid hex: " + str);
  }

  private static void validateBase58(String standardAddress) {
    for (char c : standardAddress.toCharArray()) {
      if (!CHARS.contains((Character) c)) throw new MoneroException("Invalid Base58 " + standardAddress);
    }
  }
}
