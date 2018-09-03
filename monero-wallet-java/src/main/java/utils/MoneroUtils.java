package utils;

import java.util.ArrayList;
import java.util.List;

import model.MoneroAddress;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import wallet.MoneroWallet;

/**
 * Collection of Monero utilities.
 */
public class MoneroUtils {

  private static final int MNEMONIC_SEED_NUM_WORDS = 25;
  private static final int VIEW_KEY_LENGTH = 64;
  private static final int STANDARD_ADDRESS_LENGTH = 95;
  private static final int PAYMENT_ID_LENGTH = 16;
  private static final int INTEGRATED_ADDRESS_LENGTH = 106;
  private static final char[] ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toCharArray();
  private static final List<Character> CHARS = new ArrayList<Character>();
  static {
    for (char c : ALPHABET) {
      CHARS.add(c);
    }
  }
  
  /**
   * Validates the given mnemonic seed.
   * 
   * @param mnemonicSeed is the seed to validate
   * @throws MoneroException if the given seed is invalid
   */
  public static void validateMnemonicSeed(String mnemonicSeed) {
    if (mnemonicSeed == null) throw new MoneroException("Mnemonic seed is null");
    String[] words = mnemonicSeed.split(" ");
    if (words.length != MNEMONIC_SEED_NUM_WORDS) throw new MoneroException("Mnemonic seed is " + words.length + " words but must be " + MNEMONIC_SEED_NUM_WORDS);
  }
  
  /**
   * Validates the given view key.
   * 
   * @param viewKey is the view key to validate
   * @throws MoneroException if the given view key is invalid
   */
  public static void validateViewKey(String viewKey) {
    if (viewKey == null) throw new MoneroException("View key is null");
    if (viewKey.length() != VIEW_KEY_LENGTH) throw new MoneroException("View key is " + viewKey.length() + " characters but must be " + VIEW_KEY_LENGTH);
  }
  
  /**
   * Validates the given standard address.
   * 
   * @param standardAddress is the standard address to validate
   * @throws MoneroException if the given standard address is invalid
   */
  public static void validateStandardAddress(String standardAddress) {
    if (standardAddress == null) throw new MoneroException("Standard address is null");
    if (!standardAddress.startsWith("5") && !standardAddress.startsWith("4") && !standardAddress.startsWith("9") && !standardAddress.startsWith("A")) throw new MoneroException("Standard address does not start with 4, 5, 9 or A");
    MoneroUtils.validateBase58(standardAddress);
    if (standardAddress.length() != STANDARD_ADDRESS_LENGTH) throw new MoneroException("Standard address is " + standardAddress.length() + " characters but must be " + STANDARD_ADDRESS_LENGTH);
  }
  
  /**
   * Determines if the given standard address is valid.
   * 
   * @param standardAddress is the standard address to determine the validity of
   * @return true if the given string is a valid standard address, false otherwise
   */
  public static boolean isValidStandardAddress(String standardAddress) {
    try {
      validateStandardAddress(standardAddress);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  /**
   * Determines if the given payment id is valid
   * 
   * @param paymentId is the payment id to determine the validity of
   * @return true if the payment id is valid, false otherwise
   */
  public static boolean isValidPaymentId(String paymentId) {
    try {
      validatePaymentId(paymentId);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  /**
   * Validates the given payment id.
   * 
   * @param paymentId is the payment id to validate
   * @throws MoneroException if the given payment id is invalid
   */
  public static void validatePaymentId(String paymentId) {
    if (paymentId == null) throw new MoneroException("Payment id is null");
    MoneroUtils.validateHex(paymentId);
    if (paymentId.length() != PAYMENT_ID_LENGTH) throw new MoneroException("Payment id is " + paymentId.length() + " characters but must be " + PAYMENT_ID_LENGTH);
  }
  
  /**
   * Validates the given integrated address.
   * 
   * @param integratedAddress is the integrated address to validate
   * @throws MoneroException is if the given integrated address is invalid
   */
  public static void validateIntegratedAddress(String integratedAddress) {
    if (integratedAddress == null) throw new MoneroException("Integrated address is null");
    if (integratedAddress.length() != INTEGRATED_ADDRESS_LENGTH) throw new MoneroException("Integrated address is " + integratedAddress.length() + " characters but must be " + INTEGRATED_ADDRESS_LENGTH);
  }
  
  /**
   * Validates that the given address, payment id, and integrated address are consistent.
   * 
   * @param standardAddress is the standard address
   * @param paymentId is the payment id
   * @param integratedAddress is the integrated address
   */
  public static void validateIntegratedAddress(String standardAddress, String paymentId, String integratedAddress) {
    new MoneroAddress(standardAddress);
    if (paymentId != null) validatePaymentId(paymentId);
    validateIntegratedAddress(integratedAddress);
    // TODO: make sure standard address + payment id = integrated address
  }
  
  /**
   * Determines if the given integrated address is valid.
   * 
   * @param integratedAddress is the integrated address to determine the validity of
   * @return true if the given string is a valid integrated address, false otherwise
   */
  public static boolean isValidIntegratedAddress(String integratedAddress) {
    try {
      validateIntegratedAddress(integratedAddress);
      return true;
    } catch (MoneroException e) {
      return false;
    }
  }
  
  /**
   * Validates the given Monero address which may be an address or an integrated address.
   * 
   * @param address is the address to validate
   * @throws MoneroException if if the address is invalid
   */
  public static void validateAddress(MoneroAddress address) {
    if (address instanceof MoneroIntegratedAddress) {
      MoneroIntegratedAddress integratedAddress = (MoneroIntegratedAddress) address;
      validateIntegratedAddress(integratedAddress.getStandardAddress(), integratedAddress.getPaymentId(), integratedAddress.getIntegratedAddress());
    } else {
      validateStandardAddress(address.getStandardAddress());
    }
  }
  
//  /**
//   * Converts a given string address to a MoneroAddress which may be an integrated address.
//   * 
//   * @param address is the string to convert
//   * @param wallet might be used to decode an integrated address into its standard address and payment id components
//   * @return MoneroAddress is the string address converted to a proper address object
//   */
//  public static MoneroAddress toAddress(String address, MoneroWallet wallet) {
//    if (isValidStandardAddress(address)) return new MoneroAddress(address);
//    else if (isValidIntegratedAddress(address)) return wallet.decodeIntegratedAddress(address);
//    throw new MoneroException("Address is neither standard nor integrated: " + address);
//  }
  
  /**
   * Initializes a new Monero address which may be an integrated address.
   * 
   * @param address may be standard, integrated, or sub-address formats
   * @param wallet is used as a utility to decode an integrated address
   * @return MoneroAddress may be MoneroAddress or MoneroIntegratedAddress
   */
  public static MoneroAddress newAddress(String address, MoneroWallet wallet) {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Initializes a new Monero address which may be an integrated address.
   * 
   * @param address may be standard, integrated, or sub-address formats
   * @param paymentId is a payment id to create an integated address (optional)
   * @param wallet is used as a utility to decode an integrated address
   * @return MoneroAddress may be MoneroAddress or MoneroIntegratedAddress
   */
  public static MoneroAddress newAddress(String address, String paymentId, MoneroWallet wallet) {
    throw new RuntimeException("Not implemented");
  }

  private static void validateHex(String str) {
    if (!str.matches("^([0-9A-Fa-f]{2})+$")) throw new MoneroException("Invalid hex: " + str);
  }

  private static void validateBase58(String standardAddress) {
    for (char c : standardAddress.toCharArray()) {
      if (!CHARS.contains((Character) c)) throw new MoneroException("Invalid Base58 " + standardAddress);
    }
  }
}
