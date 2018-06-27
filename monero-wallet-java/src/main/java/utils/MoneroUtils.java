package utils;

import java.util.ArrayList;
import java.util.List;

import model.MoneroAddress;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import service.MoneroWallet;

/**
 * Collection of utilities for working with Monero types.
 */
public class MoneroUtils {

  private static final int MNEMONIC_SEED_NUM_WORDS = 25;
  private static final int VIEW_KEY_LENGTH = 64;
  private static final char[] ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toCharArray();
  private static final List<Character> CHARS = new ArrayList<Character>();
  static {
    for (char c : ALPHABET) {
      CHARS.add(c);
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
   * @param wallet might be used to decode an integrated address into its standard address and payment id components
   * @return MoneroAddress is the string address converted to a proper address object
   */
  public static MoneroAddress toAddress(String address, MoneroWallet wallet) {
    if (MoneroAddress.isValidStandardAddress(address)) return new MoneroAddress(address);
    else if (MoneroIntegratedAddress.isValidIntegratedAddress(address)) return wallet.decodeIntegratedAddress(address);
    throw new MoneroException("Address is neither standard nor integrated: " + address);
  }

  public static void validateHex(String str) {
    if (!str.matches("^([0-9A-Fa-f]{2})+$")) throw new MoneroException("Invalid hex: " + str);
  }

  public static void validateBase58(String standardAddress) {
    for (char c : standardAddress.toCharArray()) {
      if (!CHARS.contains((Character) c)) throw new MoneroException("Invalid Base58 " + standardAddress);
    }
  }
}
