package utils;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import model.MoneroException;

/**
 * Collection of Monero utilities.
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
   * Converts the string to a URI.  Throws MoneroException if exception.
   * 
   * @param endpoint is the string to convert to a URI
   * @return URI is the initialized object from the string endpoint
   */
  public static URI parseUri(String endpoint) {
    try {
      return new URI(endpoint);
    } catch (Exception e) {
      throw new MoneroException(e);
    }
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
