package monero.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

/**
 * Collection of Monero utilities.
 */
public class MoneroUtils {

  private static final int NUM_MNEMONIC_WORDS = 25;
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
   * @param mnemonic is the mnemonic to validate
   * @throws MoneroException if the given seed is invalid
   */
  public static void validateMnemonic(String mnemonic) {
    assertNotNull("Mnemonic phrase is not initialized", mnemonic);
    assertFalse("Mnemonic phrase is empty", mnemonic.isEmpty());
    String[] words = mnemonic.split(" ");
    if (words.length != MoneroUtils.NUM_MNEMONIC_WORDS) throw new Error("Mnemonic phrase is " + words.length + " words but must be " + MoneroUtils.NUM_MNEMONIC_WORDS);
  }
  
  // TODO: beef this up
  public static void validatePrivateViewKey(String privateViewKey) {
    assertNotNull(privateViewKey);
    assertEquals(64, privateViewKey.length());
  }
  
  // TODO: beef this up
  public static void validateAddress(String address) {
    
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
  
  /**
   * Determines if two payment ids are functionally equal.
   * 
   * For example, 03284e41c342f032 and 03284e41c342f032000000000000000000000000000000000000000000000000 are considered equal.
   * 
   * @param paymentId1 is a payment id to compare
   * @param paymentId2 is a payment id to compare
   * @return true if the payment ids are equal, false otherwise
   */
  public static boolean paymentIdsEqual(String paymentId1, String paymentId2) {
    int maxLength = Math.max(paymentId1.length(), paymentId2.length());
    for (int i = 0; i < maxLength; i++) {
      if (i < paymentId1.length() && i < paymentId2.length() && paymentId1.charAt(i) != paymentId2.charAt(i)) return false;
      if (i >= paymentId1.length() && paymentId2.charAt(i) != '0') return false;
      if (i >= paymentId2.length() && paymentId1.charAt(i) != '0') return false;
    }
    return true;
  }
}
