package monero.common;

import java.math.BigDecimal;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.codec.binary.Hex;
import org.bouncycastle.jcajce.provider.digest.Keccak;

import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.wallet.model.MoneroAddressType;
import monero.wallet.model.MoneroDecodedAddress;

/**
 * Collection of Monero utilities.
 */
public class MoneroUtils {
  
  static {
    try {
      System.loadLibrary("monero-java");
    } catch (UnsatisfiedLinkError e) {
      // OK, but JNI utils will not work
    }
  }
  
  public static final long WALLET2_REFRESH_INTERVAL = 10000;  // core wallet2 syncs on a fixed intervals
  public static final int RING_SIZE = 12;                     // network-enforced ring size

  private static final int NUM_MNEMONIC_WORDS = 25;
  private static final int VIEW_KEY_LENGTH = 64;
  private static final String ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
  private static final List<Character> CHARS = new ArrayList<Character>();
  static {
    for (char c : ALPHABET.toCharArray()) {
      CHARS.add(c);
    }
  }
  private final static BigDecimal ALPHABET_SIZE = new BigDecimal(ALPHABET.length());
  private final static Map<Integer, Integer> ENCODED_BLOCK_SIZE = new HashMap<Integer, Integer>();
  static {
      ENCODED_BLOCK_SIZE.put(0, 0);
      ENCODED_BLOCK_SIZE.put(2, 1);
      ENCODED_BLOCK_SIZE.put(3, 2);
      ENCODED_BLOCK_SIZE.put(5, 3);
      ENCODED_BLOCK_SIZE.put(6, 4);
      ENCODED_BLOCK_SIZE.put(7, 5);
      ENCODED_BLOCK_SIZE.put(9, 6);
      ENCODED_BLOCK_SIZE.put(10, 7);
      ENCODED_BLOCK_SIZE.put(11, 8);
  };
  private final static int FULL_BLOCK_SIZE = 8;
  private final static int FULL_ENCODED_BLOCK_SIZE = 11;
  private final static BigDecimal UINT64_MAX = new BigDecimal(Math.pow(2, 64));
  private final static Pattern STANDARD_ADDRESS_PATTERN = Pattern.compile("^[" + ALPHABET + "]{95}$");
  private final static Pattern INTEGRATED_ADDRESS_PATTERN = Pattern.compile("^[" + ALPHABET + "]{106}$");

  /**
   * Validates the given mnemonic phrase.
   * 
   * @param mnemonic is the mnemonic to validate
   * @throws MoneroError if the given mnemonic is invalid
   */
  public static void validateMnemonic(String mnemonic) {
    GenUtils.assertNotNull("Mnemonic phrase is not initialized", mnemonic);
    GenUtils.assertFalse("Mnemonic phrase is empty", mnemonic.isEmpty());
    String[] words = mnemonic.split(" ");
    if (words.length != MoneroUtils.NUM_MNEMONIC_WORDS) throw new Error("Mnemonic phrase is " + words.length + " words but must be " + MoneroUtils.NUM_MNEMONIC_WORDS);
  }
  
  // TODO: improve validation
  public static void validatePrivateViewKey(String privateViewKey) {
    GenUtils.assertNotNull(privateViewKey);
    GenUtils.assertEquals(64, privateViewKey.length());
  }
  
  // TODO: improve validation
  public static void validatePrivateSpendKey(String privateSpendKey) {
    GenUtils.assertNotNull(privateSpendKey);
    GenUtils.assertEquals(64, privateSpendKey.length());
  }
  
  // TODO: improve validation
  public static void validatePublicViewKey(String publicViewKey) {
    GenUtils.assertNotNull(publicViewKey);
    GenUtils.assertEquals(64, publicViewKey.length());
  }
  
  // TODO: improve validation
  public static void validatePublicSpendKey(String publicSpendKey) {
    GenUtils.assertNotNull(publicSpendKey);
    GenUtils.assertEquals(64, publicSpendKey.length());
  }
  
  /**
   * Decodes the given address.
   * 
   * @param address is the address to decode
   * @return the decoded address and network types
   */
  public static MoneroDecodedAddress decodeAddress(String address) {
    GenUtils.assertNotNull("Address is null", address);
    
    // determine if address has integrated address pattern
    boolean isIntegrated = false;
    if (!STANDARD_ADDRESS_PATTERN.matcher(address).matches()) {
      if (INTEGRATED_ADDRESS_PATTERN.matcher(address).matches()) {
        isIntegrated = true;
      } else {
        throw new MoneroError("Address has invalid regex pattern");
      }
    }
    
    // decode address to hex string
    String addressHex = decodeAddressToHex(address);
    
    // validate address hash
    GenUtils.assertTrue("Address has invalid hash", isValidAddressHash(addressHex));
    
    // get address code
    int addressCode = Integer.parseInt(addressHex.substring(0, 2), 16);
    
    // determine network and address types
    MoneroAddressType addressType = null;
    MoneroNetworkType networkType = null;
    for (MoneroNetworkType aNetworkType : MoneroNetworkType.values()) {
      if (addressCode == aNetworkType.getPrimaryAddressCode()) {
        GenUtils.assertFalse("Address has primary address code but integrated address pattern", isIntegrated);
        addressType = MoneroAddressType.PRIMARY_ADDRESS;
        networkType = aNetworkType;
        break;
      } else if (addressCode == aNetworkType.getIntegratedAddressCode()) {
        GenUtils.assertTrue("Address has integrated address code but non-integrated address pattern", isIntegrated);
        addressType = MoneroAddressType.INTEGRATED_ADDRESS;
        networkType = aNetworkType;
        break;
      } else if (addressCode == aNetworkType.getSubaddressCode()) {
        GenUtils.assertFalse("Address has subaddress code but integrated address pattern", isIntegrated);
        addressType = MoneroAddressType.SUBADDRESS;
        networkType = aNetworkType;
        break;
      }
    }
    
    // validate address and network types
    GenUtils.assertTrue("Address has invalid code: " + addressCode, addressType != null && networkType != null);
    
    // return decoded address
    return new MoneroDecodedAddress(address, addressType, networkType);
  }
  
  /**
   * Validates the given address.
   * 
   * @param address is the address to validate
   * @param networkType is the address's network type
   */
  public static void validateAddress(String address, MoneroNetworkType networkType) {
    try {
      MoneroDecodedAddress decodedAddress = decodeAddress(address);
      GenUtils.assertEquals("Address network type mismatch: " + networkType + " vs " + decodedAddress.getNetworkType(), networkType, decodedAddress.getNetworkType());
    } catch (AssertionError e) {
      throw new MoneroError(e.getMessage());
    }
  }

  // TODO: improve validation
  public static void validatePaymentId(String paymentId) {
    GenUtils.assertTrue(paymentId.length() == 16 || paymentId.length() == 64);
  }
  
  /**
   * Validates the given view key.
   * 
   * @param viewKey is the view key to validate
   * @throws MoneroError if the given view key is invalid
   */
  public static void validateViewKey(String viewKey) {
    if (viewKey == null) throw new MoneroError("View key is null");
    if (viewKey.length() != VIEW_KEY_LENGTH) throw new MoneroError("View key is " + viewKey.length() + " characters but must be " + VIEW_KEY_LENGTH);
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
      throw new MoneroError(e);
    }
  }

  public static void validateHex(String str) {
    if (!str.matches("^([0-9A-Fa-f]{2})+$")) throw new MoneroError("Invalid hex: " + str);
  }

  public static void validateBase58(String standardAddress) {
    for (char c : standardAddress.toCharArray()) {
      if (!CHARS.contains((Character) c)) throw new MoneroError("Invalid Base58 " + standardAddress);
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
  
  /**
   * Merges a transaction into a list of existing transactions.
   * 
   * @param txs are existing transactions to merge into
   * @param tx is the transaction to merge into the list
   */
  public static <T extends MoneroTx> void mergeTx(List<T> txs, T tx) {
    for (MoneroTx aTx : txs) {
      if (aTx.getHash().equals(tx.getHash())) {
        aTx.merge(tx);
        return;
      }
    }
    txs.add(tx);
  }
  
  public static byte[] mapToBinary(Map<String, Object> map) {
    return jsonToBinaryJni(JsonUtils.serialize(map));
  }
  
  public static Map<String, Object> binaryToMap(byte[] bin) {
    return JsonUtils.deserialize(binaryToJsonJni(bin), new TypeReference<Map<String, Object>>(){});
  }
  
  @SuppressWarnings("unchecked")
  public static Map<String, Object> binaryBlocksToMap(byte[] binBlocks) {
    
    // convert binary blocks to json then to map
    Map<String, Object> map = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, binaryBlocksToJsonJni(binBlocks), new TypeReference<Map<String, Object>>(){});
    
    // parse blocks to maps
    List<Map<String, Object>> blockMaps = new ArrayList<Map<String, Object>>();
    for (String blockStr : (List<String>) map.get("blocks")) {
      blockMaps.add(JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blockStr, new TypeReference<Map<String, Object>>(){}));
    }
    map.put("blocks", blockMaps); // overwrite block strings
    
    // parse txs to maps, one array of txs per block
    List<List<Map<String, Object>>> allTxs = new ArrayList<List<Map<String, Object>>>();
    List<Object> rpcAllTxs = (List<Object>) map.get("txs");
    for (Object rpcTxs : rpcAllTxs) {
      if ("".equals(rpcTxs)) {
        allTxs.add(new ArrayList<Map<String, Object>>());
      } else {
        List<Map<String, Object>> txs = new ArrayList<Map<String, Object>>();
        allTxs.add(txs);
        for (String rpcTx : (List<String>) rpcTxs) {
          txs.add(JsonUtils.deserialize(MoneroRpcConnection.MAPPER, rpcTx.replaceFirst(",", "{") + "}", new TypeReference<Map<String, Object>>(){})); // modify tx string to proper json and parse // TODO: more efficient way than this json manipulation?
        }
      }
    }
    map.put("txs", allTxs); // overwrite tx strings

    // return map containing blocks and txs as maps
    return map;
  }
  
  public static void initJniLogging(String path, int level, boolean console) {
    initLoggingJni(path, console);
    setLogLevelJni(level);
  }
  
  public static void setJniLogLevel(int level) {
    setLogLevelJni(level);
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  private native static byte[] jsonToBinaryJni(String json);
  
  private native static String binaryToJsonJni(byte[] bin);
  
  private native static String binaryBlocksToJsonJni(byte[] binBlocks);
  
  private native static void initLoggingJni(String path, boolean console);

  private native static void setLogLevelJni(int level);

  private static boolean isValidAddressHash(String decodedAddrStr) {
    String checksumCheck = decodedAddrStr.substring(decodedAddrStr.length() - 8);
    String withoutChecksumStr = decodedAddrStr.substring(0, decodedAddrStr.length() - 8);
    byte[] withoutChecksumBytes = hexToBin(withoutChecksumStr);

    Keccak.Digest256 digest256 = new Keccak.Digest256();
    byte[] hashbytes = digest256.digest(withoutChecksumBytes);
    String encodedStr = Hex.encodeHexString(hashbytes);

    String hashChecksum = encodedStr.substring(0, 8);
    return hashChecksum != null && hashChecksum.equals(checksumCheck);
  }
  
  private static String decodeAddressToHex(String address) {
    int[] bin = new int[address.length()];
    for (int i = 0; i < address.length(); i++) {
      bin[i] = address.codePointAt(i);
    }

    int fullBlockCount = (int) Math.floor(bin.length / FULL_ENCODED_BLOCK_SIZE);
    int lastBlockSize = (int) bin.length % FULL_ENCODED_BLOCK_SIZE;
    int lastBlockDecodedSize = ENCODED_BLOCK_SIZE.get(lastBlockSize);
    if (lastBlockDecodedSize < 0) {
      throw new IllegalArgumentException("Invalid encoded length");
    }

    int dataSize = fullBlockCount * FULL_BLOCK_SIZE + lastBlockDecodedSize;
    int[] data = new int[dataSize];
    for (int i = 0; i < fullBlockCount; i++) {
      data = decodeBlock(GenUtils.subarray(bin, i * FULL_ENCODED_BLOCK_SIZE, i * FULL_ENCODED_BLOCK_SIZE + FULL_ENCODED_BLOCK_SIZE), data, i * FULL_BLOCK_SIZE);
    }
    if (lastBlockSize > 0) {
      int[] subarray = GenUtils.subarray(bin, fullBlockCount * FULL_ENCODED_BLOCK_SIZE, fullBlockCount * FULL_ENCODED_BLOCK_SIZE + FULL_BLOCK_SIZE);
      data = decodeBlock(subarray, data, fullBlockCount * FULL_BLOCK_SIZE);
    }

    return binToHex(data);
  }

  private static int[] decodeBlock(int[] data, int[] buf, int index) {

    if (data.length < 1 || data.length > FULL_ENCODED_BLOCK_SIZE) {
      throw new RuntimeException("Invalid block length: " + data.length);
    }

    int resSize = ENCODED_BLOCK_SIZE.get(data.length);
    if (resSize <= 0) {
      throw new RuntimeException("Invalid block size");
    }
    BigDecimal resNum = BigDecimal.ZERO;
    BigDecimal order = BigDecimal.ONE;
    for (int i = data.length - 1; i >= 0; i--) {
      int digit = ALPHABET.indexOf(data[i]);
      if (digit < 0) {
        throw new RuntimeException("Invalid symbol");
      }
      BigDecimal product = order.multiply(new BigDecimal(digit)).add(resNum);
      // if product > UINT64_MAX
      if (product.compareTo(UINT64_MAX) > 0) {
        throw new RuntimeException("Overflow");
      }
      resNum = product;
      order = order.multiply(ALPHABET_SIZE);
    }
    if (resSize < FULL_BLOCK_SIZE && (new BigDecimal(2).pow(8 * resSize).compareTo(resNum) <= 0)) {
      throw new RuntimeException("Overflow 2");
    }

    int[] tmpBuf = uint64To8be(resNum, resSize);
    for (int j = 0; j < tmpBuf.length; j++) {
      buf[j + index] = tmpBuf[j];
    }

    return buf;
  }

  private static int[] uint64To8be(BigDecimal num, int size) {
    int[] res = new int[size];
    if (size < 1 || size > 8) {
      throw new RuntimeException("Invalid input length");
    }
    BigDecimal twopow8 = new BigDecimal(2).pow(8);
    for (int i = size - 1; i >= 0; i--) {
      res[i] = num.remainder(twopow8).intValue();
      num = num.divide(twopow8);
    }
    return res;
  }

  private static byte[] hexToBin(String hexStr) {
    if (hexStr == null || hexStr.length() % 2 != 0) {
      return null;
    }
    byte[] res = new byte[hexStr.length() / 2];
    for (int i = 0; i < hexStr.length() / 2; ++i) {
      res[i] = (byte)Integer.parseInt(hexStr.substring(i * 2, i * 2 + 2), 16);
    }
    return res;
  }

  private static String binToHex(int[] data) {
    StringBuilder builder = new StringBuilder();
    for (int i : data) {
      builder.append(String.format("%02x", i));
    }
    return builder.toString();
  }
}
