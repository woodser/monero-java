package monero.common;

import com.fasterxml.jackson.core.type.TypeReference;
import common.utils.GenUtils;
import common.utils.JsonUtils;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroTx;
import monero.wallet.model.MoneroAddressType;
import monero.wallet.model.MoneroDecodedAddress;
import monero.wallet.model.MoneroIntegratedAddress;
import org.apache.commons.codec.binary.Hex;
import org.bouncycastle.jcajce.provider.digest.Keccak;

/**
 * Collection of Monero utilities.
 */
public class MoneroUtils {

  /**
   * Get the version of the monero-java library.
   * 
   * @return the version of this monero-java library
   */
  public static String getVersion() {
    return "0.8.21";
  }

  // try to load jni bindings
  static {
    tryLoadNativeLibrary();
  }

  /**
   * Try to load the native library if not already loaded.
   */
  public static void tryLoadNativeLibrary() {
    if (!MoneroUtils.isNativeLibraryLoaded()) {
      try { MoneroUtils.loadNativeLibrary(); }
      catch (Exception | UnsatisfiedLinkError e) { }
    }
  }

  public static void loadNativeLibrary() {

    // try to load from java library path
    try {
      String libName = (System.getProperty("os.name").toLowerCase().contains("windows") ? "lib" : "") + "monero-java";
      System.loadLibrary(libName);
    } catch (Exception | UnsatisfiedLinkError e) {
      // ignore error
    }

    // try to load from resources (e.g. in jar)
    Path tempDir = null;
    String[] libraryFiles = null;
    try {

      // get system info
      String osName = System.getProperty("os.name").toLowerCase();
      String osArch = System.getProperty("os.arch").toLowerCase();

      // get library file names and paths
      String libraryPath = "/";
      String libraryCppFile = null;
      String libraryJavaFile = null;
      if (osName.contains("windows")) {
        libraryPath += "windows/";
        libraryFiles = new String[] { "libmonero-cpp.dll", "libmonero-cpp.dll.a", "libmonero-java.dll", "libmonero-java.dll.a" };
        libraryCppFile = "libmonero-cpp.dll";
        libraryJavaFile = "libmonero-java.dll";
      } else if (osName.contains("linux")) {
        libraryPath += osArch.contains("aarch64") ? "linux-arm64/" : "linux-x86_64/";
        libraryFiles = new String[] { "libmonero-cpp.so", "libmonero-java.so" };
        libraryCppFile = "libmonero-cpp.so";
        libraryJavaFile = "libmonero-java.so";
      } else if (osName.contains("mac")) {
        libraryPath += osArch.contains("aarch64") ? "mac-arm64/" : "mac-x86_64/";
        libraryFiles = new String[] { "libmonero-cpp.dylib", "libmonero-java.dylib" };
        libraryCppFile = "libmonero-cpp.dylib";
        libraryJavaFile = "libmonero-java.dylib";
      } else {
        throw new UnsupportedOperationException("Unsupported operating system: " + osName);
      }

      // copy all library files to temp directory
      tempDir = Files.createTempDirectory("libmonero");
      for (String libraryFile : libraryFiles) {
        try (InputStream inputStream = MoneroUtils.class.getResourceAsStream(libraryPath + libraryFile); OutputStream outputStream = Files.newOutputStream(tempDir.resolve(libraryFile))) {
          byte[] buffer = new byte[1024];
          int bytesRead;
          while ((bytesRead = inputStream.read(buffer)) != -1) {
            outputStream.write(buffer, 0, bytesRead);
          }
        }
      }

      // load native libraries
      System.load(tempDir.resolve(libraryCppFile).toString());
      System.load(tempDir.resolve(libraryJavaFile).toString());
    } catch (Exception e) {
      throw new MoneroError(e);
    } finally {

      // try to delete temporary files and folder
      try {
        if (tempDir != null) {
          for (String libraryFile : libraryFiles) Files.delete(tempDir.resolve(libraryFile));
          Files.delete(tempDir);
        }
      } catch (Exception e) {
        //e.printStackTrace();
      }
    }
  }
  
  /**
   * Indicates if the native JNI library is loaded.
   * 
   * @return true if the native library is loaded, false otherwise
   */
  public static boolean isNativeLibraryLoaded() {
    try {
      mapToBinary(new HashMap<>());
      return true;
    } catch (Exception | UnsatisfiedLinkError e) {
      return false;
    }
  }
  
  public static final int RING_SIZE = 12; // network-enforced ring size
  
  private static int LOG_LEVEL = 0;
  private static long AU_PER_XMR = 1000000000000l;
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
   * Indicates if a wallet keys file exists at the given path.
   * 
   * @param path is the path with wallet name to check for existence
   * @return true if a wallet keys file exists at the given path, false otherwise
   */
  public static boolean walletExists(String path) {
    String basePath = path.lastIndexOf('.') > path.lastIndexOf(File.separatorChar) ? path.substring(0, path.lastIndexOf('.')) : path;
    File keysFile = new File(basePath + ".keys");
    return keysFile.exists() && keysFile.isFile();
  }
  
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
  
  /**
   * Indicates if a private view key is valid.
   * 
   * @param privateViewKey is the private view key to validate
   * @return true if the private view key is valid, false otherwise
   */
  public static boolean isValidPrivateViewKey(String privateViewKey) {
    try {
      validatePrivateViewKey(privateViewKey);
      return true;
    } catch (Exception e) {
      return false;
    }
  }
  
  /**
   * Indicates if a public view key is valid.
   * 
   * @param publicViewKey is the public view key to validate
   * @return true if the public view key is valid, false otherwise
   */
  public static boolean isValidPublicViewKey(String publicViewKey) {
    try {
      validatePublicViewKey(publicViewKey);
      return true;
    } catch (Exception e) {
      return false;
    }
  }
  
  /**
   * Indicates if a private spend key is valid.
   * 
   * @param privateSpendKey is the private spend key to validate
   * @return true if the private spend key is valid, false otherwise
   */
  public static boolean isValidPrivateSpendKey(String privateSpendKey) {
    try {
      validatePrivateSpendKey(privateSpendKey);
      return true;
    } catch (Exception e) {
      return false;
    }
  }
  
  /**
   * Indicates if a public spend key is valid.
   * 
   * @param publicSpendKey is the public spend key to validate
   * @return true if the public spend key is valid, false otherwise
   */
  public static boolean isValidPublicSpendKey(String publicSpendKey) {
    try {
      validatePublicSpendKey(publicSpendKey);
      return true;
    } catch (Exception e) {
      return false;
    }
  }
  
  /**
   * Validate a private view key.
   * 
   * @param privateViewKey is the private view key to validate
   * @throws MoneroError if the given private view key is invalid
   */
  public static void validatePrivateViewKey(String privateViewKey) {
    if (!isHex64(privateViewKey)) throw new MoneroError("private view key expected to be 64 hex characters");
  }
  
  /**
   * Validate a public view key.
   * 
   * @param publicViewKey is the public view key to validate
   * @throws MoneroError if the given public view key is invalid
   */
  public static void validatePublicViewKey(String publicViewKey) {
    if (!isHex64(publicViewKey)) throw new MoneroError("public view key expected to be 64 hex characters");
  }
  
  /**
   * Validate a private spend key.
   * 
   * @param privateSpendKey is the private spend key to validate
   * @throws MoneroError if the given private spend key is invalid
   */
  public static void validatePrivateSpendKey(String privateSpendKey) {
    if (!isHex64(privateSpendKey)) throw new MoneroError("private spend key expected to be 64 hex characters");
  }
  
  /**
   * Validate a public spend key.
   * 
   * @param publicSpendKey is the public spend key to validate
   * @throws MoneroError if the given public spend key is invalid
   */
  public static void validatePublicSpendKey(String publicSpendKey) {
    if (!isHex64(publicSpendKey)) throw new MoneroError("public spend key expected to be 64 hex characters");
  }
  
  /**
   * Get an integrated address.
   * 
   * @param networkType is the network type of the integrated address
   * @param standardAddress is the address to derive the integrated address from
   * @param paymentId optionally specifies the integrated address's payment id (defaults to random payment id)
   * @return the integrated address
   */
  public static MoneroIntegratedAddress getIntegratedAddress(MoneroNetworkType networkType, String standardAddress, String paymentId) {
    try {
      return JsonUtils.deserialize(getIntegratedAddressJni(networkType.ordinal(), standardAddress, paymentId == null ? "" : paymentId), MoneroIntegratedAddress.class);
    } catch (Exception err) {
      throw new MoneroError(err.getMessage());
    }
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
   * Determine if the given address is valid.
   * 
   * @param address is the address to validate
   * @param networkType is the address's network type
   * @return true if the address is valid, false otherwise
   */
  public static boolean isValidAddress(String address, MoneroNetworkType networkType) {
      try {
        validateAddress(address, networkType);
        return true;
      } catch (MoneroError e) {
        return false;
      }
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
   * @param uri is the string to convert to a URI
   * @return URI is the initialized object from the string endpoint
   */
  public static URI parseUri(String uri) {
    if (uri != null && uri.length() > 0 && !uri.toLowerCase().matches("^\\w+://.+")) uri = "http://" + uri; // assume http if protocol not given
    try {
      return new URI(uri);
    } catch (Exception e) {
      throw new MoneroError(e);
    }
  }

  public static void validateHex(String str) {
    if (!str.matches("^([0-9A-Fa-f]{2})+$")) throw new MoneroError("Invalid hex: " + str);
  }

  public static void validateBase58(String standardAddress) {
    for (char c : standardAddress.toCharArray()) {
      if (!CHARS.contains(c)) throw new MoneroError("Invalid Base58 " + standardAddress);
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
   * @param <T> is a MoneroTx or subclasses like MoneroTxWallet
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
  
  /**
   * Log a message.
   *
   * @param level log level of the message
   * @param msg message to log
   */
  public static void log(int level, String msg) {
    GenUtils.assertTrue("Log level must be an integer >= 0", level >= 0);
    if (MoneroUtils.LOG_LEVEL >= level) System.out.println(msg);
  }
  
  /**
   * Set the library's log level with 0 being least verbose.
   *
   * @param level - the library's log level
   */
  public static void setLogLevel(int level) {
    GenUtils.assertTrue("Log level must be an integer >= 0", level >= 0);
    MoneroUtils.LOG_LEVEL = level;
    if (isNativeLibraryLoaded()) setLogLevelJni(level);
  }
  
  /**
   * Get the library's log level.
   *
   * @return the library's log level
   */
  public static int getLogLevel() {
    return MoneroUtils.LOG_LEVEL;
  }
  
  /**
   * Initialize JNI logging.
   * 
   * @param path the path to write logs to
   * @param console specifies whether or not to write to the console
   */
  public static void configureNativeLogging(String path, boolean console) {
    configureLoggingJni(path, console);
  }
  
  /**
   * Convert XMR to atomic units.
   * 
   * @param amountXmr amount in XMR to convert to atomic units
   * @return amount in atomic units
   */
  public static BigInteger xmrToAtomicUnits(double amountXmr) {
    double decimalDivisor = 1;
    String amountXmrStr = "" + amountXmr;
    int decimalIdx = amountXmrStr.indexOf('.');
    if (decimalIdx > -1) {
      decimalDivisor = Math.pow(10, amountXmrStr.length() - decimalIdx - 1);
      amountXmrStr = amountXmrStr.substring(0, decimalIdx) + amountXmrStr.substring(decimalIdx + 1);
    }
    return new BigInteger(amountXmrStr).multiply(BigInteger.valueOf(MoneroUtils.AU_PER_XMR)).divide(BigInteger.valueOf((long) decimalDivisor));
  }
  
  /**
   * Convert atomic units to XMR.
   * 
   * @param amountAtomicUnits amount in atomic units to convert to XMR
   * @return amount in XMR
   */
  public static double atomicUnitsToXmr(BigInteger amountAtomicUnits) {
    BigInteger[] quotientAndRemainder = amountAtomicUnits.divideAndRemainder(BigInteger.valueOf(AU_PER_XMR));
    return quotientAndRemainder[0].doubleValue() + quotientAndRemainder[1].doubleValue() / MoneroUtils.AU_PER_XMR;
  }
  
  // ---------------------------- NATIVE BINDINGS -----------------------------
  
  private native static String getIntegratedAddressJni(int networkType, String standardAddress, String paymentId);
  private native static byte[] jsonToBinaryJni(String json);
  private native static String binaryToJsonJni(byte[] bin);
  private native static String binaryBlocksToJsonJni(byte[] binBlocks);
  private native static void configureLoggingJni(String path, boolean console);
  private native static void setLogLevelJni(int level);
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  private static boolean isHex64(String str) {
    return str != null && str.length() == 64 && GenUtils.isHex(str);
  }

  private static boolean isValidAddressHash(String decodedAddrStr) {
    String checksumCheck = decodedAddrStr.substring(decodedAddrStr.length() - 8);
    String withoutChecksumStr = decodedAddrStr.substring(0, decodedAddrStr.length() - 8);
    byte[] withoutChecksumBytes = hexToBin(withoutChecksumStr);

    Keccak.Digest256 digest256 = new Keccak.Digest256();
    byte[] hashbytes = digest256.digest(withoutChecksumBytes);
    String encodedStr = new String(Hex.encodeHex(hashbytes));

    String hashChecksum = encodedStr.substring(0, 8);
    return hashChecksum != null && hashChecksum.equals(checksumCheck);
  }
  
  private static String decodeAddressToHex(String address) {
    int[] bin = new int[address.length()];
    for (int i = 0; i < address.length(); i++) {
      bin[i] = address.codePointAt(i);
    }

    int fullBlockCount = (int) Math.floor(bin.length / FULL_ENCODED_BLOCK_SIZE);
    int lastBlockSize = bin.length % FULL_ENCODED_BLOCK_SIZE;
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
