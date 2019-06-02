package monero.cpp_bridge;

/**
 * JNI calls for the cpp utilities.
 * 
 * These are separated because otherwise JNI header file cannot be
 * automatically generated without pulling in maven dependencies. // TODO: retry?
 */
public class MoneroCppUtilsNative {
  
  static {
    System.loadLibrary("monero-java");
  }

  public native static byte[] jsonToBinary(String json);
  
  public native static String binaryToJson(byte[] bin);
  
  public native static String binaryBlocksToJson(byte[] binBlocks);
}
