package monero.cpp_bridge;

import java.util.HashMap;
import java.util.Map;

/**
 * Collection of utilties bridged from Monero Core C++ to Java.
 */
public class MoneroCppUtils {
  
  static {
    //System.out.println(System.getProperty("java.library.path"));
    //System.load("/Users/Eric/git/monero-java-rpc/lib/monero-java.dylib");
    System.loadLibrary("monero");
  }
  
  public native static void sayHello();

  public native static int[] mapToBinary(Map<String, Object> map);
  
  public static Map<String, Object> binaryToMap(int[] bin) {
    Map<String, Object> map = new HashMap<String, Object>();
    binaryToMap(bin, map);
    return map;
  }
  
  public static String binaryBlocksToJson(int[] binBlocks) {
    throw new RuntimeException("Not implemented");
  }
  
  private native static void binaryToMap(int[] bin, Map<String, Object> map);
  
//private native static int[] jsonToBinary(String json);

}
