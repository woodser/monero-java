package monero.cpp_bridge;

import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.JsonUtils;

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

  public static int[] mapToBinary(Map<String, Object> map) {
    return jsonToBinary(JsonUtils.serialize(map));
  }
  
  public static Map<String, Object> binaryToMap(int[] bin) {
    return JsonUtils.deserialize(binaryToJson(bin), new TypeReference<Map<String, Object>>(){});
  }
  
  public static Map<String, Object> binaryBlocksToMap(int[] binBlocks) {
    return JsonUtils.deserialize(binaryBlocksToJson(binBlocks), new TypeReference<Map<String, Object>>(){});
  }
  
  private native static int[] jsonToBinary(String json);
  
  private native static String binaryToJson(int[] bin);
  
  private native static String binaryBlocksToJson(int[] binBlocks);
}
