package monero.cpp_bridge;

import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.JsonUtils;

/**
 * Collection of utilties bridged from Monero Core C++ to Java.
 */
public class MoneroCppUtils {
  
  public static void sayHello() {
    MoneroCppUtilsJni.sayHello();
  }

  public static byte[] mapToBinary(Map<String, Object> map) {
    return MoneroCppUtilsJni.jsonToBinary(JsonUtils.serialize(map));
  }
  
  public static Map<String, Object> binaryToMap(byte[] bin) {
    return JsonUtils.deserialize(MoneroCppUtilsJni.binaryToJson(bin), new TypeReference<Map<String, Object>>(){});
  }
  
  public static Map<String, Object> binaryBlocksToMap(byte[] binBlocks) {
    return JsonUtils.deserialize(MoneroCppUtilsJni.binaryBlocksToJson(binBlocks), new TypeReference<Map<String, Object>>(){});
  }
}
