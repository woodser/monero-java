package test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.fasterxml.jackson.core.type.TypeReference;
import common.utils.JsonUtils;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import monero.common.MoneroRpcConnection;
import org.junit.jupiter.api.Test;

/**
 * Tests serialization and deserialization.
 */
public class TestSerialization {

  @Test
  public void testSimpleSerialization() {
    
    // construct a map to serialize and deserialize
    Map<String, Object> map1 = new HashMap<String, Object>();
    map1.put("string", "Hello");
    map1.put("amount", BigInteger.valueOf(Long.valueOf("140000000000")));
    map1.put("integer", BigInteger.valueOf(1));
    map1.put("float", 1.0);
    map1.put("null", null);
    
    // serialize
    String json = JsonUtils.serialize(MoneroRpcConnection.MAPPER, map1);
    
    // deserialize
    Map<String, Object> map2 = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, json, new TypeReference<Map<String, Object>>(){});
    Object amt = map2.get("amount");
    assertEquals(BigInteger.class, amt.getClass());
    map1.remove("null");  // nulls should be removed during serialization
    assertEquals(map1, map2);
  }
  
  @Test
  public void testListSerialization() {
    Map<String, Object> map = new HashMap<String, Object>();
    List<String> strs = new ArrayList<String>();
    for (int i = 0; i < 5; i++) strs.add("hello");
    map.put("strings", strs);
    String json = JsonUtils.serialize(map);
    Map<String, Object> deserialized = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, json, new TypeReference<Map<String, Object>>(){});
    assertEquals(map, deserialized);
  }
  
  @Test
  public void testListSerializationWithCustomTypes() {
    
    // construct a map to serialize and deserialize
    Map<String, Object> map1 = new HashMap<String, Object>();
    map1.put("string", "Hello");
    List<String> txHashes = new ArrayList<String>();
    List<BigInteger> amounts = new ArrayList<BigInteger>();
    for (int i = 0; i < 5; i++) {
      txHashes.add("c5c389846e701c27aaf1f7ab8b9dc457b471fcea5bc9710e8020d51275afbc54");
      amounts.add(new BigInteger("140000000000"));
    }
    map1.put("fee_list", amounts);
    map1.put("tx_hash_list", txHashes);
    map1.put("integer", BigInteger.valueOf(1));
    map1.put("float", 1.0);
    map1.put("null", null);
    
    // serialize
    String json = JsonUtils.serialize(map1);
    
    // deserialize
    Map<String, Object> map2 = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, json, new TypeReference<Map<String, Object>>(){});
    map1.remove("null");  // nulls should be removed during serialization
    assertEquals(map1, map2);
  }
}