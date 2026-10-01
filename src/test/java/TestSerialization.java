

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.core.type.TypeReference;
import common.utils.JsonUtils;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import monero.common.MoneroRpcConnection;
import monero.common.SslOptions;
import monero.wallet.MoneroWalletRpc;
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

  @Test
  public void testConnectionSslEquality() {
    MoneroRpcConnection connection = new MoneroRpcConnection("https://localhost:18081", "user", "password").setProxyUri("127.0.0.1:9050");
    MoneroRpcConnection copy = new MoneroRpcConnection(connection);
    assertEquals(connection, copy);
    assertEquals(connection.hashCode(), copy.hashCode());
    copy.setSslVerify(false);
    assertNotEquals(connection, copy);
    assertNotEquals(copy, connection);
    connection.setSslVerify(false);
    assertEquals(connection, copy);
    assertEquals(connection.hashCode(), copy.hashCode());
  }

  @Test
  public void testWalletRpcSslOptions() {
    Map<String, Object> params = new HashMap<String, Object>();
    MoneroWalletRpc wallet = new MoneroWalletRpc(new MoneroRpcConnection("http://localhost:18082") {
      @Override
      public Map<String, Object> sendJsonRequest(String method, Object requestParams) {
        assertEquals("set_daemon", method);
        params.clear();
        params.putAll(JsonUtils.toMap(requestParams));
        return Collections.emptyMap();
      }
    });
    MoneroRpcConnection connection = new MoneroRpcConnection("https://localhost:18081");
    wallet.setDaemonConnection(connection);
    assertEquals(false, params.get("ssl_allow_any_cert"));
    wallet.setDaemonConnection(connection.setSslVerify(false));
    assertEquals(true, params.get("ssl_allow_any_cert"));
    assertEquals("autodetect", params.get("ssl_support"));
    wallet.setDaemonConnection(connection.setSslVerify(true));
    assertEquals(false, params.get("ssl_allow_any_cert"));

    // explicit options must not be weakened or mutated by the connection's setting
    connection.setSslVerify(false);
    SslOptions options = new SslOptions();
    wallet.setDaemonConnection(connection, false, options);
    assertNull(params.get("ssl_allow_any_cert"));
    assertEquals("autodetect", params.get("ssl_support"));
    assertTrue(wallet.getDaemonConnection().getSslVerify());
    assertNotSame(connection, wallet.getDaemonConnection());
    assertFalse(connection.getSslVerify());
    options.setCertificateAuthorityFile("ca.pem");
    options.setAllowedFingerprints(Arrays.asList("fingerprint"));
    wallet.setDaemonConnection(connection, false, options);
    assertNull(params.get("ssl_allow_any_cert"));
    assertEquals("ca.pem", params.get("ssl_ca_file"));
    assertEquals(options.getAllowedFingerprints(), params.get("ssl_allowed_fingerprints"));
    assertEquals("enabled", params.get("ssl_support")); // a ca file or fingerprints must be enforced
    assertNull(options.getAllowAnyCert());
    options.setAllowAnyCert(false);
    wallet.setDaemonConnection(connection, false, options);
    assertEquals(false, params.get("ssl_allow_any_cert"));
    assertTrue(wallet.getDaemonConnection().getSslVerify());
    assertFalse(connection.getSslVerify());
    wallet.setDaemonConnection(wallet.getDaemonConnection());
    assertEquals(false, params.get("ssl_allow_any_cert"));
    options.setAllowAnyCert(true);
    wallet.setDaemonConnection(connection.setSslVerify(true), false, options);
    assertEquals(true, params.get("ssl_allow_any_cert"));
    assertEquals("autodetect", params.get("ssl_support"));
    assertFalse(wallet.getDaemonConnection().getSslVerify());
    assertTrue(connection.getSslVerify());
    wallet.setDaemonConnection(wallet.getDaemonConnection());
    assertEquals(true, params.get("ssl_allow_any_cert"));
    wallet.setDaemonConnection((MoneroRpcConnection) null);
    assertEquals("placeholder", params.get("address"));
    assertNull(params.get("ssl_allow_any_cert"));
    assertNull(wallet.getDaemonConnection());
  }
}
