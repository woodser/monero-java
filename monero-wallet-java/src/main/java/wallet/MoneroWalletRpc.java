package wallet;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.google.common.primitives.UnsignedInteger;

import types.HttpException;
import types.Pair;
import utils.JsonUtils;
import utils.StreamUtils;
import utils.UnsignedIntegerDeserializer;

/**
 * Implements a MoneroWallet backed by a monero wallet RPC endpoint.
 * 
 * @author woodser
 */
public class MoneroWalletRpc implements MoneroWallet {
  
  // json field names that map to unsigned integers
  private static Set<String> UNSIGNED_INTEGERS = new HashSet<String>(Arrays.asList("balance", "unlocked_balance"));
  
  // customer mapper to deserialize unsigned integers
  private static ObjectMapper MAPPER;
  static {
    MAPPER = new ObjectMapper();
    MAPPER.setSerializationInclusion(Include.NON_NULL);
    SimpleModule module = new SimpleModule();
    module.addDeserializer(Map.class, new UnsignedIntegerDeserializer(UNSIGNED_INTEGERS));
    MAPPER.registerModule(module);
  }
  
  // instance variables
  private String host;
  private int port;
  private URI uri;
  private HttpClient client;
  
  public MoneroWalletRpc(String endpoint) {
    this(parseUri(endpoint));
  }
  
  public MoneroWalletRpc(URI uri) {
    this.uri = uri;
    this.host = uri.getHost();
    this.port = uri.getPort();
    this.client = HttpClients.createDefault();
  }
  
  public MoneroWalletRpc(String host, int port) throws URISyntaxException {
    this.host = host;
    this.port = port;
    this.uri = new URI("http", null, host, port, "/json_rpc", null, null);
    this.client = HttpClients.createDefault();
  }

  public String getHost() {
    return host;
  }

  public int getPort() {
    return port;
  }
  
  public URI getUri() {
    return uri;
  }

  public UnsignedInteger getBalance() {
    return getBalances().getFirst();
  }
  
  public UnsignedInteger getUnlockedBalance() {
    return getBalances().getSecond();
  }

  public MoneroAddress getAddress() {
    Map<String, Object> respMap = sendRpcRequest("getaddress", null);
    validateRpcResponse(respMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return new MoneroAddress((String) resultMap.get("address"));
  }

  public MoneroAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(MoneroAddress address, UnsignedInteger amount, UnsignedInteger fee, int mixin, int unlockTime) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(MoneroPayment payment) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(Set<MoneroPayment> payments, UnsignedInteger fee, int mixin, int unlockTime) {
    throw new RuntimeException("Not yet implemented.");
  }

  public Set<MoneroTransaction> sweepDust() {
    throw new RuntimeException("Not yet implemented.");
  }

  public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight) {
    throw new RuntimeException("Not yet implemented.");
  }

  public String getSpendKey() {
    throw new RuntimeException("Not yet implemented.");
  }

  public String getViewKey() {
    throw new RuntimeException("Not yet implemented.");
  }

  public void save() {
    throw new RuntimeException("Not yet implemented.");
  }

  public URI getUri(MoneroUri uri) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroUri parseUri(URI uri) {
    throw new RuntimeException("Not yet implemented.");
  }
	
  private static URI parseUri(String endpoint) {
    try {
      return new URI(endpoint);
    } catch (Exception e) {
      throw new MoneroException(e);
    }
  }
  
  private static void validateHttpResponse(HttpResponse resp) {
    int code = resp.getStatusLine().getStatusCode();
    if (code < 200 || code > 299) {
      String content = null;
      try {
        content = StreamUtils.streamToString(resp.getEntity().getContent());
      } catch (Exception e) {
        // could not get content
      }
      throw new HttpException(code, resp.getStatusLine().getReasonPhrase() + (content != null ? (": " + content) : ""));
    }
  }
  
  @SuppressWarnings("unchecked")
  private Pair<UnsignedInteger, UnsignedInteger> getBalances() {
    Map<String, Object> respMap = sendRpcRequest("getbalance", null);
    validateRpcResponse(respMap);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return new Pair<UnsignedInteger, UnsignedInteger>((UnsignedInteger) resultMap.get("balance"), (UnsignedInteger) resultMap.get("unlocked_balance"));
  }
  
  
  /**
   * Sends a request to the RPC API.
   * 
   * @param method specifies the method to request
   * @param params specifies input parameters
   * @return Map<String, Object> is the RPC API response as a map
   */
  private Map<String, Object> sendRpcRequest(String method, Map<String, Object> params) {
    try {
      
      // build request body
      Map<String, String> body = new HashMap<String, String>();
      body.put("jsonrpc", "2.0");
      body.put("id", "0");
      body.put("method", method);
      if (params != null) body.put("params", JsonUtils.serialize(params));
      
      // send http request and validate response
      HttpPost post = new HttpPost(uri);
      HttpEntity entity = new StringEntity(JsonUtils.serialize(body));
      post.setEntity(entity);
      HttpResponse resp = client.execute(post);
      validateHttpResponse(resp);
      
      // deserialize response
      Map<String, Object> respMap = JsonUtils.toMap(MAPPER, StreamUtils.streamToString(resp.getEntity().getContent()));
      EntityUtils.consume(resp.getEntity());
      return respMap;
    } catch (HttpException e1) {
      throw e1;
    } catch (Exception e2) {
      throw new MoneroException(e2);
    }
  }
  
  @SuppressWarnings("unchecked")
  private static void validateRpcResponse(Map<String, Object> respMap) {
    Map<String, Object> error = (Map<String, Object>) respMap.get("error");
    if (error == null) return;
    int code = (Integer) error.get("code");
    String message = (String) error.get("message");
    throw new MoneroExceptionRpc(code, message);
  }
}
