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
import utils.FieldDeserializer;
import utils.JsonUtils;
import utils.MoneroUtils;
import utils.StreamUtils;

/**
 * Implements a MoneroWallet backed by a monero wallet RPC endpoint.
 * 
 * @author woodser
 */
public class MoneroWalletRpc implements MoneroWallet {
  
  // json field names that map to unsigned integers
  private static Set<String> UNSIGNED_INTEGERS = new HashSet<String>(Arrays.asList("balance", "unlocked_balance", "amount"));
  
  // customer mapper to deserialize unsigned integers
  private static ObjectMapper MAPPER;
  static {
    MAPPER = new ObjectMapper();
    MAPPER.setSerializationInclusion(Include.NON_NULL);
    SimpleModule module = new SimpleModule();
    Map<String, Class<?>> fieldClasses = new HashMap<String, Class<?>>();
    for (String field : UNSIGNED_INTEGERS) fieldClasses.put(field, UnsignedInteger.class);
    module.addDeserializer(Map.class, new FieldDeserializer(fieldClasses));
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

  public String getRpcHost() {
    return host;
  }

  public int getRpcPort() {
    return port;
  }
  
  public URI getRpcUri() {
    return uri;
  }
  
  public int getHeight() {
    Map<String, Object> respMap = sendRpcRequest("getheight", null);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (Integer) resultMap.get("height");
  }

  public UnsignedInteger getBalance() {
    return getBalances().getFirst();
  }
  
  public UnsignedInteger getUnlockedBalance() {
    return getBalances().getSecond();
  }

  public MoneroAddress getStandardAddress() {
    Map<String, Object> respMap = sendRpcRequest("getaddress", null);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    String standardAddress = (String) resultMap.get("address");
    MoneroAddress address = new MoneroAddress(standardAddress);
    MoneroUtils.validateAddress(address);
    return address;
  }

  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    if (paymentId != null) paramMap.put("payment_id", paymentId);
    Map<String, Object> respMap = sendRpcRequest("make_integrated_address", paramMap);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    paymentId = (String) resultMap.get("payment_id");
    String integratedAddress = (String) resultMap.get("integrated_address");
    MoneroIntegratedAddress address = new MoneroIntegratedAddress(getStandardAddress().getStandardAddress(), paymentId, integratedAddress);
    MoneroUtils.validateAddress(address);
    return address;
  }  

  public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress) {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("integrated_address", integratedAddress);
    Map<String, Object> respMap = sendRpcRequest("split_integrated_address", paramMap);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroIntegratedAddress address = new MoneroIntegratedAddress((String) resultMap.get("standard_address"), (String) resultMap.get("payment_id"), integratedAddress);
    MoneroUtils.validateAddress(address);
    return address;
  }

  public MoneroTransaction sendTransaction(String address, UnsignedInteger amount, UnsignedInteger fee, int mixin, int unlockTime) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(MoneroPayment payment, UnsignedInteger fee, int mixin, int unlockTime) {
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

  public String getMnemonicSeed() {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "mnemonic");
    Map<String, Object> respMap = sendRpcRequest("query_key", paramMap);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }

  public String getViewKey() {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "view_key");
    Map<String, Object> respMap = sendRpcRequest("query_key", paramMap);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }

  public URI toUri(MoneroUri uri) {
    if (uri == null) throw new MoneroWalletException("Given Monero URI is null");
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("address", uri.getAddress());
    paramMap.put("amount", uri.getAmount() == null ? null : uri.getAmount());
    paramMap.put("payment_id", uri.getPaymentId());
    paramMap.put("recipient_name", uri.getRecipientName());
    paramMap.put("tx_description", uri.getTxDescription());
    Map<String, Object> respMap = sendRpcRequest("make_uri", paramMap);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return parseUri((String) resultMap.get("uri"));
  }

  public MoneroUri fromUri(URI uri) {
    if (uri == null) throw new MoneroWalletException("Given URI is null");
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("uri", uri.toString());
    Map<String, Object> respMap = sendRpcRequest("parse_uri", paramMap);
    @SuppressWarnings("unchecked") Map<String, Object> resultMap = (Map<String, Object>) ((Map<String, Object>) respMap.get("result")).get("uri");
    MoneroUri mUri = new MoneroUri();
    mUri.setAddress((String) resultMap.get("address"));
    if ("".equals(mUri.getAddress())) mUri.setAddress(null);
    mUri.setAmount((UnsignedInteger) resultMap.get("amount"));
    mUri.setPaymentId((String) resultMap.get("payment_id"));
    if ("".equals(mUri.getPaymentId())) mUri.setPaymentId(null);
    mUri.setRecipientName((String) resultMap.get("recipient_name"));
    if ("".equals(mUri.getRecipientName())) mUri.setRecipientName(null);
    mUri.setTxDescription((String) resultMap.get("tx_description"));
    if ("".equals(mUri.getTxDescription())) mUri.setTxDescription(null);
    return mUri;
  }

  public void saveBlockchain() {
    sendRpcRequest("store", null);
  }

  public void stopWallet() {
    sendRpcRequest("stop_wallet", null);
  }
	
  private static URI parseUri(String endpoint) {
    try {
      return new URI(endpoint);
    } catch (Exception e) {
      throw new MoneroWalletException(e);
    }
  }
  
  @SuppressWarnings("unchecked")
  private Pair<UnsignedInteger, UnsignedInteger> getBalances() {
    Map<String, Object> respMap = sendRpcRequest("getbalance", null);
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
    
    // send http request
    try {
      
      // build request body
      Map<String, Object> body = new HashMap<String, Object>();
      body.put("jsonrpc", "2.0");
      body.put("id", "0");
      body.put("method", method);
      if (params != null) body.put("params", params);
      
      // send http request and validate response
      HttpPost post = new HttpPost(uri);
      HttpEntity entity = new StringEntity(JsonUtils.serialize(body));
      post.setEntity(entity);
      HttpResponse resp = client.execute(post);
      validateHttpResponse(resp);
      
      // deserialize response
      Map<String, Object> respMap = JsonUtils.toMap(MAPPER, StreamUtils.streamToString(resp.getEntity().getContent()));
      EntityUtils.consume(resp.getEntity());
      
      // check RPC response for errors
      validateRpcResponse(respMap);
      return respMap;
    } catch (HttpException e1) {
      throw e1;
    } catch (MoneroRpcException e2) {
      throw e2;
    } catch (Exception e3) {
      throw new MoneroWalletException(e3);
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
  private static void validateRpcResponse(Map<String, Object> respMap) {
    Map<String, Object> error = (Map<String, Object>) respMap.get("error");
    if (error == null) return;
    int code = (Integer) error.get("code");
    String message = (String) error.get("message");
    throw new MoneroRpcException(code, message);
  }
}
