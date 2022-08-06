package monero.common;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import common.utils.JsonUtils;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import org.apache.hc.client5.http.auth.AuthScope;
import org.apache.hc.client5.http.auth.UsernamePasswordCredentials;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.auth.BasicCredentialsProvider;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.io.entity.ByteArrayEntity;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.util.Timeout;

/**
 * Maintains a connection and sends requests to a Monero RPC API.
 * 
 * TODO: refactor MoneroRpcConnection extends MoneroConnection?
 */
public class MoneroRpcConnection {

  // custom mapper to deserialize integers to BigIntegers
  public static ObjectMapper MAPPER;
  static {
    MAPPER = new ObjectMapper();
    MAPPER.setSerializationInclusion(Include.NON_NULL);
    MAPPER.configure(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS, true);
  }

  // instance variables
  private CloseableHttpClient client;
  private String uri;
  private String username;
  private String password;
  private String zmqUri;
  private int priority = 0;
  private Boolean isOnline;
  private Boolean isAuthenticated;
  private Long responseTime;
  
  private Map<String, Object> attributes = new HashMap<String, Object>();
  
  public MoneroRpcConnection(URI uri) {
    this(uri, null, null, null);
  }
  
  public MoneroRpcConnection(String uri) {
    this(uri, null, null);
  }
  
  public MoneroRpcConnection(String uri, String username, String password) {
    this(uri == null ? null : MoneroUtils.parseUri(uri), username, password, null);
  }

  public MoneroRpcConnection(String uri, String username, String password, String zmqUri) {
    this(uri == null ? null : MoneroUtils.parseUri(uri), username, password, zmqUri == null ? null : MoneroUtils.parseUri(zmqUri));
  }
  
  public MoneroRpcConnection(URI uri, String username, String password) {
    this(uri, username, password, null);
  }
  
  public MoneroRpcConnection(URI uri, String username, String password, URI zmqUri) {
    this.uri = uri == null ? null : MoneroUtils.parseUri(uri.toString()).toString();
    this.setCredentials(username, password);;
  }
  
  public MoneroRpcConnection(MoneroRpcConnection connection) {
    this(connection.uri, connection.username, connection.password, connection.zmqUri);
    this.priority = connection.priority;
    this.isOnline = connection.isOnline;
    this.isAuthenticated = connection.isAuthenticated;
    this.responseTime = connection.responseTime;
  }
  
  public MoneroRpcConnection setCredentials(String username, String password) {
    try { if (this.client != null) this.client.close(); }
    catch (IOException e) { throw new MoneroError(e); }
    if ("".equals(username)) username = null;
    if ("".equals(password)) password = null;
    if (username != null || password != null) {
      if (username == null) throw new MoneroError("username cannot be empty because password is not empty");
      if (password == null) throw new MoneroError("password cannot be empty because username is not empty");
      URI uriObj;
      try { uriObj = new URI(uri); }
      catch (URISyntaxException e) { throw new MoneroError(e); }
      BasicCredentialsProvider creds = new BasicCredentialsProvider();
      creds.setCredentials(new AuthScope(uriObj.getHost(), uriObj.getPort()), new UsernamePasswordCredentials(username, password.toCharArray()));
      this.client = HttpClients.custom().setDefaultCredentialsProvider(creds).build();
    } else {
      this.client = HttpClients.createDefault();
    }
    if (!Objects.equals(this.username, username) || !Objects.equals(this.password, password)) {
      isOnline = null;
      isAuthenticated = null;
    }
    this.username = username;
    this.password = password;
    return this;
  }
  
  public String getUri() {
    return uri;
  }
  
  public String getUsername() {
    return username;
  }
  
  public String getPassword() {
    return password;
  }
  
  
  public String getZmqUri() {
    return zmqUri;
  }
  
  public MoneroRpcConnection setZmqUri(String zmqUri) {
    this.zmqUri = zmqUri;
    return this;
  }
  
  public int getPriority() {
    return priority;
  }

  /**
   * Set the connection's priority relative to other connections. Priority 1 is highest,
   * then priority 2, etc. The default priority of 0 is lowest priority.
   * 
   * @param priority is the connection priority (default 0)
   * @return this connection
   */
  public MoneroRpcConnection setPriority(int priority) {
    if (!(priority >= 0)) throw new MoneroError("Priority must be >= 0");
    this.priority = priority;
    return this;
  }
  
  public MoneroRpcConnection setAttribute(String key, Object value) {
    attributes.put(key, value);
    return this;
  }
  
  public Object getAttribute(String key) {
    return attributes.get(key);
  }
  
  /**
   * Check the connection to update online, authentication, and response time status.
   * 
   * @param timeoutInMs the maximum response time before considered offline
   * @return true if there is a change in status, false otherwise
   */
  public boolean checkConnection(long timeoutInMs) {
    Boolean isOnlineBefore = isOnline;
    Boolean isAuthenticatedBefore = isAuthenticated;
    long startTime = System.currentTimeMillis();
    try {
      sendJsonRequest("get_version", null, timeoutInMs);
      isOnline = true;
      isAuthenticated = true;
    } catch (Exception e) {
      if (e instanceof MoneroRpcError && ((MoneroRpcError) e).getCode() == 401) {
        isOnline = true;
        isAuthenticated = false;
      } else {
        isOnline = false;
        isAuthenticated = null;
        responseTime = null;
      }
    }
    if (isOnline) responseTime = System.currentTimeMillis() - startTime;
    return isOnlineBefore != isOnline || isAuthenticatedBefore != isAuthenticated;
  }
  
  /**
   * Indicates if the connection is connected according to the last call to checkConnection().<br><br>
   * 
   * Note: must call checkConnection() manually unless using MoneroConnectionManager.
   * 
   * @return true or false to indicate if connected, or null if checkConnection() has not been called
   */
  public Boolean isConnected() {
    return isOnline == null ? null : isOnline && !Boolean.FALSE.equals(isAuthenticated);
  }
  
  /**
   * Indicates if the connection is online according to the last call to checkConnection().<br><br>
   * 
   * Note: must call checkConnection() manually unless using MoneroConnectionManager.
   * 
   * @return true or false to indicate if online, or null if checkConnection() has not been called
   */
  public Boolean isOnline() {
    return isOnline;
  }

  /**
   * Indicates if the connection is authenticated according to the last call to checkConnection().<br><br>
   * 
   * Note: must call checkConnection() manually unless using MoneroConnectionManager.
   * 
   * @return true if authenticated or no authentication, false if not authenticated, or null if checkConnection() has not been called
   */
  public Boolean isAuthenticated() {
    return isAuthenticated;
  }

  /**
   * Get the response time of the last call to checkConnection().<br><br>
   * 
   * Note: must call checkConnection() manually unless using MoneroConnectionManager.
   * 
   * @return the response time of the last call to checkConnection() or null if checkConnection() has not been called
   */
  public Long getResponseTime() {
    return responseTime;
  }
  
  /**
   * Send a request to the RPC API.
   * 
   * @param method specifies the method to request
   * @return the RPC API response as a map
   */
  public Map<String, Object> sendJsonRequest(String method) {
    return sendJsonRequest(method, (Map<String, Object>) null);
  }
  
  /**
   * Send a request to the RPC API.
   * 
   * @param method is the method to request
   * @param params are the request's input parameters (supports &lt;Map&lt;String, Object&gt;, List&lt;Object&gt;&lt;/code&gt;, String, etc)
   * @return the RPC API response as a map
   */
  public Map<String, Object> sendJsonRequest(String method, Object params) {
    return sendJsonRequest(method, params, null);
  }
  
  /**
   * Send a request to the RPC API.
   * 
   * @param method is the method to request
   * @param params are the request's input parameters (supports &lt;Map&lt;String, Object&gt;, List&lt;Object&gt;&lt;/code&gt;, String, etc)
   * @param timeoutInMs is the request timeout in milliseconds
   * @return the RPC API response as a map
   */
  public Map<String, Object> sendJsonRequest(String method, Object params, Long timeoutInMs) {
    CloseableHttpResponse resp = null;
    try {

      // build request body
      Map<String, Object> body = new HashMap<String, Object>();
      body.put("jsonrpc", "2.0");
      body.put("id", "0");
      body.put("method", method);
      if (params != null) body.put("params", params);
      if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending json request with method '" + method + "' and body: " + JsonUtils.serialize(body));

      // send http request
      HttpPost post = new HttpPost(uri.toString() + "/json_rpc");
      post.setConfig(getTimeoutConfig(timeoutInMs));
      HttpEntity entity = new StringEntity(JsonUtils.serialize(body));
      post.setEntity(entity);
      resp = client.execute(post);
      
      // validate response
      validateHttpResponse(resp);

      // deserialize response
      Map<String, Object> respMap = JsonUtils.toMap(MAPPER, EntityUtils.toString(resp.getEntity(), "UTF-8"));
      EntityUtils.consume(resp.getEntity());
      if (MoneroUtils.getLogLevel() >= 3) {
        String respStr = JsonUtils.serialize(respMap);
        respStr = respStr.substring(0, Math.min(10000, respStr.length()));
        MoneroUtils.log(3, "Received json response: " + respStr);
      }

      // check rpc response for errors
      validateRpcResponse(respMap, method, params);
      return respMap;
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
      //e3.printStackTrace();
      throw new MoneroError(e2);
    } finally {
      try { resp.close(); }
      catch (Exception e) {}
    }
  }
  
  /**
   * Send a RPC request to the given path and with the given paramters.
   * 
   * E.g. "/get_transactions" with params
   * 
   * @param path is the url path of the request to invoke
   * @return the request's deserialized response
   */
  public Map<String, Object>sendPathRequest(String path) {
    return sendPathRequest(path, null, null);
  }
  
  /**
   * Send a RPC request to the given path and with the given paramters.
   * 
   * E.g. "/get_transactions" with params
   * 
   * @param path is the url path of the request to invoke
   * @param params are request parameters sent in the body
   * @return the request's deserialized response
   */
  public Map<String, Object> sendPathRequest(String path, Map<String, Object> params) {
    return sendPathRequest(path, params, null);
  }
  
  /**
   * Send a RPC request to the given path and with the given paramters.
   * 
   * E.g. "/get_transactions" with params
   * 
   * @param path is the url path of the request to invoke
   * @param params are request parameters sent in the body
   * @param timeoutInMs is the request timeout in milliseconds
   * @return the request's deserialized response
   */
  public Map<String, Object> sendPathRequest(String path, Map<String, Object> params, Long timeoutInMs) {
    CloseableHttpResponse resp = null;
    try {
      
      // send http request
      if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending path request with path '" + path + "' and params: " + JsonUtils.serialize(params));
      HttpPost post = new HttpPost(uri.toString() + "/" + path);
      if (params != null) {
        HttpEntity entity = new StringEntity(JsonUtils.serialize(params));
        post.setEntity(entity);
      }
      post.setConfig(getTimeoutConfig(timeoutInMs));
      resp = client.execute(post);
      
      // validate response
      validateHttpResponse(resp);
      
      // deserialize response
      Map<String, Object> respMap = JsonUtils.toMap(MAPPER, EntityUtils.toString(resp.getEntity(), "UTF-8"));
      EntityUtils.consume(resp.getEntity());
      if (MoneroUtils.getLogLevel() >= 3) {
        String respStr = JsonUtils.serialize(respMap);
        respStr = respStr.substring(0, Math.min(10000, respStr.length()));
        MoneroUtils.log(3, "Received path response: " + respStr);
      }
      
      // check rpc response for errors
      validateRpcResponse(respMap, path, params);
      return respMap;
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
      e2.printStackTrace();
      throw new MoneroError(e2);
    } finally {
      try { resp.close(); }
      catch (Exception e) {}
    }
  }
  
  /**
   * Send a binary RPC request.
   * 
   * @param path is the path of the binary RPC method to invoke
   * @param params are the request parameters
   * @return byte[] is the binary response
   */
  public byte[] sendBinaryRequest(String path, Map<String, Object> params) {
    return sendBinaryRequest(path, params, null);
  }
  
  /**
   * Send a binary RPC request.
   * 
   * @param path is the path of the binary RPC method to invoke
   * @param params are the request parameters
   * @param timeoutInMs is the request timeout in milliseconds
   * @return byte[] is the binary response
   */
  public byte[] sendBinaryRequest(String path, Map<String, Object> params, Long timeoutInMs) {
    
    // serialize params to monero's portable binary storage format
    byte[] paramsBin = MoneroUtils.mapToBinary(params);
    CloseableHttpResponse resp = null;
    try {
      
      // send http request
      if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending binary request with path '" + path + "' and params: " + JsonUtils.serialize(params));
      HttpPost post = new HttpPost(uri.toString() + "/" + path);
      post.setConfig(getTimeoutConfig(timeoutInMs));
      if (paramsBin != null) {
        HttpEntity entity = new ByteArrayEntity(paramsBin, ContentType.DEFAULT_BINARY);
        post.setEntity(entity);
      }
      resp = client.execute(post);
      
      // validate response
      validateHttpResponse(resp);
      
      // deserialize response
      return EntityUtils.toByteArray(resp.getEntity());
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
      e2.printStackTrace();
      throw new MoneroError(e2);
    } finally {
      try { resp.close(); }
      catch (Exception e) {}
    }
  }
  
  @Override
  public String toString() {
    return uri + " (uri=" + uri + ", username=" + username + ", password=" + (password == null ? "null" : "***") + ", priority=" + priority + ", isOnline=" + isOnline + ", isAuthenticated=" + isAuthenticated + ")";
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((password == null) ? 0 : password.hashCode());
    result = prime * result + ((uri == null) ? 0 : uri.hashCode());
    result = prime * result + ((username == null) ? 0 : username.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroRpcConnection other = (MoneroRpcConnection) obj;
    if (password == null) {
      if (other.password != null) return false;
    } else if (!password.equals(other.password)) return false;
    if (uri == null) {
      if (other.uri != null) return false;
    } else if (!uri.equals(other.uri)) return false;
    if (username == null) {
      if (other.username != null) return false;
    } else if (!username.equals(other.username)) return false;
    return true;
  }
  
  // ------------------------------ PRIVATE HELPERS --------------------------
  
  private static void validateHttpResponse(CloseableHttpResponse resp) {
    int code = resp.getCode();
    if (code < 200 || code > 299) {
      String content = null;
      try {
        content = EntityUtils.toString(resp.getEntity(), "UTF-8");
      } catch (Exception e) {
        // could not get content
      }
      throw new MoneroRpcError(code + " " + resp.getReasonPhrase() + (content == null || content.isEmpty() ? "" : (": " + content)), code, null, null);
    }
  }

  @SuppressWarnings("unchecked")
  private static void validateRpcResponse(Map<String, Object> respMap, String method, Object params) {
    Map<String, Object> error = (Map<String, Object>) respMap.get("error");
    if (error == null) return;
    String msg = (String) error.get("message");
    int code = ((BigInteger) error.get("code")).intValue();
    throw new MoneroRpcError(msg, code, method, params);
  }
  
  private RequestConfig getTimeoutConfig(Long timeoutInMs) {
    if (timeoutInMs == null) return null;
    return RequestConfig.custom()
            .setConnectTimeout(Timeout.ofMilliseconds(timeoutInMs))
            .setConnectionRequestTimeout(Timeout.ofMilliseconds(timeoutInMs))
            .setResponseTimeout(Timeout.ofMilliseconds(timeoutInMs))
            .build();
  }
}
