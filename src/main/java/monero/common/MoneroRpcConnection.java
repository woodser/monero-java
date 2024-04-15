package monero.common;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import common.utils.JsonUtils;

import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.net.ssl.SSLContext;

import org.apache.hc.client5.http.DnsResolver;
import org.apache.hc.client5.http.auth.AuthScope;
import org.apache.hc.client5.http.auth.UsernamePasswordCredentials;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpUriRequest;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.auth.BasicCredentialsProvider;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.client5.http.impl.io.BasicHttpClientConnectionManager;
import org.apache.hc.client5.http.protocol.HttpClientContext;
import org.apache.hc.client5.http.socket.ConnectionSocketFactory;
import org.apache.hc.client5.http.socket.PlainConnectionSocketFactory;
import org.apache.hc.client5.http.ssl.SSLConnectionSocketFactory;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpHost;
import org.apache.hc.core5.http.config.Registry;
import org.apache.hc.core5.http.config.RegistryBuilder;
import org.apache.hc.core5.http.io.entity.ByteArrayEntity;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.protocol.HttpContext;
import org.apache.hc.core5.ssl.SSLContexts;
import org.apache.hc.core5.util.TimeValue;
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
  private String proxyUri;
  private boolean printStackTrace;
  
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
    this.setCredentials(username, password);
  }
  
  public MoneroRpcConnection(MoneroRpcConnection connection) {
    this(connection.uri, connection.username, connection.password, connection.zmqUri);
    this.priority = connection.priority;
    this.isOnline = connection.isOnline;
    this.isAuthenticated = connection.isAuthenticated;
    this.responseTime = connection.responseTime;
    this.proxyUri = connection.proxyUri;
    this.printStackTrace = connection.printStackTrace;
  }

  public String getUri() {
    return uri;
  }

  public MoneroRpcConnection setUri(String uri) {
    return setUri(MoneroUtils.parseUri(uri));
  }

  public MoneroRpcConnection setUri(URI uri) {
    this.uri = MoneroUtils.parseUri(uri.toString()).toString();
    setCredentials(username, password); // update credentials
    return this;
  }

  public boolean isOnion() {
    try {
      return new URI(uri).getHost().endsWith(".onion");
    } catch (Exception e) {
      return false;
    }
  }
  
  public MoneroRpcConnection setCredentials(String username, String password) {
    try { if (this.client != null) this.client.close(); }
    catch (IOException e) { throw new MoneroError(e); }
    if ("".equals(username)) username = null;
    if ("".equals(password)) password = null;
    if (username != null || password != null) {
      if (username == null) throw new MoneroError("username cannot be empty because password is not empty");
      if (password == null) throw new MoneroError("password cannot be empty because username is not empty");
      URI uriObj = MoneroUtils.parseUri(uri);
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

  public MoneroRpcConnection setProxyUri(String proxyUri) {
    this.proxyUri = proxyUri;
    return this;
  }

  public String getProxyUri() {
    return proxyUri;
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
   * Check the connection and update online, authentication, and response time status.
   * 
   * @param timeoutMs the maximum response time before considered offline
   * @return true if there is a change in status, false otherwise
   */
  public boolean checkConnection(long timeoutMs) {
    Boolean isOnlineBefore = isOnline;
    Boolean isAuthenticatedBefore = isAuthenticated;
    long startTime = System.currentTimeMillis();
    try {
      if (MoneroUtils.isNativeLibraryLoaded()) {
        List<Long> heights = new ArrayList<Long>();
        for (long i = 0; i < 100; i++) heights.add(i);
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("heights", heights);
        sendBinaryRequest("get_blocks_by_height.bin", params, timeoutMs); // assume daemon connection
      } else {
        sendJsonRequest("get_version", null, timeoutMs);
      }
      isOnline = true;
      isAuthenticated = true;
    } catch (Exception e) {
      isOnline = false;
      isAuthenticated = null;
      responseTime = null;
      if (e instanceof MoneroRpcError) {
        if (((MoneroRpcError) e).getCode() == 401) {
          isOnline = true;
          isAuthenticated = false;
        } else if (((MoneroRpcError) e).getCode() == 404) { // fallback to latency check
          isOnline = true;
          isAuthenticated = true;
        }
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

      // send http request
      HttpPost post = new HttpPost(uri.toString() + "/json_rpc");
      post.setConfig(getRequestConfig(timeoutInMs));
      HttpEntity entity = new StringEntity(JsonUtils.serialize(body));
      post.setEntity(entity);
      Map<String, Object> respMap;
      synchronized (this) {

        // logging
        if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending json request with method='" + method + "', body=" + JsonUtils.serialize(body) + ", uri=" + uri);
        if (printStackTrace) {
          try {
            throw new RuntimeException("Debug stack trace for json request with method '" + method + "' and body " + JsonUtils.serialize(body));
          } catch (Exception e) {
            e.printStackTrace();
          }
        }

        // make request
        long startTime = System.currentTimeMillis();
        resp = request(post);
        
        // validate response
        validateHttpResponse(resp);

        // deserialize response
        respMap = JsonUtils.toMap(MAPPER, EntityUtils.toString(resp.getEntity(), "UTF-8"));
        EntityUtils.consume(resp.getEntity());
        if (MoneroUtils.getLogLevel() >= 3) {
          String respStr = JsonUtils.serialize(respMap);
          respStr = respStr.substring(0, Math.min(10000, respStr.length()));
          MoneroUtils.log(3, "Received json response from method='" + method + "', response=" + respStr + ", uri=" + uri + " (" + (System.currentTimeMillis() - startTime) + " ms)");
        }
      }
      
      // check rpc response for errors
      validateRpcResponse(respMap, method, params);
      return respMap;
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
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
      HttpPost post = new HttpPost(uri.toString() + "/" + path);
      if (params != null) {
        HttpEntity entity = new StringEntity(JsonUtils.serialize(params));
        post.setEntity(entity);
      }
      post.setConfig(getRequestConfig(timeoutInMs));
      Map<String, Object> respMap;
      synchronized (this) {

        // logging
        if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending path request with path='" + path + "', params=" + JsonUtils.serialize(params) + ", uri=" + uri);
        if (printStackTrace) {
          try {
            throw new RuntimeException("Debug stack trace for path request with path '" + path + "' and params " + JsonUtils.serialize(params));
          } catch (Exception e) {
            e.printStackTrace();
          }
        }

        // send request
        long startTime = System.currentTimeMillis();
        resp = request(post);
        
        // validate response
        validateHttpResponse(resp);
        
        // deserialize response
        respMap = JsonUtils.toMap(MAPPER, EntityUtils.toString(resp.getEntity(), "UTF-8"));
        EntityUtils.consume(resp.getEntity());
        if (MoneroUtils.getLogLevel() >= 3) {
          String respStr = JsonUtils.serialize(respMap);
          respStr = respStr.substring(0, Math.min(10000, respStr.length()));
          MoneroUtils.log(3, "Received path response from path='" + path + "', response=" + respStr + ", uri=" + uri + " (" + (System.currentTimeMillis() - startTime) + " ms)");
        }
      }
      
      // check rpc response for errors
      validateRpcResponse(respMap, path, params);
      return respMap;
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
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

      // create http request
      HttpPost post = new HttpPost(uri.toString() + "/" + path);
      post.setConfig(getRequestConfig(timeoutInMs));
      if (paramsBin != null) {
        HttpEntity entity = new ByteArrayEntity(paramsBin, ContentType.DEFAULT_BINARY);
        post.setEntity(entity);
      }
      
      // send http request
      synchronized (this) {

        // logging
        if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending binary request with path='" + path + "', params=" + JsonUtils.serialize(params) + ", uri=" + uri);
        if (printStackTrace) {
          try {
            throw new RuntimeException("Debug stack trace for binary request with path '" + path);
          } catch (Exception e) {
            e.printStackTrace();
          }
        }

        // send request
        resp = request(post);
        
        // validate response
        validateHttpResponse(resp);
        
        // deserialize response
        byte[] entity = EntityUtils.toByteArray(resp.getEntity());
        EntityUtils.consume(resp.getEntity());
        return entity;
      }
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
      throw new MoneroError(e2);
    } finally {
      try { resp.close(); }
      catch (Exception e) {}
    }
  }

  /**
   * Enable or disable printing a stack trace on each request for debug.
   * 
   * @param printStackTrace sets if the stack trace should be printed
   */
  public void setPrintStackTrace(boolean printStackTrace) {
    this.printStackTrace = printStackTrace;
  }
  
  @Override
  public String toString() {
    return uri + " (uri=" + uri + ", username=" + username + ", password=" + (password == null ? "null" : "***") + ", priority=" + priority + ", isOnline=" + isOnline + ", isAuthenticated=" + isAuthenticated + ", zmqUri=" + zmqUri + ", proxyUri=" + proxyUri + ")";
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((password == null) ? 0 : password.hashCode());
    result = prime * result + ((proxyUri == null) ? 0 : proxyUri.hashCode());
    result = prime * result + ((uri == null) ? 0 : uri.hashCode());
    result = prime * result + ((username == null) ? 0 : username.hashCode());
    result = prime * result + ((zmqUri == null) ? 0 : zmqUri.hashCode());
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
    if (proxyUri == null) {
      if (other.proxyUri != null) return false;
    } else if (!proxyUri.equals(other.proxyUri)) return false;
    if (uri == null) {
      if (other.uri != null) return false;
    } else if (!uri.equals(other.uri)) return false;
    if (username == null) {
      if (other.username != null) return false;
    } else if (!username.equals(other.username)) return false;
    if (zmqUri == null) {
      if (other.zmqUri != null) return false;
    } else if (!zmqUri.equals(other.zmqUri)) return false;
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
  private void validateRpcResponse(Map<String, Object> respMap, String method, Object params) {
    Map<String, Object> error = (Map<String, Object>) respMap.get("error");
    if (error == null) return;
    String errorMsg = (String) error.get("message");
    int code = ((BigInteger) error.get("code")).intValue();
    if ("".equals(errorMsg)) errorMsg = "Received error response from RPC request with method '" + method + "' to " + uri; // TODO (monero-project): response sometimes has empty error message
    throw new MoneroRpcError(errorMsg, code, method, params);
  }
  
  private RequestConfig getRequestConfig(Long timeoutInMs) {
    RequestConfig.Builder builder = RequestConfig.custom();
    if (timeoutInMs != null) {
      builder.setConnectTimeout(Timeout.ofMilliseconds(timeoutInMs));
      builder.setConnectionRequestTimeout(Timeout.ofMilliseconds(timeoutInMs));
      builder.setResponseTimeout(Timeout.ofMilliseconds(timeoutInMs));
    }
    return builder.build();
  }

  private CloseableHttpResponse request(HttpUriRequest request) throws IOException, URISyntaxException {
    return proxyUri == null ? client.execute(request) : requestWithProxy(request);
  }

  private CloseableHttpResponse requestWithProxy(HttpUriRequest request) throws IOException {

    // register socket factories to use socks5
    Registry<ConnectionSocketFactory> reg = RegistryBuilder.<ConnectionSocketFactory>create()
        .register("http", new SocksConnectionSocketFactory())
        .register("https", new SocksSSLConnectionSocketFactory(SSLContexts.createSystemDefault())).build();

    // create connection manager to use socket factories and fake dns resolver
    boolean isLocal = false; // use fake dns resolver if not resolving DNS locally TODO: determine if request url is local
    BasicHttpClientConnectionManager cm = isLocal ?
        new BasicHttpClientConnectionManager(reg) :
        new BasicHttpClientConnectionManager(reg, null, null, new FakeDnsResolver());

    // create http client
    CloseableHttpClient closeableHttpClient = HttpClients.custom().setConnectionManager(cm).build();
    
    // register socks address
    URI proxyParsed = MoneroUtils.parseUri(proxyUri);
    InetSocketAddress socksAddress = new InetSocketAddress(proxyParsed.getHost(), proxyParsed.getPort());
    HttpClientContext context = HttpClientContext.create();
    context.setAttribute("socks.address", socksAddress);

    // execute request
    CloseableHttpResponse httpResponse = closeableHttpClient.execute(request, context);
    return httpResponse;
  }

  /**
   * Routes connections over Socks, and avoids resolving hostnames locally.
   * 
   * Adapted from: http://stackoverflow.com/a/25203021/5616248
   */
  class SocksConnectionSocketFactory extends PlainConnectionSocketFactory {

    /**
     * creates an unconnected Socks Proxy socket
     */
    @Override
    public Socket createSocket(final HttpContext context) throws IOException {
        InetSocketAddress socksaddr = (InetSocketAddress) context.getAttribute("socks.address");
        Proxy proxy = new Proxy(Proxy.Type.SOCKS, socksaddr);
        return new Socket(proxy);
    }

    @Override
    public Socket connectSocket(TimeValue connectTimeout, Socket socket, HttpHost host, InetSocketAddress remoteAddress,
                                InetSocketAddress localAddress, HttpContext context) throws IOException {
        InetSocketAddress unresolvedRemote = InetSocketAddress.createUnresolved(host.getHostName(), remoteAddress.getPort());
        return super.connectSocket(connectTimeout, socket, host, unresolvedRemote, localAddress, context);
    }
  }

  private class SocksSSLConnectionSocketFactory extends SSLConnectionSocketFactory {

    public SocksSSLConnectionSocketFactory(final SSLContext sslContext) {
        super(sslContext);

        // TODO: Or to allow "insecure" (eg self-signed certs)
        // super(sslContext, ALLOW_ALL_HOSTNAME_VERIFIER);
    }

    @Override
    public Socket createSocket(final HttpContext context) throws IOException {
        InetSocketAddress socksaddr = (InetSocketAddress) context.getAttribute("socks.address");
        Proxy proxy = new Proxy(Proxy.Type.SOCKS, socksaddr);
        return new Socket(proxy);
    }

    @Override
    public Socket connectSocket(TimeValue connectTimeout, Socket socket, HttpHost host, InetSocketAddress remoteAddress,
                                InetSocketAddress localAddress, HttpContext context) throws IOException {
        // Convert address to unresolved
        InetSocketAddress unresolvedRemote = InetSocketAddress.createUnresolved(host.getHostName(), remoteAddress.getPort());
        return super.connectSocket(connectTimeout, socket, host, unresolvedRemote, localAddress, context);
    }
  }

  class FakeDnsResolver implements DnsResolver {
    @Override
    public InetAddress[] resolve(String host) throws UnknownHostException {
        return new InetAddress[]{InetAddress.getByAddress(new byte[]{1, 1, 1, 1})};
    }

    @Override
    public String resolveCanonicalHostname(String host) throws UnknownHostException {
      throw new UnsupportedOperationException("Unimplemented method 'resolveCanonicalHostname'");
    }
  }
}
