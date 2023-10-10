package monero.common;

import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.hc.client5.http.auth.AuthScope;
import org.apache.hc.client5.http.auth.UsernamePasswordCredentials;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.impl.auth.BasicCredentialsProvider;
import org.apache.hc.client5.http.impl.classic.HttpClients;


/**
 * Maintains a connection and sends requests to a Monero RPC API.
 * TODO: refactor MoneroRpcConnection extends MoneroConnection?
 */
public class MoneroRpcConnection extends MoneroHttpConnection {

  // instance variables
  private String username;
  private String password;
  private String zmqUri;

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
    //this.uri = uri == null ? null : MoneroUtils.parseUri(uri.toString()).toString();
    super(uri);
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

  public MoneroRpcConnection setAttribute(String key, Object value) {
    attributes.put(key, value);
    return this;
  }


  public MoneroRpcConnection setUri(String uri) {
    return setUri(MoneroUtils.parseUri(uri));
  }

  public MoneroRpcConnection setUri(URI uri) {
    super.setUri(uri);
    setCredentials(username, password); // update credentials
    return this;
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

  public MoneroRpcConnection setProxyUri(String proxyUri) {
    this.proxyUri = proxyUri;
    return this;
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

  @Override
  protected HttpPost getHttpPost()
  {
    return new HttpPost(uri + "/json_rpc");
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
        List<Long> heights = new ArrayList<>();
        for (long i = 0; i < 100; i++) heights.add(i);
        Map<String, Object> params = new HashMap<>();
        params.put("heights", heights);
        sendBinaryRequest("get_blocks_by_height.bin", params); // assume daemon connection
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
   * Send a request to the RPC API.
   *
   * @param method is the method to request
   * @param params are the request's input parameters (supports &lt;Map&lt;String, Object&gt;, List&lt;Object&gt;&lt;/code&gt;, String, etc.)
   * @param timeoutInMs is the request timeout in milliseconds
   * @return the RPC API response as a map
   */
  public Map<String, Object> sendJsonRequest(String method, Object params, Long timeoutInMs) {
    // build request body
    Map<String, Object> body = new HashMap<>();
    body.put("jsonrpc", "2.0");
    body.put("id", "0");
    body.put("method", method);
    if (params != null) body.put("params", params);

    try {
      Map<String, Object> respMap = super.sendJsonRequest(method, body, timeoutInMs);
      // check rpc response for errors
      validateRpcResponse(respMap, method, params);
      return respMap;

    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
      throw new MoneroError(e2);
    }
  }

  /**
   * Send an RPC request to the given path and with the given parameters.
   * E.g. "/get_transactions" with params
   *
   * @param path is the url path of the request to invoke
   * @param params are request parameters sent in the body
   * @param timeoutInMs is the request timeout in milliseconds
   * @return the request's deserialized response
   */
  public Map<String, Object> sendPathRequest(String path, Map<String, Object> params, Long timeoutInMs) {
    try {
      Map<String, Object> respMap = super.sendPathRequest(path, params, timeoutInMs);
      // check rpc response for errors
      validateRpcResponse(respMap, path, params);
      return respMap;
    } catch (MoneroRpcError e1) {
      throw e1;
    } catch (Exception e2) {
      throw new MoneroError(e2);
    }
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
        return other.zmqUri == null;
    } else return zmqUri.equals(other.zmqUri);
  }
  
  // ------------------------------ PRIVATE HELPERS --------------------------

  @SuppressWarnings("unchecked")
  private static void validateRpcResponse(Map<String, Object> respMap, String method, Object params) {
    Map<String, Object> error = (Map<String, Object>) respMap.get("error");
    if (error == null) return;
    String msg = (String) error.get("message");
    int code = ((BigInteger) error.get("code")).intValue();
    throw new MoneroRpcError(msg, code, method, params);
  }

}
