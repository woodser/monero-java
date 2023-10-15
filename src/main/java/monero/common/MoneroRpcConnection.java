package monero.common;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.utils.JsonUtils;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.io.entity.ByteArrayEntity;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;


/**
 * Maintains a connection and sends requests to a Monero RPC API.
 * TODO: refactor MoneroRpcConnection extends MoneroConnection?
 */
public class MoneroRpcConnection extends MoneroHttpConnection {

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
    return (MoneroRpcConnection) super.setUri(uri);
  }

  public MoneroRpcConnection setCredentials(String username, String password) {
    return (MoneroRpcConnection) super.setCredentials(username, password);
  }

  public MoneroRpcConnection setZmqUri(String zmqUri) {
    return (MoneroRpcConnection) super.setZmqUri(zmqUri);
  }

  public MoneroRpcConnection setProxyUri(String proxyUri) {
    return (MoneroRpcConnection) super.setProxyUri(proxyUri);
  }

  /**
   * Set the connection's priority relative to other connections. Priority 1 is highest,
   * then priority 2, etc. The default priority of 0 is lowest priority.
   * 
   * @param priority is the connection priority (default 0)
   * @return this connection
   */
  public MoneroRpcConnection setPriority(int priority) {
    return (MoneroRpcConnection) super.setPriority(priority);
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
