package monero.rpc;

import java.util.Map;

import monero.utils.MoneroException;

/**
 * Exception when interacting with the Monero daemon or wallet RPC API.
 */
public class MoneroRpcException extends MoneroException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private Map<String, Object> requestBody;
  
  public MoneroRpcException(String msg, int code, Map<String, Object> requestBody) {
    this(msg, code, requestBody, null);
  }
  
  public MoneroRpcException(String msg, int code, Map<String, Object> requestBody, Throwable e) {
    super(msg, code, e);
    this.requestBody = requestBody;
  }

  public Map<String, Object> getRequestBody() {
    return requestBody;
  }
}
