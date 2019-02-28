package monero.rpc;

import java.util.Map;

import monero.utils.MoneroException;

/**
 * Exception that is thrown when the RPC API returns an error.
 */
public class MoneroRpcException extends MoneroException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private int rpcCode;
  private String rpcMessage;
  private Map<String, Object> requestBody;
  
  public MoneroRpcException(Integer rpcCode, String rpcMessage, Map<String, Object> requestBody) {
    this(rpcCode, rpcMessage, requestBody, null);
  }
  
  public MoneroRpcException(int rpcCode, String rpcMessage, Map<String, Object> requestBody, Throwable e) {
    super(rpcCode + ": " + rpcMessage, e);
    this.rpcCode = rpcCode;
    this.rpcMessage = rpcMessage;
  }

  public int getRpcCode() {
    return rpcCode;
  }

  public String getRpcMessage() {
    return rpcMessage;
  }

  public Map<String, Object> getRequestBody() {
    return requestBody;
  }

  public void setRequestBody(Map<String, Object> requestBody) {
    this.requestBody = requestBody;
  }
}
