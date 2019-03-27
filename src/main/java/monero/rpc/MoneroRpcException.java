package monero.rpc;

import monero.utils.MoneroException;

/**
 * Exception when interacting with the Monero daemon or wallet RPC API.
 */
public class MoneroRpcException extends MoneroException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private String rpcMethod;
  private Object rpcParams;
  
  public MoneroRpcException(String rpcDescription, Integer rpcCode, String rpcMethod, Object rpcParams) {
    super(rpcDescription, rpcCode);
    this.rpcMethod = rpcMethod;
    this.rpcParams = rpcParams;
  }

  public String getRpcMethod() {
    return rpcMethod;
  }

  public Object getRpcParams() {
    return rpcParams;
  }
  
  public String getMessage() {
    return toString();
  }
  
  public String toString() {
    String str = super.toString();
    str += "\nRPC method: " + rpcMethod;
    str += "\nRPC params: " + rpcParams;
    return str;
  }
}
