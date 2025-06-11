package monero.common;

import common.utils.JsonUtils;

/**
 * Exception when interacting with the Monero daemon or wallet RPC API.
 */
public class MoneroRpcError extends MoneroError {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private String rpcMethod;
  private Object rpcParams;
  
  public MoneroRpcError(String rpcDescription, Integer rpcCode, String rpcMethod, Object rpcParams) {
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
  
  public String toString() {
    String str = super.toString();
    if (rpcMethod != null) str += "\nRPC request: '" + rpcMethod + "'";
    return str;
  }
}
