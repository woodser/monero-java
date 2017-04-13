package wallet;

/**
 * Exception that is thrown when the RPC API returns an error.
 * 
 * @author woodser
 */
public class MoneroExceptionRpc extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private Integer rpcCode;
  private String rpcMessage;
  
  public MoneroExceptionRpc(Integer rpcCode, String rpcMessage) {
    this(rpcCode, rpcMessage, null);
  }
  
  public MoneroExceptionRpc(Integer rpcCode, String rpcMessage, Throwable e) {
    super(rpcMessage, e);
    this.rpcCode = rpcCode;
    this.rpcMessage = rpcMessage;
  }

  public Integer getRpcCode() {
    return rpcCode;
  }

  public String getRpcMessage() {
    return rpcMessage;
  }
}
