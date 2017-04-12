package wallet;

/**
 * Exception to handle problems interacting with the Monero Wallet RPC.
 * 
 * @author woodser
 */
public class MoneroException extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private Integer rpcCode;
  private String rpcMessage;
  
  public MoneroException(Throwable e) {
    super(e);
  }
  
  public MoneroException(Integer rpcCode, String rpcMessage) {
    this(rpcCode, rpcMessage, null);
  }
  
  public MoneroException(Integer rpcCode, String rpcMessage, Throwable e) {
    super(e);
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
