package monero.utils;

/**
 * Exception when interacting with a Monero wallet or daemon.
 */
public class MoneroException extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private Integer code;
  
  public MoneroException(String msg) {
    super(msg);
  }

  public MoneroException(String msg, int code) {
    this(msg, code, null);
  }
  
  public MoneroException(String msg, Throwable e) {
    super(msg, e);
  }
  
  public MoneroException(String msg, Integer code, Throwable e) {
    super(msg, e);
    this.code = code;
  }
  
  public MoneroException(Throwable e) {
    super(e);
  }

  public Integer getCode() {
    return code;
  }
}
