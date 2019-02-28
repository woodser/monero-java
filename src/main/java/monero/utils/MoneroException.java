package monero.utils;

public class MoneroException extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;

  public MoneroException(String msg) {
    super(msg);
  }
  
  public MoneroException(String msg, Throwable e) {
    super(msg, e);
  }
  
  public MoneroException(Throwable e) {
    super(e);
  }
}
