package wallet;

public class MoneroWalletException extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;

  public MoneroWalletException(String msg) {
    super(msg);
  }
  
  public MoneroWalletException(String msg, Throwable e) {
    super(msg, e);
  }
  
  public MoneroWalletException(Throwable e) {
    super(e);
  }
}
