package monero.common;

import common.utils.GenUtils;

/**
 * Exception when interacting with a Monero wallet or daemon.
 */
public class MoneroError extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private Integer code;
  
  /**
   * Construct the exception with an existing exception.
   * 
   * @param e is the existing exception
   */
  public MoneroError(Throwable e) {
    super(e);
  }
  
  /**
   * Construct the exception.
   * 
   * @param message is a human-readable description of the error
   */
  public MoneroError(String message) {
    this(message, null);
  }
  
  /**
   * Construct the exception.
   * 
   * @param message is a human-readable description of the error
   * @param code is the error code (optional)
   */
  public MoneroError(String message, Integer code) {
    super(message);
    GenUtils.assertNotNull("Exeption message cannot be null", message);
    this.code = code;
  }

  public Integer getCode() {
    return code;
  }
  
  public String toString() {
    if (code == null) return getMessage();
    return code + ": " + getMessage();
  }
}
