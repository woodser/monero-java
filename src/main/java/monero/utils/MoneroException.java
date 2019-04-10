package monero.utils;

/**
 * Exception when interacting with a Monero wallet or daemon.
 */
public class MoneroException extends RuntimeException {

  private static final long serialVersionUID = -6282368684634114151L;
  
  private String description;
  private Integer code;
  
  /**
   * Construct the exception with an existing exception.
   * 
   * @param e is the existing exception
   */
  public MoneroException(Throwable e) {
    super(e);
  }
  
  /**
   * Construct the exception.
   * 
   * @param description is a human-readable description of the error
   */
  public MoneroException(String description) {
    this(description, null);
  }
  
  /**
   * Construct the exception.
   * 
   * @param description is a human-readable description of the error
   * @param code is the error code (optional)
   */
  public MoneroException(String description, Integer code) {
    super();
    this.description = description;
    this.code = code;
  }
  
  /**
   * Get a human-readable description of the error without error code, etc.
   * 
   * @return String is a human-readable description of the error
   */
  public String getDescription() {
    return description;
  }

  public Integer getCode() {
    return code;
  }
  
  public String getMessage() {
    return toString();
  }
  
  public String toString() {
    String str = "";
    if (code != null) str += code + ": ";
    str += getDescription();
    return str;
  }
}
