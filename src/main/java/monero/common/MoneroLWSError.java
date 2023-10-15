package monero.common;

public class MoneroLWSError extends MoneroError {

    /**
     * Construct the exception.
     *
     * @param message is a human-readable description of the error
     * @param code    is the error code (optional)
     */
    public MoneroLWSError(String message, Integer code) {
        super(message, code);
    }

    public MoneroLWSError(Exception e)
    {
        super(e);
    }
}
