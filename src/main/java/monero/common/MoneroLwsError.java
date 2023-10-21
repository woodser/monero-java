package monero.common;

public class MoneroLwsError extends MoneroError {

    /**
     * Construct the exception.
     *
     * @param message is a human-readable description of the error
     * @param code    is the error code (optional)
     */
    public MoneroLwsError(String message, Integer code) {
        super(message, code);
    }

    public MoneroLwsError(Exception e)
    {
        super(e);
    }
}
