package monero.common;

public class MoneroHttpNotFoundError extends MoneroHttpError {
    public MoneroHttpNotFoundError(String httpMethod)
    {
        this(httpMethod, null);
    }

    public MoneroHttpNotFoundError(String httpMethod, Object httpParams) {
        super("The requested url was not found on this server.", 403, httpMethod, httpParams);
    }
}
