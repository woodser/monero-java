package monero.common;

public class MoneroHttpNotImplementedError extends MoneroHttpError {


    public MoneroHttpNotImplementedError(String httpMethod)
    {
        this(httpMethod, null);
    }

    public MoneroHttpNotImplementedError(String httpMethod, Object httpParams) {
        super("Not supported", 501, httpMethod, httpParams);
    }
}
