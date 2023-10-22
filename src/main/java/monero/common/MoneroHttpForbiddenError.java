package monero.common;

public class MoneroHttpForbiddenError extends MoneroHttpError {


    public MoneroHttpForbiddenError(String httpMethod)
    {
        this(httpMethod, null);
    }

    public MoneroHttpForbiddenError(String httpMethod, Object httpParams) {
        super("You don't have permission to access this resource", 403, httpMethod, httpParams);
    }
}
