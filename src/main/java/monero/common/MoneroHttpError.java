package monero.common;

import common.utils.JsonUtils;

/**
 * Exception when interacting with the Monero daemon or wallet RPC API.
 */
public class MoneroHttpError extends MoneroError {

    private static final long serialVersionUID = -6282368684634114151L;

    private String httpMethod;
    private Object httpParams;

    public MoneroHttpError(String httpDescription, Integer httpCode, String httpMethod, Object httpParams) {
        super(httpDescription, httpCode);
        this.httpMethod = httpMethod;
        this.httpParams = httpParams;
    }

    public String getHttpMethod() {
        return httpMethod;
    }

    public Object getHttpParams() {
        return httpParams;
    }

    public String toString() {
        String str = super.toString();
        if (httpMethod != null || httpParams != null) str += "\nRPC request: '" + httpMethod + "' with params: " + JsonUtils.serialize(httpParams);
        return str;
    }
}