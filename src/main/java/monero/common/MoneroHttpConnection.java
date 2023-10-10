package monero.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import common.utils.JsonUtils;

import org.apache.hc.client5.http.DnsResolver;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpUriRequest;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.client5.http.impl.io.BasicHttpClientConnectionManager;
import org.apache.hc.client5.http.protocol.HttpClientContext;
import org.apache.hc.client5.http.socket.ConnectionSocketFactory;
import org.apache.hc.client5.http.socket.PlainConnectionSocketFactory;
import org.apache.hc.client5.http.ssl.SSLConnectionSocketFactory;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpHost;
import org.apache.hc.core5.http.config.Registry;
import org.apache.hc.core5.http.config.RegistryBuilder;
import org.apache.hc.core5.http.io.entity.ByteArrayEntity;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.protocol.HttpContext;
import org.apache.hc.core5.ssl.SSLContexts;
import org.apache.hc.core5.util.TimeValue;
import org.apache.hc.core5.util.Timeout;

import javax.net.ssl.SSLContext;
import java.io.IOException;
import java.net.*;
import java.util.HashMap;
import java.util.Map;

public abstract class MoneroHttpConnection {

    // custom mapper to deserialize integers to BigIntegers
    public static ObjectMapper MAPPER;

    static {
        MAPPER = new ObjectMapper();
        MAPPER.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        MAPPER.configure(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS, true);
    }

    // instance variables
    protected CloseableHttpClient client;
    protected String uri;
    protected String proxyUri;
    protected int priority = 0;
    protected Boolean isOnline;
    protected Long responseTime;
    protected Boolean isAuthenticated;
    protected Boolean printStackTrace;
    protected Map<String, Object> attributes = new HashMap<>();


    protected MoneroHttpConnection(URI uri)
    {
        this.uri = uri == null ? null : MoneroUtils.parseUri(uri.toString()).toString();
    }

    protected MoneroHttpConnection(String uri)
    {
        this(uri == null ? null : MoneroUtils.parseUri(uri));
    }

    public MoneroHttpConnection setAttribute(String key, Object value) {
        attributes.put(key, value);
        return this;
    }

    public Object getAttribute(String key) {
        return attributes.get(key);
    }

    public boolean isOnion() {
        try {
            return new URI(uri).getHost().endsWith(".onion");
        } catch (Exception e) {
            return false;
        }
    }

    public String getUri() {
        return this.uri;
    }

    public MoneroHttpConnection setUri(String uri) {
        return setUri(MoneroUtils.parseUri(uri));
    }

    public MoneroHttpConnection setUri(URI uri) {
        this.uri = MoneroUtils.parseUri(uri.toString()).toString();
        return this;
    }

    public int getPriority() {
        return priority;
    }

    public MoneroHttpConnection setProxyUri(String proxyUri) {
        this.proxyUri = proxyUri;
        return this;
    }

    public String getProxyUri() {
        return proxyUri;
    }


    /**
     * Set the connection's priority relative to other connections. Priority 1 is highest,
     * then priority 2, etc. The default priority of 0 is lowest priority.
     *
     * @param priority is the connection priority (default 0)
     * @return this connection
     */
    public MoneroHttpConnection setPriority(int priority) {
        if (!(priority >= 0)) throw new MoneroError("Priority must be >= 0");
        this.priority = priority;
        return this;
    }

    /**
     * Indicates if the connection is connected according to the last call to checkConnection().<br><br>
     *
     * Note: must call checkConnection() manually unless using MoneroConnectionManager.
     *
     * @return true or false to indicate if connected, or null if checkConnection() has not been called
     */
    public Boolean isConnected() {
        return isOnline == null ? null : isOnline && !Boolean.FALSE.equals(isAuthenticated);
    }

    /**
     * Indicates if the connection is online according to the last call to checkConnection().<br><br>
     *
     * Note: must call checkConnection() manually unless using MoneroConnectionManager.
     *
     * @return true or false to indicate if online, or null if checkConnection() has not been called
     */
    public Boolean isOnline() {
        return isOnline;
    }

    /**
     * Indicates if the connection is authenticated according to the last call to checkConnection().<br><br>
     *
     * Note: must call checkConnection() manually unless using MoneroConnectionManager.
     *
     * @return true if authenticated or no authentication, false if not authenticated, or null if checkConnection() has not been called
     */
    public Boolean isAuthenticated() {
        return isAuthenticated;
    }

    /**
     * Get the response time of the last call to checkConnection().<br><br>
     *
     * Note: must call checkConnection() manually unless using MoneroConnectionManager.
     *
     * @return the response time of the last call to checkConnection() or null if checkConnection() has not been called
     */
    public Long getResponseTime() {
        return responseTime;
    }

    protected HttpPost getHttpPost()
    {
        return new HttpPost(uri);
    }

    /**
     * Send a request to the RPC API.
     *
     * @param method specifies the method to request
     * @return the RPC API response as a map
     */
    public Map<String, Object> sendJsonRequest(String method) {
        return sendJsonRequest(method, null);
    }

    /**
     * Send a request to the RPC API.
     *
     * @param method is the method to request
     * @param params are the request's input parameters (supports &lt;Map&lt;String, Object&gt;, List&lt;Object&gt;&lt;/code&gt;, String, etc.)
     * @return the RPC API response as a map
     */
    public Map<String, Object> sendJsonRequest(String method, Object params) {
        return sendJsonRequest(method, params, null);
    }

    /**
     * Send a request to the RPC API.
     *
     * @param method is the method to request
     * @param params are the request's input parameters (supports &lt;Map&lt;String, Object&gt;, List&lt;Object&gt;&lt;/code&gt;, String, etc.)
     * @param timeoutInMs is the request timeout in milliseconds
     * @return the RPC API response as a map
     */
    public Map<String, Object> sendJsonRequest(String method, Object params, Long timeoutInMs) {
        CloseableHttpResponse resp = null;
        try {

            // build request body
            //Map<String, Object> body = new HashMap<String, Object>();
            //body.put("jsonrpc", "2.0");
            //body.put("id", "0");
            //body.put("method", method);
            //if (params != null) body.put("params", params);

            // send http request
            //HttpPost post = new HttpPost(uri.toString() + "/json_rpc");
            HttpPost post = this.getHttpPost();
            post.setConfig(getRequestConfig(timeoutInMs));
            HttpEntity entity = new StringEntity(JsonUtils.serialize(params));
            post.setEntity(entity);
            Map<String, Object> respMap;
            synchronized (this) {

                // logging
                if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending json request with method='" + method + "', body=" + JsonUtils.serialize(params) + ", uri=" + uri);
                if (printStackTrace) {
                    try {
                        throw new RuntimeException("Debug stack trace for json request with method '" + method + "' and body " + JsonUtils.serialize(params));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                // make request
                long startTime = System.currentTimeMillis();
                resp = request(post);

                // validate response
                validateHttpResponse(resp);

                // deserialize response
                respMap = JsonUtils.toMap(MAPPER, EntityUtils.toString(resp.getEntity(), "UTF-8"));
                EntityUtils.consume(resp.getEntity());
                if (MoneroUtils.getLogLevel() >= 3) {
                    String respStr = JsonUtils.serialize(respMap);
                    respStr = respStr.substring(0, Math.min(10000, respStr.length()));
                    MoneroUtils.log(3, "Received json response from method='" + method + "', response=" + respStr + ", uri=" + uri + " (" + (System.currentTimeMillis() - startTime) + " ms)");
                }
            }

            // check rpc response for errors
            //validateRpcResponse(respMap, method, params);
            return respMap;
        } catch (MoneroHttpError e1) {
            throw e1;
        } catch (Exception e2) {
            throw new MoneroError(e2);
        } finally {
            try { resp.close(); }
            catch (Exception ignored) {}
        }
    }


    /**
     * Send an RPC request to the given path and with the given parameters.
     * E.g. "/get_transactions" with params
     *
     * @param path is the url path of the request to invoke
     * @return the request's deserialized response
     */
    public Map<String, Object>sendPathRequest(String path) {
        return sendPathRequest(path, null, null);
    }

    /**
     * Send an RPC request to the given path and with the given parameters.
     * E.g. "/get_transactions" with params
     *
     * @param path is the url path of the request to invoke
     * @param params are request parameters sent in the body
     * @return the request's deserialized response
     */
    public Map<String, Object> sendPathRequest(String path, Map<String, Object> params) {
        return sendPathRequest(path, params, null);
    }

    /**
     * Send an RPC request to the given path and with the given parameters.
     * E.g. "/get_transactions" with params
     *
     * @param path is the url path of the request to invoke
     * @param params are request parameters sent in the body
     * @param timeoutInMs is the request timeout in milliseconds
     * @return the request's deserialized response
     */
    public Map<String, Object> sendPathRequest(String path, Map<String, Object> params, Long timeoutInMs) {
        CloseableHttpResponse resp = null;
        try {

            // send http request
            HttpPost post = new HttpPost(uri + "/" + path);
            if (params != null) {
                HttpEntity entity = new StringEntity(JsonUtils.serialize(params));
                post.setEntity(entity);
            }
            post.setConfig(getRequestConfig(timeoutInMs));
            Map<String, Object> respMap;
            synchronized (this) {

                // logging
                if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending path request with path='" + path + "', params=" + JsonUtils.serialize(params) + ", uri=" + uri);
                if (printStackTrace) {
                    try {
                        throw new RuntimeException("Debug stack trace for path request with path '" + path + "' and params " + JsonUtils.serialize(params));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                // send request
                long startTime = System.currentTimeMillis();
                resp = request(post);

                // validate response
                validateHttpResponse(resp);

                // deserialize response
                respMap = JsonUtils.toMap(MAPPER, EntityUtils.toString(resp.getEntity(), "UTF-8"));
                EntityUtils.consume(resp.getEntity());
                if (MoneroUtils.getLogLevel() >= 3) {
                    String respStr = JsonUtils.serialize(respMap);
                    respStr = respStr.substring(0, Math.min(10000, respStr.length()));
                    MoneroUtils.log(3, "Received path response from path='" + path + "', response=" + respStr + ", uri=" + uri + " (" + (System.currentTimeMillis() - startTime) + " ms)");
                }
            }

            // check rpc response for errors
            //validateRpcResponse(respMap, path, params);
            return respMap;
        } catch (MoneroHttpError e1) {
            throw e1;
        } catch (Exception e2) {
            throw new MoneroError(e2);
        } finally {
            try { resp.close(); }
            catch (Exception ignored) {}
        }
    }

    /**
     * Send a binary RPC request.
     *
     * @param path is the path of the binary RPC method to invoke
     * @param params are the request parameters
     * @return byte[] is the binary response
     */
    public byte[] sendBinaryRequest(String path, Map<String, Object> params) {
        return sendBinaryRequest(path, params, null);
    }

    /**
     * Send a binary RPC request.
     *
     * @param path is the path of the binary RPC method to invoke
     * @param params are the request parameters
     * @param timeoutInMs is the request timeout in milliseconds
     * @return byte[] is the binary response
     */
    public byte[] sendBinaryRequest(String path, Map<String, Object> params, Long timeoutInMs) {

        // serialize params to monero's portable binary storage format
        byte[] paramsBin = MoneroUtils.mapToBinary(params);
        CloseableHttpResponse resp = null;
        try {

            // create http request
            HttpPost post = new HttpPost(uri + "/" + path);
            post.setConfig(getRequestConfig(timeoutInMs));
            if (paramsBin != null) {
                HttpEntity entity = new ByteArrayEntity(paramsBin, ContentType.DEFAULT_BINARY);
                post.setEntity(entity);
            }

            // send http request
            synchronized (this) {

                // logging
                if (MoneroUtils.getLogLevel() >= 2) MoneroUtils.log(2, "Sending binary request with path='" + path + "', params=" + JsonUtils.serialize(params) + ", uri=" + uri);
                if (printStackTrace) {
                    try {
                        throw new RuntimeException("Debug stack trace for binary request with path '" + path);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                // send request
                resp = request(post);

                // validate response
                validateHttpResponse(resp);

                // deserialize response
                byte[] entity = EntityUtils.toByteArray(resp.getEntity());
                EntityUtils.consume(resp.getEntity());
                return entity;
            }
        } catch (MoneroHttpError e1) {
            throw e1;
        } catch (Exception e2) {
            throw new MoneroError(e2);
        } finally {
            try { resp.close(); }
            catch (Exception ignored) {}
        }
    }

    /**
     * Enable or disable printing a stack trace on each request for debug.
     *
     * @param printStackTrace sets if the stack trace should be printed
     */
    public void setPrintStackTrace(boolean printStackTrace) {
        this.printStackTrace = printStackTrace;
    }


    @Override
    public String toString() {
        return uri + " (uri=" + uri + ", " + ", priority=" + priority + ", isOnline=" + isOnline + ", isAuthenticated=" + isAuthenticated + ", proxyUri=" + proxyUri + ")";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((proxyUri == null) ? 0 : proxyUri.hashCode());
        result = prime * result + ((uri == null) ? 0 : uri.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        MoneroHttpConnection other = (MoneroRpcConnection) obj;
        if (proxyUri == null) {
            if (other.proxyUri != null) return false;
        } else if (!proxyUri.equals(other.proxyUri)) return false;
        if (uri == null) {
            return other.uri == null;
        } else return uri.equals(other.uri);
    }

    // ------------------------------ PRIVATE HELPERS --------------------------

    private static void validateHttpResponse(CloseableHttpResponse resp) {
        int code = resp.getCode();
        if (code < 200 || code > 299) {
            String content = null;
            try {
                content = EntityUtils.toString(resp.getEntity(), "UTF-8");
            } catch (Exception e) {
                // could not get content
            }
            throw new MoneroHttpError(code + " " + resp.getReasonPhrase() + (content == null || content.isEmpty() ? "" : (": " + content)), code, null, null);
        }
    }

    private RequestConfig getRequestConfig(Long timeoutInMs) {
        RequestConfig.Builder builder = RequestConfig.custom();
        if (timeoutInMs != null) {
            builder.setConnectTimeout(Timeout.ofMilliseconds(timeoutInMs));
            builder.setConnectionRequestTimeout(Timeout.ofMilliseconds(timeoutInMs));
            builder.setResponseTimeout(Timeout.ofMilliseconds(timeoutInMs));
        }
        return builder.build();
    }

    private CloseableHttpResponse request(HttpUriRequest request) throws IOException, URISyntaxException {
        return proxyUri == null ? client.execute(request) : requestWithProxy(request);
    }

    private CloseableHttpResponse requestWithProxy(HttpUriRequest request) throws IOException {

        // register socket factories to use socks5
        Registry<ConnectionSocketFactory> reg = RegistryBuilder.<ConnectionSocketFactory>create()
                .register("http", new SocksConnectionSocketFactory())
                .register("https", new MoneroHttpConnection.SocksSSLConnectionSocketFactory(SSLContexts.createSystemDefault())).build();

        // create connection manager to use socket factories and fake dns resolver
        boolean isLocal = false; // use fake dns resolver if not resolving DNS locally TODO: determine if request url is local
        BasicHttpClientConnectionManager cm = isLocal ?
                new BasicHttpClientConnectionManager(reg) :
                new BasicHttpClientConnectionManager(reg, null, null, new MoneroHttpConnection.FakeDnsResolver());

        // create http client
        CloseableHttpClient closeableHttpClient = HttpClients.custom().setConnectionManager(cm).build();

        // register socks address
        URI proxyParsed = MoneroUtils.parseUri(proxyUri);
        InetSocketAddress socksAddress = new InetSocketAddress(proxyParsed.getHost(), proxyParsed.getPort());
        HttpClientContext context = HttpClientContext.create();
        context.setAttribute("socks.address", socksAddress);

        // execute request
        return closeableHttpClient.execute(request, context);
    }

    /**
     * Routes connections over Socks, and avoids resolving hostnames locally.
     * Adapted from: http://stackoverflow.com/a/25203021/5616248
     */
    static class SocksConnectionSocketFactory extends PlainConnectionSocketFactory {

        /**
         * creates an unconnected Socks Proxy socket
         */
        @Override
        public Socket createSocket(final HttpContext context) throws IOException {
            InetSocketAddress socksaddr = (InetSocketAddress) context.getAttribute("socks.address");
            Proxy proxy = new Proxy(Proxy.Type.SOCKS, socksaddr);
            return new Socket(proxy);
        }

        @Override
        public Socket connectSocket(TimeValue connectTimeout, Socket socket, HttpHost host, InetSocketAddress remoteAddress,
                                    InetSocketAddress localAddress, HttpContext context) throws IOException {
            InetSocketAddress unresolvedRemote = InetSocketAddress.createUnresolved(host.getHostName(), remoteAddress.getPort());
            return super.connectSocket(connectTimeout, socket, host, unresolvedRemote, localAddress, context);
        }
    }

    private static class SocksSSLConnectionSocketFactory extends SSLConnectionSocketFactory {

        public SocksSSLConnectionSocketFactory(final SSLContext sslContext) {
            super(sslContext);

            // TODO: Or to allow "insecure" (eg self-signed certs)
            // super(sslContext, ALLOW_ALL_HOSTNAME_VERIFIER);
        }

        @Override
        public Socket createSocket(final HttpContext context) throws IOException {
            InetSocketAddress socksaddr = (InetSocketAddress) context.getAttribute("socks.address");
            Proxy proxy = new Proxy(Proxy.Type.SOCKS, socksaddr);
            return new Socket(proxy);
        }

        @Override
        public Socket connectSocket(TimeValue connectTimeout, Socket socket, HttpHost host, InetSocketAddress remoteAddress,
                                    InetSocketAddress localAddress, HttpContext context) throws IOException {
            // Convert address to unresolved
            InetSocketAddress unresolvedRemote = InetSocketAddress.createUnresolved(host.getHostName(), remoteAddress.getPort());
            return super.connectSocket(connectTimeout, socket, host, unresolvedRemote, localAddress, context);
        }
    }

    static class FakeDnsResolver implements DnsResolver {
        @Override
        public InetAddress[] resolve(String host) throws UnknownHostException {
            return new InetAddress[]{InetAddress.getByAddress(new byte[]{1, 1, 1, 1})};
        }

        @Override
        public String resolveCanonicalHostname(String host) throws UnknownHostException {
            throw new UnsupportedOperationException("Unimplemented method 'resolveCanonicalHostname'");
        }
    }
}
