package monero.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import common.utils.JsonUtils;

import org.apache.hc.client5.http.DnsResolver;
import org.apache.hc.client5.http.auth.AuthScope;
import org.apache.hc.client5.http.auth.UsernamePasswordCredentials;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpUriRequest;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.auth.BasicCredentialsProvider;
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
import java.util.Objects;

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
    protected String username;
    protected String password;
    protected String zmqUri;
    protected String proxyUri;
    protected int priority = 0;
    protected Boolean isOnline;
    protected Long responseTime;
    protected Boolean isAuthenticated;
    protected Boolean printStackTrace;
    protected Map<String, Object> attributes = new HashMap<>();


    protected MoneroHttpConnection(URI uri, String username, String password, URI zmqUri) {
        this.uri = uri == null ? null : MoneroUtils.parseUri(uri.toString()).toString();
        this.zmqUri = zmqUri == null ? null : MoneroUtils.parseUri(zmqUri.toString()).toString();
        this.setCredentials(username, password);
    }

    protected MoneroHttpConnection(String uri, String username, String password, String zmqUri)
    {
        this(MoneroUtils.parseUri(uri), username, password, MoneroUtils.parseUri(zmqUri));
    }

    protected MoneroHttpConnection(String uri, String username, String password)
    {
        this(MoneroUtils.parseUri(uri), username, password);
    }

    protected MoneroHttpConnection(URI uri, String username, String password)
    {
        this(uri, username, password, null);
    }

    protected MoneroHttpConnection(URI uri, URI zqmUri)
    {
        this(uri, null, null, zqmUri);
    }

    protected MoneroHttpConnection(URI uri) {
        this(uri, null);
    }

    protected MoneroHttpConnection(String uri, String zmqUri)
    {
        this(MoneroUtils.parseUri(uri), MoneroUtils.parseUri(uri));
    }

    protected MoneroHttpConnection(String uri) {
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
        setCredentials(username, password);
        return this;
    }

    public MoneroHttpConnection setCredentials(String username, String password) {
        try { if (this.client != null) this.client.close(); }
        catch (IOException e) { throw new MoneroError(e); }
        if ("".equals(username)) username = null;
        if ("".equals(password)) password = null;
        if (username != null || password != null) {
            if (username == null) throw new MoneroError("username cannot be empty because password is not empty");
            if (password == null) throw new MoneroError("password cannot be empty because username is not empty");
            URI uriObj = MoneroUtils.parseUri(uri);
            BasicCredentialsProvider creds = new BasicCredentialsProvider();
            creds.setCredentials(new AuthScope(uriObj.getHost(), uriObj.getPort()), new UsernamePasswordCredentials(username, password.toCharArray()));
            this.client = HttpClients.custom().setDefaultCredentialsProvider(creds).build();
        } else {
            this.client = HttpClients.createDefault();
        }
        if (!Objects.equals(this.username, username) || !Objects.equals(this.password, password)) {
            isOnline = null;
            isAuthenticated = null;
        }
        this.username = username;
        this.password = password;
        return this;
    }

    public String getZmqUri() {
        return zmqUri;
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }


    public int getPriority() {
        return priority;
    }

    public MoneroHttpConnection setZmqUri(String zmqUri) {
        this.zmqUri = zmqUri;
        return this;
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

    /**
     * Check the connection and update online, authentication, and response time status.
     *
     * @param timeoutMs the maximum response time before considered offline
     * @return true if there is a change in status, false otherwise
     */
    public abstract boolean checkConnection(long timeoutMs);

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

    protected static void validateHttpResponse(CloseableHttpResponse resp) {
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

    protected RequestConfig getRequestConfig(Long timeoutInMs) {
        RequestConfig.Builder builder = RequestConfig.custom();
        if (timeoutInMs != null) {
            builder.setConnectTimeout(Timeout.ofMilliseconds(timeoutInMs));
            builder.setConnectionRequestTimeout(Timeout.ofMilliseconds(timeoutInMs));
            builder.setResponseTimeout(Timeout.ofMilliseconds(timeoutInMs));
        }
        return builder.build();
    }

    protected CloseableHttpResponse request(HttpUriRequest request) throws IOException, URISyntaxException {
        return proxyUri == null ? client.execute(request) : requestWithProxy(request);
    }

    protected CloseableHttpResponse requestWithProxy(HttpUriRequest request) throws IOException {

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
