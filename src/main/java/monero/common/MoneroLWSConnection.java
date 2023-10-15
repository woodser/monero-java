package monero.common;

import common.utils.JsonUtils;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;

import java.util.HashMap;
import java.util.Map;

public class MoneroLWSConnection extends MoneroHttpConnection {

    private String auth;
    private String adminPath;
    private Long timeoutInMs;

    public MoneroLWSConnection(String uri) {
        this(uri, "");
    }

    public MoneroLWSConnection(String uri, String auth)
    {
        this(uri, auth, 180000L);
    }

    public MoneroLWSConnection(String uri, String auth, Long timeoutInMs) {
        super(uri);

        this.auth = auth;
        this.timeoutInMs = timeoutInMs;
    }

    @Override
    public Boolean isAuthenticated() {
        return auth != null && !auth.isEmpty();
    }

    public Boolean requiresAuthentication()
    {
        return false;
    }

    /**
     * Check the connection and update online, authentication, and response time status.
     *
     * @param timeoutMs the maximum response time before considered offline
     * @return true if there is a change in status, false otherwise
     */
    public boolean checkConnection(long timeoutMs)
    {
        return true;
    }

    public void setTimeout(Long timeoutInMs)
    {
        this.timeoutInMs = timeoutInMs;
    }

    public Long getTimeout()
    {
        return this.timeoutInMs;
    }


    /**
     * Send a request to the RPC API.
     *
     * @param method specifies the method to request
     * @return the RPC API response as a map
     */
    protected Map<String, Object> sendJsonRequest(String method) {
        return sendJsonRequest(method, (Map<String, Object>) null);
    }

    /**
     * Send a request to the RPC API.
     *
     * @param method is the method to request
     * @param params are the request's input parameters (supports &lt;Map&lt;String, Object&gt;, List&lt;Object&gt;&lt;/code&gt;, String, etc.)
     * @return the RPC API response as a map
     */
    protected Map<String, Object> sendJsonRequest(String method, Object params) {
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
    protected Map<String, Object> sendJsonRequest(String method, Object params, Long timeoutInMs) {
        CloseableHttpResponse resp = null;
        try {

            // build request body
            //Map<String, Object> body = new HashMap<String, Object>();
            //body.put("jsonrpc", "2.0");
            //body.put("id", "0");
            //body.put("method", method);
            //if (params != null) body.put("params", params);

            // send http request
            HttpPost post = new HttpPost(uri + "/" + method);
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

            return respMap;
        } catch (MoneroRpcError e1) {
            throw e1;
        } catch (Exception e2) {
            throw new MoneroError(e2);
        } finally {
            try { if (resp != null) resp.close(); }
            catch (Exception ignored) {}
        }
    }

    protected Map<String, Object> sendAdminJsonRequest(String method)
    {
        return sendAdminJsonRequest(method, null);
    }

    protected Map<String, Object> sendAdminJsonRequest(String method, Object params)
    {
        return sendAdminJsonRequest(method, params, null);
    }

    protected Map<String, Object> sendAdminJsonRequest(String method, Object params, Long timeoutInMs)
    {
        Map<String, Object> body = new HashMap<>();

        if (isAuthenticated() && requiresAuthentication())
        {
            body.put("auth", auth);
            body.put("params", params);

            return sendJsonRequest(method, body, timeoutInMs);
        }
        else if (requiresAuthentication())
        {
            throw new MoneroLWSError("Unauthorized ", 401);
        }

        return sendJsonRequest(method, params, timeoutInMs);
    }

    public String getAuth() {
        return this.auth;
    }

    public void setAuth(String auth)
    {
        this.auth = auth;
    }

    public Map<String, Object> getAddressInfo(String address, String privateViewKey) {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        return this.sendJsonRequest("get_address_info", params);
    }

    public Map<String, Object> getAddressTxs(String address, String privateViewKey) {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        return this.sendJsonRequest("get_address_txs", params);
    }

    public Map<String, Object> getRandomOuts(Long count, String[] amounts) {
        Map<String, Object> params = new HashMap<>();
        params.put("count", count);
        params.put("amounts", amounts);
        return this.sendJsonRequest("get_random_outs", params);
    }

    public Map<String, Object> getUnspentOuts(
            String address, String privateViewKey, String amount,
            Long mixin, Boolean useDust
    ) {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        params.put("amount", amount);
        params.put("mixin", mixin);
        params.put("use_dust", useDust);
        return this.sendJsonRequest("get_random_outs", params);
    }

    public Map<String, Object> getUnspentOuts(
            String address, String privateViewKey, String amount,
            Long mixin, Boolean useDust, String dustThreshold
    )
    {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        params.put("amount", amount);
        params.put("mixin", mixin);
        params.put("use_dust", useDust);
        params.put("dust_threshold", dustThreshold);
        return this.sendJsonRequest("get_random_outs", params);
    }

    public Map<String, Object> importRequest(String address, String privateViewKey)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        return this.sendJsonRequest("import_request", params);
    }

    public Map<String, Object> login(String address, String privateViewKey)
    {
        return this.login(address, privateViewKey, false);
    }

    public Map<String, Object> login(String address, String privateViewKey, Boolean createAccount)
    {
        return this.login(address, privateViewKey, createAccount, false);
    }

    public Map<String, Object> login(String address, String privateViewKey, Boolean createAccount, Boolean generatedLocally)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        params.put("create_account", createAccount);
        params.put("generated_locally", generatedLocally);
        return this.sendJsonRequest("login", params);
    }

    public Map<String, Object> submitRawTx(String tx)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("tx", tx);
        return this.sendJsonRequest("submit_raw_tx", params);
    }

    // ------- ADMIN -------

    public void acceptRequests(String type, String[] addresses)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("type", type);
        params.put("addresses", addresses);
        this.sendAdminJsonRequest("accept_requests", params);
    }

    public void addAccount(String address, String key)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("key", key);
        this.sendAdminJsonRequest("add_account", params);
    }

    public Map<String, Object> listAccounts()
    {
        return this.sendAdminJsonRequest("list_accounts");
    }

    public Map<String, Object> listRequests()
    {
        return this.sendAdminJsonRequest("list_requests");
    }

    public void modifyAccountStatus(String status, String[] addresses)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("status", status);
        params.put("addresses", addresses);

        this.sendAdminJsonRequest("modify_account_status", params);
    }

    public void rejectRequests(String type, String[] addresses)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("type", type);
        params.put("addresses", addresses);

        this.sendAdminJsonRequest("reject_requests", params);
    }

    public void rescan(Long height, String[] addresses)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("height", height);
        params.put("addresses", addresses);

        this.sendAdminJsonRequest("rescan", params);
    }

    public Map<String, Object> validate(String viewPublicHex, String spendPublicHex, String viewKeyHex)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("view_public_hex", viewPublicHex);
        params.put("spend_public_hex", spendPublicHex);
        params.put("view_key_hex", viewKeyHex);

        return this.sendAdminJsonRequest("validate", params);
    }

    public Map<String, Object> webhookAdd(String type, String address, String url, String paymentId, String token)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("type", type);
        params.put("address", address);
        params.put("url", url);
        params.put("payment_id", paymentId);
        params.put("token", token);

        if (this.isAuthenticated)
        {
            return this.sendAdminJsonRequest("webhook_add", params);
        }

        Map<String, Object> body = new HashMap<>();
        body.put("params", params);

        return this.sendJsonRequest("webhook_add", body);
    }

    public void webhookDelete(String[] addresses)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("addresses", addresses);

        this.sendAdminJsonRequest("webhook_delete", params);
    }

    public void webhookDeleteUUID(String[] eventIds)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("event_ids", eventIds);

        this.sendAdminJsonRequest("webhook_delete_uuid", params);
    }

    public Map<String, Object> webhookList()
    {
        return this.sendAdminJsonRequest("webhook_list");
    }
}
