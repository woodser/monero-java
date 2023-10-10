package monero.common;

import java.util.HashMap;
import java.util.Map;

public class MoneroLWSConnection extends MoneroHttpConnection {

    public MoneroLWSConnection(String uri) {
        super(uri);
    }

    public Map<String, Object> getAddressInfo(String address, String privateViewKey)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        return this.sendJsonRequest("get_address_info", params);
    }

    public Map<String, Object> getAddressTxs(String address, String privateViewKey)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("address", address);
        params.put("view_key", privateViewKey);
        return this.sendJsonRequest("get_address_txs", params);
    }

    public Map<String, Object> getRandomOuts(Long count, String[] amounts)
    {
        Map<String, Object> params = new HashMap<>();
        params.put("count", count);
        params.put("amounts", amounts);
        return this.sendJsonRequest("get_random_outs", params);
    }

    public Map<String, Object> getUnspentOuts(
            String address, String privateViewKey, String amount,
            Long mixin, Boolean useDust
    )
    {
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

}
