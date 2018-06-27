package service.rpc;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.log4j.Logger;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import model.MoneroAddress;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import model.MoneroOutput;
import model.MoneroPayment;
import model.MoneroTransaction;
import model.MoneroTransaction.MoneroTransactionType;
import model.MoneroUri;
import service.MoneroWallet;
import types.HttpException;
import types.Pair;
import utils.JsonUtils;
import utils.MoneroUtils;
import utils.StreamUtils;

/**
 * Implements a Monero wallet backed by a Monero wallet RPC endpoint.
 */
public class MoneroWalletRpc implements MoneroWallet {

  // logger
  private static final Logger LOGGER = Logger.getLogger(MoneroWalletRpc.class);

  // custom mapper to deserialize integers to BigIntegers
  public static ObjectMapper MAPPER;
  static {
    MAPPER = new ObjectMapper();
    MAPPER.setSerializationInclusion(Include.NON_NULL);
    MAPPER.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
    MAPPER.configure(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS, true);
  }

  // instance variables
  private String rpcHost;
  private int rpcPort;
  private URI rpcUri;
  private HttpClient client;

  public MoneroWalletRpc(String endpoint) {
    this(parseUri(endpoint));
  }

  public MoneroWalletRpc(URI rpcUri) {
    this.rpcUri = rpcUri;
    this.rpcHost = rpcUri.getHost();
    this.rpcPort = rpcUri.getPort();
    this.client = HttpClients.createDefault();
  }

  public MoneroWalletRpc(String rpcHost, int rpcPort) throws URISyntaxException {
    this.rpcHost = rpcHost;
    this.rpcPort = rpcPort;
    this.rpcUri = new URI("http", null, rpcHost, rpcPort, "/json_rpc", null, null);
    this.client = HttpClients.createDefault();
  }

  public MoneroWalletRpc(String rpcHost, int rpcPort, String username, String password) throws URISyntaxException {
    this.rpcHost = rpcHost;
    this.rpcPort = rpcPort;
    this.rpcUri = new URI("http", null, rpcHost, rpcPort, "/json_rpc", null, null);
    CredentialsProvider creds = new BasicCredentialsProvider();
    creds.setCredentials(new AuthScope(rpcUri.getHost(), rpcUri.getPort()), new UsernamePasswordCredentials(username, password));
    this.client = HttpClients.custom().setDefaultCredentialsProvider(creds).build();
  }

  public String getRpcHost() {
    return rpcHost;
  }

  public int getRpcPort() {
    return rpcPort;
  }

  public URI getRpcUri() {
    return rpcUri;
  }

  public BigInteger getBalance() {
    return getBalances().getFirst();
  }

  public BigInteger getUnlockedBalance() {
    return getBalances().getSecond();
  }

  public int getHeight() {
    Map<String, Object> respMap = sendRpcRequest("getheight", null);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return ((BigInteger) resultMap.get("height")).intValue();
  }

  public MoneroAddress getStandardAddress() {
    Map<String, Object> respMap = sendRpcRequest("getaddress", null);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    String standardAddress = (String) resultMap.get("address");
    MoneroAddress address = new MoneroAddress(standardAddress);
    MoneroUtils.validateAddress(address);
    return address;
  }

  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    if (paymentId != null) paramMap.put("payment_id", paymentId);
    Map<String, Object> respMap = sendRpcRequest("make_integrated_address", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    paymentId = (String) resultMap.get("payment_id");
    String integratedAddress = (String) resultMap.get("integrated_address");
    MoneroIntegratedAddress address = new MoneroIntegratedAddress(getStandardAddress().getStandardAddress(), paymentId, integratedAddress);
    MoneroUtils.validateAddress(address);
    return address;
  }

  public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress) {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("integrated_address", integratedAddress);
    Map<String, Object> respMap = sendRpcRequest("split_integrated_address", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroIntegratedAddress address = new MoneroIntegratedAddress((String) resultMap.get("standard_address"), (String) resultMap.get("payment_id"), integratedAddress);
    MoneroUtils.validateAddress(address);
    return address;
  }

  public String getMnemonicSeed() {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "mnemonic");
    Map<String, Object> respMap = sendRpcRequest("query_key", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }

  public String getViewKey() {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "view_key");
    Map<String, Object> respMap = sendRpcRequest("query_key", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }

  public URI toUri(MoneroUri moneroUri) {
    if (moneroUri == null) throw new MoneroException("Given Monero URI is null");
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("address", moneroUri.getAddress());
    paramMap.put("amount", moneroUri.getAmount() == null ? null : moneroUri.getAmount());
    paramMap.put("payment_id", moneroUri.getPaymentId());
    paramMap.put("recipient_name", moneroUri.getRecipientName());
    paramMap.put("tx_description", moneroUri.getTxDescription());
    Map<String, Object> respMap = sendRpcRequest("make_uri", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return parseUri((String) resultMap.get("uri"));
  }

  public MoneroUri toMoneroUri(URI uri) {
    if (uri == null) throw new MoneroException("Given URI is null");
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("uri", uri.toString());
    Map<String, Object> respMap = sendRpcRequest("parse_uri", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) ((Map<String, Object>) respMap.get("result")).get("uri");
    MoneroUri mUri = new MoneroUri();
    mUri.setAddress((String) resultMap.get("address"));
    if ("".equals(mUri.getAddress())) mUri.setAddress(null);
    mUri.setAmount((BigInteger) resultMap.get("amount"));
    mUri.setPaymentId((String) resultMap.get("payment_id"));
    if ("".equals(mUri.getPaymentId())) mUri.setPaymentId(null);
    mUri.setRecipientName((String) resultMap.get("recipient_name"));
    if ("".equals(mUri.getRecipientName())) mUri.setRecipientName(null);
    mUri.setTxDescription((String) resultMap.get("tx_description"));
    if ("".equals(mUri.getTxDescription())) mUri.setTxDescription(null);
    return mUri;
  }

  public void saveBlockchain() {
    sendRpcRequest("store", null);
  }

  public void stopWallet() {
    sendRpcRequest("stop_wallet", null);
  }

  @SuppressWarnings("unchecked")
  public MoneroTransaction send(List<MoneroPayment> payments, String paymentId, int mixin, int unlockTime) {

    // build parameter map
    Map<String, Object> paramMap = new HashMap<String, Object>();
    List<Map<String, Object>> destinations = new ArrayList<Map<String, Object>>();
    paramMap.put("destinations", destinations);
    for (MoneroPayment payment : payments) {
      Map<String, Object> destination = new HashMap<String, Object>();
      destination.put("address", payment.getAddress().toString());
      destination.put("amount", payment.getAmount());
      destinations.add(destination);
    }
    paramMap.put("payment_id", paymentId);
    paramMap.put("mixin", mixin);
    paramMap.put("unlockTime", unlockTime);
    paramMap.put("get_tx_key", true);

    // send request
    Map<String, Object> respMap = sendRpcRequest("transfer", paramMap);

    // interpret response
    Map<String, Object> txMap = (Map<String, Object>) respMap.get("result");
    MoneroTransaction tx = interpretTransaction(txMap);
    tx.setPayments(payments);
    tx.setMixin(mixin);
    tx.setUnlockTime(unlockTime);
    return tx;
  }

  @SuppressWarnings("unchecked")
  public List<MoneroTransaction> sendSplit(List<MoneroPayment> payments, String paymentId, int mixin, int unlockTime, Boolean newAlgorithm) {

    // build parameter map
    Map<String, Object> paramMap = new HashMap<String, Object>();
    List<Map<String, Object>> destinations = new ArrayList<Map<String, Object>>();
    paramMap.put("destinations", destinations);
    for (MoneroPayment payment : payments) {
      Map<String, Object> destination = new HashMap<String, Object>();
      destination.put("address", payment.getAddress().toString());
      destination.put("amount", payment.getAmount());
      destinations.add(destination);
    }
    paramMap.put("payment_id", paymentId);
    paramMap.put("mixin", mixin);
    paramMap.put("unlockTime", unlockTime);
    paramMap.put("new_algorithm", newAlgorithm);

    // send request
    Map<String, Object> respMap = sendRpcRequest("transfer_split", paramMap);

    // interpret response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<BigInteger> fees = (List<BigInteger>) resultMap.get("fee_list");
    List<String> txHashes = (List<String>) resultMap.get("tx_hash_list");
    List<MoneroTransaction> transactions = new ArrayList<MoneroTransaction>();
    for (int i = 0; i < fees.size(); i++) {
      MoneroTransaction tx = new MoneroTransaction();
      tx.setFee(fees.get(i));
      tx.setMixin(mixin);
      tx.setHash(txHashes.get(0));
      transactions.add(tx);
      tx.setUnlockTime(unlockTime);
    }
    return transactions;
  }

  @SuppressWarnings("unchecked")
  public List<MoneroTransaction> sweepDust() {

    // send request
    Map<String, Object> respMap = sendRpcRequest("sweep_dust", null);

    // interpret response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<String> txHashes = (List<String>) resultMap.get("tx_hash_list");
    List<MoneroTransaction> txs = new ArrayList<MoneroTransaction>();
    if (txHashes == null) return txs;
    for (String txHash : txHashes) {
      MoneroTransaction tx = new MoneroTransaction();
      tx.setHash(txHash);
      txs.add(tx);
    }
    return txs;
  }

  public List<MoneroTransaction> getAllTransactions() {
    return getTransactions(true, true, true, true, true, null, null, null);
  }

  @SuppressWarnings("unchecked")
  public List<MoneroTransaction> getTransactions(boolean getIncoming, boolean getOutgoing, boolean getPending, boolean getFailed, boolean getMemPool, Collection<String> paymentIds, Integer minHeight, Integer maxHeight) {

    // collect transactions bucketed by type then hash
    Map<MoneroTransactionType, Map<String, MoneroTransaction>> txTypeMap = new HashMap<MoneroTransactionType, Map<String, MoneroTransaction>>();

    // get_transfers rpc call
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("in", getIncoming);
    paramMap.put("out", getOutgoing);
    paramMap.put("pending", getPending);
    paramMap.put("failed", getFailed);
    paramMap.put("pool", getMemPool);
    paramMap.put("filter_by_height", false);
    Map<String, Object> respMap = sendRpcRequest("get_transfers", paramMap);

    // interpret get_transfers response
    Map<String, Object> result = (Map<String, Object>) respMap.get("result");
    for (String key : result.keySet()) {
      for (Map<String, Object> txMap : (List<Map<String, Object>>) result.get(key)) {

        // build transaction
        MoneroTransaction tx = interpretTransaction(txMap);
        MoneroPayment payment = new MoneroPayment(tx, null, (BigInteger) txMap.get("amount"));
        List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
        payments.add(payment);
        tx.setPayments(payments);
        addTransaction(txTypeMap, tx);
      }
    }

    if (getIncoming) {

      // incoming_transfers rpc call to get incoming outputs
      paramMap = new HashMap<String, Object>();
      paramMap.put("transfer_type", "all");
      respMap = sendRpcRequest("incoming_transfers", paramMap);
      result = (Map<String, Object>) respMap.get("result");

      // interpret incoming_transfers response
      List<Map<String, Object>> outputMaps = (List<Map<String, Object>>) result.get("transfers");
      if (outputMaps == null) return new ArrayList<MoneroTransaction>();
      for (Map<String, Object> outputMap : outputMaps) {
        MoneroOutput output = new MoneroOutput();
        output.setAmount((BigInteger) outputMap.get("amount"));
        output.setIsSpent((Boolean) outputMap.get("spent"));
        MoneroTransaction tx = interpretTransaction(outputMap);
        tx.setType(MoneroTransactionType.INCOMING);
        output.setTransaction(tx);
        List<MoneroOutput> outputs = new ArrayList<MoneroOutput>();
        outputs.add(output);
        tx.setOutputs(outputs);
        addTransaction(txTypeMap, tx);
      }

      // get_bulk_payments rpc call to get incoming payments by id
      if (paymentIds != null && !paymentIds.isEmpty()) {
        paramMap = new HashMap<String, Object>();
        paramMap.put("payment_ids", paymentIds);
        respMap = sendRpcRequest("get_bulk_payments", paramMap);
        result = (Map<String, Object>) respMap.get("result");

        // interpret get_bulk_payments response
        List<Map<String, Object>> paymentMaps = (List<Map<String, Object>>) result.get("payments");
        for (Map<String, Object> paymentMap : paymentMaps) {
          MoneroTransaction tx = interpretTransaction(paymentMap);
          tx.setType(MoneroTransactionType.INCOMING);
          // payment data is redundant with get_transfers rpc call, so it's not added because merging would create duplicates
          // MoneroPayment payment = new MoneroPayment();
          // payment.setAmount((BigInteger) paymentMap.get("amount"));
          // List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
          // payments.add(payment);
          // tx.setPayments(payments);
          addTransaction(txTypeMap, tx);
        }
      }
    }

    // build return type
    List<MoneroTransaction> txs = new ArrayList<MoneroTransaction>();
    for (Entry<MoneroTransactionType, Map<String, MoneroTransaction>> entry : txTypeMap.entrySet()) {
      txs.addAll(entry.getValue().values());
    }

    // filter final results
    Collection<MoneroTransaction> toRemoves = new HashSet<MoneroTransaction>();
    for (MoneroTransaction tx : txs) {
      if (paymentIds != null && !paymentIds.contains(tx.getPaymentId())) toRemoves.add(tx);
      else if (minHeight != null && (tx.getHeight() == null || tx.getHeight() < minHeight)) toRemoves.add(tx);
      else if (maxHeight != null && (tx.getHeight() == null || tx.getHeight() > maxHeight)) toRemoves.add(tx);
    }
    txs.removeAll(toRemoves);
    return txs;
  }

  private static URI parseUri(String endpoint) {
    try {
      return new URI(endpoint);
    } catch (Exception e) {
      throw new MoneroException(e);
    }
  }

  @SuppressWarnings("unchecked")
  private Pair<BigInteger, BigInteger> getBalances() {
    Map<String, Object> respMap = sendRpcRequest("getbalance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return new Pair<BigInteger, BigInteger>((BigInteger) resultMap.get("balance"), (BigInteger) resultMap.get("unlocked_balance"));
  }

  /**
   * Initializes a MoneroTransaction from a transaction response map.
   * 
   * @param txMap is the map to initialize the transaction from
   * @return MoneroTransaction is the initialized transaction
   */
  @SuppressWarnings("unchecked")
  private MoneroTransaction interpretTransaction(Map<String, Object> txMap) {
    MoneroTransaction tx = new MoneroTransaction();
    for (String key : txMap.keySet()) {
      Object val = txMap.get(key);
      if (key.equals("amount")) {
      } // this method does not process amount since it could be output or payment depending on context
      else if (key.equals("spent")) {
      } // this method does not process spent which is specific to outputs
      else if (key.equalsIgnoreCase("fee")) tx.setFee((BigInteger) val);
      else if (key.equalsIgnoreCase("height")) tx.setHeight(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("block_height")) tx.setHeight(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("note")) tx.setNote((String) val);
      else if (key.equalsIgnoreCase("payment_id")) tx.setPaymentId((String) val);
      else if (key.equalsIgnoreCase("timestamp")) tx.setTimestamp(((BigInteger) val).longValue());
      else if (key.equalsIgnoreCase("tx_hash")) tx.setHash((String) val);
      else if (key.equalsIgnoreCase("tx_key")) tx.setKey((String) val);
      else if (key.equalsIgnoreCase("txid")) tx.setHash((String) val);
      else if (key.equalsIgnoreCase("type")) tx.setType(getTransactionType((String) val));
      else if (key.equalsIgnoreCase("tx_size")) tx.setSize(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("unlock_time")) tx.setUnlockTime(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("global_index")) {
      } // ignore
      else if (key.equalsIgnoreCase("destinations")) {
        List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
        tx.setPayments(payments);
        for (Map<String, Object> paymentMap : (List<Map<String, Object>>) val) {
          MoneroPayment payment = new MoneroPayment();
          for (String paymentKey : paymentMap.keySet()) {
            if (paymentKey.equals("address")) payment.setAddress(MoneroUtils.toAddress((String) paymentMap.get(paymentKey), this));
            else if (paymentKey.equals("amount")) payment.setAmount((BigInteger) paymentMap.get(paymentKey));
            else throw new MoneroException("Unrecognized transaction destination field: " + paymentKey);
          }
        }
      } else LOGGER.warn("Ignoring unexpected transaction field: '" + key + "'");
    }
    return tx;
  }
  
  /**
   * Sends a request to the RPC API.
   * 
   * @param method specifies the method to request
   * @param params specifies input parameters
   * @return Map<String, Object> is the RPC API response as a map
   */
  private Map<String, Object> sendRpcRequest(String method, Map<String, Object> params) {

    // send http request
    try {

      // build request body
      Map<String, Object> body = new HashMap<String, Object>();
      body.put("jsonrpc", "2.0");
      body.put("id", "0");
      body.put("method", method);
      if (params != null) body.put("params", params);
      LOGGER.debug("Sending method '" + method + "' with body: " + JsonUtils.serialize(body));

      // send http request and validate response
      HttpPost post = new HttpPost(rpcUri);
      HttpEntity entity = new StringEntity(JsonUtils.serialize(body));
      post.setEntity(entity);
      HttpResponse resp = client.execute(post);
      validateHttpResponse(resp);

      // deserialize response
      Map<String, Object> respMap = JsonUtils.toMap(MAPPER, StreamUtils.streamToString(resp.getEntity().getContent()));
      LOGGER.debug("Received response to method '" + method + "': " + JsonUtils.serialize(respMap));
      EntityUtils.consume(resp.getEntity());

      // check RPC response for errors
      validateRpcResponse(respMap, body);
      return respMap;
    } catch (HttpException e1) {
      throw e1;
    } catch (MoneroExceptionRpc e2) {
      throw e2;
    } catch (Exception e3) {
      throw new MoneroException(e3);
    }
  }
  
  // ------------------------------ STATIC UTILITIES --------------------------

  private static void addTransaction(Map<MoneroTransactionType, Map<String, MoneroTransaction>> txTypeMap, MoneroTransaction tx) {
    if (tx.getType() == null) throw new MoneroException("Transaction type cannot be null: \n" + tx.toString());
    if (tx.getHash() == null) throw new MoneroException("Transaction hash cannot be null: \n" + tx.getHash());
    Map<String, MoneroTransaction> txHashMap = txTypeMap.get(tx.getType());
    if (txHashMap == null) {
      txHashMap = new HashMap<String, MoneroTransaction>();
      txTypeMap.put(tx.getType(), txHashMap);
    }
    MoneroTransaction targetTx = txHashMap.get(tx.getHash());
    if (targetTx == null) {
      txHashMap.put(tx.getHash(), tx);
    } else {
      targetTx.merge(tx);
    }
  }

  private static MoneroTransactionType getTransactionType(String type) {
    if (type == null) throw new MoneroException("Transaction type is null");
    else if (type.equalsIgnoreCase("in")) return MoneroTransactionType.INCOMING;
    else if (type.equalsIgnoreCase("out")) return MoneroTransactionType.OUTGOING;
    else if (type.equalsIgnoreCase("pending")) return MoneroTransactionType.PENDING;
    else if (type.equalsIgnoreCase("failed")) return MoneroTransactionType.FAILED;
    else if (type.equalsIgnoreCase("pool")) return MoneroTransactionType.MEMPOOL;
    throw new MoneroException("Unrecognized transaction type: " + type);
  }

  private static void validateHttpResponse(HttpResponse resp) {
    int code = resp.getStatusLine().getStatusCode();
    if (code < 200 || code > 299) {
      String content = null;
      try {
        content = StreamUtils.streamToString(resp.getEntity().getContent());
      } catch (Exception e) {
        // could not get content
      }
      throw new HttpException(code, resp.getStatusLine().getReasonPhrase() + (content != null ? (": " + content) : ""));
    }
  }

  @SuppressWarnings("unchecked")
  private static void validateRpcResponse(Map<String, Object> respMap, Map<String, Object> requestBody) {
    Map<String, Object> error = (Map<String, Object>) respMap.get("error");
    if (error == null) return;
    int code = ((BigInteger) error.get("code")).intValue();
    String message = (String) error.get("message");
    throw new MoneroExceptionRpc(code, message, requestBody);
  }
}
