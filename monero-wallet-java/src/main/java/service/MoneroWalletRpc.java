package service;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

import model.MoneroAccount;
import model.MoneroAddressBookEntry;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import model.MoneroKeyImage;
import model.MoneroPayment;
import model.MoneroSubaddress;
import model.MoneroTx;
import model.MoneroTxConfig;
import model.MoneroTxFilter;
import model.MoneroUri;
import model.MoneroTx.MoneroTxType;
import service.rpc.MoneroRpcException;
import service.rpc.MoneroWalletRpcOld;
import types.HttpException;
import types.Pair;
import utils.JsonUtils;
import utils.MoneroUtils;
import utils.StreamUtils;

/**
 * Implements a Monero Wallet using a monero-wallet-rpc backend.
 */
public class MoneroWalletRpc extends MoneroWalletDefault {
  
  // logger
  private static final Logger LOGGER = Logger.getLogger(MoneroWalletRpcOld.class);

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

  @Override
  public int getHeight() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getMnemonicSeed() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts(String tag) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount getAccount(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount createAccount(String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, Collection<Integer> subaddressIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTx send(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> sendSplit(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> sweepAll(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> sweepDust() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs(MoneroTxFilter filter) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTx getTx(String txId, Integer accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setTxNotes(List<String> txIds, List<String> txNotes) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxNotes(List<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public Map<String, BigInteger> importKeyImages(List<MoneroKeyImage> keyImages) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int addAddressBookEntry(String address, String paymentId, String description) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getLanguages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void createWallet(String filename, String password, String language) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void openWallet(String filename, String password) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String sign(String data) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean verify(String data, String address, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public URI toUri(MoneroUri moneroUri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroUri toMoneroUri(URI uri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void saveBlockchain() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void rescanBlockchain() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void rescanSpent() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopWallet() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void startMining(int numThreads, boolean backgroundMining, boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopMining() {
    throw new RuntimeException("Not implemented");
  }
  
  // -------------------------------- PRIVATE ---------------------------------
  
  @SuppressWarnings("unchecked")
  private Pair<BigInteger, BigInteger> getBalances() {
    Map<String, Object> respMap = sendRpcRequest("getbalance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return new Pair<BigInteger, BigInteger>((BigInteger) resultMap.get("balance"), (BigInteger) resultMap.get("unlocked_balance"));
  }

  /**
   * Initializes a MoneroTx from a transaction response map.
   * 
   * @param txMap is the map to initialize the transaction from
   * @return MoneroTx is the initialized transaction
   */
  @SuppressWarnings("unchecked")
  private MoneroTx interpretTx(Map<String, Object> txMap) {
    MoneroTx tx = new MoneroTx();
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
      else if (key.equalsIgnoreCase("type")) tx.setType(getTxType((String) val));
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
    } catch (MoneroRpcException e2) {
      throw e2;
    } catch (Exception e3) {
      throw new MoneroException(e3);
    }
  }
  
  // ------------------------------ STATIC UTILITIES --------------------------

  private static void addTx(Map<MoneroTxType, Map<String, MoneroTx>> txTypeMap, MoneroTx tx) {
    if (tx.getType() == null) throw new MoneroException("Transaction type cannot be null: \n" + tx.toString());
    if (tx.getHash() == null) throw new MoneroException("Transaction hash cannot be null: \n" + tx.getHash());
    Map<String, MoneroTx> txHashMap = txTypeMap.get(tx.getType());
    if (txHashMap == null) {
      txHashMap = new HashMap<String, MoneroTx>();
      txTypeMap.put(tx.getType(), txHashMap);
    }
    MoneroTx targetTx = txHashMap.get(tx.getHash());
    if (targetTx == null) {
      txHashMap.put(tx.getHash(), tx);
    } else {
      targetTx.merge(tx);
    }
  }

  private static MoneroTxType getTxType(String type) {
    if (type == null) throw new MoneroException("Transaction type is null");
    else if (type.equalsIgnoreCase("in")) return MoneroTxType.INCOMING;
    else if (type.equalsIgnoreCase("out")) return MoneroTxType.OUTGOING;
    else if (type.equalsIgnoreCase("pending")) return MoneroTxType.PENDING;
    else if (type.equalsIgnoreCase("failed")) return MoneroTxType.FAILED;
    else if (type.equalsIgnoreCase("pool")) return MoneroTxType.MEMPOOL;
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
    throw new MoneroRpcException(code, message, requestBody);
  }
  
  private static URI parseUri(String endpoint) {
    try {
      return new URI(endpoint);
    } catch (Exception e) {
      throw new MoneroException(e);
    }
  }
}
