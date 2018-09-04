package wallet.rpc;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import model.MoneroAccount;
import model.MoneroAddress;
import model.MoneroAddressBookEntry;
import model.MoneroException;
import model.MoneroIntegratedAddress;
import model.MoneroKeyImage;
import model.MoneroOutput;
import model.MoneroPayment;
import model.MoneroSubaddress;
import model.MoneroTx;
import model.MoneroTx.MoneroTxType;
import model.MoneroTxConfig;
import model.MoneroTxFilter;
import model.MoneroUri;
import types.Pair;
import utils.MoneroUtils;
import wallet.MoneroWallet;
import wallet.MoneroWalletDefault;

/**
 * Implements a Monero Wallet using monero-wallet-rpc.
 */
public class MoneroWalletRpc extends MoneroWalletDefault {
  
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
  
  private MoneroRpc rpc;  // handles rpc interactions

  public MoneroWalletRpc(String endpoint) {
    rpc = new MoneroRpc(endpoint);
  }

  public MoneroWalletRpc(URI rpcUri) {
    rpc = new MoneroRpc(rpcUri);
  }

  public MoneroWalletRpc(String rpcHost, int rpcPort) throws URISyntaxException {
    rpc = new MoneroRpc(rpcHost, rpcPort);
  }

  public MoneroWalletRpc(String rpcHost, int rpcPort, String username, String password) throws URISyntaxException {
    rpc = new MoneroRpc(rpcHost, rpcPort, username, password);
  }
  
  public MoneroRpc getRpc() {
    return rpc;
  }

  @Override
  public int getHeight() {
    Map<String, Object> respMap = rpc.sendRpcRequest("getheight", null);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return ((BigInteger) resultMap.get("height")).intValue();
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
  public MoneroAddress getPrimaryAddress() {
    return getSubaddress(0, 0).getAddress();
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

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, Collection<Integer> subaddressIndices) {
    
    // fetch subaddresses
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    if (subaddressIndices != null) params.put("address_index", subaddressIndices);
    Map<String, Object> respMap = rpc.sendRpcRequest("getaddress", params);
    
    // build subaddresses
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> addresses = (List<Map<String, Object>>) resultMap.get("addresses");
    for (Map<String, Object> address : addresses) {
      MoneroSubaddress subaddress = new MoneroSubaddress();
      subaddresses.add(subaddress);
      subaddress.setIndex(((BigInteger) address.get("address_index")).intValue());
      subaddress.setLabel((String) address.get("label"));
      subaddress.setAddress(MoneroUtils.newAddress((String) address.get("address"), this));
      subaddress.setUsed((boolean) address.get("used"));
    }
    
    return subaddresses;
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public BigInteger getBalance(int accountIdx) {
    Map<String, Object> respMap = rpc.sendRpcRequest("getbalance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (BigInteger) resultMap.get("balance");
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    Map<String, Object> respMap = rpc.sendRpcRequest("getbalance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (BigInteger) resultMap.get("unlocked_balance");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public MoneroTx send(MoneroAddress address, BigInteger amount, Integer mixin) {
    
    // create payment
    MoneroPayment payment = new MoneroPayment();
    payment.setAddress(address);
    payment.setAmount(amount);
    
    // create and send tx config
    MoneroTxConfig txConfig = new MoneroTxConfig();
    txConfig.setDestinations(Arrays.asList(payment));
    txConfig.setMixin(mixin);
    return send(txConfig);
  }
  
  @Override
  public MoneroTx send(String address, String paymentId, BigInteger amount, Integer mixin) {
    return send(MoneroUtils.newAddress(address, paymentId, this), amount, mixin);
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroTx send(MoneroTxConfig config) {
    
    // build parameter map
    Map<String, Object> paramMap = new HashMap<String, Object>();
    List<Map<String, Object>> destinationMaps = new ArrayList<Map<String, Object>>();
    paramMap.put("destinations", destinationMaps);
    for (MoneroPayment destination : config.getDestinations()) {
      Map<String, Object> destinationMap = new HashMap<String, Object>();
      destinationMap.put("address", destination.getAddress().toString());
      destinationMap.put("amount", destination.getAmount());
      destinationMaps.add(destinationMap);
    }
    paramMap.put("payment_id", config.getPaymentId());
    paramMap.put("mixin", config.getMixin());
    paramMap.put("unlock_time", config.getUnlockTime());
    paramMap.put("get_tx_key", true);

    // send request
    Map<String, Object> respMap = rpc.sendRpcRequest("transfer", paramMap);

    // interpret response
    Map<String, Object> txMap = (Map<String, Object>) respMap.get("result");
    MoneroTx tx = interpretTx(txMap, this);
    tx.setPayments(config.getDestinations());
    tx.setMixin(config.getMixin());
    tx.setUnlockTime(config.getUnlockTime());
    return tx;
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
    return getTxs(null);
  }

  // TODO: revisit this method to see if it can be optimized
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> getTxs(MoneroTxFilter filter) {
    if (filter == null) filter = new MoneroTxFilter();
    
    // collect transactions bucketed by type then hash
    Map<MoneroTxType, Map<String, MoneroTx>> txTypeMap = new HashMap<MoneroTxType, Map<String, MoneroTx>>();

    // get_transfers rpc call
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("in", filter.isIncoming());
    paramMap.put("out", filter.isOutgoing());
    paramMap.put("pending", filter.isPending());
    paramMap.put("failed", filter.isFailed());
    paramMap.put("pool", filter.isMempool());
    paramMap.put("filter_by_height", filter.getMinHeight() != null || filter.getMaxHeight() != null);
    if (filter.getMinHeight() != null) paramMap.put("min_height", filter.getMinHeight());
    if (filter.getMaxHeight() != null) paramMap.put("max_height", filter.getMaxHeight());
    Map<String, Object> respMap = rpc.sendRpcRequest("get_transfers", paramMap);

    // interpret get_transfers response
    Map<String, Object> result = (Map<String, Object>) respMap.get("result");
    for (String key : result.keySet()) {
      for (Map<String, Object> txMap : (List<Map<String, Object>>) result.get(key)) {

        // build transaction
        MoneroTx tx = interpretTx(txMap, this);
        MoneroPayment payment = new MoneroPayment(tx, null, (BigInteger) txMap.get("amount"));
        List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
        payments.add(payment);
        tx.setPayments(payments);
        addTx(txTypeMap, tx);
      }
    }

    if (filter.isIncoming()) {

      // incoming_transfers rpc call to get incoming outputs
      paramMap = new HashMap<String, Object>();
      paramMap.put("transfer_type", "all"); // TODO: suppport all | available | unavailable 'types' which is different from MoneroTxType
      respMap = rpc.sendRpcRequest("incoming_transfers", paramMap);
      result = (Map<String, Object>) respMap.get("result");

      // interpret incoming_transfers response
      List<Map<String, Object>> outputMaps = (List<Map<String, Object>>) result.get("transfers");
      if (outputMaps == null) return new ArrayList<MoneroTx>();
      for (Map<String, Object> outputMap : outputMaps) {
        MoneroOutput output = new MoneroOutput();
        output.setAmount((BigInteger) outputMap.get("amount"));
        output.setIsSpent((Boolean) outputMap.get("spent"));
        MoneroTx tx = interpretTx(outputMap, this);
        tx.setType(MoneroTxType.INCOMING);
        output.setTransaction(tx);
        List<MoneroOutput> outputs = new ArrayList<MoneroOutput>();
        outputs.add(output);
        tx.setOutputs(outputs);
        addTx(txTypeMap, tx);
      }

      // get_bulk_payments rpc call to get incoming payments by id
      if (filter.getPaymentIds() != null && !filter.getPaymentIds().isEmpty()) {
        paramMap = new HashMap<String, Object>();
        paramMap.put("payment_ids", filter.getPaymentIds());
        respMap = rpc.sendRpcRequest("get_bulk_payments", paramMap);
        result = (Map<String, Object>) respMap.get("result");

        // interpret get_bulk_payments response
        List<Map<String, Object>> paymentMaps = (List<Map<String, Object>>) result.get("payments");
        for (Map<String, Object> paymentMap : paymentMaps) {
          MoneroTx tx = interpretTx(paymentMap, this);
          tx.setType(MoneroTxType.INCOMING);
          // payment data is redundant with get_transfers rpc call, so it's not added because merging would create duplicates
          // MoneroPayment payment = new MoneroPayment();
          // payment.setAmount((BigInteger) paymentMap.get("amount"));
          // List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
          // payments.add(payment);
          // tx.setPayments(payments);
          addTx(txTypeMap, tx);
        }
      }
    }

    // build return type
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (Entry<MoneroTxType, Map<String, MoneroTx>> entry : txTypeMap.entrySet()) {
      txs.addAll(entry.getValue().values());
    }

    // filter final results
    Collection<MoneroTx> toRemoves = new HashSet<MoneroTx>();
    for (MoneroTx tx : txs) {
      if (filter.getPaymentIds() != null && !filter.getPaymentIds().contains(tx.getPaymentId())) toRemoves.add(tx);
      else if (filter.getMinHeight() != null && (tx.getHeight() == null || tx.getHeight() < filter.getMinHeight())) toRemoves.add(tx);
      else if (filter.getMaxHeight() != null && (tx.getHeight() == null || tx.getHeight() > filter.getMaxHeight())) toRemoves.add(tx);
    }
    txs.removeAll(toRemoves);
    return txs;
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
  public Collection<MoneroKeyImage> getKeyImages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public Map<String, BigInteger> importKeyImages(Collection<MoneroKeyImage> keyImages) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int addAddressBookEntry(MoneroAddress address, String description) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> getLanguages() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_languages", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (List<String>) resultMap.get("languages");
  }

  @Override
  public void createWallet(String filename, String password, String language) {
    if (filename == null || filename.isEmpty()) throw new MoneroException("Filename is not initialized");
    if (password == null || password.isEmpty()) throw new MoneroException("Password is not initialized");
    if (language == null || language.isEmpty()) throw new MoneroException("Language is not initialized");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", filename);
    params.put("password", password);
    params.put("language", language);
    rpc.sendRpcRequest("create_wallet", params);
  }

  @Override
  public void openWallet(String filename, String password) {
    if (filename == null || filename.isEmpty()) throw new MoneroException("Filename is not initialized");
    if (password == null || password.isEmpty()) throw new MoneroException("Password is not initialized");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", filename);
    params.put("password", password);
    rpc.sendRpcRequest("open_wallet", params);
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
    Map<String, Object> respMap = rpc.sendRpcRequest("getbalance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return new Pair<BigInteger, BigInteger>((BigInteger) resultMap.get("balance"), (BigInteger) resultMap.get("unlocked_balance"));
  }
  
  // ------------------------------ STATIC UTILITIES --------------------------
  
  /**
   * Initializes a MoneroTx from a transaction response map.
   * 
   * @param txMap is the map to initialize the transaction from
   * @param wallet is necessary to initialize a MoneroAddress
   * @return MoneroTx is the initialized transaction
   */
  @SuppressWarnings("unchecked")
  private static MoneroTx interpretTx(Map<String, Object> txMap, MoneroWallet wallet) {
    MoneroTx tx = new MoneroTx();
    for (String key : txMap.keySet()) {
      Object val = txMap.get(key);
      if (key.equals("amount")) { } // this method does not process amount since it could be output or payment depending on context
      else if (key.equals("spent")) { } // this method does not process spent which is specific to outputs
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
      else if (key.equalsIgnoreCase("global_index")) { } // ignore
      else if (key.equalsIgnoreCase("destinations")) {
        List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
        tx.setPayments(payments);
        for (Map<String, Object> paymentMap : (List<Map<String, Object>>) val) {
          MoneroPayment payment = new MoneroPayment();
          for (String paymentKey : paymentMap.keySet()) {
            if (paymentKey.equals("address")) payment.setAddress(MoneroUtils.newAddress((String) paymentMap.get(paymentKey), wallet));
            else if (paymentKey.equals("amount")) payment.setAmount((BigInteger) paymentMap.get(paymentKey));
            else throw new MoneroException("Unrecognized transaction destination field: " + paymentKey);
          }
        }
      } else LOGGER.warn("Ignoring unexpected transaction field: '" + key + "'");
    }
    return tx;
  }

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
}
