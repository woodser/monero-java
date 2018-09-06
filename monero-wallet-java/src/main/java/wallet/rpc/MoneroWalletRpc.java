package wallet.rpc;

import static org.junit.Assert.assertEquals;

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
import utils.MoneroUtils;
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
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "mnemonic");
    Map<String, Object> respMap = rpc.sendRpcRequest("query_key", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }

  @Override
  public String getViewKey() {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "view_key");
    Map<String, Object> respMap = rpc.sendRpcRequest("query_key", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }
  
  @Override
  public String getPrimaryAddress() {
    return getSubaddress(0, 0).getAddress();
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    if (paymentId != null) paramMap.put("payment_id", paymentId);
    Map<String, Object> respMap = rpc.sendRpcRequest("make_integrated_address", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    String integratedAddress = (String) resultMap.get("integrated_address");
    return decodeIntegratedAddress(integratedAddress);
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("integrated_address", integratedAddress);
    Map<String, Object> respMap = rpc.sendRpcRequest("split_integrated_address", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroIntegratedAddress address = new MoneroIntegratedAddress((String) resultMap.get("standard_address"), (String) resultMap.get("payment_id"), integratedAddress);
    return address;
  }

  @Override
  public List<MoneroAccount> getAccounts() {
    return getAccounts(null);
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAccount> getAccounts(String tag) {
    Map<String, Object> params = new HashMap<String, Object>();
    Map<String, Object> resultMap = (Map<String, Object>) rpc.sendRpcRequest("get_accounts", params).get("result");
    List<Map<String, Object>> accountMaps = (List<Map<String, Object>>) resultMap.get("subaddress_accounts");
    List<MoneroAccount> accounts = new ArrayList<MoneroAccount>();
    for (Map<String, Object> accountMap : accountMaps) {
      int accountIdx = ((BigInteger) accountMap.get("account_index")).intValue();
      BigInteger balance = (BigInteger) accountMap.get("balance");
      BigInteger unlockedBalance = (BigInteger) accountMap.get("unlocked_balance");
      String primaryAddress = (String) accountMap.get("base_address");
      String label = (String) accountMap.get("label");
      boolean isMultisigImportNeeded = false;  // TODO: get this value, may need to make another rpc call for balance info
      MoneroAccount account = new MoneroAccount(accountIdx, primaryAddress, label, balance, unlockedBalance, isMultisigImportNeeded, null);
      accounts.add(account);
    }
    return accounts;
  }

  @Override
  public MoneroAccount getAccount(int accountIdx) {
    for (MoneroAccount account : getAccounts()) {
      if (account.getIndex() == accountIdx) return account;
    }
    throw new MoneroException("Account with index " + accountIdx + " does not exist");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroAccount createAccount(String label) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("label", label);
    Map<String, Object> respMap = rpc.sendRpcRequest("create_account", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    int accountIdx = ((BigInteger) resultMap.get("account_index")).intValue();
    String address = (String) resultMap.get("address");
    return new MoneroAccount(accountIdx, address, label, null, null, null, null);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, Collection<Integer> subaddressIndices) {
    
    // fetch subaddresses
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    if (subaddressIndices != null) params.put("address_index", subaddressIndices);
    Map<String, Object> respMap = rpc.sendRpcRequest("getaddress", params);
    
    // initialize subaddresses
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> addresses = (List<Map<String, Object>>) resultMap.get("addresses");
    for (Map<String, Object> address : addresses) {
      MoneroSubaddress subaddress = new MoneroSubaddress();
      subaddresses.add(subaddress);
      subaddress.setIndex(((BigInteger) address.get("address_index")).intValue());
      subaddress.setLabel((String) address.get("label"));
      subaddress.setAddress((String) address.get("address"));
      subaddress.setUsed((boolean) address.get("used"));
    }
    
    // fetch and initialize subaddress balances
    respMap = rpc.sendRpcRequest("getbalance", params);
    resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> subaddressMaps = (List<Map<String, Object>>) resultMap.get("per_subaddress");
    for (Map<String, Object> subaddressMap : subaddressMaps) {
      int subaddressIdx = ((BigInteger) subaddressMap.get("address_index")).intValue();
      for (MoneroSubaddress subaddress : subaddresses) {
        if (subaddressIdx != subaddress.getIndex()) continue; // find matching subaddress
        assertEquals(subaddress.getAddress().toString(), (String) subaddressMap.get("address"));
        subaddress.setBalance((BigInteger) subaddressMap.get("balance"));
        subaddress.setUnlockedBalance((BigInteger) subaddressMap.get("unlocked_balance"));
        subaddress.setNumUnspentOutputs(((BigInteger) subaddressMap.get("num_unspent_outputs")).intValue());
        subaddress.setMultisigImportNeeded((boolean) resultMap.get("multisig_import_needed"));
      }
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
    return getSubaddresses(accountIdx, Arrays.asList(subaddressIdx)).get(0).getBalance();
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
    return getSubaddresses(accountIdx, Arrays.asList(subaddressIdx)).get(0).getUnlockedBalance();
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public boolean isMultisigImportNeeded() {
    Map<String, Object> respMap = rpc.sendRpcRequest("getbalance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (boolean) resultMap.get("multisig_import_needed");
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
    MoneroTx tx = interpretTx(txMap);
    tx.setAmount((BigInteger) txMap.get("amount"));
    tx.setPayments(config.getDestinations());
    tx.setMixin(config.getMixin());
    tx.setUnlockTime(config.getUnlockTime());
    return tx;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> sendSplit(MoneroTxConfig config) {
    
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
    paramMap.put("unlockTime", config.getUnlockTime());
    paramMap.put("new_algorithm", true);

    // send request
    Map<String, Object> respMap = rpc.sendRpcRequest("transfer_split", paramMap);

    // interpret response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<BigInteger> fees = (List<BigInteger>) resultMap.get("fee_list");
    List<String> txIds = (List<String>) resultMap.get("tx_hash_list");
    List<MoneroTx> transactions = new ArrayList<MoneroTx>();
    for (int i = 0; i < fees.size(); i++) {
      MoneroTx tx = new MoneroTx();
      tx.setFee(fees.get(i));
      tx.setMixin(config.getMixin());
      tx.setId(txIds.get(0));
      transactions.add(tx);
      tx.setUnlockTime(config.getUnlockTime());
    }
    return transactions;
  }

  @Override
  public List<MoneroTx> sweepAll(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> sweepDust() {
    
    // send request
    Map<String, Object> respMap = rpc.sendRpcRequest("sweep_dust", null);

    // interpret response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<String> txIds = (List<String>) resultMap.get("tx_hash_list");
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    if (txIds == null) return txs;
    for (String txId : txIds) {
      MoneroTx tx = new MoneroTx();
      tx.setId(txId);
      txs.add(tx);
    }
    return txs;
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
    
    // collect transactions bucketed by type then id
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
        MoneroTx tx = interpretTx(txMap);
        if (txMap.containsKey("amount")) tx.setAmount((BigInteger) txMap.get("amount"));
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
        MoneroTx tx = interpretTx(outputMap);
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
          MoneroTx tx = interpretTx(paymentMap);
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

  @SuppressWarnings("unchecked")
  @Override
  public Collection<MoneroKeyImage> getKeyImages() {
    Map<String, Object> respMap = rpc.sendRpcRequest("export_key_images", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> keyImageMaps = (List<Map<String, Object>>) resultMap.get("signed_key_images");
    List<MoneroKeyImage> keyImages = new ArrayList<MoneroKeyImage>();
    for (Map<String, Object> keyImageMap : keyImageMaps) {
      keyImages.add(new MoneroKeyImage((String) keyImageMap.get("key_image"), (String) keyImageMap.get("signature")));
    }
    return keyImages;
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map<String, BigInteger> importKeyImages(Collection<MoneroKeyImage> keyImages) {
    
    // convert key images to maps
    List<Map<String, String>> keyImageMaps = new ArrayList<Map<String, String>>();
    for (MoneroKeyImage keyImage : keyImages) {
      Map<String, String> keyImageMap = new HashMap<String, String>();
      keyImageMaps.add(keyImageMap);
      keyImageMap.put("key_image", keyImage.getKeyImage());
      keyImageMap.put("signature", keyImage.getSignature());
    }
    
    // send and interpret rpc request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("signed_key_images", keyImageMaps);
    Map<String, Object> respMap = rpc.sendRpcRequest("import_key_images", params);
    Map<String, BigInteger> resultMap = (Map<String, BigInteger>) respMap.get("result");
    return resultMap;
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

  @SuppressWarnings("unchecked")
  @Override
  public String sign(String data) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("data", data);
    Map<String, Object> result = (Map<String, Object>) rpc.sendRpcRequest("sign", params).get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean verify(String data, String address, String signature) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("data", data);
    params.put("address", address);
    params.put("signature", signature);
    Map<String, Object> result = (Map<String, Object>) rpc.sendRpcRequest("verify", params).get("result");
    return (boolean) result.get("good");
  }

  @Override
  public URI toUri(MoneroUri moneroUri) {
    if (moneroUri == null) throw new MoneroException("Given Monero URI is null");
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("address", moneroUri.getAddress());
    paramMap.put("amount", moneroUri.getAmount() == null ? null : moneroUri.getAmount());
    paramMap.put("payment_id", moneroUri.getPaymentId());
    paramMap.put("recipient_name", moneroUri.getRecipientName());
    paramMap.put("tx_description", moneroUri.getTxDescription());
    Map<String, Object> respMap = rpc.sendRpcRequest("make_uri", paramMap);
    @SuppressWarnings("unchecked")
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return MoneroUtils.parseUri((String) resultMap.get("uri"));
  }

  @Override
  public MoneroUri toMoneroUri(URI uri) {
    if (uri == null) throw new MoneroException("Given URI is null");
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("uri", uri.toString());
    Map<String, Object> respMap = rpc.sendRpcRequest("parse_uri", paramMap);
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

  @Override
  public void saveBlockchain() {
    rpc.sendRpcRequest("store", null);
  }

  @Override
  public void rescanBlockchain() {
    rpc.sendRpcRequest("rescan_blockchain", null);
  }

  @Override
  public void rescanSpent() {
    rpc.sendRpcRequest("rescan_spent", null);
  }

  @Override
  public void stopWallet() {
    rpc.sendRpcRequest("stop_wallet", null);
  }

  @Override
  public void startMining(int numThreads, boolean backgroundMining, boolean ignoreBattery) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("threads_count", numThreads);
    params.put("do_background_mining", backgroundMining);
    params.put("ignore_battery", ignoreBattery);
    rpc.sendRpcRequest("start_mining", params);
  }

  @Override
  public void stopMining() {
    rpc.sendRpcRequest("stop_mining", null);
  }
  
  // ------------------------------ STATIC UTILITIES --------------------------
  
  /**
   * Initializes a MoneroTx from a transaction response map.
   * 
   * @param txMap is the map to initialize the transaction from
   * @return MoneroTx is the initialized transaction
   */
  @SuppressWarnings("unchecked")
  private static MoneroTx interpretTx(Map<String, Object> txMap) {
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
      else if (key.equalsIgnoreCase("txid")) tx.setId((String) val);
      else if (key.equalsIgnoreCase("tx_hash")) tx.setId((String) val);
      else if (key.equalsIgnoreCase("tx_key")) tx.setKey((String) val);
      else if (key.equalsIgnoreCase("type")) tx.setType(getTxType((String) val));
      else if (key.equalsIgnoreCase("tx_size")) tx.setSize(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("unlock_time")) tx.setUnlockTime(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("global_index")) { } // ignore
      else if (key.equalsIgnoreCase("tx_blob")) tx.setBlob((String) val);
      else if (key.equalsIgnoreCase("tx_metadata")) tx.setMetadata((String) val);
      else if (key.equalsIgnoreCase("destinations")) {
        List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
        tx.setPayments(payments);
        for (Map<String, Object> paymentMap : (List<Map<String, Object>>) val) {
          MoneroPayment payment = new MoneroPayment();
          payments.add(payment);
          for (String paymentKey : paymentMap.keySet()) {
            if (paymentKey.equals("address")) payment.setAddress((String) paymentMap.get(paymentKey));
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
    if (tx.getId() == null) throw new MoneroException("Transaction id cannot be null: \n" + tx.getId());
    Map<String, MoneroTx> txIdMap = txTypeMap.get(tx.getType());
    if (txIdMap == null) {
      txIdMap = new HashMap<String, MoneroTx>();
      txTypeMap.put(tx.getType(), txIdMap);
    }
    MoneroTx targetTx = txIdMap.get(tx.getId());
    if (targetTx == null) {
      txIdMap.put(tx.getId(), tx);
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
