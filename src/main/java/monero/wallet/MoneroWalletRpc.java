package monero.wallet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

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
import java.util.Set;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import monero.rpc.MoneroRpc;
import monero.utils.MoneroUtils;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroException;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImage;
import monero.wallet.model.MoneroPayment;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTx.MoneroTxType;
import monero.wallet.model.MoneroTxCheck;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxFilter;
import monero.wallet.model.MoneroUri;

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
  private Map<Integer, Map<Integer, String>> addressCache;  // cache static addresses to reduce requests

  public MoneroWalletRpc(String endpoint) {
    super();
    rpc = new MoneroRpc(endpoint);
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }

  public MoneroWalletRpc(URI rpcUri) {
    super();
    rpc = new MoneroRpc(rpcUri);
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }

  public MoneroWalletRpc(String rpcHost, int rpcPort) throws URISyntaxException {
    this(rpcHost, rpcPort, null, null);
  }

  public MoneroWalletRpc(String rpcHost, int rpcPort, String username, String password) throws URISyntaxException {
    super();
    rpc = new MoneroRpc(rpcHost, rpcPort, username, password);
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }
  
  public MoneroRpc getRpc() {
    return rpc;
  }

  @Override
  public int getHeight() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_height", null);
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

  @SuppressWarnings("unchecked")
  @Override
  public String getViewKey() {
    Map<String, Object> paramMap = new HashMap<String, Object>();
    paramMap.put("key_type", "view_key");
    Map<String, Object> respMap = rpc.sendRpcRequest("query_key", paramMap);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("key");
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public String getPrimaryAddress() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", 0);
    params.put("address_index", 0);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_address", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("address");
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
  
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAccount> getAccounts(String tag, boolean includeSubaddresses) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    Map<String, Object> resultMap = (Map<String, Object>) rpc.sendRpcRequest("get_accounts", params).get("result");
    List<Map<String, Object>> accountMaps = (List<Map<String, Object>>) resultMap.get("subaddress_accounts");
    List<MoneroAccount> accounts = new ArrayList<MoneroAccount>();
    for (Map<String, Object> accountMap : accountMaps) {
      int accountIdx = ((BigInteger) accountMap.get("account_index")).intValue();
      BigInteger balance = (BigInteger) accountMap.get("balance");
      BigInteger unlockedBalance = (BigInteger) accountMap.get("unlocked_balance");
      String primaryAddress = (String) accountMap.get("base_address");
      String label = (String) accountMap.get("label");
      MoneroAccount account = new MoneroAccount(accountIdx, primaryAddress, label, balance, unlockedBalance, null);
      accounts.add(account);
      if (includeSubaddresses) account.setSubaddresses(getSubaddresses(account.getIndex()));
    }
    return accounts;
  }
  
  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    for (MoneroAccount account : getAccounts()) {
      if (account.getIndex() == accountIdx) {
        if (includeSubaddresses) account.setSubaddresses(getSubaddresses(accountIdx));
        return account;
      }
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
    return new MoneroAccount(accountIdx, address, label, BigInteger.valueOf(0), BigInteger.valueOf(0), null);
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAccountTag> getAccountTags() {
    List<MoneroAccountTag> tags = new ArrayList<MoneroAccountTag>();
    Map<String, Object> respMap = rpc.sendRpcRequest("get_account_tags");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> accountTagMaps = (List<Map<String, Object>>) resultMap.get("account_tags");
    if (accountTagMaps != null) {
      for (Map<String, Object> accountTagMap : accountTagMaps) {
        MoneroAccountTag tag = new MoneroAccountTag();
        tags.add(tag);
        tag.setTag((String) accountTagMap.get("tag"));
        tag.setLabel((String) accountTagMap.get("label"));
        List<BigInteger> accountIndicesBI = (List<BigInteger>) accountTagMap.get("accounts");
        List<Integer> accountIndices = new ArrayList<Integer>();
        for (BigInteger idx : accountIndicesBI) accountIndices.add(idx.intValue());
        tag.setAccountIndices(accountIndices);
      }
    }
    return tags;
  }
  
  @Override
  public void setAccountTagLabel(String tag, String label) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    params.put("description", label);
    rpc.sendRpcRequest("set_account_tag_description", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, Collection<Integer> subaddressIndices) {
    
    // fetch subaddresses
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    if (subaddressIndices != null) params.put("address_index", subaddressIndices);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_address", params);
    
    // initialize subaddresses
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> addresses = (List<Map<String, Object>>) resultMap.get("addresses");
    for (Map<String, Object> address : addresses) {
      MoneroSubaddress subaddress = new MoneroSubaddress();
      subaddresses.add(subaddress);
      subaddress.setAccountIndex(accountIdx);
      subaddress.setSubaddrIndex(((BigInteger) address.get("address_index")).intValue());
      subaddress.setLabel((String) address.get("label"));
      subaddress.setAddress((String) address.get("address"));
      subaddress.setIsUsed((boolean) address.get("used"));
      
      // set defaults
      subaddress.setBalance(BigInteger.valueOf(0));
      subaddress.setUnlockedBalance(BigInteger.valueOf(0));
      subaddress.setNumUnspentOutputs(0);
    }
    
    // fetch and initialize subaddress balances
    respMap = rpc.sendRpcRequest("get_balance", params);
    resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> subaddressMaps = (List<Map<String, Object>>) resultMap.get("per_subaddress");
    if (subaddressMaps != null) {
      for (Map<String, Object> subaddressMap : subaddressMaps) {
        int subaddressIdx = ((BigInteger) subaddressMap.get("address_index")).intValue();
        for (MoneroSubaddress subaddress : subaddresses) {
          if (subaddressIdx != subaddress.getSubaddrIndex()) continue; // find matching subaddress
          assertEquals(subaddress.getAddress().toString(), (String) subaddressMap.get("address"));
          if (subaddressMap.containsKey("balance")) subaddress.setBalance((BigInteger) subaddressMap.get("balance"));
          if (subaddressMap.containsKey("unlocked_balance")) subaddress.setUnlockedBalance((BigInteger) subaddressMap.get("unlocked_balance"));
          subaddress.setNumUnspentOutputs(((BigInteger) subaddressMap.get("num_unspent_outputs")).intValue());
        }
      }
    }
    
    // cache addresses
    Map<Integer, String> subaddressMap = addressCache.get(accountIdx);
    if (subaddressMap == null) {
      subaddressMap = new HashMap<Integer, String>();
      addressCache.put(accountIdx, subaddressMap);
    }
    for (MoneroSubaddress subaddress : subaddresses) {
      subaddressMap.put(subaddress.getSubaddrIndex(), subaddress.getAddress());
    }
    
    // return results
    return subaddresses;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    params.put("label", label);
    Map<String, Object> respMap = rpc.sendRpcRequest("create_address", params);
    
    // build subaddress from response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroSubaddress subaddress = new MoneroSubaddress();
    subaddress.setAccountIndex(accountIdx);
    subaddress.setSubaddrIndex(((BigInteger) resultMap.get("address_index")).intValue());
    subaddress.setAddress((String) resultMap.get("address"));
    subaddress.setLabel(label == null ? "" : label);
    subaddress.setBalance(BigInteger.valueOf(0));
    subaddress.setUnlockedBalance(BigInteger.valueOf(0));
    subaddress.setNumUnspentOutputs(0);
    subaddress.setIsUsed(false);
    return subaddress;
  }
  
  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    Map<Integer, String> subaddressMap = addressCache.get(accountIdx);
    if (subaddressMap == null) {
      getSubaddresses(accountIdx, null);            // cache's all addresses at this account
      return getAddress(accountIdx, subaddressIdx); // uses cache
    }
    String address = subaddressMap.get(subaddressIdx);
    if (address == null) {
      getSubaddresses(accountIdx, null);            // cache's all addresses at this account
      return getAddress(accountIdx, subaddressIdx); // uses cache
    }
    return address;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public boolean isMultisigImportNeeded() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_balance", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (boolean) resultMap.get("multisig_import_needed");
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> getTxs(MoneroTxFilter filter) {
    if (filter == null) filter = new MoneroTxFilter();
    
    // stores merged txs across calls
    List<MoneroTx> txs = new ArrayList<MoneroTx>();    
    
    // determine account and subaddress indices to be queried
    Map<Integer, List<Integer>> indices = new HashMap<Integer, List<Integer>>();
    if (filter.getAccountIndex() != null) {
      indices.put(filter.getAccountIndex(), filter.getSubaddressIndices() == null || filter.getSubaddressIndices().isEmpty() ? getSubaddressIndices(filter.getAccountIndex()) : new ArrayList<Integer>(filter.getSubaddressIndices()));
    } else {
      if (filter.getSubaddressIndices() != null) throw new RuntimeException("Filter specifies subaddress indices but not an account index");
      indices = getAllAccountAndSubaddressIndices();
    }
    
    // build common params for get_transfers
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("in", filter.isIncoming());
    params.put("out", filter.isOutgoing());
    params.put("pending", filter.isPending());
    params.put("failed", filter.isFailed());
    params.put("pool", filter.isMempool());
    params.put("filter_by_height", filter.getMinHeight() != null || filter.getMaxHeight() != null);
    if (filter.getMinHeight() != null) params.put("min_height", filter.getMinHeight());
    if (filter.getMaxHeight() != null) params.put("max_height", filter.getMaxHeight());
    
    // get transactions using get_transfers
    for (Integer accountIdx : indices.keySet()) {
      params.put("account_index", accountIdx);
      params.put("subaddr_indices", indices.get(accountIdx));
      Map<String, Object> respMap = rpc.sendRpcRequest("get_transfers", params);
      Map<String, Object> result = (Map<String, Object>) respMap.get("result");
      for (String key : result.keySet()) {
        for (Map<String, Object> txMap : (List<Map<String, Object>>) result.get(key)) {
          MoneroTx tx = txMapToTx(txMap, this);
          if (tx.getType() == MoneroTxType.INCOMING) {  // prevent duplicates when populated by incoming_transfers  // TODO (monero-wallet-rpc): merge payments when incoming txs work (https://github.com/monero-project/monero/issues/4500)
            tx.setTotalAmount(BigInteger.valueOf(0));
            tx.setPayments(null);
          }
          addTx(txs, tx, false);
        }
      }
    }
    
    // get incoming transactions
    if (filter.isIncoming()) {
      
      // get transactions using incoming_transfers
      params.clear();
      params.put("transfer_type", "all"); // TODO: suppport all | available | unavailable 'types' which is different from MoneroTxType
      for (Integer accountIdx : indices.keySet()) {
        params.put("account_index", accountIdx);
        params.put("subaddr_indices", filter.getSubaddressIndices()); // null subaddr_indices will fetch all incoming_transfers
        Map<String, Object> respMap = rpc.sendRpcRequest("incoming_transfers", params);
        Map<String, Object> result = (Map<String, Object>) respMap.get("result");

        // interpret incoming_transfers response
        List<Map<String, Object>> txMaps = (List<Map<String, Object>>) result.get("transfers");
        if (txMaps != null) {
          for (Map<String, Object> txMap : txMaps) {
            MoneroTx tx = txMapToTx(txMap, MoneroTxType.INCOMING, this);
            String address = getAddress(accountIdx, tx.getPayments().get(0).getDestination().getSubaddrIndex());
            tx.getPayments().get(0).getDestination().setAddress(address);
            addTx(txs, tx, false);
          }
        }
      }
    }

    // filter final result
    Collection<MoneroTx> toRemoves = new HashSet<MoneroTx>();
    for (MoneroTx tx : txs) {
      if (filter.getPaymentIds() != null && !filter.getPaymentIds().contains(tx.getPaymentId())) toRemoves.add(tx);
      else if (filter.getTxIds() != null && !filter.getTxIds().contains(tx.getId())) toRemoves.add(tx);
      else if (filter.getMinHeight() != null && (tx.getHeight() == null || tx.getHeight() < filter.getMinHeight())) toRemoves.add(tx);
      else if (filter.getMaxHeight() != null && (tx.getHeight() == null || tx.getHeight() > filter.getMaxHeight())) toRemoves.add(tx);
    }
    txs.removeAll(toRemoves);
    return txs;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroTx send(MoneroTxConfig config) {
    
    // determine account and subaddresses to send from
    Integer accountIdx = config.getAccountIndex();
    if (accountIdx == null) throw new MoneroException("Must specify account index to send from");
    Collection<Integer> subaddressIndices = config.getSubaddressIndices();
    if (subaddressIndices == null) subaddressIndices = getSubaddressIndices(accountIdx);    
    
    // build parameter map
    Map<String, Object> paramMap = new HashMap<String, Object>();
    List<Map<String, Object>> destinationMaps = new ArrayList<Map<String, Object>>();
    paramMap.put("destinations", destinationMaps);
    for (MoneroPayment payment : config.getPayments()) {
      Map<String, Object> destinationMap = new HashMap<String, Object>();
      destinationMap.put("address", payment.getDestination().getAddress());
      destinationMap.put("amount", payment.getAmount());
      destinationMaps.add(destinationMap);
    }
    paramMap.put("account_index", accountIdx);
    paramMap.put("subaddr_indices", subaddressIndices);
    paramMap.put("payment_id", config.getPaymentId());
    paramMap.put("mixin", config.getMixin());
    paramMap.put("unlock_time", config.getUnlockTime());
    paramMap.put("do_not_relay", config.getDoNotRelay());
    paramMap.put("get_tx_key", true);
    paramMap.put("get_tx_hex", true);
    paramMap.put("get_tx_metadata", true);
  
    // send request
    Map<String, Object> respMap = rpc.sendRpcRequest("transfer", paramMap);
  
    // interpret response
    Map<String, Object> txMap = (Map<String, Object>) respMap.get("result");
    MoneroTxType type = Boolean.TRUE.equals(config.getDoNotRelay()) ? MoneroTxType.NOT_RELAYED : MoneroTxType.PENDING;
    MoneroTx tx = txMapToTx(txMap, type, this);
    
    // set final fields
    tx.setMixin(config.getMixin());
    tx.setPayments(config.getPayments());
    tx.setPaymentId(config.getPaymentId());
    MoneroSubaddress srcSubaddress = new MoneroSubaddress(accountIdx, 0, getAddress(accountIdx, 0));  // TODO (monero-wallet-rpc): outgoing subaddress idx is always 0
    tx.setSrcSubaddress(srcSubaddress);
    if (tx.getType() != MoneroTxType.NOT_RELAYED) {
      if (tx.getTimestamp() == null) tx.setTimestamp(System.currentTimeMillis());  // TODO (monero-wallet-rpc): provide timestamp on response; unconfirmed timestamps vary
      if (tx.getUnlockTime() == null) tx.setUnlockTime(config.getUnlockTime() == null ? 0 : config.getUnlockTime());
      if (tx.getIsDoubleSpend() == null) tx.setIsDoubleSpend(false);
    }

    return tx;

    // TODO: can we get away with this?
//    // done if not relayed
//    if (Boolean.TRUE.equals(config.getDoNotRelay())) return tx;
//    
//    // merge tx for complete data
//    MoneroTxFilter filter = new MoneroTxFilter();
//    filter.setAccountIndex(accountIdx);
//    filter.setSubaddressIndices(subaddressIndices);
//    filter.setIncoming(false);
//    filter.setMempool(false);
//    filter.setTxIds(Arrays.asList(tx.getId()));
//    List<MoneroTx> filtered = getTxs(filter);
//    assertEquals(1, filtered.size());
//    tx.merge(getTxs(filter).get(0), false); // TODO: need to make retrieval by id much more efficient
//    return tx;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> sendSplit(MoneroTxConfig config) {
    
    // determine account and subaddresses to send from
    Integer accountIdx = config.getAccountIndex();
    if (accountIdx == null) throw new MoneroException("Must specify account index to send from");
    Collection<Integer> subaddressIndices = config.getSubaddressIndices();
    if (subaddressIndices == null) subaddressIndices = getSubaddressIndices(accountIdx);    
    
    // build parameter map
    Map<String, Object> paramMap = new HashMap<String, Object>();
    List<Map<String, Object>> destinationMaps = new ArrayList<Map<String, Object>>();
    paramMap.put("destinations", destinationMaps);
    for (MoneroPayment payment : config.getPayments()) {
      Map<String, Object> destinationMap = new HashMap<String, Object>();
      destinationMap.put("address", payment.getDestination().getAddress());
      destinationMap.put("amount", payment.getAmount());
      destinationMaps.add(destinationMap);
    }
    paramMap.put("account_index", accountIdx);
    paramMap.put("subaddr_indices", subaddressIndices);
    paramMap.put("payment_id", config.getPaymentId());
    paramMap.put("mixin", config.getMixin());
    paramMap.put("unlock_time", config.getUnlockTime());
    paramMap.put("do_not_relay", config.getDoNotRelay());
    paramMap.put("get_tx_key", true);
    paramMap.put("get_tx_hex", true);
    paramMap.put("get_tx_metadata", true);
  
    // send request
    Map<String, Object> respMap = rpc.sendRpcRequest("transfer_split", paramMap);
    
    // interpret response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroTxType type = Boolean.TRUE.equals(config.getDoNotRelay()) ? MoneroTxType.NOT_RELAYED : MoneroTxType.PENDING;
    List<MoneroTx> txs = txListMapToTxs(resultMap, accountIdx, type, this);
    
    // set final fields
    for (MoneroTx tx : txs) {
      tx.setMixin(config.getMixin());
      tx.setPayments(config.getPayments());
      tx.setPaymentId(config.getPaymentId());
      MoneroSubaddress srcSubaddress = new MoneroSubaddress(accountIdx, 0, getAddress(accountIdx, 0));  // TODO (monero-wallet-rpc): outgoing subaddress idx is always 0
      tx.setSrcSubaddress(srcSubaddress);
      if (tx.getType() != MoneroTxType.NOT_RELAYED) {
        if (tx.getTimestamp() == null) tx.setTimestamp(System.currentTimeMillis());  // TODO (monero-wallet-rpc): provide timestamp on response; unconfirmed timestamps vary
        if (tx.getUnlockTime() == null) tx.setUnlockTime(config.getUnlockTime() == null ? 0 : config.getUnlockTime());
        if (tx.getIsDoubleSpend() == null) tx.setIsDoubleSpend(false);
      }      
    }
    
//    // merge txs for complete data
//    Collection<String> ids = new ArrayList<String>();
//    for (MoneroTx tx : txs) ids.add(tx.getId());
//    MoneroTxFilter filter = new MoneroTxFilter();
//    filter.setAccountIndex(accountIdx);
//    filter.setSubaddressIndices(subaddressIndices);
//    filter.setIncoming(false);
//    filter.setMempool(false);
//    filter.setTxIds(ids);
//    List<MoneroTx> filtereds = getTxs(filter);
//    assertEquals(txs.size(), filtereds.size());
//    for (MoneroTx tx : txs) {
//      for (MoneroTx filtered : filtereds) {
//        if (tx.getId().equals(filtered.getId())) tx.merge(filtered, false);
//      }
//    }
    return txs;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> sweepAll(MoneroTxConfig config) {
    
    // common request params
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", config.getPayments().get(0).getDestination().getAddress());
    params.put("priority", config.getPriority());
    params.put("mixin", config.getMixin());
    params.put("unlock_time", config.getUnlockTime());
    params.put("payment_id", config.getPaymentId());
    params.put("do_not_relay", config.getDoNotRelay());
    params.put("below_amount", config.getBelowAmount());
    params.put("get_tx_keys", true);
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    
    // determine accounts to sweep from; default to all with unlocked balance if not specified
    List<Integer> accountIndices = new ArrayList<Integer>();
    if (config.getAccountIndex() != null) {
      accountIndices.add(config.getAccountIndex());
    } else {
      for (MoneroAccount account : getAccounts()) {
        if (account.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) {
          accountIndices.add(account.getIndex());
        }
      }
    }
    
    // sweep from each account and collect unique transactions
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (Integer accountIdx : accountIndices) {
      params.put("account_index", accountIdx);
      
      // collect transactions for account
      List<MoneroTx> accountTxs = new ArrayList<MoneroTx>();
      
      // determine subaddresses to sweep from; default to all with unlocked balance if not specified
      List<Integer> subaddressIndices = new ArrayList<Integer>();
      if (config.getSubaddressIndices() != null) {
        subaddressIndices.addAll(config.getSubaddressIndices());
      } else {
        for (MoneroSubaddress subaddress : getSubaddresses(accountIdx)) {
          if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) {
            subaddressIndices.add(subaddress.getSubaddrIndex());
          }
        }
      }
      if (subaddressIndices.isEmpty()) throw new MoneroException("No subaddresses to sweep from");
      
      // sweep each subaddress individually
      if (config.getSweepEachSubaddress() == null || config.getSweepEachSubaddress()) {
        for (Integer subaddressIdx : subaddressIndices) {
          params.put("subaddr_indices", Arrays.asList(subaddressIdx));
          Map<String, Object> respMap = rpc.sendRpcRequest("sweep_all", params);
          Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
          accountTxs.addAll(txListMapToTxs(resultMap, accountIdx, MoneroTxType.PENDING, this));
        }
      }
      
      // sweep all subaddresses together
      else {
        params.put("subaddr_indices", Arrays.asList(subaddressIndices));
        Map<String, Object> respMap = rpc.sendRpcRequest("sweep_all", params);
        Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
        accountTxs.addAll(txListMapToTxs(resultMap, accountIdx, MoneroTxType.PENDING, this));
      }
      
      // customize and merge transactions from account
      for (MoneroTx tx : accountTxs) {
        tx.setMixin(config.getMixin());
        addTx(txs, tx, true);
      }
      
      // fetch transactions by id and merge complete data
      assertFalse(accountTxs.isEmpty());
      List<String> ids = new ArrayList<String>();
      for (MoneroTx tx : accountTxs) if (tx.getId() != null) ids.add(tx.getId());
      if (!ids.isEmpty()) assertEquals(accountTxs.size(), ids.size());
      if (!ids.isEmpty()) {
        MoneroTxFilter filter = new MoneroTxFilter();
        filter.setAccountIndex(accountIdx);
        filter.setTxIds(ids);
        filter.setIncoming(false);
        for (MoneroTx tx : getTxs(filter)) addTx(txs, tx, true);
      }
    }
    
    // return transactions from all accounts
    return txs;
  }

  // TODO (v0.13.0): rpc call has input params in new version
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> sweepDust() {
    
    // send request
    Map<String, Object> respMap = rpc.sendRpcRequest("sweep_dust", null);
  
    // interpret response
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<String> ids = (List<String>) resultMap.get("tx_hash_list");
    if (ids == null) return new ArrayList<MoneroTx>();
    else {
      MoneroTxFilter filter = new MoneroTxFilter();
      filter.setTxIds(ids);
      filter.setIncoming(false);
      filter.setMempool(false);
      List<MoneroTx> txs = getTxs(filter);
      assertEquals("Sweep dust transactions not fetched: " + ids, ids.size(), txs.size());
      return txs;
    }
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTx> relayTxs(List<MoneroTx> txs) {
    
    // relay transactions and collect resulting ids
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs)  {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("hex", tx.getMetadata());
      Map<String, Object> respMap = rpc.sendRpcRequest("relay_tx", params);
      Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
      txIds.add((String) resultMap.get("tx_hash"));
    }
    
    // fetch transactions by id
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setIncoming(false);
    filter.setMempool(false);
    filter.setTxIds(txIds);
    List<MoneroTx> relayedTxs = getTxs(filter);
    
    // transfer tx data
    assertEquals(txs.size(), relayedTxs.size());
    for (int i = 0; i < txs.size(); i++) {
      assertEquals(txs.get(i).getId(), relayedTxs.get(i).getId());
      relayedTxs.get(i).setMixin(txs.get(i).getMixin());
      relayedTxs.get(i).setKey(txs.get(i).getKey());
      relayedTxs.get(i).setPayments(txs.get(i).getPayments());
      relayedTxs.get(i).setBlob(txs.get(i).getBlob());
      relayedTxs.get(i).setMetadata(txs.get(i).getMetadata());
    }
    
    return relayedTxs;
  }

  @Override
  public void tagAccounts(String tag, Collection<Integer> accountIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    params.put("accounts", accountIndices);
    rpc.sendRpcRequest("tag_accounts", params);
  }

  @Override
  public void untagAccounts(Collection<Integer> accountIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("accounts", accountIndices);
    rpc.sendRpcRequest("untag_accounts", params);
  }

  @Override
  public void setTxNotes(List<String> txIds, List<String> txNotes) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txIds);
    params.put("notes", txNotes);
    rpc.sendRpcRequest("set_tx_notes", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> getTxNotes(List<String> txIds) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txIds);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_tx_notes", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (List<String>) resultMap.get("notes");
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public String getTxKey(String txId) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_tx_key", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("tx_key");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroTxCheck checkTxKey(String txId, String txKey, String address) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    params.put("tx_key", txKey);
    params.put("address", address);
    Map<String, Object> respMap = rpc.sendRpcRequest("check_tx_key", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    
    // interpret result
    MoneroTxCheck check = new MoneroTxCheck();
    check.setIsGood(true);
    check.setNumConfirmations(((BigInteger) resultMap.get("confirmations")).intValue());
    check.setIsInPool((Boolean) resultMap.get("in_pool"));
    check.setAmountReceived((BigInteger) resultMap.get("received"));
    return check;
  }

  @Override
  public String getTxProof(String txId, String address, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxCheck checkTxProof(String txId, String address, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getSpendProof(String txId, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean checkSpendProof(String txId, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProof(String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProof(int accountIdx, BigInteger amount, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxCheck checkReserveProof(String address, String message, String signature) {
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
  
  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("entries", entryIndices);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_address_book", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<MoneroAddressBookEntry> entries = new ArrayList<MoneroAddressBookEntry>();
    if (!resultMap.containsKey("entries")) return entries;
    for (Map<String, Object> entryMap : (List<Map<String, Object>>) resultMap.get("entries")) {
      MoneroAddressBookEntry entry = new MoneroAddressBookEntry(
              ((BigInteger) entryMap.get("index")).intValue(),
              (String) entryMap.get("address"),
              (String) entryMap.get("payment_id"),
              (String) entryMap.get("description")
      );
      entries.add(entry);
    }
    return entries;
  }

  @SuppressWarnings("unchecked")
  @Override
  public int addAddressBookEntry(String address, String paymentId, String description) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", address);
    params.put("payment_id", paymentId);
    params.put("description", description);
    Map<String, Object> respMap = rpc.sendRpcRequest("add_address_book", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return ((BigInteger) resultMap.get("index")).intValue();
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("index", entryIdx);
    rpc.sendRpcRequest("delete_address_book", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> getLanguages() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_languages", null);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (List<String>) resultMap.get("languages");
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
    addressCache.clear();
  }

  @Override
  public void stopWallet() {
    rpc.sendRpcRequest("stop_wallet", null);
    addressCache.clear();
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
  
  @Override
  public void setAttribute(String key, String value) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key", key);
    params.put("value", value);
    rpc.sendRpcRequest("set_attribute", params);
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public String getAttribute(String key) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key", key);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_attribute", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return (String) resultMap.get("value");
  }
  
  // ------------------------------ STATIC UTILITIES --------------------------
  
  /**
   * Converts a transaction map to a MoneroTx.
   * 
   * @param txMap is the map to create a MoneroTx from
   * @return MoneroTx is the transaction created from the map
   */
  private static MoneroTx txMapToTx(Map<String, Object> txMap, MoneroWallet wallet) {
    return txMapToTx(txMap, null, wallet);
  }
  
  /**
   * Converts a transaction map to a MoneroTx.
   * 
   * @param txMap is the map to create a MoneroTx from
   * @param isOutgoing specifies if the transaction is outgoing xor incoming (only necessary if type information not available)
   * @return MoneroTx is the transaction created from the map
   */
  @SuppressWarnings("unchecked")
  private static MoneroTx txMapToTx(Map<String, Object> txMap, MoneroTxType type, MoneroWallet wallet) {
    
    // determine the type upfront
    if (type == null) {
      assertTrue("Transaction type is unknown so type must be specified:\n" + txMap, txMap.containsKey("type"));
      type = getTxType((String) txMap.get("type"));
    }
    
    // determine if the type is outgoing
    boolean isSend = MoneroUtils.isSendTx(type);
    
    // build transaction
    MoneroTx tx = new MoneroTx();
    tx.setType(type);
    MoneroPayment payment = null;
    Integer accountIdx = null;
    Integer subaddressIdx = null;
    for (String key : txMap.keySet()) {
      Object val = txMap.get(key);
      if (key.equalsIgnoreCase("fee")) tx.setFee((BigInteger) val);
      else if (key.equalsIgnoreCase("block_height")) tx.setHeight(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("note")) tx.setNote("".equals(val) ? null : (String) val);
      else if (key.equalsIgnoreCase("timestamp")) tx.setTimestamp(((BigInteger) val).longValue());
      else if (key.equalsIgnoreCase("txid")) tx.setId((String) val);
      else if (key.equalsIgnoreCase("tx_hash")) tx.setId((String) val);
      else if (key.equalsIgnoreCase("tx_key")) tx.setKey((String) val);
      else if (key.equalsIgnoreCase("type")) assertEquals(type, getTxType((String) val)); // type already set
      else if (key.equalsIgnoreCase("tx_size")) tx.setSize(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("unlock_time")) tx.setUnlockTime(((BigInteger) val).intValue());
      else if (key.equalsIgnoreCase("global_index")) { } // ignore
      else if (key.equalsIgnoreCase("tx_blob")) tx.setBlob((String) val);
      else if (key.equalsIgnoreCase("tx_metadata")) tx.setMetadata((String) val);
      else if (key.equalsIgnoreCase("double_spend_seen")) tx.setIsDoubleSpend((Boolean) val);
      else if (key.equalsIgnoreCase("confirmations")) {
        if (!MoneroUtils.isConfirmed(tx.getType())) tx.setNumConfirmations(0);
        else tx.setNumConfirmations(((BigInteger) val).intValue());
      }
      else if (key.equalsIgnoreCase("suggested_confirmations_threshold")) {
        if (tx.getType() == MoneroTxType.PENDING || tx.getType() == MoneroTxType.MEMPOOL) tx.setNumEstimatedBlocksUntilConfirmed(((BigInteger) val).intValue());
        else tx.setNumEstimatedBlocksUntilConfirmed(null);
      }
      else if (key.equalsIgnoreCase("height")) {
        int height = ((BigInteger) val).intValue();
        tx.setHeight(height == 0 ? null : height);
      }
      else if (key.equals("amount")) {
        tx.setTotalAmount((BigInteger) val);
        if (!isSend) {
          if (payment == null) payment = new MoneroPayment();
          payment.setAmount((BigInteger) val);
        }
      }
      else if (key.equals("address")) {
        if (isSend) {
          MoneroSubaddress subaddress = tx.getSrcSubaddress();
          if (subaddress == null) {
            subaddress = new MoneroSubaddress();
            tx.setSrcSubaddress(subaddress);
          }
          subaddress.setAddress((String) val);
        }
        else {
          if (payment == null) payment = new MoneroPayment();
          MoneroSubaddress destination = payment.getDestination();
          if (destination == null) {
            destination = new MoneroSubaddress();
            payment.setDestination(destination);
          }
          destination.setAddress((String) val);
        }
      }
      else if (key.equalsIgnoreCase("key_image")) {
        assertFalse(isSend);
        if (payment == null) payment = new MoneroPayment();
        payment.setKeyImage((String) val);
      }
      else if (key.equals("spent")) {
        assertFalse(isSend);
        if (payment == null) payment = new MoneroPayment();
        payment.setIsSpent((boolean) val);
      }
      else if (key.equalsIgnoreCase("payment_id")) {
        if (!MoneroTx.DEFAULT_PAYMENT_ID.equals((String) val)) tx.setPaymentId((String) val); // convert default to null
      }
      else if (key.equalsIgnoreCase("subaddr_index")) {
        if (val instanceof Map) {
          Map<String, Object> subaddrMap = (Map<String, Object>) val;
          accountIdx = ((BigInteger) subaddrMap.get("major")).intValue();
          subaddressIdx = ((BigInteger) subaddrMap.get("minor")).intValue();
        } else {
          subaddressIdx = ((BigInteger) val).intValue();
        }
      }
      else if (key.equalsIgnoreCase("destinations")) {
        assertTrue(isSend);
        List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
        for (Map<String, Object> paymentMap : (List<Map<String, Object>>) val) {
          MoneroPayment aPayment = new MoneroPayment();
          payments.add(aPayment);
          for (String paymentKey : paymentMap.keySet()) {
            if (paymentKey.equals("address")) {
              MoneroSubaddress subaddress = new MoneroSubaddress();
              subaddress.setAddress((String) paymentMap.get(paymentKey));
              aPayment.setDestination(subaddress);
            }
            else if (paymentKey.equals("amount")) aPayment.setAmount((BigInteger) paymentMap.get(paymentKey));
            else throw new MoneroException("Unrecognized transaction destination field: " + paymentKey);
          }
        }
        tx.setPayments(payments);
      }
      else LOGGER.warn("Ignoring unexpected transaction field: '" + key + "'");
    }
    
    // initialize final fields
    if (tx.getPayments() != null) assertNull(payment);
    else if (payment != null) tx.setPayments(new ArrayList<MoneroPayment>(Arrays.asList(payment)));
    if (isSend) {
      MoneroSubaddress subaddress = tx.getSrcSubaddress();
      if (subaddress == null) {
        subaddress = new MoneroSubaddress();
        tx.setSrcSubaddress(subaddress);
      }
      subaddress.setAccountIndex(accountIdx);
      subaddress.setSubaddrIndex(subaddressIdx);
    } else {
      assertNotNull(payment);
      assertEquals(1, tx.getPayments().size());
      MoneroSubaddress destination = payment.getDestination();
      if (destination == null) {
        destination = new MoneroSubaddress();
        payment.setDestination(destination);
      }
      destination.setAccountIndex(accountIdx);
      destination.setSubaddrIndex(subaddressIdx);
    }
    if (type == MoneroTxType.MEMPOOL && tx.getPayments() != null) {
      for (MoneroPayment aPayment : tx.getPayments()) aPayment.setIsSpent(false); // mempool payments are not spent
    }
    
    return tx;
  }
  
  /**
   * Converts a map of transaction lists to a list of MoneroTx.
   * 
   * @param txListMap is the map listing transactions
   * @param accountIdx specifies the tx account index
   * @param type specifies the tx type
   * @param wallet provides context to fully populate transaction (e.g. addresses)
   * @return List<MoneroTx> are the transactions created from the list map
   */
  @SuppressWarnings("unchecked")
  private static List<MoneroTx> txListMapToTxs(Map<String, Object> txListMap, int accountIdx, MoneroTxType type, MoneroWallet wallet) {
    assertTrue("Only send tx conversion supported", MoneroUtils.isSendTx(type));
    
    // get lists
    List<String> ids = (List<String>) txListMap.get("tx_hash_list");
    List<String> keys = (List<String>) txListMap.get("tx_key_list");
    List<String> blobs = (List<String>) txListMap.get("tx_blob_list");
    List<String> metadatas = (List<String>) txListMap.get("tx_metadata_list");
    List<BigInteger> fees = (List<BigInteger>) txListMap.get("fee_list");
    List<BigInteger> amounts = (List<BigInteger>) txListMap.get("amount_list");
    //String multisigTxSet = (String) resultMap.get("multisig_txset");  // TODO (v0.13.0): what to do with this?
    
    // ensure lists are same size
    Set<Integer> sizes = new HashSet<Integer>();
    sizes.addAll(Arrays.asList(ids.size(), blobs.size(), metadatas.size(), fees.size(), amounts.size()));
    if (keys != null) sizes.add(keys.size());
    assertEquals("Response lists are different sizes", 1, sizes.size());
    
    // build transactions
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 0; i < ids.size(); i++) {
      MoneroTx tx = new MoneroTx();
      txs.add(tx);
      int subaddressIdx = 0;  // TODO (monero-wallet-rpc): outgoing transactions do not indicate originating subaddresses
      MoneroSubaddress srcSubaddress = new MoneroSubaddress(accountIdx, subaddressIdx, wallet.getAddress(accountIdx, subaddressIdx)); // TODO: batch fetch addresses outside of this
      tx.setSrcSubaddress(srcSubaddress);
      tx.setTotalAmount(amounts.get(i));
      tx.setFee(fees.get(i));
      tx.setId(ids.get(i));
      if (keys != null) tx.setKey(keys.get(i));
      tx.setBlob(blobs.get(i));
      tx.setMetadata(metadatas.get(i));
      tx.setType(type);
    }
    return txs;
  }
  
  /**
   * Merges a transaction into a unique set of transactions.
   * 
   * @param txs are the collection of transactions to merge into
   * @param tx is the transaction to merge into the collection
   * @param mergePayments specifies if payments should be merged with xor appended to existing payments
   */
  private static void addTx(Collection<MoneroTx> txs, MoneroTx tx, boolean mergePayments) {
    assertNotNull(tx.getId());
    assertNotNull(tx.getType());
    MoneroTx mergedTx = null;
    for (MoneroTx aTx : txs) {
      if (aTx.getId().equals(tx.getId()) && aTx.getType() == tx.getType()) {
        aTx.merge(tx, mergePayments);
        mergedTx = aTx;
      }
    }
    if (mergedTx == null) txs.add(tx);  // add tx if it wasn't merged
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
  
  private Map<Integer, List<Integer>> getAllAccountAndSubaddressIndices() {
    Map<Integer, List<Integer>> indices = new HashMap<Integer, List<Integer>>();
    for (MoneroAccount account : getAccounts()) {
      indices.put(account.getIndex(), getSubaddressIndices(account.getIndex()));
    }
    return indices;
  }
  
  @SuppressWarnings("unchecked")
  private List<Integer> getSubaddressIndices(int accountIdx) {
    List<Integer> subaddressIndices = new ArrayList<Integer>();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_address", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> addresses = (List<Map<String, Object>>) resultMap.get("addresses");
    for (Map<String, Object> address : addresses) {
      subaddressIndices.add(((BigInteger) address.get("address_index")).intValue());
    }
    return subaddressIndices;
  }
}
