package monero.wallet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
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

import common.types.Filter;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroKeyImage;
import monero.rpc.MoneroRpc;
import monero.utils.MoneroException;
import monero.wallet.config.MoneroSendConfig;
import monero.wallet.config.MoneroTransferFilter;
import monero.wallet.config.MoneroTxFilter;
import monero.wallet.config.MoneroVoutFilter;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncProgressListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;

/**
 * Implements a Monero Wallet using monero-wallet-rpc.
 */
public class MoneroWalletRpc extends MoneroWalletDefault {

  private MoneroRpc rpc;  // handles rpc interactions
  private Map<Integer, Map<Integer, String>> addressCache;  // cache static addresses to reduce requests
  
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
  
  public MoneroWalletRpc(MoneroRpc rpc) {
    this.rpc = rpc;
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }
  
  // TODO: overloaded constructors
  
  // --------------------------- RPC WALLET METHODS ---------------------------
  
  public MoneroRpc getRpc() {
    return rpc;
  }

  /**
   * Rescan the blockchain for spent outputs.
   */
  public void rescanSpent() {
    rpc.sendJsonRequest("rescan_spent");
  }
  
  /**
   * Rescan the blockchain from scratch, losing any information which can not
   * be recovered from the blockchain itself.
   * 
   * WARNING: This method discards local wallet data like destination
   * addresses, tx secret keys, tx notes, etc.
   */
  public void rescanBlockchain() {
    rpc.sendJsonRequest("rescan_blockchain");
  }
  
  /**
   * Create a new wallet file at the remote endpoint.
   * 
   * @param filename is the name of the wallet file to create
   * @param password is the password to decrypt the wallet file
   * @param language is the language for the wallet's mnemonic seed
   */
  public void createWallet(String filename, String password, String language) {
    if (filename == null || filename.isEmpty()) throw new MoneroException("Filename is not initialized");
    if (password == null || password.isEmpty()) throw new MoneroException("Password is not initialized");
    if (language == null || language.isEmpty()) throw new MoneroException("Language is not initialized");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", filename);
    params.put("password", password);
    params.put("language", language);
    rpc.sendJsonRequest("create_wallet", params);
  }
  
  /**
   * Open a wallet file at the remote endpoint.
   * 
   * @param filename is the name of the wallet file to open
   * @param password is the password to decrypt the wallet file
   */
  public void openWallet(String filename, String password) {
    if (filename == null || filename.isEmpty()) throw new MoneroException("Filename is not initialized");
    if (password == null || password.isEmpty()) throw new MoneroException("Password is not initialized");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("filename", filename);
    params.put("password", password);
    rpc.sendJsonRequest("open_wallet", params);
    addressCache.clear();
  }
  
  /**
   * Save the currently open wallet file at the remote endpoint.
   */
  public void save() {
    rpc.sendJsonRequest("store");
  }
  
  /**
   * Close the wallet at the remote endpoint, saving the current state.
   */
  public void close() {
    rpc.sendJsonRequest("stop_wallet");
    addressCache.clear();
  }
  
  // -------------------------- COMMON WALLET METHODS -------------------------

  @Override
  public String getSeed() {
    throw new MoneroException("monero-wallet-rpc does not support getting the wallet seed");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getMnemonic() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_type", "mnemonic");
    Map<String, Object> resp = rpc.sendJsonRequest("query_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("key");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getPublicViewKey() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_type", "view_key");
    Map<String, Object> resp = rpc.sendJsonRequest("query_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("key");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getPrivateViewKey() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key_type", "view_key");
    Map<String, Object> resp = rpc.sendJsonRequest("query_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("key");
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> getLanguages() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_languages");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (List<String>) result.get("languages");
  }

  @SuppressWarnings("unchecked")
  @Override
  public int getHeight() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_height");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return ((BigInteger) result.get("height")).intValue();
  }

  @Override
  public int getChainHeight() {
    throw new MoneroException("monero-wallet-rpc does not support getting the chain height");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getPrimaryAddress() {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", 0);
    params.put("address_index", 0);
    Map<String, Object> resp = rpc.sendJsonRequest("get_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("address");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("payment_id", paymentId);
    Map<String, Object> resp = rpc.sendJsonRequest("make_integrated_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    String integratedAddressStr = (String) result.get("integrated_address");
    return decodeIntegratedAddress(integratedAddressStr);
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("integrated_address", integratedAddress);
    Map<String, Object> resp = rpc.sendJsonRequest("split_integrated_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return new MoneroIntegratedAddress((String) result.get("standard_address"), (String) result.get("payment_id"), integratedAddress);
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroSyncResult sync(Integer startHeight, Integer endHeight, MoneroSyncProgressListener progressListener) {
    if (endHeight != null) throw new MoneroException("Monero Wallet RPC does not support syncing to an end height");
    if (progressListener != null) throw new MoneroException("Monero Wallet RPC does not support reporting sync progress");
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("start_height", startHeight);
    Map<String, Object> resp = rpc.sendJsonRequest("refresh", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return new MoneroSyncResult(((BigInteger) result.get("blocks_fetched")).intValue(), (Boolean) result.get("received_money"));
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean isMultisigImportNeeded() {
    Map<String, Object> resp = rpc.sendJsonRequest("get_balance");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return Boolean.TRUE.equals((Boolean) result.get("multisig_import_needed"));
  }
  
  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    return getAccounts(includeSubaddresses, tag, false);
  }

  @SuppressWarnings("unchecked")
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag, boolean skipBalances) {
    
    // fetch accounts from rpc
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    Map<String, Object> resp = rpc.sendJsonRequest("get_accounts", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // build account objects and fetch subaddresses per account using get_address
    // TODO monero-wallet-rpc: get_address should support all_accounts so not called once per account
    List<MoneroAccount> accounts = new ArrayList<MoneroAccount>();
    for (Map<String, Object> rpcAccount : (List<Map<String, Object>>) result.get("subaddress_accounts")) {
      MoneroAccount account = convertRpcAccount(rpcAccount);
      if (includeSubaddresses) account.setSubaddresses(getSubaddresses(account.getIndex(), null, true));
      accounts.add(account);
    }
    
    // fetch and merge fields from get_balance across all accounts
    if (includeSubaddresses && !skipBalances) {
      
      // these fields are not returned from rpc if 0 so pre-initialize them
      for (MoneroAccount account : accounts) {
        for (MoneroSubaddress subaddress : account.getSubaddresses()) {
          subaddress.setBalance(BigInteger.valueOf(0));
          subaddress.setUnlockedBalance(BigInteger.valueOf(0));
          subaddress.setNumUnspentOutputs(0);
        }
      }
      
      // fetch and merge info from get_balance
      params.clear();
      params.put("all_accounts", true);
      resp = rpc.sendJsonRequest("get_balance", params);
      result = (Map<String, Object>) resp.get("result");
      if (result.containsKey("per_subaddress")) {
        for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("per_subaddress")) {
          MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
          
          // merge info
          MoneroAccount account = accounts.get(subaddress.getAccountIndex());
          assertEquals("RPC accounts are out of order", account.getIndex(), subaddress.getAccountIndex());  // would need to switch lookup to loop
          MoneroSubaddress tgtSubaddress = account.getSubaddresses().get(subaddress.getIndex());
          assertEquals("RPC subaddresses are out of order", tgtSubaddress.getIndex(), subaddress.getIndex());
          if (subaddress.getBalance() != null) tgtSubaddress.setBalance(subaddress.getBalance());
          if (subaddress.getUnlockedBalance() != null) tgtSubaddress.setUnlockedBalance(subaddress.getUnlockedBalance());
          if (subaddress.getNumUnspentOutputs() != null) tgtSubaddress.setNumUnspentOutputs(subaddress.getNumUnspentOutputs());
        }
      }
    }
    
    // return accounts
    return accounts;
  }

  // TODO: getAccountByIndex(), getAccountByTag()
  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    return getAccount(accountIdx, includeSubaddresses, false);
  }
  
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses, boolean skipBalances) {
    if (accountIdx < 0) throw new MoneroException("Account index must be greater than or equal to 0");
    for (MoneroAccount account : getAccounts()) {
      if (account.getIndex() == accountIdx) {
        if (includeSubaddresses) account.setSubaddresses(getSubaddresses(accountIdx, null, skipBalances));
        return account;
      }
    }
    throw new MoneroException("Account with index " + accountIdx + " does not exist");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroAccount createAccount(String label) {
    label = label == null || label.isEmpty() ? null : label;
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("label", label);
    Map<String, Object> resp = rpc.sendJsonRequest("create_account", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return new MoneroAccount(((BigInteger) result.get("account_index")).intValue(), (String) result.get("address"), label, BigInteger.valueOf(0), BigInteger.valueOf(0), null);
  }
  
  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    return getSubaddresses(accountIdx, subaddressIndices, false);
  }

  @SuppressWarnings("unchecked")
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices, boolean skipBalances) {
    
    // fetch subaddresses
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    if (subaddressIndices != null && !subaddressIndices.isEmpty()) params.put("address_index", subaddressIndices);
    Map<String, Object> resp = rpc.sendJsonRequest("get_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // initialize subaddresses
    List<MoneroSubaddress> subaddresses = new ArrayList<MoneroSubaddress>();
    for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("addresses")) {
      MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
      subaddress.setAccountIndex(accountIdx);
      subaddresses.add(subaddress);
    }
    
    // fetch and initialize subaddress balances
    if (!skipBalances) {
      
      // these fields are not returned from rpc if 0 so pre-initialize them
      for (MoneroSubaddress subaddress : subaddresses) {
        subaddress.setBalance(BigInteger.valueOf(0));
        subaddress.setUnlockedBalance(BigInteger.valueOf(0));
        subaddress.setNumUnspentOutputs(0);
      }

      // fetch and initialize balances
      resp = rpc.sendJsonRequest("get_balance", params);
      result = (Map<String, Object>) resp.get("result");
      if (result.containsKey("per_subaddress")) {
        for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("per_subaddress")) {
          MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
          
          // transfer info to existing subaddress object
          for (MoneroSubaddress tgtSubaddress : subaddresses) {
            if (tgtSubaddress.getIndex() != subaddress.getIndex()) continue; // skip to subaddress with same index
            if (subaddress.getBalance() != null) tgtSubaddress.setBalance(subaddress.getBalance());
            if (subaddress.getUnlockedBalance() != null) tgtSubaddress.setUnlockedBalance(subaddress.getUnlockedBalance());
            if (subaddress.getNumUnspentOutputs() != null) tgtSubaddress.setNumUnspentOutputs(subaddress.getNumUnspentOutputs());
          }
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
      subaddressMap.put(subaddress.getIndex(), subaddress.getAddress());
    }
    
    // return results
    return subaddresses;
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance() {
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
  public BigInteger getUnlockedBalance() {
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
  public List<MoneroTxWallet> getTxs(MoneroTxFilter filter) {
    
    // normalize tx filter
    if (filter == null) filter = new MoneroTxFilter();
    if (filter.getTransferFilter() == null) filter.setTransferFilter(new MoneroTransferFilter());
    MoneroTransferFilter transferFilter = filter.getTransferFilter();
    
    // temporarily disable transfer filter
    filter.setTransferFilter(null);
    
    // fetch all transfers that meet tx filter
    List<MoneroTransfer> transfers = getTransfers(new MoneroTransferFilter().setTxFilter(filter));
    
    // collect unique txs from transfers as ordered list
    Set<MoneroTxWallet> txSet = new HashSet<MoneroTxWallet>();
    for (MoneroTransfer transfer : transfers) txSet.add(transfer.getTx());
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>(txSet);
    
    // fetch and merge vouts if configured
    if (Boolean.TRUE.equals(filter.getIncludeVouts())) {
      List<MoneroOutputWallet> vouts = getVouts(new MoneroVoutFilter().setTxFilter(filter));
      Set<MoneroTxWallet> voutTxs = new HashSet<MoneroTxWallet>();
      for (MoneroOutputWallet vout : vouts) voutTxs.add(vout.getTx());
      for (MoneroTxWallet tx : voutTxs) mergeTx(txs, tx, true);
    }
    
    // filter and return txs that meet transfer filter
    filter.setTransferFilter(transferFilter);
    txs = Filter.apply(filter, txs);
    
    // special case: re-fetch txs if inconsistency caused by needing to make multiple rpc calls
    for (MoneroTxWallet tx : txs) {
      if (tx.getIsConfirmed() && tx.getBlock() == null) return getTxs(filter);
    }
    
    // otherwise return txs
    return txs;
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferFilter filter) {
    
    // normalize transfer filter
    if (filter == null) filter = new MoneroTransferFilter();
//    
//    
//    // initialize filters from config
//    let transferFilter;
//    if (config instanceof MoneroTransferFilter) transferFilter = config;
//    else {
//      config = Object.assign({}, config);
//      if (!config.id) config.id = config.txId;  // support txId TODO: move into MoneroTransaction?
//      transferFilter = new MoneroTransferFilter(config);
//      transferFilter.setTxFilter(new MoneroTxFilter(config));
//    }
//    if (!transferFilter.getTxFilter()) transferFilter.setTxFilter(new MoneroTxFilter());
//    let txFilter = transferFilter.getTxFilter();
//    
//    // build params for get_transfers rpc call
//    let params = {};
//    let canBeConfirmed = txFilter.getIsConfirmed() !== false && txFilter.getInTxPool() !== true && txFilter.getIsFailed() !== true && txFilter.getIsRelayed() !== false;
//    let canBeInTxPool = txFilter.getIsConfirmed() !== true && txFilter.getInTxPool() !== false && txFilter.getIsFailed() !== true & txFilter.getIsRelayed() !== false && txFilter.getHeight() === undefined && txFilter.getMinHeight() === undefined;
//    let canBeIncoming = transferFilter.getIsIncoming() !== false && transferFilter.getIsOutgoing() !== true && transferFilter.getHasDestinations() !== true;
//    let canBeOutgoing = transferFilter.getIsOutgoing() !== false && transferFilter.getIsIncoming() !== true;
//    params.in = canBeIncoming && canBeConfirmed;
//    params.out = canBeOutgoing && canBeConfirmed;
//    params.pool = canBeIncoming && canBeInTxPool;
//    params.pending = canBeOutgoing && canBeInTxPool;
//    params.failed = txFilter.getIsFailed() !== false && txFilter.getIsConfirmed() !== true && txFilter.getInTxPool() != true;
//    if (txFilter.getMinHeight() !== undefined) params.min_height = txFilter.getMinHeight(); 
//    if (txFilter.getMaxHeight() !== undefined) params.max_height = txFilter.getMaxHeight();
//    params.filter_by_height = txFilter.getMinHeight() !== undefined || txFilter.getMaxHeight() !== undefined;
//    if (transferFilter.getAccountIndex() === undefined) {
//      assert(transferFilter.getSubaddressIndex() === undefined && transferFilter.getSubaddressIndices() === undefined, "Filter specifies a subaddress index but not an account index");
//      params.all_accounts = true;
//    } else {
//      params.account_index = transferFilter.getAccountIndex();
//      
//      // set subaddress indices param
//      let subaddressIndices = new Set();
//      if (transferFilter.getSubaddressIndex() !== undefined) subaddressIndices.add(transferFilter.getSubaddressIndex());
//      if (transferFilter.getSubaddressIndices() !== undefined) transferFilter.getSubaddressIndices().map(subaddressIdx => subaddressIndices.add(subaddressIdx));
//      if (subaddressIndices.size) params.subaddr_indices = Array.from(subaddressIndices);
//    }
//    
//    // build txs using `get_transfers`
//    let txs = [];
//    let resp = await this.config.rpc.sendJsonRequest("get_transfers", params);
//    for (let key of Object.keys(resp.result)) {
//      for (let rpcTx of resp.result[key]) {
//        if (rpcTx.txid === config.debugTxId) console.log(rpcTx);
//        let tx = MoneroWalletRpc._convertRpcTxWallet(rpcTx);
//        
//        // replace transfer amount with destination sum
//        // TODO monero-wallet-rpc: confirmed tx from/to same account has amount 0 but cached transfers
//        if (tx.getOutgoingTransfer() !== undefined && tx.getIsRelayed() && !tx.getIsFailed() &&
//            tx.getOutgoingTransfer().getDestinations() && tx.getOutgoingAmount().compare(new BigInteger(0)) === 0) {
//          let outgoingTransfer = tx.getOutgoingTransfer();
//          let transferTotal = new BigInteger(0);
//          for (let destination of outgoingTransfer.getDestinations()) transferTotal = transferTotal.add(destination.getAmount());
//          tx.getOutgoingTransfer().setAmount(transferTotal);
//        }
//        
//        // merge tx
//        MoneroWalletRpc._mergeTx(txs, tx);
//      }
//    }
//    
//    // filter and return transfers
//    let transfers = [];
//    for (let tx of txs) {
//      if (transferFilter.meetsCriteria(tx.getOutgoingTransfer())) transfers.push(tx.getOutgoingTransfer());
//      if (tx.getIncomingTransfers()) Filter.apply(transferFilter, tx.getIncomingTransfers()).map(transfer => transfers.push(transfer));
//    }
//    return transfers;
    
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputWallet> getVouts(MoneroVoutFilter filter) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxWallet send(MoneroSendConfig config) {
    
    // normalize configuration
    assertNotNull("Send config must not be null", config);
    if (config.getCanSplit() == null) config.setCanSplit(false);
    else assertEquals(false, config.getCanSplit());
    
    // send with common method
    return sendCommon(config).get(0);
  }

  @Override
  public List<MoneroTxWallet> sendSplit(MoneroSendConfig config) {
    
    // normalize configuration
    assertNotNull("Send config must not be null", config);
    if (config.getCanSplit() == null) config.setCanSplit(true);
    else assertEquals(true, config.getCanSplit());
    
    // send with common method
    return sendCommon(config);
  }
  
  @SuppressWarnings("unchecked")
  private List<MoneroTxWallet> sendCommon(MoneroSendConfig config) {
    
    // validate configuration
    assertNotNull(config.getDestinations());
    assertNotNull(config.getCanSplit());
    assertNull(config.getSweepEachSubaddress());
    assertNull(config.getBelowAmount());
    
    // determine account and subaddresses to send from
    Integer accountIdx = config.getAccountIndex();
    if (accountIdx == null) accountIdx = 0; // default to account 0
    List<Integer> subaddressIndices = config.getSubaddressIndices();
    if (subaddressIndices == null) subaddressIndices = getSubaddressIndices(accountIdx);   
    
    // build request parameters
    Map<String, Object> params = new HashMap<String, Object>();
    List<Map<String, Object>> destinationMaps = new ArrayList<Map<String, Object>>();
    params.put("destinations", destinationMaps);
    for (MoneroDestination destination : config.getDestinations()) {
      assertNotNull("Destination address is not defined", destination.getAddress());
      assertNotNull("Destination amount is not defined", destination.getAmount());
      Map<String, Object> destinationMap = new HashMap<String, Object>();
      destinationMap.put("address", destination.getAddress());
      destinationMap.put("amount", destination.getAmount().toString());
      destinationMaps.add(destinationMap);
    }
    params.put("account_index", accountIdx);
    params.put("subaddr_indices", subaddressIndices);
    params.put("payment_id", config.getPaymentId());
    params.put("mixin", config.getMixin());
    params.put("ring_size", config.getRingSize());
    params.put("unlock_time", config.getUnlockTime());
    params.put("do_not_relay", config.getDoNotRelay());
    params.put("priority", config.getPriority());
    params.put("get_tx_key", true);
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    
    // send request
    Map<String, Object> resp = rpc.sendJsonRequest(config.getCanSplit() ? "transfer_split" : "transfer", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // pre-initialize txs to return
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    if (!config.getCanSplit()) txs.add(new MoneroTxWallet());
    else {
      List<String> txHashes = (List<String>) result.get("tx_hash_list");
      for (int i = 0; i < txHashes.size(); i++) txs.add(new MoneroTxWallet());
    }
    
    // initialize known fields of txs
    for (MoneroTxWallet tx : txs) {
      initSentTxWallet(config, tx);
      tx.getOutgoingTransfer().setAccountIndex(accountIdx);
    }
    
    // initialize txs from rpc response
    if (config.getCanSplit()) convertRpcSentTxWallets(result, txs);
    else convertRpcTxWallet(result, txs.get(0), true);
    return txs;
  }

  @Override
  public List<MoneroTxWallet> sweepUnlocked(MoneroSendConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sweepDust(boolean doNotRelay) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxWallet sweepOutput(MoneroSendConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> relayTxs(List<String> txMetadatas) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxNotes(List<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setTxNotes(List<String> txIds, List<String> notes) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String sign(String msg) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean verify(String msg, String address, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxKey(String txId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckTx checkTxKey(String txId, String txKey, String address) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxProof(String txId, String address, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckTx checkTxProof(String txId, String address, String message, String signature) {
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
  public String getReserveProofWallet(String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int addAddressBookEntry(String address, String description, String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<Integer> tagAccounts(String tag, List<Integer> accountIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void untagAccounts(List<Integer> accountIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccountTag> getAccountTags() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setAccountTagLabel(String tag, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String createPaymentUri(MoneroSendConfig sendConfig) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSendConfig parsePaymentUri(String uri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getOutputsHex() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int importOutputsHex(String outputsHex) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setAttribute(String key, String val) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getAttribute(String key) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void startMining(Integer numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopMining() {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------------------ PRIVATE -----------------------------------
  
  private static MoneroAccount convertRpcAccount(Map<String, Object> rpcAccount) {
    MoneroAccount account = new MoneroAccount();
    for (String key : rpcAccount.keySet()) {
      Object val = rpcAccount.get(key);
      if (key.equals("account_index")) account.setIndex(((BigInteger) val).intValue());
      else if (key.equals("balance")) account.setBalance((BigInteger) val);
      else if (key.equals("unlocked_balance")) account.setUnlockedBalance((BigInteger) val);
      else if (key.equals("base_address")) account.setPrimaryAddress((String) val);
      else if (key.equals("label")) { if (!"".equals(val)) account.setLabel((String) val); }
      else if (key.equals("tag")) account.setTag((String) val);
      else LOGGER.warn("WARNING: ignoring unexpected account field: " + key + ": " + val);
    }
    return account;
  }
  
  private static MoneroSubaddress convertRpcSubaddress(Map<String, Object> rpcSubaddress) {
    MoneroSubaddress subaddress = new MoneroSubaddress();
    for (String key : rpcSubaddress.keySet()) {
      Object val = rpcSubaddress.get(key);
      if (key.equals("account_index")) subaddress.setAccountIndex(((BigInteger) val).intValue());
      else if (key.equals("address_index")) subaddress.setIndex(((BigInteger) val).intValue());
      else if (key.equals("address")) subaddress.setAddress((String) val);
      else if (key.equals("balance")) subaddress.setBalance((BigInteger) val);
      else if (key.equals("unlocked_balance")) subaddress.setUnlockedBalance((BigInteger) val);
      else if (key.equals("num_unspent_outputs")) subaddress.setNumUnspentOutputs(((BigInteger) val).intValue());
      else if (key.equals("label")) { if (!"".equals(val)) subaddress.setLabel((String) val); }
      else if (key.equals("used")) subaddress.setIsUsed((Boolean) val);
      else LOGGER.warn("WARNING: ignoring unexpected subaddress field: " + key + ": " + val);
    }
    return subaddress;
  }
  
  private Map<Integer, List<Integer>> getAccountIndices(boolean getSubaddressIndices) {
    Map<Integer, List<Integer>> indices = new HashMap<Integer, List<Integer>>();
    for (MoneroAccount account : getAccounts()) {
      indices.put(account.getIndex(), getSubaddressIndices ? getSubaddressIndices(account.getIndex()) : null);
    }
    return indices;
  }
  
  @SuppressWarnings("unchecked")
  private List<Integer> getSubaddressIndices(int accountIdx) {
    List<Integer> subaddressIndices = new ArrayList<Integer>();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    Map<String, Object> resp = rpc.sendJsonRequest("get_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    for (Map<String, Object> address : (List<Map<String, Object>>) result.get("addresses")) {
      subaddressIndices.add(((BigInteger) address.get("address_index")).intValue());
    }
    return subaddressIndices;
  }
  
  /**
   * Initializes a sent transaction.
   * 
   * @param {MoneroSendConfig} config is the send configuration
   * @param {MoneroTxWallet} is an existing transaction to initialize (optional)
   * @return {MoneroTxWallet} is the initialized send tx
   */
  private static MoneroTxWallet initSentTxWallet(MoneroSendConfig config, MoneroTxWallet tx) {
    if (tx == null) tx = new MoneroTxWallet();
    tx.setIsConfirmed(false);
    tx.setNumConfirmations(0);
    tx.setInTxPool(Boolean.TRUE.equals(config.getDoNotRelay()) ? false : true);
    tx.setDoNotRelay(Boolean.TRUE.equals(config.getDoNotRelay()) ? true : false);
    tx.setIsRelayed(!Boolean.TRUE.equals(tx.getDoNotRelay()));
    tx.setIsCoinbase(false);
    tx.setIsFailed(false);
    tx.setMixin(config.getMixin());
    MoneroTransfer transfer = new MoneroTransfer().setTx(tx);
    transfer.setSubaddressIndex(0); // TODO (monero-wallet-rpc): outgoing subaddress idx is always 0
    transfer.setDestinations(config.getDestinations());
    tx.setOutgoingTransfer(transfer);
    tx.setPaymentId(config.getPaymentId());
    if (tx.getUnlockTime() == null) tx.setUnlockTime(config.getUnlockTime() == null ? 0 : config.getUnlockTime());
    if (!Boolean.TRUE.equals(tx.getDoNotRelay())) {
      if (tx.getLastRelayedTimestamp() == null) tx.setLastRelayedTimestamp(System.currentTimeMillis());  // TODO (monero-wallet-rpc): provide timestamp on response; unconfirmed timestamps vary
      if (tx.getIsDoubleSpend() == null) tx.setIsDoubleSpend(false);
    }
    return tx;
  }
  
  /**
   * Initializes sent MoneroTxWallet[] from a list of rpc txs.
   * 
   * @param rpcTxs are sent rpc txs to initialize the MoneroTxWallets from
   * @param txs are existing txs to initialize (optional)
   */
  @SuppressWarnings("unchecked")
  private static List<MoneroTxWallet> convertRpcSentTxWallets(Map<String, Object> rpcTxs, List<MoneroTxWallet> txs) {
    
    // get lists
    List<String> ids = (List<String>) rpcTxs.get("tx_hash_list");
    List<String> keys = (List<String>) rpcTxs.get("tx_key_list");
    List<String> blobs = (List<String>) rpcTxs.get("tx_blob_list");
    List<String> metadatas = (List<String>) rpcTxs.get("tx_metadata_list");
    List<BigInteger> fees = (List<BigInteger>) rpcTxs.get("fee_list");
    List<BigInteger> amounts = (List<BigInteger>) rpcTxs.get("amount_list");
    
    // ensure all lists are the same size
    Set<Integer> sizes = new HashSet<Integer>(Arrays.asList(ids.size(), blobs.size(), metadatas.size(), fees.size(), amounts.size()));
    assertEquals("RPC lists are different sizes", 1, sizes.size());
    
    // pre-initialize txs if necessary
    if (txs == null) {
      txs = new ArrayList<MoneroTxWallet>();
      for (int i = 0; i < ids.size(); i++) txs.add(new MoneroTxWallet());
    }
    
    // build transactions
    for (int i = 0; i < ids.size(); i++) {
      MoneroTxWallet tx = txs.get(i);
      tx.setId(ids.get(i));
      if (keys != null) tx.setKey(keys.get(i));
      tx.setFullHex(blobs.get(i));
      tx.setMetadata(metadatas.get(i));
      tx.setFee((BigInteger) fees.get(i));
      if (tx.getOutgoingTransfer() != null) tx.getOutgoingTransfer().setAmount((BigInteger) amounts.get(i));
      else tx.setOutgoingTransfer(new MoneroTransfer().setTx(tx).setAmount((BigInteger) amounts.get(i)));
    }
    return txs;
  }
  
  /**
   * Builds a MoneroTxWallet from a RPC tx.
   * 
   * @param rpcTx is the rpc tx to build from
   * @param tx is an existing tx to continue initializing (optional)
   * @param isOutgoing specifies if the tx is outgoing if true, incoming if false, or decodes from type if undefined
   * @returns {MoneroTxWallet} is the initialized tx
   */
  @SuppressWarnings("unchecked")
  private static MoneroTxWallet convertRpcTxWallet(Map<String, Object> rpcTx, MoneroTxWallet tx, Boolean isOutgoing) {  // TODO: change everything to safe set
        
    // initialize tx to return
    if (tx == null) tx = new MoneroTxWallet();
    
    // initialize tx state from rpc type
    if (rpcTx.containsKey("type")) isOutgoing = decodeRpcType((String) rpcTx.get("type"), tx);
    else {
      assertNotNull("Must indicate if tx is outgoing (true) xor incoming (false) since unknown", isOutgoing);
      assertNotNull(tx.getIsConfirmed());
      assertNotNull(tx.getInTxPool());
      assertNotNull(tx.getIsCoinbase());
      assertNotNull(tx.getIsFailed());
      assertNotNull(tx.getDoNotRelay());
    }
    
    // TODO: safe set
    // initialize remaining fields  TODO: seems this should be part of common function with DaemonRpc._convertRpcTx
    MoneroBlockHeader header = null;
    MoneroTransfer transfer = null;
    Integer accountIdx = null;
    Integer subaddressIdx = null;
    for (String key : rpcTx.keySet()) {
      Object val = rpcTx.get(key);
      if (key.equals("txid")) tx.setId((String) val);
      else if (key.equals("tx_hash")) tx.setId((String) val);
      else if (key.equals("fee")) tx.setFee((BigInteger) val);
      else if (key.equals("note")) { if (!"".equals(val)) tx.setNote((String) val); }
      else if (key.equals("tx_key")) tx.setKey((String) val);
      else if (key.equals("type")) { } // type already handled
      else if (key.equals("tx_size")) tx.setSize(((BigInteger) val).intValue());
      else if (key.equals("unlock_time")) tx.setUnlockTime(((BigInteger) val).intValue());
      else if (key.equals("tx_blob")) tx.setFullHex((String) val);
      else if (key.equals("tx_metadata")) tx.setMetadata((String) val);
      else if (key.equals("double_spend_seen")) tx.setIsDoubleSpend((Boolean) val);
      else if (key.equals("block_height") || key.equals("height")) {
        if (tx.getIsConfirmed()) {
          if (header == null) header = new MoneroBlockHeader();
          header.setHeight(((BigInteger) val).intValue());
        }
      }
      else if (key.equals("timestamp")) {
        if (tx.getIsConfirmed()) {
          if (header == null) header = new MoneroBlockHeader();
          header.setTimestamp(((BigInteger) val).longValue());
        } else {
          tx.setReceivedTimestamp(((BigInteger) val).longValue());
        }
      }
      else if (key.equals("confirmations")) {
        if (!tx.getIsConfirmed()) tx.setNumConfirmations(0);
        else tx.setNumConfirmations(((BigInteger) val).intValue());
      }
      else if (key.equals("suggested_confirmations_threshold")) {
        if (tx.getInTxPool()) tx.setNumEstimatedBlocksUntilConfirmed(((BigInteger) val).intValue());
        else tx.setNumEstimatedBlocksUntilConfirmed(null);
      }
      else if (key.equals("amount")) {
        if (transfer == null) transfer = new MoneroTransfer().setTx(tx);
        transfer.setAmount((BigInteger) val);
      }
      else if (key.equals("address")) {
        if (transfer == null) transfer = new MoneroTransfer().setTx(tx);
        transfer.setAddress((String) val);
      }
      else if (key.equals("payment_id")) {
        if (MoneroTxWallet.DEFAULT_PAYMENT_ID != val) tx.setPaymentId((String) val);  // default is undefined
      }
      else if (key.equals("subaddr_index")) {
        if (val instanceof Map) { // returned structure can be a subaddress index or a map containing major and minor
          Map<String, Object> indices = (Map<String, Object>) val;
          accountIdx = ((BigInteger) indices.get("major")).intValue();
          subaddressIdx = ((BigInteger) indices.get("minor")).intValue();
        } else {
          subaddressIdx = ((BigInteger) val).intValue();
        }
      }
      else if (key.equals("destinations")) {
        assertTrue(isOutgoing);
        List<MoneroDestination> destinations = new ArrayList<MoneroDestination>();
        for (Map<String, Object> rpcDestination : (List<Map<String, Object>>) val) {
          MoneroDestination destination = new MoneroDestination();
          destinations.add(destination);
          for (String destinationKey : rpcDestination.keySet()) {
            if (destinationKey.equals("address")) destination.setAddress((String) rpcDestination.get(destinationKey));
            else if (destinationKey.equals("amount")) destination.setAmount((BigInteger) rpcDestination.get(destinationKey));
            else throw new MoneroException("Unrecognized transaction destination field: " + destinationKey);
          }
        }
        if (transfer == null) transfer = new MoneroTransfer().setTx(tx);
        transfer.setDestinations(destinations);
      }
      else if (key.equals("multisig_txset") && val != null) {} // TODO: handle this with value
      else if (key.equals("unsigned_txset") && val != null) {} // TODO: handle this with value
      else LOGGER.warn("WARNING: ignoring unexpected transaction field: " + key + ": " + val);
    }
    
    // link block and tx
    if (header != null) tx.setBlock(new MoneroBlock(header).setTxs(Arrays.asList(tx)));
    
    // initialize final fields
    if (transfer != null) {
      transfer.setAccountIndex(accountIdx);
      transfer.setSubaddressIndex(subaddressIdx);
      if (isOutgoing) {
        if (tx.getOutgoingTransfer() != null) tx.getOutgoingTransfer().merge(transfer);
        else tx.setOutgoingTransfer(transfer);
      } else {
        tx.setIncomingTransfers(Arrays.asList(transfer));
      }
    }
    
    // return initialized transaction
    return tx;
  }
  
  /**
   * Decodes a "type" from monero-wallet-rpc to initialize type and state
   * fields in the given transaction.
   * 
   * TODO: these should be safe set
   * 
   * @param rpcType is the type to decode
   * @param tx is the transaction to decode known fields to
   * @return {boolean} true if the rpc type indicates outgoing xor incoming
   */
  private static boolean decodeRpcType(String rpcType, MoneroTxWallet tx) {
    boolean isOutgoing;
    if (rpcType.equals("in")) {
      isOutgoing = false;
      tx.setIsConfirmed(true);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setDoNotRelay(false);
      tx.setIsFailed(false);
      tx.setIsCoinbase(false);
    } else if (rpcType.equals("out")) {
      isOutgoing = true;
      tx.setIsConfirmed(true);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setDoNotRelay(false);
      tx.setIsFailed(false);
      tx.setIsCoinbase(false);
    } else if (rpcType.equals("pool")) {
      isOutgoing = false;
      tx.setIsConfirmed(false);
      tx.setInTxPool(true);
      tx.setIsRelayed(true);
      tx.setDoNotRelay(false);
      tx.setIsFailed(false);
      tx.setIsCoinbase(false);  // TODO: but could it be?
    } else if (rpcType.equals("pending")) {
      isOutgoing = true;
      tx.setIsConfirmed(false);
      tx.setInTxPool(true);
      tx.setIsRelayed(true);
      tx.setDoNotRelay(false);
      tx.setIsFailed(false);
      tx.setIsCoinbase(false);
    } else if (rpcType.equals("block")) {
      isOutgoing = false;
      tx.setIsConfirmed(true);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setDoNotRelay(false);
      tx.setIsFailed(false);
      tx.setIsCoinbase(true);
    } else if (rpcType.equals("failed")) {
      isOutgoing = true;
      tx.setIsConfirmed(false);
      tx.setInTxPool(false);
      tx.setIsRelayed(true);
      tx.setDoNotRelay(false);
      tx.setIsFailed(true);
      tx.setIsCoinbase(false);
    } else {
      throw new MoneroException("Unrecognized transfer type: " + rpcType);
    }
    return isOutgoing;
  }
  
  /**
   * Merges a transaction into a unique set of transactions.
   * 
   * TODO monero-wallet-rpc: skipIfAbsent only necessary because incoming payments not returned
   * when sent from/to same account
   * 
   * @param txs are existing transactions to merge into
   * @param tx is the transaction to merge into the existing txs
   * @param skipIfAbsent specifies if the tx should not be added
   *        if it doesn't already exist.  Only necessasry to handle
   *        missing incoming payments from #4500. // TODO
   * @returns the merged tx
   */
  private static void mergeTx(List<MoneroTxWallet> txs, MoneroTxWallet tx, boolean skipIfAbsent) {
    assertNotNull(tx.getId());
    for (MoneroTxWallet aTx : txs) {
      
      // merge tx
      if (aTx.getId().equals(tx.getId())) {
        
        // merge blocks which only exist when confirmed
        if (aTx.getBlock() != null || tx.getBlock() != null) {
          if (aTx.getBlock() == null) aTx.setBlock(new MoneroBlock().setTxs(Arrays.asList(aTx)).setHeight(tx.getHeight()));
          if (tx.getBlock() == null) tx.setBlock(new MoneroBlock().setTxs(Arrays.asList(tx)).setHeight(aTx.getHeight()));
          aTx.getBlock().merge(tx.getBlock());
        } else {
          aTx.merge(tx);
        }
        break;
      }
      
      // merge common block of different txs
      if (tx.getHeight() != null && aTx.getHeight() == tx.getHeight()) {
        aTx.getBlock().merge(tx.getBlock());
      }
    }
    
    // add tx if it doesn't already exist unless skipped
    if (!skipIfAbsent) {
      txs.add(tx);
    } else {
      LOGGER.warn("WARNING: tx does not already exist"); 
    }
  }
}
