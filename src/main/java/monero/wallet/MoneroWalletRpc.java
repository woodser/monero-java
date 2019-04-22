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
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.rpc.MoneroRpc;
import monero.rpc.MoneroRpcException;
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
      
      // these fields are not initialized if subaddress is unused and therefore not returned from `get_balance`
      for (MoneroAccount account : accounts) {
        for (MoneroSubaddress subaddress : account.getSubaddresses()) {
          subaddress.setBalance(BigInteger.valueOf(0));
          subaddress.setUnlockedBalance(BigInteger.valueOf(0));
          subaddress.setNumUnspentOutputs(0);
          subaddress.setNumBlocksToUnlock(0);
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
      
      // these fields are not initialized if subaddress is unused and therefore not returned from `get_balance`
      for (MoneroSubaddress subaddress : subaddresses) {
        subaddress.setBalance(BigInteger.valueOf(0));
        subaddress.setUnlockedBalance(BigInteger.valueOf(0));
        subaddress.setNumUnspentOutputs(0);
        subaddress.setNumBlocksToUnlock(0);
      }

      // fetch and initialize balances
      resp = rpc.sendJsonRequest("get_balance", params);
      result = (Map<String, Object>) resp.get("result");
      if (result.containsKey("per_subaddress")) {
        for (Map<String, Object> rpcSubaddress : (List<Map<String, Object>>) result.get("per_subaddress")) {
          MoneroSubaddress subaddress = convertRpcSubaddress(rpcSubaddress);
          
          // transfer info to existing subaddress object
          for (MoneroSubaddress tgtSubaddress : subaddresses) {
            if (!tgtSubaddress.getIndex().equals(subaddress.getIndex())) continue; // skip to subaddress with same index
            if (subaddress.getBalance() != null) tgtSubaddress.setBalance(subaddress.getBalance());
            if (subaddress.getUnlockedBalance() != null) tgtSubaddress.setUnlockedBalance(subaddress.getUnlockedBalance());
            if (subaddress.getNumUnspentOutputs() != null) tgtSubaddress.setNumUnspentOutputs(subaddress.getNumUnspentOutputs());
            if (subaddress.getNumBlocksToUnlock() != null) tgtSubaddress.setNumBlocksToUnlock(subaddress.getNumBlocksToUnlock());
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

  @SuppressWarnings("unchecked")
  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    params.put("label", label);
    Map<String, Object> resp = rpc.sendJsonRequest("create_address", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // build subaddress object
    MoneroSubaddress subaddress = new MoneroSubaddress();
    subaddress.setAccountIndex(accountIdx);
    subaddress.setIndex(((BigInteger) result.get("address_index")).intValue());
    subaddress.setAddress((String) result.get("address"));
    subaddress.setLabel(label);
    subaddress.setBalance(BigInteger.valueOf(0));
    subaddress.setUnlockedBalance(BigInteger.valueOf(0));
    subaddress.setNumUnspentOutputs(0);
    subaddress.setIsUsed(false);
    subaddress.setNumBlocksToUnlock(0);
    return subaddress;
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    Map<Integer, String> subaddressMap = addressCache.get(accountIdx);
    if (subaddressMap == null) {
      getSubaddresses(accountIdx, null, true);      // cache's all addresses at this account
      return getAddress(accountIdx, subaddressIdx); // uses cache
    }
    String address = subaddressMap.get(subaddressIdx);
    if (address == null) {
      getSubaddresses(accountIdx, null, true);      // cache's all addresses at this account
      return addressCache.get(accountIdx).get(subaddressIdx);
    }
    return address;
  }

  // TODO: use cache
  @SuppressWarnings("unchecked")
  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    
    // fetch result and normalize error if address does not belong to the wallet
    Map<String, Object> result;
    try {
      Map<String, Object> params =  new HashMap<String, Object>();
      params.put("address", address);
      Map<String, Object> resp = rpc.sendJsonRequest("get_address_index", params);
      result = (Map<String, Object>) resp.get("result");
    } catch (MoneroRpcException e) {
      if (e.getCode() == -2) throw new MoneroException("Address does not belong to the wallet");
      throw e;
    }
    
    // convert rpc response
    Map<String, BigInteger> rpcIndices = (Map<String, BigInteger>) result.get("index");
    MoneroSubaddress subaddress = new MoneroSubaddress(address);
    subaddress.setAccountIndex(rpcIndices.get("major").intValue());
    subaddress.setIndex(rpcIndices.get("minor").intValue());
    return subaddress;
  }


  @Override
  public BigInteger getBalance() {
    return getBalances(null, null)[0];
  }

  @Override
  public BigInteger getBalance(int accountIdx) {
    return getBalances(accountIdx, null)[0];
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    return getBalances(accountIdx, subaddressIdx)[0];
  }

  @Override
  public BigInteger getUnlockedBalance() {
    return getBalances(null, null)[1];
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    return getBalances(accountIdx, null)[1];
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    return getBalances(accountIdx, subaddressIdx)[1];
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

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferFilter filter) {
    
    // normalize transfer filter
    if (filter == null) filter = new MoneroTransferFilter();
    if (filter.getTxFilter() == null) filter.setTxFilter(new MoneroTxFilter());
    MoneroTxFilter txFilter = filter.getTxFilter();
    
    // build params for get_transfers rpc call
    Map<String, Object> params = new HashMap<String, Object>();
    boolean canBeConfirmed = !Boolean.FALSE.equals(txFilter.getIsConfirmed()) && !Boolean.TRUE.equals(txFilter.getInTxPool()) && !Boolean.TRUE.equals(txFilter.getIsFailed()) && !Boolean.FALSE.equals(txFilter.getIsRelayed());
    boolean canBeInTxPool = !Boolean.TRUE.equals(txFilter.getIsConfirmed()) && !Boolean.FALSE.equals(txFilter.getInTxPool()) && !Boolean.TRUE.equals(txFilter.getIsFailed()) & !Boolean.FALSE.equals(txFilter.getIsRelayed()) && txFilter.getHeight() == null && txFilter.getMinHeight() == null;
    boolean canBeIncoming = !Boolean.FALSE.equals(filter.getIsIncoming()) && !Boolean.TRUE.equals(filter.getIsOutgoing()) && !Boolean.TRUE.equals(filter.getHasDestinations());
    boolean canBeOutgoing = !Boolean.FALSE.equals(filter.getIsOutgoing()) && !Boolean.TRUE.equals(filter.getIsIncoming());
    params.put("in", canBeIncoming && canBeConfirmed);
    params.put("out", canBeOutgoing && canBeConfirmed);
    params.put("pool", canBeIncoming && canBeInTxPool);
    params.put("pending", canBeOutgoing && canBeInTxPool);
    params.put("failed", !Boolean.FALSE.equals(txFilter.getIsFailed()) && !Boolean.TRUE.equals(txFilter.getIsConfirmed()) && !Boolean.TRUE.equals(txFilter.getInTxPool()));
    if (txFilter.getMinHeight() != null) params.put("min_height", txFilter.getMinHeight()); 
    if (txFilter.getMaxHeight() != null) params.put("max_height", txFilter.getMaxHeight());
    params.put("filter_by_height", txFilter.getMinHeight() != null || txFilter.getMaxHeight() != null);
    if (filter.getAccountIndex() == null) {
      assertTrue("Filter specifies a subaddress index but not an account index", filter.getSubaddressIndex() == null && filter.getSubaddressIndices() == null);
      params.put("all_accounts", true);
    } else {
      params.put("account_index", filter.getAccountIndex());
      
      // set subaddress indices param
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      if (filter.getSubaddressIndex() != null) subaddressIndices.add(filter.getSubaddressIndex());
      if (filter.getSubaddressIndices() != null) {
        for (int subaddressIdx : filter.getSubaddressIndices()) subaddressIndices.add(subaddressIdx);
      }
      if (!subaddressIndices.isEmpty()) params.put("subaddr_indices", new ArrayList<Integer>(subaddressIndices));
    }
    
    // build txs using `get_transfers`
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    Map<String, Object> resp = rpc.sendJsonRequest("get_transfers", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    for (String key : result.keySet()) {
      for (Map<String, Object> rpcTx :((List<Map<String, Object>>) result.get(key))) {
        MoneroTxWallet tx = convertRpcTxWallet(rpcTx, null, null);
        if (tx.getIsConfirmed()) assertTrue(tx.getBlock().getTxs().contains(tx));
//        if (tx.getId().equals("38436c710dfbebfb24a14cddfd430d422e7282bbe94da5e080643a1bd2880b44")) {
//          System.out.println(rpcTx);
//          System.out.println(tx.getOutgoingAmount().compareTo(BigInteger.valueOf(0)) == 0);
//        }
        
        // replace transfer amount with destination sum
        // TODO monero-wallet-rpc: confirmed tx from/to same account has amount 0 but cached transfers
        if (tx.getOutgoingTransfer() != null && Boolean.TRUE.equals(tx.getIsRelayed()) && !Boolean.TRUE.equals(tx.getIsFailed()) &&
            tx.getOutgoingTransfer().getDestinations() != null && tx.getOutgoingAmount().compareTo(BigInteger.valueOf(0)) == 0) {
          MoneroTransfer outgoingTransfer = tx.getOutgoingTransfer();
          BigInteger transferTotal = BigInteger.valueOf(0);
          for (MoneroDestination destination : outgoingTransfer.getDestinations()) transferTotal = transferTotal.add(destination.getAmount());
          tx.getOutgoingTransfer().setAmount(transferTotal);
        }
        
        // merge tx
        mergeTx(txs, tx);
      }
    }
    
    // filter and return transfers
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    for (MoneroTxWallet tx : txs) {
      if (filter.meetsCriteria(tx.getOutgoingTransfer())) transfers.add(tx.getOutgoingTransfer());
      if (tx.getIncomingTransfers() != null) {
        transfers.addAll(Filter.apply(filter, tx.getIncomingTransfers()));
      }
    }
    return transfers;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroOutputWallet> getVouts(MoneroVoutFilter filter) {
    
    // normalize vout filter
    if (filter == null) filter = new MoneroVoutFilter();
    if (filter.getTxFilter() == null) filter.setTxFilter(new MoneroTxFilter());
    
    // determine account and subaddress indices to be queried
    Map<Integer, List<Integer>> indices = new HashMap<Integer, List<Integer>>();
    if (filter.getAccountIndex() != null) {
      Set<Integer> subaddressIndices = new HashSet<Integer>();
      if (filter.getSubaddressIndex() != null) subaddressIndices.add(filter.getSubaddressIndex());
      if (filter.getSubaddressIndices() != null) for (int subaddressIdx : filter.getSubaddressIndices()) subaddressIndices.add(subaddressIdx);
      indices.put(filter.getAccountIndex(), subaddressIndices.isEmpty() ? null : new ArrayList<Integer>(subaddressIndices));  // null will fetch from all subaddresses
    } else {
      assertEquals("Filter specifies a subaddress index but not an account index", null, filter.getSubaddressIndex());
      assertTrue("Filter specifies subaddress indices but not an account index", filter.getSubaddressIndices() == null || filter.getSubaddressIndices().size() == 0);
      indices = getAccountIndices(false);  // fetch all account indices without subaddresses
    }
    
    // collect txs with vouts for each indicated account using `incoming_transfers` rpc call
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("transfer_type", filter.getIsSpent() == null ? "all" : filter.getIsSpent() ? "unavailable" : "available");
    params.put("verbose", true);
    for (int accountIdx : indices.keySet()) {
    
      // send request
      params.put("account_index", accountIdx);
      params.put("subaddr_indices", indices.get(accountIdx));
      Map<String, Object> resp = rpc.sendJsonRequest("incoming_transfers", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      
      // convert response to txs with vouts and merge
      if (!result.containsKey("transfers")) continue;
      for (Map<String, Object> rpcVout : ((List<Map<String, Object>>) result.get("transfers"))) {
        MoneroTxWallet tx = convertRpcTxWalletWithVout(rpcVout);
        mergeTx(txs, tx);
      }
    }
    
    // filter and return vouts
    List<MoneroOutputWallet> vouts = new ArrayList<MoneroOutputWallet>();
    for (MoneroTxWallet tx : txs) {
      vouts.addAll(Filter.apply(filter, tx.getVoutsWallet()));
    }
    return vouts;
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    return rpcExportKeyImages(true);
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    
    // convert key images to rpc parameter
    List<Map<String, Object>> rpcKeyImages = new ArrayList<Map<String, Object>>();
    for (MoneroKeyImage keyImage : keyImages) {
      Map<String, Object> rpcKeyImage = new HashMap<String, Object>();
      rpcKeyImage.put("key_image", keyImage.getHex());
      rpcKeyImage.put("signature", keyImage.getSignature());
      rpcKeyImages.add(rpcKeyImage);
    }
    
    // send rpc request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("signed_key_images", rpcKeyImages);
    Map<String, Object> resp = rpc.sendJsonRequest("import_key_images", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // build and return result
    MoneroKeyImageImportResult importResult = new MoneroKeyImageImportResult();
    importResult.setHeight(((BigInteger) result.get("height")).intValue());
    importResult.setSpentAmount((BigInteger) result.get("spent"));
    importResult.setUnspentAmount((BigInteger) result.get("unspent"));
    return importResult;
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    return rpcExportKeyImages(false);
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
      if (subaddressIndices.size() == 1) tx.getOutgoingTransfer().setSubaddressIndex(subaddressIndices.get(0));
    }
    
    // initialize txs from rpc response
    if (config.getCanSplit()) convertRpcSentTxWallets(result, txs);
    else convertRpcTxWallet(result, txs.get(0), true);
    return txs;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTxWallet> sweepUnlocked(MoneroSendConfig config) {
    
    // validate config
    assertNotNull("Must specify sweep configuration", config);
    assertTrue("Must specify exactly one destination address to sweep to", config.getDestinations() != null && config.getDestinations().size() == 1);
    assertNotNull(config.getDestinations().get(0).getAddress());
    assertNull(config.getDestinations().get(0).getAmount());
    assertNull("Key image defined; use sweepOutput() to sweep an output by its key image", config.getKeyImage());
    
    // determine accounts to sweep from; default to all with unlocked balance if not specified
    List<Integer> accountIndices = new ArrayList<Integer>();
    if (config.getAccountIndex() != null) accountIndices.add(config.getAccountIndex());
    else {
      for (MoneroAccount account : this.getAccounts()) {
        if (account.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) {
          accountIndices.add(account.getIndex());
        }
      }
    }
    
    // common request params
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", config.getDestinations().get(0).getAddress());
    params.put("priority", config.getPriority());
    params.put("mixin", config.getMixin());
    params.put("ring_size", config.getRingSize());
    params.put("unlock_time", config.getUnlockTime());
    params.put("payment_id", config.getPaymentId());
    params.put("do_not_relay", config.getDoNotRelay());
    params.put("below_amount", config.getBelowAmount());
    params.put("get_tx_keys", true);
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    
    // sweep from each account and collect unique transactions
    List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
    for (int accountIdx : accountIndices) {
      params.put("account_index", accountIdx);
      
      // collect transactions for account
      List<MoneroTxWallet> accountTxs = new ArrayList<MoneroTxWallet>();
      
      // determine subaddresses to sweep from; default to all with unlocked balance if not specified
      List<Integer> subaddressIndices = new ArrayList<Integer>();
      if (config.getSubaddressIndices() != null) {
        for (int subaddressIdx : config.getSubaddressIndices()) {
          subaddressIndices.add(subaddressIdx);
        }
      } else {
        for (MoneroSubaddress subaddress : getSubaddresses(accountIdx)) {
          if (subaddress.getUnlockedBalance().compareTo(BigInteger.valueOf(0)) > 0) {
            subaddressIndices.add(subaddress.getIndex());
          }
        }
      }
      if (subaddressIndices.size() == 0) throw new MoneroException("No subaddresses to sweep from");
      
      // sweep each subaddress individually
      if (config.getSweepEachSubaddress() == null || Boolean.TRUE.equals(config.getSweepEachSubaddress())) {
        for (int subaddressIdx : subaddressIndices) {
          params.put("subaddr_indices", new int[] { subaddressIdx });
          Map<String, Object> resp = rpc.sendJsonRequest("sweep_all", params);
          Map<String, Object> result = (Map<String, Object>) resp.get("result");
          
          // initialize tx per subaddress
          List<String> txIds = (List<String>) result.get("tx_hash_list");
          List<MoneroTxWallet> respTxs = new ArrayList<MoneroTxWallet>();
          for (int i = 0; i < txIds.size(); i++) {
            respTxs.add(new MoneroTxWallet());
          }
          
          // initialize fields from response
          convertRpcSentTxWallets(result, respTxs);
          for (MoneroTxWallet tx : respTxs) accountTxs.add(tx);
        }
      }
      
      // sweep all subaddresses together  // TODO monero-wallet-rpc: doesn't this reveal outputs belong to same wallet?
      else {
        params.put("subaddr_indices", subaddressIndices);
        Map<String, Object> resp = rpc.sendJsonRequest("sweep_all", params);  // TODO: test this
        Map<String, Object> result = (Map<String, Object>) resp.get("result");
        
        // initialize tx per subaddress
        List<String> txIds = (List<String>) result.get("tx_hash_list");
        List<MoneroTxWallet> respTxs = new ArrayList<MoneroTxWallet>();
        for (int i = 0; i < txIds.size(); i++) {
          respTxs.add(new MoneroTxWallet());
        }
        
        // initialize fields from response
        convertRpcSentTxWallets(result, respTxs);
        for (MoneroTxWallet tx : respTxs) accountTxs.add(tx);
      }
      
      // initialize known fields of tx and merge transactions from account
      for (MoneroTxWallet tx : accountTxs) {
        tx.setIsConfirmed(false);
        tx.setNumConfirmations(0);
        tx.setInTxPool(config.getDoNotRelay() ? false : true);
        tx.setDoNotRelay(config.getDoNotRelay() ? true : false);
        tx.setIsRelayed(!tx.getDoNotRelay());
        tx.setIsCoinbase(false);
        tx.setIsFailed(false);
        tx.setMixin(config.getMixin());
        MoneroTransfer transfer = tx.getOutgoingTransfer();
        transfer.setAddress(getAddress(accountIdx, 0));
        transfer.setAccountIndex(accountIdx);
        //transfer.setSubaddressIndex(0); // TODO (monero-wallet-rpc): outgoing subaddress idx is always 0
        MoneroDestination destination = new MoneroDestination(config.getDestinations().get(0).getAddress(), transfer.getAmount());
        transfer.setDestinations(Arrays.asList(destination));
        tx.setOutgoingTransfer(transfer);
        tx.setPaymentId(config.getPaymentId());
        if (tx.getUnlockTime() == null) tx.setUnlockTime(config.getUnlockTime() == null ? 0 : config.getUnlockTime());
        if (!tx.getDoNotRelay()) {
          if (tx.getLastRelayedTimestamp() == null) tx.setLastRelayedTimestamp(System.currentTimeMillis());  // TODO (monero-wallet-rpc): provide timestamp on response; unconfirmed timestamps vary
          if (tx.getIsDoubleSpend() == null) tx.setIsDoubleSpend(false);
        }
        mergeTx(txs, tx);
      }
    }
    
    // return transactions from all accounts
    return txs;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroTxWallet> sweepDust(boolean doNotRelay) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("do_not_relay", doNotRelay);
    Map<String, Object> resp = rpc.sendJsonRequest("sweep_dust", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    if (!result.containsKey("tx_hash_list")) return new ArrayList<MoneroTxWallet>();  // no dust to sweep
    List<MoneroTxWallet> txs = convertRpcSentTxWallets(result, null);
    for (MoneroTxWallet tx : txs) {
      tx.setIsRelayed(!doNotRelay);
      tx.setInTxPool(tx.getIsRelayed());
    }
    return txs;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroTxWallet sweepOutput(MoneroSendConfig config) {
    
    // validate config
    assertNull(config.getSweepEachSubaddress());
    assertNull(config.getBelowAmount());
    assertNull("Splitting is not applicable when sweeping output", config.getCanSplit());
    
    // build request parameters
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", config.getDestinations().get(0).getAddress());
    params.put("account_index", config.getAccountIndex());
    params.put("subaddr_indices", config.getSubaddressIndices());
    params.put("key_image", config.getKeyImage());
    params.put("mixin", config.getMixin());
    params.put("ring_size", config.getRingSize());
    params.put("unlock_time", config.getUnlockTime());
    params.put("do_not_relay", config.getDoNotRelay());
    params.put("priority", config.getPriority());
    params.put("payment_id", config.getPaymentId());
    params.put("get_tx_key", true);
    params.put("get_tx_hex", true);
    params.put("get_tx_metadata", true);
    
    // send request
    Map<String, Object> resp = (Map<String, Object>) rpc.sendJsonRequest("sweep_single", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");

    // build and return tx response
    MoneroTxWallet tx = initSentTxWallet(config, null);
    convertRpcTxWallet(result, tx, true);
    tx.getOutgoingTransfer().getDestinations().get(0).setAmount(tx.getOutgoingTransfer().getAmount());  // initialize destination amount
    return tx;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> relayTxs(List<String> txMetadatas) {
    if (txMetadatas == null || txMetadatas.isEmpty()) throw new MoneroException("Must provide an array of tx metadata to relay");
    List<String> txIds = new ArrayList<String>();
    for (String txMetadata : txMetadatas) {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("hex", txMetadata);
      Map<String, Object> resp = rpc.sendJsonRequest("relay_tx", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      txIds.add((String) result.get("tx_hash"));
    }
    return txIds;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<String> getTxNotes(List<String> txIds) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txIds);
    Map<String, Object> resp = rpc.sendJsonRequest("get_tx_notes", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (List<String>) result.get("notes");
  }

  @Override
  public void setTxNotes(List<String> txIds, List<String> notes) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txIds);
    params.put("notes", notes);
    rpc.sendJsonRequest("set_tx_notes", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public String sign(String msg) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("data", msg);
    Map<String, Object> resp = rpc.sendJsonRequest("sign", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean verify(String msg, String address, String signature) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("data", msg);
    params.put("address", address);
    params.put("signature", signature);
    Map<String, Object> resp = rpc.sendJsonRequest("verify", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (boolean) result.get("good");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getTxKey(String txId) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    Map<String, Object> resp = rpc.sendJsonRequest("get_tx_key", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("tx_key");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCheckTx checkTxKey(String txId, String txKey, String address) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    params.put("tx_key", txKey);
    params.put("address", address);
    Map<String, Object> resp = rpc.sendJsonRequest("check_tx_key", params);
    
    // interpret result
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    MoneroCheckTx check = new MoneroCheckTx();
    check.setIsGood(true);
    check.setNumConfirmations(((BigInteger) result.get("confirmations")).intValue());
    check.setInTxPool((Boolean) result.get("in_pool"));
    check.setReceivedAmount((BigInteger) result.get("received"));
    return check;
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getTxProof(String txId, String address, String message) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    params.put("address", address);
    params.put("message", message);
    Map<String, Object> resp = rpc.sendJsonRequest("get_tx_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCheckTx checkTxProof(String txId, String address, String message, String signature) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    params.put("address", address);
    params.put("message", message);
    params.put("signature", signature);
    Map<String, Object> resp = rpc.sendJsonRequest("check_tx_proof", params);
    
    // interpret response
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    boolean isGood = (boolean) result.get("good");
    MoneroCheckTx check = new MoneroCheckTx();
    check.setIsGood(isGood);
    if (isGood) {
      check.setNumConfirmations(((BigInteger) result.get("confirmations")).intValue());
      check.setInTxPool((boolean) result.get("in_pool"));
      check.setReceivedAmount((BigInteger) result.get("received"));
    }
    return check;
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getSpendProof(String txId, String message) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    params.put("message", message);
    Map<String, Object> resp = rpc.sendJsonRequest("get_spend_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean checkSpendProof(String txId, String message, String signature) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txid", txId);
    params.put("message", message);
    params.put("signature", signature);
    Map<String, Object> resp = rpc.sendJsonRequest("check_spend_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (boolean) result.get("good");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getReserveProofWallet(String message) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("all", true);
    params.put("message", message);
    Map<String, Object> resp = rpc.sendJsonRequest("get_reserve_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("account_index", accountIdx);
    params.put("amount", amount.toString());
    params.put("message", message);
    Map<String, Object> resp = rpc.sendJsonRequest("get_reserve_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("signature");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    
    // send request
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", address);
    params.put("message", message);
    params.put("signature", signature);
    Map<String, Object> resp = rpc.sendJsonRequest("check_reserve_proof", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    
    // interpret results
    boolean isGood = (boolean) result.get("good");
    MoneroCheckReserve check = new MoneroCheckReserve();
    check.setIsGood(isGood);
    if (isGood) {
      check.setSpentAmount((BigInteger) result.get("spent"));
      check.setTotalAmount((BigInteger) result.get("total"));
    }
    return check;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("entries", entryIndices);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_address_book", params);
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
  public int addAddressBookEntry(String address, String description, String paymentId) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", address);
    params.put("payment_id", paymentId);
    params.put("description", description);
    Map<String, Object> respMap = rpc.sendJsonRequest("add_address_book", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return ((BigInteger) resultMap.get("index")).intValue();
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("index", entryIdx);
    rpc.sendJsonRequest("delete_address_book", params);
  }
  
  @Override
  public void tagAccounts(String tag, List<Integer> accountIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("tag", tag);
    params.put("accounts", accountIndices);
    rpc.sendJsonRequest("tag_accounts", params);
  }

  @Override
  public void untagAccounts(List<Integer> accountIndices) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("accounts", accountIndices);
    rpc.sendJsonRequest("untag_accounts", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroAccountTag> getAccountTags() {
    List<MoneroAccountTag> tags = new ArrayList<MoneroAccountTag>();
    Map<String, Object> respMap = rpc.sendJsonRequest("get_account_tags");
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
    rpc.sendJsonRequest("set_account_tag_description", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public String createPaymentUri(MoneroSendConfig sendConfig) {
    assertNotNull("Must provide send configuration to create a payment URI", sendConfig);
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("address", sendConfig.getDestinations().get(0).getAddress());
    params.put("amount", sendConfig.getDestinations().get(0).getAmount() != null ? sendConfig.getDestinations().get(0).getAmount().toString() : null);
    params.put("payment_id", sendConfig.getPaymentId());
    params.put("recipient_name", sendConfig.getRecipientName());
    params.put("tx_description", sendConfig.getNote());
    Map<String, Object> resp = rpc.sendJsonRequest("make_uri", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("uri");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroSendConfig parsePaymentUri(String uri) {
    assertNotNull("Must provide URI to parse", uri);
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("uri", uri);
    Map<String, Object> resp = rpc.sendJsonRequest("parse_uri", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    Map<String, Object> rpcUri = (Map<String, Object>) result.get("uri");
    MoneroSendConfig sendConfig = new MoneroSendConfig((String) rpcUri.get("address"), (BigInteger) rpcUri.get("amount"));
    sendConfig.setPaymentId((String) rpcUri.get("payment_id"));
    sendConfig.setRecipientName((String) rpcUri.get("recipient_name"));
    sendConfig.setNote((String) rpcUri.get("tx_description"));
    if ("".equals(sendConfig.getDestinations().get(0).getAddress())) sendConfig.getDestinations().get(0).setAddress(null);
    if ("".equals(sendConfig.getPaymentId())) sendConfig.setPaymentId(null);
    if ("".equals(sendConfig.getRecipientName())) sendConfig.setRecipientName(null);
    if ("".equals(sendConfig.getNote())) sendConfig.setNote(null);
    return sendConfig;
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getOutputsHex() {
    Map<String, Object> resp = rpc.sendJsonRequest("export_outputs");
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("outputs_data_hex");
  }

  @SuppressWarnings("unchecked")
  @Override
  public int importOutputsHex(String outputsHex) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("outputs_data_hex", outputsHex);
    Map<String, Object> resp = rpc.sendJsonRequest("import_outputs", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return ((BigInteger) result.get("num_imported")).intValue();
  }

  @Override
  public void setAttribute(String key, String val) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key", key);
    params.put("value", val);
    rpc.sendJsonRequest("set_attribute", params);
  }

  @SuppressWarnings("unchecked")
  @Override
  public String getAttribute(String key) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("key", key);
    Map<String, Object> resp = rpc.sendJsonRequest("get_attribute", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    return (String) result.get("value");
  }

  @Override
  public void startMining(Integer numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("threads_count", numThreads);
    params.put("backgroundMining", backgroundMining);
    params.put("ignoreBattery", ignoreBattery);
    rpc.sendJsonRequest("start_mining", params);
  }

  @Override
  public void stopMining() {
    rpc.sendJsonRequest("stop_mining");
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
      else if (key.equals("blocks_to_unlock")) subaddress.setNumBlocksToUnlock(((BigInteger) val).intValue());
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
   * Common method to get key images.
   * 
   * @param all specifies to get all xor only new images from last import
   * @return {MoneroKeyImage[]} are the key images
   */
  @SuppressWarnings("unchecked")
  private List<MoneroKeyImage> rpcExportKeyImages(boolean all) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("all", all);
    Map<String, Object> resp = rpc.sendJsonRequest("export_key_images", params);
    Map<String, Object> result = (Map<String, Object>) resp.get("result");
    List<MoneroKeyImage> images = new ArrayList<MoneroKeyImage>();
    if (!result.containsKey("signed_key_images")) return images;
    for (Map<String, Object> rpcImage : (List<Map<String, Object>>) result.get("signed_key_images")) {
      images.add(new MoneroKeyImage((String) rpcImage.get("key_image"), (String) rpcImage.get("signature")));
    }
    return images;
  }
  
  @SuppressWarnings("unchecked")
  private BigInteger[] getBalances(Integer accountIdx, Integer subaddressIdx) {
    if (accountIdx == null) {
      assertNull("Must provide account index with subaddress index", subaddressIdx);
      BigInteger balance = BigInteger.valueOf(0);
      BigInteger unlockedBalance = BigInteger.valueOf(0);
      for (MoneroAccount account : getAccounts()) {
        balance = balance.add(account.getBalance());
        unlockedBalance = unlockedBalance.add(account.getUnlockedBalance());
      }
      return new BigInteger[] { balance, unlockedBalance };
    } else {
      Map<String, Object> params = new HashMap<String, Object>();
      params.put("account_index", accountIdx);
      params.put("address_indices", subaddressIdx == null ? null : new Integer[] { subaddressIdx });
      Map<String, Object> resp = rpc.sendJsonRequest("get_balance", params);
      Map<String, Object> result = (Map<String, Object>) resp.get("result");
      if (subaddressIdx == null) return new BigInteger[] { (BigInteger) result.get("balance"), (BigInteger) result.get("unlocked_balance") };
      else {
        List<Map<String, Object>> rpcBalancesPerSubaddress = (List<Map<String, Object>>) result.get("per_subaddress");
        return new BigInteger[] { (BigInteger) rpcBalancesPerSubaddress.get(0).get("balance"), (BigInteger) rpcBalancesPerSubaddress.get(0).get("unlocked_balance") };
      }
    }
  }
  
  // ---------------------------- PRIVATE STATIC ------------------------------
  
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
    //transfer.setSubaddressIndex(0); // TODO (monero-wallet-rpc): outgoing subaddress idx is always 0
    List<MoneroDestination> destCopies = new ArrayList<MoneroDestination>();
    for (MoneroDestination dest : config.getDestinations()) destCopies.add(dest.copy());
    transfer.setDestinations(destCopies);
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
        if (tx.getInTxPool()) tx.setNumSuggestedConfirmations(((BigInteger) val).intValue());
        else tx.setNumSuggestedConfirmations(null);
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
        if (!MoneroTxWallet.DEFAULT_PAYMENT_ID.equals(val)) tx.setPaymentId((String) val);  // default is undefined
      }
      else if (key.equals("subaddr_index")) assertTrue(rpcTx.containsKey("subaddr_indices")); // handled by subaddr_indices
      else if (key.equals("subaddr_indices")) {
        Map<String, Object> rpcIndices = ((List<Map<String, Object>>) val).get(0);
        accountIdx = ((BigInteger) rpcIndices.get("major")).intValue();
        subaddressIdx = ((BigInteger) rpcIndices.get("minor")).intValue();
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
    if (header != null) tx.setBlock(new MoneroBlock(header).setTxs(new ArrayList<MoneroTx>(Arrays.asList(tx))));
    
    // initialize final fields
    if (transfer != null) {
      transfer.setAccountIndex(accountIdx);
      transfer.setSubaddressIndex(subaddressIdx);
      if (isOutgoing) {
        if (tx.getOutgoingTransfer() != null) tx.getOutgoingTransfer().merge(transfer);
        else tx.setOutgoingTransfer(transfer);
      } else {
        tx.setIncomingTransfers(new ArrayList<MoneroTransfer>(Arrays.asList(transfer)));
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
  
  private static void mergeTx(List<MoneroTxWallet> txs, MoneroTxWallet tx) {
    mergeTx(txs, tx, false);
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
          if (aTx.getBlock() == null) aTx.setBlock(new MoneroBlock().setTxs(new ArrayList<MoneroTx>(Arrays.asList(aTx))).setHeight(tx.getHeight()));
          if (tx.getBlock() == null) tx.setBlock(new MoneroBlock().setTxs(new ArrayList<MoneroTx>(Arrays.asList(tx))).setHeight(aTx.getHeight()));
          aTx.getBlock().merge(tx.getBlock());
        } else {
          aTx.merge(tx);
        }
        return;
      }
      
      // merge common block of different txs
      if (tx.getHeight() != null && tx.getHeight().equals(aTx.getHeight())) {
        aTx.getBlock().merge(tx.getBlock());
        if (aTx.getIsConfirmed()) assertTrue(aTx.getBlock().getTxs().contains(aTx));
      }
    }
    
    // add tx if it doesn't already exist unless skipped
    if (!skipIfAbsent) {
      txs.add(tx);
    } else {
      LOGGER.warn("WARNING: tx does not already exist"); 
    }
  }
  
  @SuppressWarnings("unchecked")
  private static MoneroTxWallet convertRpcTxWalletWithVout(Map<String, Object> rpcVout) {
    
    // initialize tx
    MoneroTxWallet tx = new MoneroTxWallet();
    tx.setIsConfirmed(true);
    tx.setIsRelayed(true);
    tx.setIsFailed(false);
    
    // initialize vout
    MoneroOutputWallet vout = new MoneroOutputWallet().setTx(tx);
    for (String key : rpcVout.keySet()) {
      Object val = rpcVout.get(key);
      if (key.equals("amount")) vout.setAmount((BigInteger) val);
      else if (key.equals("spent")) vout.setIsSpent((Boolean) val);
      else if (key.equals("key_image")) vout.setKeyImage(new MoneroKeyImage((String) val));
      else if (key.equals("global_index")) vout.setIndex(((BigInteger) val).intValue());
      else if (key.equals("tx_hash")) tx.setId((String) val);
      else if (key.equals("unlocked")) vout.setIsUnlocked((Boolean) val);
      else if (key.equals("subaddr_index")) {
        Map<String, BigInteger> rpcIndices = (HashMap<String, BigInteger>) val;
        vout.setAccountIndex(rpcIndices.get("major").intValue());
        vout.setSubaddressIndex(rpcIndices.get("minor").intValue());
      }
      else LOGGER.warn("WARNING: ignoring unexpected transaction field: " + key + ": " + val);
    }
    
    // initialize tx with vout
    List<MoneroOutput> vouts = new ArrayList<MoneroOutput>();
    vouts.add((MoneroOutput) vout); // have to cast to extended type because Java paramaterized types do not recognize inheritance
    tx.setVouts(vouts);
    return tx;
  }
}
