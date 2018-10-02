package monero.wallet;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroException;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxFilter;

/**
 * Default implementation of a Monero Wallet.
 */
public abstract class MoneroWalletDefault implements MoneroWallet {
  
  private Map<Integer, Map<Integer, String>> addressCache;
  
  public MoneroWalletDefault() {
    addressCache = new HashMap<Integer, Map<Integer, String>>();
  }
  
  @Override
  public List<MoneroAccount> getAccounts() {
    return getAccounts(null, false);
  }
  
  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses) {
    return getAccounts(null, includeSubaddresses);
  }
  
  @Override
  public List<MoneroAccount> getAccounts(String tag) {
    return getAccounts(tag, false);
  }
  
  @Override
  public MoneroAccount getAccount(int accountIdx) {
    return getAccount(accountIdx, false);
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx) {
    return getSubaddresses(accountIdx, null);
  }
  
  @Override
  public MoneroSubaddress getSubaddress(int accountIdx, int subaddressIdx) {
    List<MoneroSubaddress> subaddresses = getSubaddresses(accountIdx, Arrays.asList(subaddressIdx));
    if (subaddresses.isEmpty()) throw new MoneroException("Subaddress at index " + subaddressIdx + " is not initialized");
    assertEquals("Only 1 subaddress should be returned", 1, subaddresses.size());
    return subaddresses.get(0);
  }
  
  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    Map<Integer, String> subaddressMap = addressCache.get(accountIdx);
    if (subaddressMap == null) {
      subaddressMap = new HashMap<Integer, String>();
      addressCache.put(accountIdx, subaddressMap);
    } else {
      
    }
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs() {
    return getTxs(null);
  }
  
  @Override
  public List<MoneroTx> getTxs(int accountIdx) {
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setAccountIndex(accountIdx);
    return getTxs(filter);
  }
  
  @Override
  public List<MoneroTx> getTxs(int accountIdx, int subaddressIdx) {
    MoneroTxFilter filter = new MoneroTxFilter();
    filter.setAccountIndex(accountIdx);
    filter.setSubaddressIndices(Arrays.asList(subaddressIdx));
    return getTxs(filter);
  }
  
  @Override
  public MoneroAccount createAccount() {
    return createAccount(null);
  }
  
  @Override
  public MoneroSubaddress createSubaddress(int accountIdx) {
    return createSubaddress(accountIdx,  null);
  }
  
  @Override
  public MoneroTx send(String address, BigInteger amount) {
    return send(new MoneroTxConfig(address, null, amount));
  }
  
  @Override
  public MoneroTx send(String address, String paymentId, BigInteger amount) {
    return send(new MoneroTxConfig(address, paymentId, amount));
  }
  
  @Override
  public List<MoneroTx> sendSplit(String address, BigInteger amount) {
    return sendSplit(new MoneroTxConfig(address, null, amount));
  }
  
  @Override
  public List<MoneroTx> sendSplit(String address, String paymentId, BigInteger amount) {
    return sendSplit(new MoneroTxConfig(address, paymentId, amount));
  }
  
  @Override
  public List<MoneroTx> sweepWallet(String address) {
    return sweepAll(new MoneroTxConfig(address, null, null));
  }
  
  @Override
  public List<MoneroTx> sweepAccount(String address, int accountIdx) {
    MoneroTxConfig config = new MoneroTxConfig(address, null, null);
    config.setAccountIndex(accountIdx);
    return sweepAll(config);
  }
  
  @Override
  public List<MoneroTx> sweepSubaddress(String address, int accountIdx, int subaddressIdx) {
    MoneroTxConfig config = new MoneroTxConfig(address, null, null);
    config.setAccountIndex(accountIdx);
    config.setSubaddressIndices(Arrays.asList(subaddressIdx));
    return sweepAll(config);
  }
  
  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries() {
    return getAddressBookEntries(null);
  }
  
  @Override
  public int addAddressBookEntry(String address, String description) {
    return addAddressBookEntry(address, null, description);
  }
}
