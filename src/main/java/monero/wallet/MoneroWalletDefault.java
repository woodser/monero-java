package monero.wallet;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

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
  
  @Override
  public BigInteger getBalance() {
    BigInteger balance = BigInteger.valueOf(0);
    for (MoneroAccount account : getAccounts()) {
      balance = balance.add(account.getBalance());
    }
    return balance;
  }
  
  @Override
  public BigInteger getUnlockedBalance() {
    BigInteger unlockedBalance = BigInteger.valueOf(0);
    for (MoneroAccount account : getAccounts()) {
      unlockedBalance = unlockedBalance.add(account.getUnlockedBalance());
    }
    return unlockedBalance;
  }
  
  @Override
  public List<MoneroAccount> getAccounts() {
    return getAccounts(false, null);
  }
  
  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses) {
    return getAccounts(includeSubaddresses, null);
  }
  
  @Override
  public List<MoneroAccount> getAccounts(String tag) {
    return getAccounts(false, tag);
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
  public List<MoneroTx> sweepAccount(int accountIdx, String address) {
    MoneroTxConfig config = new MoneroTxConfig(address, null, null);
    config.setAccountIndex(accountIdx);
    return sweepAll(config);
  }
  
  @Override
  public List<MoneroTx> sweepSubaddress(int accountIdx, int subaddressIdx, String address) {
    MoneroTxConfig config = new MoneroTxConfig(address, null, null);
    config.setAccountIndex(accountIdx);
    config.setSubaddressIndices(Arrays.asList(subaddressIdx));
    return sweepAll(config);
  }
  
  @Override
  public MoneroTx relayTx(MoneroTx tx) {
    return relayTxs(Arrays.asList(tx)).get(0);
  }
  
  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries() {
    return getAddressBookEntries(null);
  }
  
  @Override
  public int addAddressBookEntry(String address, String description) {
    return addAddressBookEntry(address, null, description);
  }
  
  @Override
  public String getTxNote(String txId) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public void setTxNote(String txId, String note) {
    throw new RuntimeException("Not implemented");
  }
}
