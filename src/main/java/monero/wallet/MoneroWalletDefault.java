package monero.wallet;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import monero.utils.MoneroException;
import monero.wallet.config.MoneroSendConfig;
import monero.wallet.config.MoneroTransferFilter;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroSendPriority;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncProgressListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroWalletOutput;
import monero.wallet.model.MoneroWalletTx;

/**
 * Default implementation of a Monero Wallet.
 */
public abstract class MoneroWalletDefault implements MoneroWallet {
  
  @Override
  public MoneroSyncResult sync(Integer startHeight, MoneroSyncProgressListener listener) {
    return sync(startHeight, null, listener);
  }
  
  @Override
  public MoneroSyncResult sync(Integer startHeight) {
    return sync(startHeight, null, null);
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
  public MoneroAccount createAccount() {
    return createAccount(null);
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
  public MoneroSubaddress createSubaddress(int accountIdx) {
    return createSubaddress(accountIdx,  null);
  }
  
  @Override
  public List<MoneroWalletTx> getTxs() {
    return getTxs(null);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers() {
    return getTransfers(null);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(int accountIdx) {
    MoneroTransferFilter filter = new MoneroTransferFilter();
    filter.setTransfer(new MoneroTransfer().setAccountIndex(accountIdx));
    return getTransfers(filter);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(int accountIdx, int subaddressIdx) {
    MoneroTransferFilter filter = new MoneroTransferFilter();
    filter.setTransfer(new MoneroTransfer().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx));
    return getTransfers(filter);
  }
  
  @Override
  public List<MoneroWalletOutput> getVouts() {
    return getVouts(null);
  }
  
  @Override
  public MoneroWalletTx send(String address, BigInteger sendAmount) {
    return send(new MoneroSendConfig(address, sendAmount));
  }
  
  @Override
  public List<MoneroWalletTx> sendSplit(String address, BigInteger sendAmount) {
    return sendSplit(new MoneroSendConfig(address, sendAmount));
  }
  
  @Override
  public List<MoneroWalletTx> sweepWallet(String address) {
    return sweepUnlocked(new MoneroSendConfig(address));
  }
  
  @Override
  public List<MoneroWalletTx> sweepAccount(int accountIdx, String address) {
    MoneroSendConfig config = new MoneroSendConfig(address);
    config.setAccountIndex(accountIdx);
    return sweepUnlocked(config);
  }
  
  @Override
  public List<MoneroWalletTx> sweepSubaddress(int accountIdx, int subaddressIdx, String address) {
    MoneroSendConfig config = new MoneroSendConfig(address);
    config.setAccountIndex(accountIdx);
    config.setSubaddressIndices(Arrays.asList(subaddressIdx));
    return sweepUnlocked(config);
  }
  
  @Override
  public List<MoneroWalletTx> sweepDust() {
    return sweepDust(false);
  }
  
  @Override
  public MoneroWalletTx sweepOutput(String address, String keyImage, MoneroSendPriority priority) {
    MoneroSendConfig config = new MoneroSendConfig(address, null, priority);
    config.setKeyImage(keyImage);
    return sweepOutput(config);
  }
  
  @Override
  public MoneroWalletTx relayTx(String txMetadata) {
    return relayTxs(Arrays.asList(txMetadata)).get(0);
  }
  
  @Override
  public String getTxNote(String txId) {
    return getTxNotes(Arrays.asList(txId)).get(0);
  }
  
  @Override
  public void setTxNote(String txId, String note) {
    setTxNotes(Arrays.asList(txId), Arrays.asList(note));
  }
  
  @Override
  public String getTxProof(String txId, String address) {
    return getTxProof(txId, address, null);
  }
  
  @Override
  public String getSpendProof(String txId) {
    return getSpendProof(txId, null);
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
