package monero.wallet;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import monero.utils.MoneroException;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSendPriority;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroSendRequest;
import monero.wallet.request.MoneroTransferRequest;
import monero.wallet.request.MoneroTxRequest;

/**
 * Default implementation of a Monero Wallet.
 */
public abstract class MoneroWalletDefault implements MoneroWallet {
  
  @Override
  public String getPrimaryAddress() {
    return getAddress(0, 0);
  }
  
  @Override
  public MoneroIntegratedAddress getIntegratedAddress() {
    return getIntegratedAddress(null);
  }
  
  @Override
  public MoneroSyncResult sync() {
    return sync(null);
  }
  
  @Override
  public MoneroSyncResult sync(Integer startHeight) {
    return sync(startHeight, null, null);
  }
  
  @Override
  public MoneroSyncResult sync(Integer startHeight, MoneroSyncListener listener) {
    return sync(startHeight, null, listener);
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
  public MoneroTxWallet getTx(String txId) {
    return getTxs(txId).get(0);
  }
  
  @Override
  public List<MoneroTxWallet> getTxs() {
    return getTxs(new MoneroTxRequest());
  }
  
  public List<MoneroTxWallet> getTxs(String... txIds) {
    return getTxs(new MoneroTxRequest().setTxIds(txIds));
  }
  
  public List<MoneroTxWallet> getTxs(Collection<String> txIds) {
    return getTxs(new MoneroTxRequest().setTxIds(txIds));
  }
  
  @Override
  public List<MoneroTransfer> getTransfers() {
    return getTransfers(null);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(int accountIdx) {
    MoneroTransferRequest request = new MoneroTransferRequest().setAccountIndex(accountIdx);
    return getTransfers(request);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(int accountIdx, int subaddressIdx) {
    MoneroTransferRequest request = new MoneroTransferRequest().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx);
    return getTransfers(request);
  }
  
  @Override
  public List<MoneroOutputWallet> getOutputs() {
    return getOutputs(null);
  }
  
  @Override
  public MoneroTxWallet send(int accountIndex, String address, BigInteger sendAmount) {
    return send(accountIndex, address, sendAmount, null);
  }
  
  @Override
  public MoneroTxWallet send(int accountIndex, String address, BigInteger sendAmount, MoneroSendPriority priority) {
    return send(new MoneroSendRequest(accountIndex, address, sendAmount, priority));
  }
  
  @Override
  public List<MoneroTxWallet> sendSplit(int accountIndex, String address, BigInteger sendAmount) {
    return sendSplit(new MoneroSendRequest(accountIndex, address, sendAmount));
  }
  
  @Override
  public List<MoneroTxWallet> sendSplit(int accountIndex, String address, BigInteger sendAmount, MoneroSendPriority priority) {
    return sendSplit(new MoneroSendRequest(accountIndex, address, sendAmount, priority));
  }
  
  @Override
  public MoneroTxWallet sweepOutput(String address, String keyImage) {
    return sweepOutput(address, keyImage, null);
  }
  
  @Override
  public MoneroTxWallet sweepOutput(String address, String keyImage, MoneroSendPriority priority) {
    MoneroSendRequest request = new MoneroSendRequest(address).setPriority(priority);
    request.setKeyImage(keyImage);
    return sweepOutput(request);
  }
  
  @Override
  public List<MoneroTxWallet> sweepSubaddress(int accountIdx, int subaddressIdx, String address) {
    MoneroSendRequest request = new MoneroSendRequest(address);
    request.setAccountIndex(accountIdx);
    request.setSubaddressIndices(subaddressIdx);
    return sweepAllUnlocked(request);
  }
  
  @Override
  public List<MoneroTxWallet> sweepAccount(int accountIdx, String address) {
    MoneroSendRequest request = new MoneroSendRequest(address);
    request.setAccountIndex(accountIdx);
    return sweepAllUnlocked(request);
  }
  
  @Override
  public List<MoneroTxWallet> sweepWallet(String address) {
    return sweepAllUnlocked(new MoneroSendRequest(address));
  }
  
  @Override
  public List<MoneroTxWallet> sweepDust() {
    return sweepDust(false);
  }
  
  @Override
  public String relayTx(String txMetadata) {
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
    return addAddressBookEntry(address, description, null);
  }
}
