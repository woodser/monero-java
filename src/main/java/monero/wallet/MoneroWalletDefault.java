/**
 * Copyright (c) 2017-2019 woodser
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package monero.wallet;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import monero.utils.MoneroException;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSendPriority;
import monero.wallet.model.MoneroSendRequest;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;

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
    return sync(null, null);
  }
  
  @Override
  public MoneroSyncResult sync(MoneroSyncListener listener) {
    return sync(null, listener);
  }
  
  @Override
  public MoneroSyncResult sync(Long startHeight) {
    return sync(startHeight, null);
  }
  
  @Override
  public MoneroSyncResult sync(Long startHeight, MoneroSyncListener listener) {
    return sync(startHeight, listener);
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
    return getTxs(new MoneroTxQuery());
  }
  
  public List<MoneroTxWallet> getTxs(String... txIds) {
    return getTxs(new MoneroTxQuery().setTxIds(txIds));
  }
  
  public List<MoneroTxWallet> getTxs(List<String> txIds) {
    return getTxs(new MoneroTxQuery().setTxIds(txIds));
  }
  
  @Override
  public List<MoneroTransfer> getTransfers() {
    return getTransfers(null);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(int accountIdx) {
    MoneroTransferQuery query = new MoneroTransferQuery().setAccountIndex(accountIdx);
    return getTransfers(query);
  }
  
  @Override
  public List<MoneroTransfer> getTransfers(int accountIdx, int subaddressIdx) {
    MoneroTransferQuery query = new MoneroTransferQuery().setAccountIndex(accountIdx).setSubaddressIndex(subaddressIdx);
    return getTransfers(query);
  }
  
  @Override
  public List<MoneroOutputWallet> getOutputs() {
    return getOutputs(null);
  }
  
  @Override
  public MoneroTxWallet createTx(MoneroSendRequest request) {
    if (request == null) throw new MoneroException("Send request cannot be null");
    if (Boolean.TRUE.equals(request.getCanSplit())) throw new MoneroException("Cannot request split transactions with createTx() which prevents splitting; use sendTxs() instead");
    request.setCanSplit(false);
    return createTxs(request).get(0);
  }
  
  @Override
  public MoneroTxWallet createTx(int accountIndex, String address, BigInteger sendAmount) {
    return createTx(accountIndex, address, sendAmount, null);
  }
  
  @Override
  public MoneroTxWallet createTx(int accountIndex, String address, BigInteger sendAmount, MoneroSendPriority priority) {
    return createTx(new MoneroSendRequest(accountIndex, address, sendAmount, priority));
  }
  
  @Override
  public List<MoneroTxWallet> createTxs(MoneroSendRequest request) {
    if (request == null) throw new MoneroException("Send request cannot be null");
    
    // modify request to not relay
    Boolean requestedDoNotRelay = request.getDoNotRelay();
    request.setDoNotRelay(true);
    
    // invoke common method which doesn't relay
    List<MoneroTxWallet> createdTxs = sendSplit(request);
    
    // restore doNotRelay of request and txs
    request.setDoNotRelay(requestedDoNotRelay);
    for (MoneroTxWallet tx : createdTxs) tx.setDoNotRelay(requestedDoNotRelay);
    
    // return results
    return createdTxs;
  }
  
  @Override
  public String relayTx(String txMetadata) {
    return relayTxs(Arrays.asList(txMetadata)).get(0);
  }
  
  @Override
  public String relayTx(MoneroTxWallet tx) {
    return relayTx(tx.getMetadata());
  }
  
  // TODO: this method is not tested
  @Override
  public List<String> relayTxs(List<MoneroTxWallet> txs) {
    List<String> txHexes = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) txHexes.add(tx.getMetadata());
    return relayTxs(txHexes);
  }
  
  @Override
  public MoneroTxWallet send(MoneroSendRequest request) {
    if (request == null) throw new MoneroException("Send request cannot be null");
    if (Boolean.TRUE.equals(request.getCanSplit())) throw new MoneroException("Cannot request split transactions with send() which prevents splitting; use sendSplit() instead");
    request.setCanSplit(false);
    return sendSplit(request).get(0);
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
  public List<MoneroTxWallet> sendSplit(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
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
