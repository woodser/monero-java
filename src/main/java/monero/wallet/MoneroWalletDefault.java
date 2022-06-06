/**
 * Copyright (c) woodser
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

import common.utils.GenUtils;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroKeyImage;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroMessageSignatureType;
import monero.wallet.model.MoneroOutgoingTransfer;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTransferQuery;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletListenerI;

/**
 * Abstract default implementation of a Monero wallet.
 */
abstract class MoneroWalletDefault implements MoneroWallet {
  
  protected Set<MoneroWalletListenerI> listeners;
  
  public MoneroWalletDefault() {
    this.listeners = new LinkedHashSet<MoneroWalletListenerI>();
  }
  
  @Override
  public void addListener(MoneroWalletListenerI listener) {
    listeners.add(listener);
  }
  
  @Override
  public void removeListener(MoneroWalletListenerI listener) {
    if (!listeners.contains(listener)) throw new MoneroError("Listener is not registered with wallet");
    listeners.remove(listener);
  }
  
  @Override
  public Set<MoneroWalletListenerI> getListeners() {
    return new HashSet<MoneroWalletListenerI>(listeners);
  }
  
  @Override
  public void setDaemonConnection(String uri) {
    setDaemonConnection(uri, null, null);
  }
  
  @Override
  public void setDaemonConnection(String uri, String username, String password) {
    if (uri == null) setDaemonConnection((MoneroRpcConnection) null);
    else setDaemonConnection(new MoneroRpcConnection(uri, username, password));
  }
  
  @Override
  public String getPrimaryAddress() {
    return getAddress(0, 0);
  }
  
  @Override
  public MoneroIntegratedAddress getIntegratedAddress() {
    return getIntegratedAddress(null, null);
  }
  
  @Override
  public MoneroSyncResult sync() {
    return sync(null, null);
  }
  
  @Override
  public MoneroSyncResult sync(MoneroWalletListenerI listener) {
    return sync(null, listener);
  }
  
  @Override
  public MoneroSyncResult sync(Long startHeight) {
    return sync(startHeight, null);
  }
  
  @Override
  public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener) {
    return sync(startHeight, listener);
  }
  
  @Override
  public void startSyncing() {
    startSyncing(null);
  }
  
  @Override
  public BigInteger getBalance() {
    return getBalance(null, null);
  }

  @Override
  public BigInteger getBalance(Integer accountIdx) {
    return getBalance(accountIdx, null);
  }
  
  @Override
  public BigInteger getUnlockedBalance() {
    return getUnlockedBalance(null, null);
  }

  @Override
  public BigInteger getUnlockedBalance(Integer accountIdx) {
    return getUnlockedBalance(accountIdx, null);
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
    if (subaddresses.isEmpty()) throw new MoneroError("Subaddress at index " + subaddressIdx + " is not initialized");
    GenUtils.assertEquals("Only 1 subaddress should be returned", 1, subaddresses.size());
    return subaddresses.get(0);
  }
  
  @Override
  public MoneroSubaddress createSubaddress(int accountIdx) {
    return createSubaddress(accountIdx,  null);
  }
  
  @Override
  public MoneroTxWallet getTx(String txHash) {
    return getTxs(txHash).get(0);
  }
  
  @Override
  public List<MoneroTxWallet> getTxs() {
    return getTxs(new MoneroTxQuery());
  }
  
  @Override
  public List<MoneroTxWallet> getTxs(String... txHashes) {
    return getTxs(new MoneroTxQuery().setHashes(txHashes));
  }
  
  @Override
  public List<MoneroTxWallet> getTxs(List<String> txHashes) {
    return getTxs(txHashes, null);
  }
  
  @Override
  public List<MoneroTxWallet> getTxs(List<String> txHashes, Collection<String> missingTxHashes) {
    return getTxs(new MoneroTxQuery().setHashes(txHashes), missingTxHashes);
  }
  
  @Override
  public List<MoneroTxWallet> getTxs(MoneroTxQuery query) {
    return getTxs(query, null);
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
  public List<MoneroIncomingTransfer> getIncomingTransfers() {
    return getIncomingTransfers(null);
  }

  @Override
  public List<MoneroIncomingTransfer> getIncomingTransfers(MoneroTransferQuery query) {
    
    // copy query and set direction
    query = normalizeTransferQuery(query);
    if (Boolean.FALSE.equals(query.isIncoming())) throw new MoneroError("Transfer query contradicts getting incoming transfers");
    query.setIsIncoming(true);
    
    // fetch and cast transfers
    List<MoneroIncomingTransfer> inTransfers = new ArrayList<MoneroIncomingTransfer>();
    for (MoneroTransfer transfer : getTransfers(query)) {
      inTransfers.add((MoneroIncomingTransfer) transfer);
    }
    return inTransfers;
  }

  @Override
  public List<MoneroOutgoingTransfer> getOutgoingTransfers() {
    return getOutgoingTransfers(null);
  }

  @Override
  public List<MoneroOutgoingTransfer> getOutgoingTransfers(MoneroTransferQuery query) {
    
    // copy query and set direction
    query = normalizeTransferQuery(query);
    if (Boolean.FALSE.equals(query.isOutgoing())) throw new MoneroError("Transfer query contradicts getting outgoing transfers");
    query.setIsOutgoing(true);
    
    // fetch and cast transfers
    List<MoneroOutgoingTransfer> outTransfers = new ArrayList<MoneroOutgoingTransfer>();
    for (MoneroTransfer transfer : getTransfers(query)) {
      outTransfers.add((MoneroOutgoingTransfer) transfer);
    }
    return outTransfers;
  }
  
  @Override
  public List<MoneroOutputWallet> getOutputs() {
    return getOutputs(null);
  }
  
  @Override
  public String exportOutputs() {
    return exportOutputs(false);
  }
  
  @Override
  public List<MoneroKeyImage> exportKeyImages() {
    return exportKeyImages(false);
  }
  
  @Override
  public MoneroTxWallet createTx(MoneroTxConfig config) {
    if (config == null) throw new MoneroError("Send request cannot be null");
    if (Boolean.TRUE.equals(config.getCanSplit())) throw new MoneroError("Cannot request split transactions with createTx() which prevents splitting; use createTxs() instead");
    config = config.copy();
    config.setCanSplit(false);
    return createTxs(config).get(0);
  }
  
  @Override
  public String relayTx(String txMetadata) {
    return relayTxs(Arrays.asList(txMetadata)).get(0);
  }
  
  @Override
  public String relayTx(MoneroTxWallet tx) {
    return relayTx(tx.getMetadata());
  }
  
  @Override
  public List<String> relayTxs(List<MoneroTxWallet> txs) {
    List<String> txHexes = new ArrayList<String>();
    for (MoneroTxWallet tx : txs) txHexes.add(tx.getMetadata());
    return relayTxs(txHexes);
  }
  
  @Override
  public MoneroTxSet describeUnsignedTxSet(String unsignedTxHex) {
    return describeTxSet(new MoneroTxSet().setUnsignedTxHex(unsignedTxHex));
  }
  
  @Override
  public MoneroTxSet describeMultisigTxSet(String multisigTxHex) {
    return describeTxSet(new MoneroTxSet().setMultisigTxHex(multisigTxHex));
  }
  
  @Override
  public String signMessage(String message) {
    return signMessage(message, MoneroMessageSignatureType.SIGN_WITH_SPEND_KEY, 0, 0);
  }
  
  @Override
  public String getTxProof(String txHash, String address) {
    return getTxProof(txHash, address, null);
  }
  
  @Override
  public String getSpendProof(String txHash) {
    return getSpendProof(txHash, null);
  }
  
  @Override
  public String getTxNote(String txHash) {
    return getTxNotes(Arrays.asList(txHash)).get(0);
  }
  
  @Override
  public void setTxNote(String txHash, String note) {
    setTxNotes(Arrays.asList(txHash), Arrays.asList(note));
  }
  
  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries() {
    return getAddressBookEntries(null);
  }
  
  @Override
  public boolean isMultisig() {
    return getMultisigInfo().isMultisig();
  }
  
  @Override
  public int importMultisigHex(String... multisigHexes) {
    return importMultisigHex(Arrays.asList(multisigHexes));
  }
  
  @Override
  public void close() {
    close(false); // close without saving
  }
  
  protected MoneroTransferQuery normalizeTransferQuery(MoneroTransferQuery query) {
    if (query == null) query = new MoneroTransferQuery();
    else {
      if (query.getTxQuery() == null) query = query.copy();
      else {
        MoneroTxQuery txQuery = query.getTxQuery().copy();
        if (query.getTxQuery().getTransferQuery() == query) query = txQuery.getTransferQuery();
        else {
          GenUtils.assertNull("Transfer query's tx query must be circular reference or null", query.getTxQuery().getTransferQuery());
          query = query.copy();
          query.setTxQuery(txQuery);
        }
      }
    }
    if (query.getTxQuery() == null) query.setTxQuery(new MoneroTxQuery());
    query.getTxQuery().setTransferQuery(query);
    if (query.getTxQuery().getBlock() == null) query.getTxQuery().setBlock(new MoneroBlock().setTxs(query.getTxQuery()));
    return query;
  }
}
