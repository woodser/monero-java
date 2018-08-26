package service;

import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import model.MoneroAccount;
import model.MoneroAddressBookEntry;
import model.MoneroIntegratedAddress;
import model.MoneroKeyImage;
import model.MoneroSubaddress;
import model.MoneroTx;
import model.MoneroTxConfig;
import model.MoneroTxFilter;
import model.MoneroUri;

/**
 * Implements a Monero Wallet using a monero-wallet-rpc backend.
 */
public class MoneroWalletRpc extends MoneroWalletDefault {

  @Override
  public int getHeight() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getMnemonicSeed() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts(String tag) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount getAccount(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount createAccount(String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, Collection<Integer> subaddressIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
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
  public BigInteger getUnlockedBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTx send(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> sendSplit(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> sweepAll(MoneroTxConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> sweepDust() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs(MoneroTxFilter filter) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTx getTx(String txId, Integer accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setTxNotes(List<String> txIds, List<String> txNotes) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxNotes(List<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public Map<String, BigInteger> importKeyImages(List<MoneroKeyImage> keyImages) {
    throw new RuntimeException("Not implemented");
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

  @Override
  public List<String> getLanguages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void createWallet(String filename, String password, String language) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void openWallet(String filename, String password) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String sign(String data) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean verify(String data, String address, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public URI toUri(MoneroUri moneroUri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroUri toMoneroUri(URI uri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void saveBlockchain() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void rescanBlockchain() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void rescanSpent() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopWallet() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void startMining(int numThreads, boolean backgroundMining, boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopMining() {
    throw new RuntimeException("Not implemented");
  }
}
