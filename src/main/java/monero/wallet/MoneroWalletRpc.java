package monero.wallet;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

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
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncProgressListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroWalletOutput;
import monero.wallet.model.MoneroWalletTx;

/**
 * Implements a Monero Wallet using monero-wallet-rpc.
 */
public class MoneroWalletRpc extends MoneroWalletDefault {

  private MoneroRpc rpc;  // handles rpc interactions
  private Map<Integer, Map<Integer, String>> addressCache;  // cache static addresses to reduce requests
  
  // logger
  private static final Logger LOGGER = Logger.getLogger(MoneroWalletRpcOld.class);
  
  // custom mapper to deserialize integers to BigIntegers
  public static ObjectMapper MAPPER;
  static {
    MAPPER = new ObjectMapper();
    MAPPER.setSerializationInclusion(Include.NON_NULL);
    MAPPER.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
    MAPPER.configure(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS, true);
  }
  
  public MoneroWalletRpc(MoneroRpc rpc) {
    throw new RuntimeException("Not implemented");
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
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getMnemonic() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getPublicViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getPrivateViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getLanguages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int getHeight() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int getChainHeight() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getPrimaryAddress() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSyncResult sync(Integer startHeight, Integer endHeight, MoneroSyncProgressListener progressListener) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean isMultisigImportNeeded() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount createAccount(String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    throw new RuntimeException("Not implemented");
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
  public List<MoneroWalletTx> getTxs(MoneroTxFilter filter) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferFilter filter) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroWalletOutput> getVouts(MoneroVoutFilter filter) {
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
  public MoneroWalletTx send(MoneroSendConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroWalletTx> sendSplit(MoneroSendConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroWalletTx> sweepUnlocked(MoneroSendConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroWalletTx> sweepDust(boolean doNotRelay) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroWalletTx sweepOutput(MoneroSendConfig config) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroWalletTx> relayTxs(List<String> txMetadatas) {
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
}
