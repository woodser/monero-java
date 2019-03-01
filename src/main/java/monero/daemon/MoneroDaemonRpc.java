package monero.daemon;

import java.math.BigInteger;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockListener;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroCoinbaseTxSum;
import monero.daemon.model.MoneroDaemonConnection;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonPeer;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroOutputHistogramEntry;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxBacklogEntry;
import monero.daemon.model.MoneroTxPoolStats;
import monero.rpc.MoneroRpc;

/**
 * Implements a Monero daemon using monero-daemon-rpc.
 */
public class MoneroDaemonRpc extends MoneroDaemonDefault {
  
  private MoneroRpc rpc;
  private static final Logger LOGGER = Logger.getLogger(MoneroDaemonRpc.class);

  public MoneroDaemonRpc(MoneroRpc rpc) {
    this.rpc = rpc;
  }
  
  public MoneroRpc getRpc() {
    return this.rpc;
  }

  @Override
  public boolean getIsTrusted() {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public int getHeight() {
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block_count");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    return ((BigInteger) resultMap.get("count")).intValue();
  }

  @Override
  public String getBlockId(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getLastBlockHeader() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getBlockHeaderById(String blockId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getBlockHeaderByHeight(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroBlockHeader> getBlockHeadersByRange(Integer startHeight, Integer endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlock getBlockById(String blockId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroBlock> getBlocksById(List<String> blockIds, Integer startHeight, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlock getBlockByHeight(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroBlock> getBlocksByHeight(List<Integer> heights) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroBlock> getBlocksByRange(Integer startHeight, Integer endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getBlockIds(List<String> blockIds, Integer startHeight) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTx getTx(String txId, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs(List<String> txIds, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxHex(String txId, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxHexes(String txIds, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCoinbaseTxSum getCoinbaseTxSum(int height, int numBlocks) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getFeeEstimate(int graceBlocks) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSubmitTxResult submitTxHex(String txHex, Boolean doNotRelay) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void relayTxsById(List<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxPool() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxPoolIds() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxBacklogEntry> getTxPoolBacklog() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxPoolStats getTxPoolStats() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void flushTxPool() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void flushTxPool(String id) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void flushTxPool(List<String> ids) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImageSpentStatus> getSpentStatuses(List<String> keyImages) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<MoneroOutput> getOutputs(List<MoneroOutput> outputs) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputHistogramEntry> getOutputHistogram(List<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts, Boolean isCumulative, Integer startHeight, Integer endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonInfo getInfo() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonSyncInfo getSyncInfo() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroHardForkInfo getHardForkInfo() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAltChain> getAltChains() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getAltBlockIds() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int getDownloadLimit() {
    // TODO Auto-generated method stub
    return 0;
  }

  @Override
  public void setDownloadLimit(int limit) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int resetDownloadLimit() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int getUploadLimit() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setUploadLimit(int limit) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int resetUploadLimit() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroDaemonPeer> getKnownPeers() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroDaemonConnection> getConnections() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setOutgoingPeerLimit(int limit) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setIncomingPeerLimit(int limit) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroBan> getPeerBans() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setPeerBans(List<MoneroBan> bans) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void startMining(String address, Integer numThreads, Boolean isBackground, Boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopMining() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroMiningStatus getMiningStatus() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void submitBlocks(List<String> blockBlobs) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonUpdateCheckResult checkForUpdate(String path) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonUpdateDownloadResult downloadUpdate(String path) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stop() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getNextBlockHeader() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void addBlockListener(MoneroBlockListener listener) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void removeBlockListener(MoneroBlockListener listener) {
    throw new RuntimeException("Not implemented");
  }
}
