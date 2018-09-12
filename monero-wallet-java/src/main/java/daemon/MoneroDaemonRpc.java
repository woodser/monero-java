package daemon;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import daemon.model.MoneroBan;
import daemon.model.MoneroBlock;
import daemon.model.MoneroBlockCount;
import daemon.model.MoneroBlockHashes;
import daemon.model.MoneroBlockHeader;
import daemon.model.MoneroBlockTemplate;
import daemon.model.MoneroChain;
import daemon.model.MoneroCoinbaseTxSum;
import daemon.model.MoneroDaemonBandwidth;
import daemon.model.MoneroDaemonConnection;
import daemon.model.MoneroDaemonInfo;
import daemon.model.MoneroDaemonModel;
import daemon.model.MoneroDaemonResponseInfo;
import daemon.model.MoneroFeeEstimate;
import daemon.model.MoneroHardForkInfo;
import daemon.model.MoneroMiningStatus;
import daemon.model.MoneroOutputDistributionEntry;
import daemon.model.MoneroOutputHistogramEntry;
import daemon.model.MoneroSyncInfo;
import daemon.model.MoneroTxPoolBacklog;
import rpc.MoneroRpc;
import wallet.model.MoneroKeyImage;
import wallet.model.MoneroTx;

/**
 * Implements a Monero daemon using monero-daemon-rpc.
 */
public class MoneroDaemonRpc extends MoneroDaemonDefault {
  
  private MoneroRpc rpc;
  
  /**
   * Constructs a daemon with a RPC connection.
   * 
   * @param rpc is the rpc connection to a remote daemon.
   */
  public MoneroDaemonRpc(MoneroRpc rpc) {
    this.rpc = rpc;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockCount getBlockCount() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_block_count");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockCount blockCount = new MoneroBlockCount();
    setResponseInfo(resultMap, blockCount);
    blockCount.setCount(((BigInteger) resultMap.get("count")).intValue());
    return blockCount;
  }

  @Override
  public String getBlockHash(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel submitBlock(String blockBlob) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getLastBlockHeader() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getBlockHeader(String hash) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHeader getBlockHeader(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroBlockHeader> getBlockHeaders(int startHeight, int endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlock getBlock(String hash) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlock getBlock(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroDaemonConnection> getConnections() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonInfo getInfo() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSyncInfo getSyncInfo() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroHardForkInfo getHardForkInfo() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel setBans(Collection<MoneroBan> bans) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel flushTxPool() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel flushTxPool(Collection<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputHistogramEntry> getOutputHistogram(List<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts, Boolean cumulative, Integer startHeight, Integer endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCoinbaseTxSum getCoinbaseTxSum(Integer height, Integer count) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroFeeEstimate getFeeEstimate(Integer graceBlocks) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroChain> getAlternativeChains() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel relayTxs(Collection<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxPoolBacklog getTxPoolBacklog() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroBlockHashes getAltBlockHashes() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> isKeyImageSpent(Collection<String> keyImageHexes) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTx> getTxs(Collection<String> hashes, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel startMining(String address, Integer numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel stopMining() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroMiningStatus getMiningStatus() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel setBandwidthLimit(Integer limitDown, Integer limitUp) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonBandwidth getBandwidthLimit() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel setNumOutgoingLimit(int limit) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel setNumIncomingLimit(int limit) {
    throw new RuntimeException("Not implemented");
  }
  
  private static void setResponseInfo(Map<String, Object> resultMap, MoneroDaemonModel model) {
    MoneroDaemonResponseInfo responseInfo = new MoneroDaemonResponseInfo();
    responseInfo.setStatus((String) resultMap.get("status"));
    Boolean trusted = (Boolean) resultMap.get("untrusted");
    if (trusted != null) trusted = !trusted;
    responseInfo.setIsTrusted(trusted);
    model.setResponseInfo(responseInfo);
  }
}
