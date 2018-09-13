package daemon;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

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
import daemon.model.MoneroDaemonSyncInfo;
import daemon.model.MoneroFeeEstimate;
import daemon.model.MoneroHardForkInfo;
import daemon.model.MoneroMiningStatus;
import daemon.model.MoneroOutputDistributionEntry;
import daemon.model.MoneroOutputHistogramEntry;
import daemon.model.MoneroTxPoolBacklog;
import rpc.MoneroRpc;
import wallet.MoneroWalletRpc;
import wallet.model.MoneroKeyImage;
import wallet.model.MoneroTx;

/**
 * Implements a Monero daemon using monero-daemon-rpc.
 */
public class MoneroDaemonRpc extends MoneroDaemonDefault {
  
  // logger
  private static final Logger LOGGER = Logger.getLogger(MoneroWalletRpc.class);
  
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
     Map<String, Object> respMap = rpc.sendRpcRequest("on_get_block_hash", Arrays.asList(height));
     return (String) respMap.get("result");
  }

  @Override
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroDaemonModel submitBlock(String blockBlob) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockHeader getLastBlockHeader() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_last_block_header");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockHeader header = interpretBlockHeader((Map<String, Object>) resultMap.get("block_header"));
    setResponseInfo(resultMap, header);
    return header;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockHeader getBlockHeader(String hash) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("hash", hash);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_block_header_by_hash", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockHeader header = interpretBlockHeader((Map<String, Object>) resultMap.get("block_header"));
    setResponseInfo(resultMap, header);
    return header;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockHeader getBlockHeader(int height) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("height", height);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_block_header_by_height", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockHeader header = interpretBlockHeader((Map<String, Object>) resultMap.get("block_header"));
    setResponseInfo(resultMap, header);
    return header;
  }

  @Override
  public List<MoneroBlockHeader> getBlockHeaders(int startHeight, int endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlock getBlock(String hash) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("hash", hash);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_block", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlock block = interpretBlock((Map<String, Object>) resultMap);
    setResponseInfo(resultMap, block);
    return block;
  }

  @Override
  public MoneroBlock getBlock(int height) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroDaemonConnection> getConnections() {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroDaemonInfo getInfo() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_info");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroDaemonInfo info = interpretInfo(resultMap);
    setResponseInfo(resultMap, info);
    return info;
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
  public MoneroDaemonModel setBans(Collection<MoneroBan> bans) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public Collection<MoneroBan> getBans() {
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
  
  /**
   * Initializes a MoneroBlockHeader from a RPC header response map.
   * 
   * @param headerMap is the map to initialize the block header from
   * @return MoneroBlockHeader is the initialized block header
   */
  private static MoneroBlockHeader interpretBlockHeader(Map<String, Object> headerMap) {
    MoneroBlockHeader header = new MoneroBlockHeader();
    for (String key : headerMap.keySet()) {
      Object val = headerMap.get(key);
      if (key.equals("block_size")) header.setBlockSize(((BigInteger) val).intValue());
      else if (key.equals("depth")) header.setDepth(((BigInteger) val).intValue());
      else if (key.equals("difficulty")) header.setDifficulty((BigInteger) val);
      else if (key.equals("hash")) header.setHash((String) val);
      else if (key.equals("height")) header.setHeight(((BigInteger) val).intValue());
      else if (key.equals("major_version")) header.setMajorVersion(((BigInteger) val).intValue());
      else if (key.equals("minor_version")) header.setMinorVersion(((BigInteger) val).intValue());
      else if (key.equals("nonce")) header.setNonce((BigInteger) val);
      else if (key.equals("num_txes")) header.setNumTxs(((BigInteger) val).intValue());
      else if (key.equals("orphan_status")) header.setOrphanStatus((Boolean) val);
      else if (key.equals("prev_hash")) header.setPrevHash((String) val);
      else if (key.equals("reward")) header.setReward((BigInteger) val);
      else if (key.equals("timestamp")) header.setTimestamp(((BigInteger) val).longValue());
      else LOGGER.warn("Ignoring unexpected block header field: '" + key + "'");
    }
    return header;
  }
  
  /**
   * Initializes daemon info from a RPC info result map.
   * 
   * @param resultMap is the RPC info result map to initialize from
   * @return MoneroDaemonInfo is an object initialized from the RPC result map
   */
  private static MoneroDaemonInfo interpretInfo(Map<String, Object> resultMap) {
    MoneroDaemonInfo info = new MoneroDaemonInfo();
    for (String key : resultMap.keySet()) {
      Object val = resultMap.get(key);
      if (key.equals("alt_blocks_count")) info.setAltBlocksCount(((BigInteger) val).intValue());
      else if (key.equals("block_size_limit")) info.setBlockSizeLimit(((BigInteger) val).intValue());
      else if (key.equals("block_size_median")) info.setBlockSizeMedian(((BigInteger) val).intValue());
      else if (key.equals("bootstrap_daemon_address")) info.setBootstrapDaemonAddress((String) val);
      else if (key.equals("cumulative_difficulty")) info.setCumulativeDifficulty((BigInteger) val);
      else if (key.equals("difficulty")) info.setDifficulty((BigInteger) val);
      else if (key.equals("free_space")) info.setFreeSpace((BigInteger) val);
      else if (key.equals("grey_peerlist_size")) info.setGreyPeerlistSize(((BigInteger) val).intValue());
      else if (key.equals("height")) info.setHeight(((BigInteger) val).intValue());
      else if (key.equals("height_without_bootstrap")) info.setHeightWithoutBootstrap(((BigInteger) val).intValue());
      else if (key.equals("incoming_connections_count")) info.setIncomingConnectionsCount(((BigInteger) val).intValue());
      else if (key.equals("mainnet")) { if ((Boolean) val) info.setNetworkType(MoneroNetworkType.MAINNET); }
      else if (key.equals("offline")) info.setIsOffline((Boolean) val);
      else if (key.equals("outgoing_connections_count")) info.setOutgoingConnectionsCount(((BigInteger) val).intValue());
      else if (key.equals("rpc_connections_count")) info.setRpcConnectionsCount(((BigInteger) val).intValue());
      else if (key.equals("stagenet")) { if ((Boolean) val) info.setNetworkType(MoneroNetworkType.STAGENET); }
      else if (key.equals("start_time")) info.setStartTime(((BigInteger) val).longValue());
      else if (key.equals("status")) { }  // initialized elsewhere
      else if (key.equals("target")) info.setTarget(((BigInteger) val).intValue());
      else if (key.equals("target_height")) info.setTargetHeight(((BigInteger) val).intValue());
      else if (key.equals("testnet")) { if ((Boolean) val) info.setNetworkType(MoneroNetworkType.TESTNET); }
      else if (key.equals("top_block_hash")) info.setTopBlockHash((String) val);
      else if (key.equals("tx_count")) info.setTxCount(((BigInteger) val).intValue());
      else if (key.equals("tx_pool_size")) info.setTxPoolSize(((BigInteger) val).intValue());
      else if (key.equals("untrusted")) { } // initialized elsewhere
      else if (key.equals("was_bootstrap_ever_used")) info.setWasBootstrapEverUsed((Boolean) val);
      else if (key.equals("white_peerlist_size")) info.setWhitePeerlistSize(((BigInteger) val).intValue());
      else LOGGER.warn("Ignoring unexpected info field: '" + key + "'");
    }
    return info;
  }
  
  /**
   * Initializes a MoneroBlock from a RPC response map.
   * 
   * @param resultMap is the RPC response map for a block
   * @return MoneroBlock is a block initialized from the map
   */
  @SuppressWarnings("unchecked")
  private static MoneroBlock interpretBlock(Map<String, Object> resultMap) {
    MoneroBlock block = new MoneroBlock();
    block.setBlob((String) resultMap.get("blob"));
    block.setHeader(interpretBlockHeader((Map<String, Object>) resultMap.get("block_header")));
    return block;
  }
}
