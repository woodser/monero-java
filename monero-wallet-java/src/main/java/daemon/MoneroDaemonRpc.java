package daemon;

import java.math.BigInteger;
import java.util.ArrayList;
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
import daemon.model.MoneroDaemonConnectionSpan;
import daemon.model.MoneroDaemonInfo;
import daemon.model.MoneroDaemonModel;
import daemon.model.MoneroDaemonResponseInfo;
import daemon.model.MoneroDaemonSyncInfo;
import daemon.model.MoneroFeeEstimate;
import daemon.model.MoneroHardForkInfo;
import daemon.model.MoneroMinerTx;
import daemon.model.MoneroMiningStatus;
import daemon.model.MoneroOutputDistributionEntry;
import daemon.model.MoneroOutputHistogramEntry;
import daemon.model.MoneroTxPoolBacklog;
import rpc.MoneroRpc;
import utils.JsonUtils;
import wallet.model.MoneroKeyImage;
import wallet.model.MoneroTx;

/**
 * Implements a Monero daemon using monero-daemon-rpc.
 */
public class MoneroDaemonRpc extends MoneroDaemonDefault {
  
  // logger
  private static final Logger LOGGER = Logger.getLogger(MoneroDaemonRpc.class);
  
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

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("wallet_address", walletAddress);
    params.put("reserve_size", reserveSize);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_block_template", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockTemplate temmplate = initializeBlockTemplate(resultMap);
    setResponseInfo(resultMap, temmplate);
    return temmplate;
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
    MoneroBlockHeader header = initializeBlockHeader((Map<String, Object>) resultMap.get("block_header"));
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
    MoneroBlockHeader header = initializeBlockHeader((Map<String, Object>) resultMap.get("block_header"));
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
    MoneroBlockHeader header = initializeBlockHeader((Map<String, Object>) resultMap.get("block_header"));
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
    MoneroBlock block = initializeBlock((Map<String, Object>) resultMap);
    setResponseInfo(resultMap, block);
    return block;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlock getBlock(int height) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("height", height);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_block", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlock block = initializeBlock((Map<String, Object>) resultMap);
    setResponseInfo(resultMap, block);
    return block;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroDaemonConnection> getConnections() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_connections");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> connectionMaps = (List<Map<String, Object>>) resultMap.get("connections");
    List<MoneroDaemonConnection> connections = new ArrayList<MoneroDaemonConnection>();
    for (Map<String, Object> connectionMap : connectionMaps) {
      MoneroDaemonConnection connection = initializeConnection(connectionMap);
      setResponseInfo(resultMap, connection);
      connections.add(connection);
    }
    return connections;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroDaemonInfo getInfo() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_info");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroDaemonInfo info = initializeInfo(resultMap);
    setResponseInfo(resultMap, info);
    return info;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroDaemonSyncInfo getSyncInfo() {
    Map<String, Object> respMap = rpc.sendRpcRequest("sync_info");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroDaemonSyncInfo syncInfo = initializeSyncInfo(resultMap);
    setResponseInfo(resultMap, syncInfo);
    
    // initialize response info
    if (syncInfo.getPeers() != null) {
      for (MoneroDaemonConnection peer : syncInfo.getPeers()) {
        peer.setResponseInfo(syncInfo.getResponseInfo());
      }
    }
    if (syncInfo.getSpans() != null) {
      for (MoneroDaemonConnectionSpan span : syncInfo.getSpans()) {
        span.setResponseInfo(syncInfo.getResponseInfo());
      }
    }
    
    return syncInfo;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroHardForkInfo getHardForkInfo() {
    Map<String, Object> respMap = rpc.sendRpcRequest("hard_fork_info");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroHardForkInfo hardForkInfo = initializeHardForkInfo(resultMap);
    setResponseInfo(resultMap, hardForkInfo);
    return hardForkInfo;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroDaemonModel setBans(Collection<MoneroBan> bans) {
    List<Map<String, Object>> banMaps = new ArrayList<Map<String, Object>>();
    for (MoneroBan ban : bans)  banMaps.add(banToMap(ban));
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("bans", banMaps);
    Map<String, Object> respMap = rpc.sendRpcRequest("set_bans", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroDaemonModel model = new MoneroDaemonModel();
    setResponseInfo(resultMap, model);
    return model;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public Collection<MoneroBan> getBans() {
    Map<String, Object> respMap = rpc.sendRpcRequest("get_bans");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> banMaps = (List<Map<String, Object>>) resultMap.get("bans");
    List<MoneroBan> bans = new ArrayList<MoneroBan>();
    for (Map<String, Object> banMap : banMaps) {
      MoneroBan ban = new MoneroBan();
      bans.add(ban);
      ban.setHost((String) banMap.get("host"));
      ban.setIp(((BigInteger) banMap.get("ip")).intValue());
      ban.setSeconds(((BigInteger) banMap.get("seconds")).longValue());
      setResponseInfo(resultMap, ban);
    }
    return bans;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroDaemonModel flushTxPool(Collection<String> txIds) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("txids", txIds);
    Map<String, Object> respMap = rpc.sendRpcRequest("flush_txpool", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroDaemonModel model = new MoneroDaemonModel();
    setResponseInfo(resultMap, model);
    return model;
  }

  @Override
  public List<MoneroOutputHistogramEntry> getOutputHistogram(List<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts, Boolean cumulative, Integer startHeight, Integer endHeight) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroCoinbaseTxSum getCoinbaseTxSum(int height, int count) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("height", height);
    params.put("count", count);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_coinbase_tx_sum", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroCoinbaseTxSum txSum = new MoneroCoinbaseTxSum();
    txSum.setTotalEmission((BigInteger) resultMap.get("emission_amount"));
    txSum.setTotalFees((BigInteger) resultMap.get("fee_amount"));
    setResponseInfo(resultMap, txSum);
    return txSum;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroFeeEstimate getFeeEstimate(Integer graceBlocks) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("grace_blocks", graceBlocks);
    Map<String, Object> respMap = rpc.sendRpcRequest("get_fee_estimate", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroFeeEstimate feeEstimate = new MoneroFeeEstimate();
    feeEstimate.setFeeEstimate((BigInteger) resultMap.get("fee"));
    setResponseInfo(resultMap, feeEstimate);
    return feeEstimate;
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
  private static MoneroBlockHeader initializeBlockHeader(Map<String, Object> headerMap) {
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
  private static MoneroDaemonInfo initializeInfo(Map<String, Object> resultMap) {
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
      else if (key.equals("status")) {}  // set elsewhere
      else if (key.equals("target")) info.setTarget(((BigInteger) val).intValue());
      else if (key.equals("target_height")) info.setTargetHeight(((BigInteger) val).intValue());
      else if (key.equals("testnet")) { if ((Boolean) val) info.setNetworkType(MoneroNetworkType.TESTNET); }
      else if (key.equals("top_block_hash")) info.setTopBlockHash((String) val);
      else if (key.equals("tx_count")) info.setTxCount(((BigInteger) val).intValue());
      else if (key.equals("tx_pool_size")) info.setTxPoolSize(((BigInteger) val).intValue());
      else if (key.equals("untrusted")) {} // set elsewhere
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
  private static MoneroBlock initializeBlock(Map<String, Object> resultMap) {
    MoneroBlock block = new MoneroBlock();
    block.setBlob((String) resultMap.get("blob"));
    block.setHeader(initializeBlockHeader((Map<String, Object>) resultMap.get("block_header")));
    
    // convert json string field to map
    String jsonStr = (String) resultMap.get("json");
    Map<String, Object> json = JsonUtils.toMap(MoneroRpc.MAPPER, jsonStr);
    
    // initialize tx hashes from json
    block.setTxHashes((List<String>) json.get("tx_hashes"));
    
    // initialize miner tx from json
    Map<String, Object> minerTxMap = (Map<String, Object>) json.get("miner_tx");
    MoneroMinerTx minerTx = new MoneroMinerTx();
    block.setMinerTx(minerTx);
    minerTx.setVersion(((BigInteger) minerTxMap.get("version")).intValue());
    minerTx.setUnlockTime(((BigInteger) minerTxMap.get("unlock_time")).intValue());
    List<BigInteger> extraNums = (List<BigInteger>) minerTxMap.get("extra");
    int[] extra = new int[extraNums.size()];
    for (int i = 0; i < extraNums.size(); i++) {
      extra[i] = extraNums.get(i).intValue();
    }
    minerTx.setExtra(extra);
    return block;
  }
  
  /**
   * Initializes a connection from a RPC connection map.
   * 
   * @param connectionMap is the connection map to initialize the connection object from
   * @return MoneroDaemonConnection connection is the connection initialized from the map
   */
  private static MoneroDaemonConnection initializeConnection(Map<String, Object> connectionMap) {
    MoneroDaemonConnection connection = new MoneroDaemonConnection();
    for (String key : connectionMap.keySet()) {
      Object val = connectionMap.get(key);
      if (key.equals("address")) connection.setAddress((String) val);
      else if (key.equals("avg_download")) connection.setAvgDownload(((BigInteger) val).intValue());
      else if (key.equals("avg_upload")) connection.setAvgUpload(((BigInteger) val).intValue());
      else if (key.equals("connection_id")) connection.setId((String) val);
      else if (key.equals("current_download")) connection.setCurrentDownload(((BigInteger) val).intValue());
      else if (key.equals("current_upload")) connection.setCurrentUpload(((BigInteger) val).intValue());
      else if (key.equals("height")) connection.setHeight(((BigInteger) val).intValue());
      else if (key.equals("host")) connection.setHost((String) val);
      else if (key.equals("incoming")) connection.setIsIncoming((Boolean) val);
      else if (key.equals("ip")) connection.setIp((String) val);
      else if (key.equals("live_time")) connection.setLiveTime(((BigInteger) val).intValue());
      else if (key.equals("local_ip")) connection.setIsLocalIp((Boolean) val);
      else if (key.equals("localhost")) connection.setIsLocalHost((Boolean) val);
      else if (key.equals("peer_id")) connection.setPeerId((String) val);
      else if (key.equals("port")) connection.setPort((String) val);
      else if (key.equals("recv_count")) connection.setReceiveCount(((BigInteger) val).intValue());
      else if (key.equals("recv_idle_time")) connection.setReceiveIdleTime(((BigInteger) val).longValue());
      else if (key.equals("send_count")) connection.setSendCount(((BigInteger) val).intValue());
      else if (key.equals("send_idle_time")) connection.setSendIdleTime(((BigInteger) val).longValue());
      else if (key.equals("state")) connection.setState((String) val);
      else if (key.equals("support_flags")) connection.setNumSupportFlags(((BigInteger) val).intValue());
      else LOGGER.warn("Ignoring unexpected connection field: '" + key + "'");
    }
    return connection;
  }
  
  /**
   * Initializes sync info from a RPC sync info map.
   * 
   * @param syncInfoMap is the sync info map to initialize the sync info object from
   * @return MoneroDaemonSyncInfo is sync info initialized from the map
   */
  @SuppressWarnings("unchecked")
  private static MoneroDaemonSyncInfo initializeSyncInfo(Map<String, Object> syncInfoMap) {
    MoneroDaemonSyncInfo syncInfo = new MoneroDaemonSyncInfo();
    for (String key : syncInfoMap.keySet()) {
      Object val = syncInfoMap.get(key);
      if (key.equals("height")) syncInfo.setHeight(((BigInteger) val).intValue());
      else if (key.equals("peers")) {
        syncInfo.setPeers(new ArrayList<MoneroDaemonConnection>());
        List<Map<String, Object>> peerMaps = (List<Map<String, Object>>) val;
        for (Map<String, Object> peerMap : peerMaps) {
          syncInfo.getPeers().add(initializeConnection((Map<String, Object>) peerMap.get("info")));
        }
      } else if (key.equals("spans")) {
        syncInfo.setSpans(new ArrayList<MoneroDaemonConnectionSpan>());
        List<Map<String, Object>> spanMaps = (List<Map<String, Object>>) val;
        for (Map<String, Object> spanMap : spanMaps) {
          syncInfo.getSpans().add(initializeConnectionSpan(spanMap));
        }
      } else if (key.equals("status")) {}   // set elsewhere
      else if (key.equals("target_height")) syncInfo.setTargetHeight(((BigInteger) val).intValue());
      else LOGGER.warn("Ignoring unexpected sync info field: '" + key + "'");
    }
    return syncInfo;
  }
  
  /**
   * Initializes a connection span from a RPC span map.
   * 
   * @param spanMap is the RPC result map to initialize the connection span from
   * @return MoneroDaemonConnectionSpan is the initialized span from the RPC result map
   */
  private static MoneroDaemonConnectionSpan initializeConnectionSpan(Map<String, Object> spanMap) {
    MoneroDaemonConnectionSpan span = new MoneroDaemonConnectionSpan();
    for (String key : spanMap.keySet()) {
      Object val = spanMap.get(key);
      if (key.equals("connection_id")) span.setConnectionId((String) val);
      else if (key.equals("nblocks")) span.setNumBlocks(((BigInteger) val).intValue());
      else if (key.equals("remote_address")) span.setRemoteAddress((String) val);
      else if (key.equals("rate")) span.setRate((BigInteger) val);
      else if (key.equals("speed")) span.setSpeed((BigInteger) val);
      else if (key.equals("size")) span.setSize((BigInteger) val);
      else if (key.equals("start_block_height")) span.setStartBlockHeight(((BigInteger) val).intValue());
      else LOGGER.warn("Ignoring unexpected connection span field: '" + key + "'");
    }
    return span;
  }
  
  private static MoneroHardForkInfo initializeHardForkInfo(Map<String, Object> hardForkInfoMap) {
    MoneroHardForkInfo info = new MoneroHardForkInfo();
    for (String key : hardForkInfoMap.keySet()) {
      Object val = hardForkInfoMap.get(key);
      if (key.equals("earliest_height")) info.setEarliestHeight(((BigInteger) val).intValue());
      else if (key.equals("enabled")) info.setIsEnabled((Boolean) val);
      else if (key.equals("state")) info.setState(((BigInteger) val).intValue());
      else if (key.equals("status")) {} // set elsewhere
      else if (key.equals("threshold")) info.setThreshold(((BigInteger) val).intValue());
      else if (key.equals("version")) info.setVersion(((BigInteger) val).intValue());
      else if (key.equals("votes")) info.setVotes(((BigInteger) val).intValue());
      else if (key.equals("voting")) info.setVoting(((BigInteger) val).intValue());
      else if (key.equals("window")) info.setWindow(((BigInteger) val).intValue());
      else LOGGER.warn("Ignoring unexpected connection span field: '" + key + "'");
    }
    return info;
  }
  
  private static MoneroBlockTemplate initializeBlockTemplate(Map<String, Object> templateMap) {
    MoneroBlockTemplate template = new MoneroBlockTemplate();
    for (String key : templateMap.keySet()) {
      Object val = templateMap.get(key);
      if (key.equals("blocktemplate_blob")) template.setTemplateBlob((String) val);
      else if (key.equals("blockhashing_blob")) template.setHashBlob((String) val);
      else if (key.equals("difficulty")) template.setDifficulty(((BigInteger) val).intValue());
      else if (key.equals("expected_reward")) template.setExpectedReward((BigInteger) val);
      else if (key.equals("height")) template.setHeight(((BigInteger) val).intValue());
      else if (key.equals("prev_hash")) template.setPrevHash((String) val);
      else if (key.equals("reserved_offset")) template.setReservedOffset(((BigInteger) val).intValue());
      else if (key.equals("status")) {}  // set elsewhere
      else if (key.equals("untrusted")) {}  // set elsewhere
      else LOGGER.warn("Ignoring unexpected connection span field: '" + key + "'");
    }
    return template;
  }
  
  private static Map<String, Object> banToMap(MoneroBan ban) {
    Map<String, Object> banMap = new HashMap<String, Object>();
    banMap.put("host", ban.getHost());
    banMap.put("ip", ban.getIp());
    banMap.put("ban", ban.getIsBanned());
    banMap.put("seconds", ban.getSeconds());
    return banMap;
  }
}
