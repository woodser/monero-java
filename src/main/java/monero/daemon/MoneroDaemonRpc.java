package monero.daemon;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.JsonUtils;
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
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWalletRpcOld;

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
    Map<String, Object> respMap = rpc.sendJsonRequest("on_get_block_hash", Arrays.asList(height));
    return (String) respMap.get("result");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, Integer reserveSize) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("wallet_address", walletAddress);
    params.put("reserve_size", reserveSize);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block_template", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockTemplate template = convertRpcBlockTemplate(resultMap);
    return template;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockHeader getLastBlockHeader() {
    Map<String, Object> respMap = rpc.sendJsonRequest("get_last_block_header");
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockHeader header = convertRpcBlockHeader((Map<String, Object>) resultMap.get("block_header"));
    return header;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockHeader getBlockHeaderById(String blockId) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("hash", blockId);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block_header_by_hash", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockHeader header = convertRpcBlockHeader((Map<String, Object>) resultMap.get("block_header"));
    return header;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlockHeader getBlockHeaderByHeight(int height) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("height", height);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block_header_by_height", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlockHeader header = convertRpcBlockHeader((Map<String, Object>) resultMap.get("block_header"));
    return header;
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<MoneroBlockHeader> getBlockHeadersByRange(Integer startHeight, Integer endHeight) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("start_height", startHeight);
    params.put("end_height", endHeight);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block_headers_range", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    List<Map<String, Object>> rpcHeaders = (List<Map<String, Object>>) resultMap.get("headers");
    List<MoneroBlockHeader> headers = new ArrayList<MoneroBlockHeader>();
    for (Map<String, Object> rpcHeader : rpcHeaders) {
      MoneroBlockHeader header = convertRpcBlockHeader(rpcHeader);
      headers.add(header);
    }
    return headers;
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlock getBlockById(String blockId) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("hash", blockId);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block", params);
    Map<String, Object> resultMap = (Map<String, Object>) respMap.get("result");
    MoneroBlock block = convertBlock(resultMap);
    return block;
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
  public List<String> getTxHexes(List<String> txIds, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCoinbaseTxSum getCoinbaseTxSum(int height, int numBlocks) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getFeeEstimate(Integer graceBlocks) {
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
  
  //---------------------------------- PRIVATE -------------------------------
  
  private static MoneroBlockTemplate convertRpcBlockTemplate(Map<String, Object> rpcTemplate) {
    MoneroBlockTemplate template = new MoneroBlockTemplate();
    for (String key : rpcTemplate.keySet()) {
      Object val = rpcTemplate.get(key);
      if (key.equals("blockhashing_blob")) template.setBlockTemplateBlob((String) val);
      else if (key.equals("blocktemplate_blob")) template.setBlockHashingBlob((String) val);
      else if (key.equals("difficulty")) template.setDifficulty((BigInteger) val);
      else if (key.equals("expected_reward")) template.setExpectedReward((BigInteger) val);
      else if (key.equals("height")) template.setHeight(((BigInteger) val).intValue());
      else if (key.equals("prev_hash")) template.setPrevId((String) val);
      else if (key.equals("reserved_offset")) template.setReservedOffset(((BigInteger) val).intValue());
      else if (key.equals("status")) {}  // handled elsewhere
      else if (key.equals("untrusted")) {}  // handled elsewhere
      else LOGGER.warn("WARNING: ignoring unexpected field in block template: " + key + ": " + val);
    }
    return template;
  }
  
  private static MoneroBlockHeader convertRpcBlockHeader(Map<String, Object> rpcHeader) {
    MoneroBlockHeader header = new MoneroBlockHeader();
    for (String key : rpcHeader.keySet()) {
      Object val = rpcHeader.get(key);
      if (key.equals("block_size")) header.setSize(MoneroUtils.reconcile(header.getSize(), ((BigInteger) val).longValue()));
      else if (key.equals("depth")) header.setDepth(MoneroUtils.reconcile(header.getDepth(), ((BigInteger) val).longValue()));
      else if (key.equals("difficulty")) header.setDifficulty(MoneroUtils.reconcile(header.getDifficulty(), (BigInteger) val));
      else if (key.equals("cumulative_difficulty")) header.setCumulativeDifficulty(MoneroUtils.reconcile(header.getCumulativeDifficulty(), (BigInteger) val));
      else if (key.equals("hash")) header.setId(MoneroUtils.reconcile(header.getId(), (String) val));
      else if (key.equals("height")) header.setHeight(MoneroUtils.reconcile(header.getHeight(), ((BigInteger) val).intValue()));
      else if (key.equals("major_version")) header.setMajorVersion(MoneroUtils.reconcile(header.getMajorVersion(), ((BigInteger) val).intValue()));
      else if (key.equals("minor_version")) header.setMinorVersion(MoneroUtils.reconcile(header.getMinorVersion(), ((BigInteger) val).intValue()));
      else if (key.equals("nonce")) header.setNonce(MoneroUtils.reconcile(header.getNonce(), ((BigInteger) val).longValue()));
      else if (key.equals("num_txes")) header.setNumTxs(MoneroUtils.reconcile(header.getNumTxs(), ((BigInteger) val).intValue()));
      else if (key.equals("orphan_status")) header.setOrphanStatus(MoneroUtils.reconcile(header.getOrphanStatus(), (Boolean) val));
      else if (key.equals("prev_hash") || key.equals("prev_id")) header.setPrevId(MoneroUtils.reconcile(header.getPrevId(), (String) val));
      else if (key.equals("reward")) header.setReward(MoneroUtils.reconcile(header.getReward(), (BigInteger) val));
      else if (key.equals("timestamp")) header.setTimestamp(MoneroUtils.reconcile(header.getTimestamp(), ((BigInteger) val).longValue()));
      else if (key.equals("block_weight")) header.setWeight(MoneroUtils.reconcile(header.getWeight(), ((BigInteger) val).longValue()));
      else if (key.equals("long_term_weight")) header.setLongTermWeight(MoneroUtils.reconcile(header.getLongTermWeight(), ((BigInteger) val).longValue()));
      else if (key.equals("pow_hash")) header.setPowHash(MoneroUtils.reconcile(header.getPowHash(), "".equals(val) ? null : (String) val));
      else if (key.equals("tx_hashes")) {}  // used in block model, not header model
      else if (key.equals("miner_tx")) {}   // used in block model, not header model
      else LOGGER.warn("WARNING: ignoring unexpected block header field: '" + key + "': " + val);
    }
    return header;
  }
  
  @SuppressWarnings("unchecked")
  private static MoneroBlock convertRpcBlock(Map<String, Object> rpcBlock) {
    
    // build block
    MoneroBlock block = new MoneroBlock();
    block.setHex((String) rpcBlock.get("blob"));
    block.setHeader(convertRpcBlockHeader(rpcBlock.containsKey("block_header") ? (Map<String, Object>) rpcBlock.get("block_header") : rpcBlock));
    block.setTxIds(rpcBlock.containsKey("tx_hashes") ? (List<String>) rpcBlock.get("tx_hashes") : new ArrayList<String>());
    
    // build coinbase tx
    Map<String, Object> rpcCoinbaseTx = (Map<String, Object>) (rpcBlock.containsKey("json") ? JsonUtils.deserialize(MoneroRpc.MAPPER, (String) rpcBlock.get("json"), new TypeReference<Map<String, Object>>(){}).get("miner_tx") : rpcBlock.get("miner_tx")); // may need to be parsed from json
    MoneroTx coinbaseTx = new MoneroTx().setIsConfirmed(true).setIsCoinbase(true);
    MoneroDaemonRpc.convertRpcTx(rpcCoinbaseTx, coinbaseTx);
    block.setCoinbaseTx(coinbaseTx);
    
    return block;
  }
}
