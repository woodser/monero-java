package monero.daemon;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.core.type.TypeReference;

import common.utils.GenUtils;
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
import monero.daemon.model.MoneroKeyImage;
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

/**
 * Implements a Monero daemon using monero-daemon-rpc.
 */
public class MoneroDaemonRpc extends MoneroDaemonDefault {
  
  private MoneroRpc rpc;
  private static final String DEFAULT_ID = "0000000000000000000000000000000000000000000000000000000000000000";
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
    MoneroBlock block = convertRpcBlock(resultMap);
    return block;
  }

  @Override
  public List<MoneroBlock> getBlocksById(List<String> blockIds, Integer startHeight, Boolean prune) {
    throw new RuntimeException("Not implemented");
  }

  @SuppressWarnings("unchecked")
  @Override
  public MoneroBlock getBlockByHeight(int height) {
    Map<String, Object> params = new HashMap<String, Object>();
    params.put("height", height);
    Map<String, Object> respMap = rpc.sendJsonRequest("get_block", params);
    Map<String, Object> rpcBlock = (Map<String, Object>) respMap.get("result");
    MoneroBlock block = convertRpcBlock((Map<String, Object>) rpcBlock);
    return block;
  }

  @Override
  public List<MoneroBlock> getBlocksByHeight(List<Integer> heights) {
    throw new RuntimeException("Binary requests not implemented");
//    await this._initOneTime();
//    
//    // fetch blocks in binary
//    let respBin = await this.config.rpc.sendBinaryRequest("get_blocks_by_height.bin", { heights: heights });
//    
//    // convert binary blocks to json
//    let rpcBlocks = this.coreUtils.binary_blocks_to_json(respBin);
//    MoneroDaemonRpc._checkResponseStatus(rpcBlocks);
//    //console.log(JSON.stringify(rpcBlocks));
//    
//    // build blocks with transactions
//    assert.equal(rpcBlocks.txs.length, rpcBlocks.blocks.length);    
//    let blocks = [];
//    for (let blockIdx = 0; blockIdx < rpcBlocks.blocks.length; blockIdx++) {
//      
//      // build block
//      let block = MoneroDaemonRpc._buildBlock(rpcBlocks.blocks[blockIdx]);
//      block.getHeader().setHeight(heights[blockIdx]);
//      blocks.push(block);
//      
//      // build transactions
//      let txs = [];
//      for (let txIdx = 0; txIdx < rpcBlocks.txs[blockIdx].length; txIdx++) {
//        let tx = new MoneroTx();
//        txs.push(tx);
//        tx.setId(rpcBlocks.blocks[blockIdx].tx_hashes[txIdx]);
//        tx.setIsConfirmed(true);
//        tx.setInTxPool(false);
//        tx.setIsCoinbase(false);
//        tx.setDoNotRelay(false);
//        tx.setIsRelayed(true);
//        tx.setIsFailed(false);
//        tx.setIsDoubleSpend(false);
//        MoneroDaemonRpc._buildTx(rpcBlocks.txs[blockIdx][txIdx], tx);
//      }
//      
//      // merge into one block
//      block.setTxs([]);
//      for (let tx of txs) {
//        if (tx.getBlock()) block.merge(tx.getBlock());
//        else block.getTxs().push(tx.setBlock(block));
//      }
//    }
//    
//    return blocks;
  }

  @Override
  public List<MoneroBlock> getBlocksByRange(Integer startHeight, Integer endHeight) {
    if (startHeight == null) startHeight = 0;
    if (endHeight == null) endHeight = getHeight() - 1;
    List<Integer> heights = new ArrayList<Integer>();
    for (int height = startHeight; height <= endHeight; height++) heights.add(height);
    return getBlocksByHeight(heights);
  }

  @Override
  public List<String> getBlockIds(List<String> blockIds, Integer startHeight) {
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
  
  /**
   * Transfers RPC tx fields to a given MoneroTx without overwriting previous values.
   * 
   * TODO: switch from safe set
   * 
   * @param rpcTx is the RPC map containing transaction fields
   * @param tx is the MoneroTx to populate with values (optional)
   * @returns tx is the same tx that was passed in or a new one if none given
   */
  @SuppressWarnings("unchecked")
  private static MoneroTx convertRpcTx(Map<String, Object> rpcTx, MoneroTx tx) {
    if (rpcTx == null) return null;
    if (tx == null) tx = new MoneroTx();
    
//    System.out.println("******** BUILDING TX ***********");
//    System.out.println(rpcTx);
//    System.out.println(tx.toString());
    
    // initialize from rpc map
    MoneroBlockHeader header = null;
    for (String key : rpcTx.keySet()) {
      Object val = rpcTx.get(key);
      if (key.equals("tx_hash") || key.equals("id_hash")) tx.setId(MoneroUtils.reconcile(tx.getId(), (String) val));
      else if (key.equals("block_timestamp")) {
        if (header == null) header = new MoneroBlockHeader();
        header.setTimestamp(MoneroUtils.reconcile(header.getTimestamp(), ((BigInteger) val).longValue()));
      }
      else if (key.equals("block_height")) {
        if (header == null) header = new MoneroBlockHeader();
        header.setHeight(MoneroUtils.reconcile(header.getHeight(), ((BigInteger) val).intValue()));
      }
      else if (key.equals("last_relayed_time")) tx.setLastRelayedTimestamp(MoneroUtils.reconcile(tx.getLastRelayedTimestamp(), ((BigInteger) val).longValue()));
      else if (key.equals("receive_time")) tx.setReceivedTimestamp(MoneroUtils.reconcile(tx.getReceivedTimestamp(), ((BigInteger) val).longValue()));
      else if (key.equals("in_pool")) {
        tx.setIsConfirmed(MoneroUtils.reconcile(tx.getIsConfirmed(), (Boolean) val));
        tx.setInTxPool(MoneroUtils.reconcile(tx.getInTxPool(), (Boolean) val));
      }
      else if (key.equals("double_spend_seen")) tx.setIsDoubleSpend(MoneroUtils.reconcile(tx.getIsDoubleSpend(), (Boolean) val));
      else if (key.equals("version")) tx.setVersion(MoneroUtils.reconcile(tx.getVersion(), ((BigInteger) val).intValue()));
      else if (key.equals("extra")) tx.setExtra(MoneroUtils.reconcile(tx.getExtra(), GenUtils.listToIntArray((List<Integer>) val)));
      else if (key.equals("vin")) {
        List<Map<String, Object>> rpcVins = (List<Map<String, Object>>) val;
        if (rpcVins.size() != 1 || !rpcVins.get(0).containsKey("gen")) {  // ignore coinbase vin TODO: why? probably needs re-enabled
          List<MoneroOutput> vouts = new ArrayList<MoneroOutput>();
          for (Map<String, Object> rpcVin : rpcVins) vouts.add(convertRpcOutput(rpcVin, null));
          tx.setVouts(vouts);
        }
      }
      else if (key.equals("vout")) {
        List<Map<String, Object>> rpcVouts = (List<Map<String, Object>>) val;
        List<MoneroOutput> vouts = new ArrayList<MoneroOutput>();
        for (Map<String, Object> rpcVout : rpcVouts) vouts.add(convertRpcOutput(rpcVout, null));
        tx.setVouts(vouts);
      }
      else if (key.equals("rct_signatures")) tx.setRctSignatures(MoneroUtils.reconcile(tx.getRctSignatures(), (List<String>) val));
      else if (key.equals("rctsig_prunable")) tx.setRctSigPrunable(MoneroUtils.reconcile(tx.getRctSigPrunable(), val));
      else if (key.equals("unlock_time")) tx.setUnlockTime(MoneroUtils.reconcile(tx.getUnlockTime(), ((BigInteger) val).intValue()));
      else if (key.equals("as_json") || key.equals("tx_json")) { }  // handled last so tx is as initialized as possible
      else if (key.equals("as_hex") || key.equals("tx_blob")) tx.setHex(MoneroUtils.reconcile(tx.getHex(), (String) val));
      else if (key.equals("blob_size")) tx.setSize(MoneroUtils.reconcile(tx.getSize(), ((BigInteger) val).intValue()));
      else if (key.equals("weight")) tx.setWeight(MoneroUtils.reconcile(tx.getWeight(), ((BigInteger) val).intValue()));
      else if (key.equals("fee")) tx.setFee(MoneroUtils.reconcile(tx.getFee(), (BigInteger) val));
      else if (key.equals("relayed")) tx.setIsRelayed(MoneroUtils.reconcile(tx.getIsRelayed(), (Boolean) val));
      else if (key.equals("output_indices")) tx.setOutputIndices(MoneroUtils.reconcile(tx.getOutputIndices(), (List<Integer>) val));
      else if (key.equals("do_not_relay")) tx.setDoNotRelay(MoneroUtils.reconcile(tx.getDoNotRelay(), (Boolean) val));
      else if (key.equals("kept_by_block")) tx.setIsKeptByBlock(MoneroUtils.reconcile(tx.getIsKeptByBlock(), (Boolean) val));
      else if (key.equals("signatures")) tx.setSignatures(MoneroUtils.reconcile(tx.getSignatures(), (List<String>) val));
      else if (key.equals("last_failed_height")) {
        int lastFailedHeight = ((BigInteger) val).intValue();
        if (lastFailedHeight == 0) tx.setIsFailed(MoneroUtils.reconcile(tx.getIsFailed(), false));
        else {
          tx.setIsFailed(MoneroUtils.reconcile(tx.getIsFailed(), true));
          tx.setLastFailedHeight(MoneroUtils.reconcile(tx.getLastFailedHeight(), lastFailedHeight));
        }
      }
      else if (key.equals("last_failed_id_hash")) {
        if (DEFAULT_ID.equals((String) val)) tx.setIsFailed(MoneroUtils.reconcile(tx.getIsFailed(), false));
        else {
          tx.setIsFailed(MoneroUtils.reconcile(tx.getIsFailed(), true));
          tx.setLastFailedId(MoneroUtils.reconcile(tx.getLastFailedId(), (String) val));
        }
      }
      else if (key.equals("max_used_block_height")) tx.setMaxUsedBlockHeight(MoneroUtils.reconcile(tx.getMaxUsedBlockHeight(), ((BigInteger) val).intValue()));
      else if (key.equals("max_used_block_id_hash")) tx.setMaxUsedBlockId(MoneroUtils.reconcile(tx.getMaxUsedBlockId(), (String) val));
      else if (key.equals("prunable_hash")) tx.setPrunableHash(MoneroUtils.reconcile(tx.getPrunableHash(), (String) val));
      else if (key.equals("prunable_as_hex")) tx.setPrunableHex(MoneroUtils.reconcile(tx.getPrunableHex(), (String) val));
      else LOGGER.warn("WARNING: ignoring unexpected field in rpc tx: " + key + ": " + val);
    }
    
    // link block and tx
    if (header != null) tx.setBlock(new MoneroBlock().setHeader(header).setTxs(Arrays.asList(tx)));
    
    // TODO monero-daemon-rpc: unconfirmed txs block height and timestamp are actually received timestamp; overloading data model variables is bad juju
    if (tx.getBlock() != null && tx.getBlock().getHeader().getHeight() != null && (long) tx.getBlock().getHeader().getHeight() == tx.getBlock().getHeader().getTimestamp()) {
      tx.setReceivedTimestamp((long) tx.getBlock().getHeader().getHeight());
      tx.setBlock(null);
      tx.setIsConfirmed(false);
    }
    
    // initialize remaining known fields
    if (tx.getIsConfirmed()) {
      tx.setIsRelayed(MoneroUtils.reconcile(tx.getIsRelayed(), true));
      tx.setDoNotRelay(MoneroUtils.reconcile(tx.getDoNotRelay(), false));
      tx.setIsFailed(MoneroUtils.reconcile(tx.getIsFailed(), false));
    } else {
      tx.setNumConfirmations(0);
    }
    if (tx.getIsFailed() == null) tx.setIsFailed(false);
    if (tx.getOutputIndices() != null && tx.getVouts() != null)  {
      assertEquals(tx.getOutputIndices().size(), (int) tx.getVouts().size());
      for (int i = 0; i < tx.getVouts().size(); i++) {
        tx.getVouts().get(i).setIndex(tx.getOutputIndices().get(i));  // transfer output indices to vouts
      }
    }
    if (rpcTx.containsKey("as_json")) convertRpcTx(JsonUtils.deserialize(MoneroRpc.MAPPER, (String) rpcTx.get("as_json"), new TypeReference<Map<String, Object>>(){}), tx);
    if (rpcTx.containsKey("tx_json")) convertRpcTx(JsonUtils.deserialize(MoneroRpc.MAPPER, (String) rpcTx.get("tx_json"), new TypeReference<Map<String, Object>>(){}), tx);
    if (!tx.getIsRelayed()) tx.setLastRelayedTimestamp(null);  // TODO monero-daemon-rpc: returns last_relayed_timestamp despite relayed: false, self inconsistent
    
    // return built transaction
    return tx;
  }
  
  @SuppressWarnings("unchecked")
  private static MoneroOutput convertRpcOutput(Map<String, Object> rpcOutput, MoneroTx tx) {
    MoneroOutput output = new MoneroOutput();
    output.setTx(tx);
    for (String key : rpcOutput.keySet()) {
      Object val = rpcOutput.get(key);
      if (key.equals("gen")) throw new Error("Output with 'gen' from daemon rpc is coinbase tx which we ignore (i.e. each coinbase vin is null)");
      else if (key.equals("key")) {
        Map<String, Object> rpcKey = (Map<String, Object>) val;
        output.setAmount(MoneroUtils.reconcile(output.getAmount(), (BigInteger) rpcKey.get("amount")));
        output.setKeyImage(MoneroUtils.reconcile(output.getKeyImage(), new MoneroKeyImage((String) rpcKey.get("k_image"))));
        output.setRingOutputIndices(MoneroUtils.reconcile(output.getRingOutputIndices(), (List<Integer>) rpcKey.get("key_offsets")));
      }
      else if (key.equals("amount")) output.setAmount(MoneroUtils.reconcile(output.getAmount(), (BigInteger) val));
      else if (key.equals("target")) output.setStealthPublicKey(MoneroUtils.reconcile(output.getStealthPublicKey(), (String) ((Map<String, Object>) val).get("key")));
      else LOGGER.warn("WARNING: ignoring unexpected field output: " + key + ": " + val);
    }
    return output;
  }
}
