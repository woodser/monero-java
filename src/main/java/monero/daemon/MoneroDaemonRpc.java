package monero.daemon;

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
    MoneroBlockHeader header;
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
          tx.setVins(new ArrayList<MoneroOutput>());
          for (Map<String, Object> rpcVin : rpcVins) tx.getVins().add(convertRpcOutput(rpcVin, null));
        }
      }
      else if (key.equals("vout")) {
        List<Map<String, Object>> rpcVouts = (List<Map<String, Object>>) val;
        List<MoneroOutput> vouts = new ArrayList<MoneroOutput>();
        for (Map<String, Object> rpcVout : rpcVouts) vouts.add(convertRpcOutput(rpcVout, null));
        tx.setVouts(vouts);
      }
      else if (key.equals("rct_signatures")) MoneroUtils.safeSet(tx, tx.getRctSignatures, tx.setRctSignatures, val);
      else if (key.equals("rctsig_prunable")) MoneroUtils.safeSet(tx, tx.getRctSigPrunable, tx.setRctSigPrunable, val);
      else if (key.equals("unlock_time")) MoneroUtils.safeSet(tx, tx.getUnlockTime, tx.setUnlockTime, val);
      else if (key.equals("as_json" || key.equals("tx_json")) { }  // handled last so tx is as initialized as possible
      else if (key.equals("as_hex" || key.equals("tx_blob")) MoneroUtils.safeSet(tx, tx.getHex, tx.setHex, val ? val : null);
      else if (key.equals("blob_size")) MoneroUtils.safeSet(tx, tx.getSize, tx.setSize, val);
      else if (key.equals("weight")) MoneroUtils.safeSet(tx, tx.getWeight, tx.setWeight, val);
      else if (key.equals("fee")) MoneroUtils.safeSet(tx, tx.getFee, tx.setFee, new BigInteger(val));
      else if (key.equals("relayed")) MoneroUtils.safeSet(tx, tx.getIsRelayed, tx.setIsRelayed, val);
      else if (key.equals("output_indices")) MoneroUtils.safeSet(tx, tx.getOutputIndices, tx.setOutputIndices, val);
      else if (key.equals("do_not_relay")) MoneroUtils.safeSet(tx, tx.getDoNotRelay, tx.setDoNotRelay, val);
      else if (key.equals("kept_by_block")) MoneroUtils.safeSet(tx, tx.getIsKeptByBlock, tx.setIsKeptByBlock, val);
      else if (key.equals("signatures")) MoneroUtils.safeSet(tx, tx.getSignatures, tx.setSignatures, val);
      else if (key.equals("last_failed_height")) {
        if (val === 0) MoneroUtils.safeSet(tx, tx.getIsFailed, tx.setIsFailed, false);
        else {
          MoneroUtils.safeSet(tx, tx.getIsFailed, tx.setIsFailed, true);
          MoneroUtils.safeSet(tx, tx.getLastFailedHeight, tx.setLastFailedHeight, val);
        }
      }
      else if (key.equals("last_failed_id_hash")) {
        if (val === MoneroDaemonRpc.DEFAULT_ID) MoneroUtils.safeSet(tx, tx.getIsFailed, tx.setIsFailed);
        else {
          MoneroUtils.safeSet(tx, tx.getIsFailed, tx.setIsFailed, true);
          MoneroUtils.safeSet(tx, tx.getLastFailedId, tx.setLastFailedId, val);
        }
      }
      else if (key.equals("max_used_block_height")) MoneroUtils.safeSet(tx, tx.getMaxUsedBlockHeight, tx.setMaxUsedBlockHeight, val);
      else if (key.equals("max_used_block_id_hash")) MoneroUtils.safeSet(tx, tx.getMaxUsedBlockId, tx.setMaxUsedBlockId, val);
      else if (key.equals("prunable_hash")) MoneroUtils.safeSet(tx, tx.getPrunableHash, tx.setPrunableHash, val ? val : null);
      else if (key.equals("prunable_as_hex")) MoneroUtils.safeSet(tx, tx.getPrunableHex, tx.setPrunableHex, val ? val : null);
      else console.log("WARNING: ignoring unexpected field in rpc tx: " + key + ": " + val);
    }
    
    // link block and tx
    if (header) tx.setBlock(new MoneroBlock().setHeader(header).setTxs([tx]));
    
    // TODO monero-daemon-rpc: unconfirmed txs block height and timestamp are actually received timestamp; overloading data model variables is bad juju
    if (tx.getBlock() && tx.getBlock().getHeader().getHeight() !== null && tx.getBlock().getHeader().getHeight() === tx.getBlock().getHeader().getTimestamp()) {
      tx.setReceivedTimestamp(tx.getBlock().getHeader().getHeight());
      tx.setBlock(null);
      tx.setIsConfirmed(false);
    }
    
    // initialize remaining known fields
    if (tx.getIsConfirmed()) {
      MoneroUtils.safeSet(tx, tx.getIsRelayed, tx.setIsRelayed, true);
      MoneroUtils.safeSet(tx, tx.getDoNotRelay, tx.setDoNotRelay, false);
      MoneroUtils.safeSet(tx, tx.getIsFailed, tx.setIsFailed, false);
    } else {
      tx.setNumConfirmations(0);
    }
    if (tx.getIsFailed() === null) tx.setIsFailed(false);
    if (tx.getOutputIndices() && tx.getVouts())  {
      assert.equal(tx.getVouts().length, tx.getOutputIndices().length);
      for (let i = 0; i < tx.getVouts().length; i++) {
        tx.getVouts()[i].setIndex(tx.getOutputIndices()[i]);  // transfer output indices to vouts
      }
    }
    if (rpcTx.as_json) MoneroDaemonRpc._buildTx(JSON.parse(rpcTx.as_json), tx);
    if (rpcTx.tx_json) MoneroDaemonRpc._buildTx(JSON.parse(rpcTx.tx_json), tx);
    if (!tx.getIsRelayed()) tx.setLastRelayedTimestamp(null);  // TODO monero-daemon-rpc: returns last_relayed_timestamp despite relayed: false, self inconsistent
    
    // return built transaction
    return tx;
  }
  
  private static MoneroOutput convertRpcOutput(Map<String, Object> rpcOutput, MoneroTx tx) {
    let output = new MoneroOutput();
    output.setTx(tx);
    for (let key of Object.keys(rpcOutput)) {
      let val = rpcOutput[key];
      if (key.equals("gen")) throw new Error("Output with 'gen' from daemon rpc is coinbase tx which we ignore (i.e. each coinbase vin is null)");
      else if (key.equals("key")) {
        MoneroUtils.safeSet(output, output.getAmount, output.setAmount, new BigInteger(val.amount));
        MoneroUtils.safeSet(output, output.getKeyImage, output.setKeyImage, new MoneroKeyImage(val.k_image));
        MoneroUtils.safeSet(output, output.getRingOutputIndices, output.setRingOutputIndices, val.key_offsets);
      }
      else if (key.equals("amount")) MoneroUtils.safeSet(output, output.getAmount, output.setAmount, new BigInteger(val));
      else if (key.equals("target")) MoneroUtils.safeSet(output, output.getStealthPublicKey, output.setStealthPublicKey, val.key);
      else console.log("WARNING: ignoring unexpected field output: " + key + ": " + val);
    }
    return output;
  }
}
