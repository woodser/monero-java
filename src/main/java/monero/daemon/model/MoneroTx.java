package monero.daemon.model;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import common.utils.GenUtils;
import monero.utils.MoneroUtils;

/**
 * Represents a transaction on the Monero network.
 */
public class MoneroTx {
  
  public static final String DEFAULT_PAYMENT_ID = "0000000000000000";

  private MoneroBlock block;
  private Integer height;
  private String id;
  private Integer version;
  private Boolean isCoinbase;
  private String paymentId;
  private BigInteger fee;
  private Integer mixin;
  private Boolean doNotRelay;
  private Boolean isRelayed;
  private Boolean isConfirmed;
  private Boolean inTxPool;
  private Integer numConfirmations;
  private Integer numEstimatedBlocksUntilConfirmed;
  private Integer unlockTime;
  private Long lastRelayedTimestamp;
  private Long receivedTimestamp;
  private Boolean isDoubleSpend;
  private String key;
  private String fullHex;
  private String prunedHex;
  private String prunableHex;
  private String prunableHash;
  private Integer size;
  private Integer weight;
  private List<MoneroOutput> vins;
  private List<MoneroOutput> vouts;
  private List<Integer> outputIndices;
  private String metadata;
  private String commonTxSets;
  private int[] extra;
  private Object rctSignatures; // TODO: implement
  private Object rctSigPrunable;  // TODO: implement
  private Boolean isKeptByBlock;
  private Boolean isFailed;
  private Integer lastFailedHeight;
  private String lastFailedId;
  private Integer maxUsedBlockHeight;
  private String maxUsedBlockId;
  private List<String> signatures;
  
  public MoneroTx() {
    
  }
  
  /**
   * Construct this transaction as a deep copy of the given transaction.
   * 
   * @param tx is the transaction to make a deep copy of
   */
  public MoneroTx(MoneroTx tx) {
    throw new RuntimeException("Not implemented");
  }
  
  public MoneroBlock getBlock() {
    return block;
  }
  
  public MoneroTx setBlock(MoneroBlock block) {
    this.block = block;
    return this;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public MoneroTx setHeight(Integer height) {
    this.height = height;
    return this;
  }
  
  public String getId() {
    return id;
  }
  
  public MoneroTx setId(String id) {
    this.id = id;
    return this;
  }
  
  public Integer getVersion() {
    return version;
  }
  
  public MoneroTx setVersion(Integer version) {
    this.version = version;
    return this;
  }
  
  public Boolean getIsCoinbase() {
    return isCoinbase;
  }
  
  public MoneroTx setIsCoinbase(Boolean isCoinbase) {
    this.isCoinbase = isCoinbase;
    return this;
  }
  
  public String getPaymentId() {
    return paymentId;
  }
  
  public MoneroTx setPaymentId(String paymentId) {
    this.paymentId = paymentId;
    return this;
  }
  
  public BigInteger getFee() {
    return fee;
  }
  
  public MoneroTx setFee(BigInteger fee) {
    this.fee = fee;
    return this;
  }
  
  public Integer getMixin() {
    return mixin;
  }
  
  public MoneroTx setMixin(Integer mixin) {
    this.mixin = mixin;
    return this;
  }
  
  public Boolean getDoNotRelay() {
    return doNotRelay;
  }
  
  public MoneroTx setDoNotRelay(Boolean doNotRelay) {
    this.doNotRelay = doNotRelay;
    return this;
  }
  
  public Boolean getIsRelayed() {
    return isRelayed;
  }
  
  public MoneroTx setIsRelayed(Boolean isRelayed) {
    this.isRelayed = isRelayed;
    return this;
  }
  
  public Boolean getIsConfirmed() {
    return isConfirmed;
  }
  
  public MoneroTx setIsConfirmed(Boolean isConfirmed) {
    this.isConfirmed = isConfirmed;
    return this;
  }
  
  public Boolean getInTxPool() {
    return inTxPool;
  }
  
  public MoneroTx setInTxPool(Boolean inTxPool) {
    this.inTxPool = inTxPool;
    return this;
  }
  
  public Integer getNumConfirmations() {
    return numConfirmations;
  }
  
  public MoneroTx setNumConfirmations(Integer numConfirmations) {
    this.numConfirmations = numConfirmations;
    return this;
  }
  
  public Integer getNumEstimatedBlocksUntilConfirmed() {
    return numEstimatedBlocksUntilConfirmed;
  }
  
  public MoneroTx setNumEstimatedBlocksUntilConfirmed(Integer numEstimatedBlocksUntilConfirmed) {
    this.numEstimatedBlocksUntilConfirmed = numEstimatedBlocksUntilConfirmed;
    return this;
  }
  
  public Integer getUnlockTime() {
    return unlockTime;
  }
  
  public MoneroTx setUnlockTime(Integer unlockTime) {
    this.unlockTime = unlockTime;
    return this;
  }
  
  public Long getLastRelayedTimestamp() {
    return lastRelayedTimestamp;
  }
  
  public MoneroTx setLastRelayedTimestamp(Long lastRelayedTimestamp) {
    this.lastRelayedTimestamp = lastRelayedTimestamp;
    return this;
  }
  
  public Long getReceivedTimestamp() {
    return receivedTimestamp;
  }
  
  public MoneroTx setReceivedTimestamp(Long receivedTimestamp) {
    this.receivedTimestamp = receivedTimestamp;
    return this;
  }
  
  public Boolean getIsDoubleSpend() {
    return isDoubleSpend;
  }
  
  public MoneroTx setIsDoubleSpend(Boolean isDoubleSpend) {
    this.isDoubleSpend = isDoubleSpend;
    return this;
  }
  
  public String getKey() {
    return key;
  }
  
  public MoneroTx setKey(String key) {
    this.key = key;
    return this;
  }
  
  public String getFullHex() {
    return fullHex;
  }
  
  public MoneroTx setFullHex(String fullHex) {
    this.fullHex = fullHex;
    return this;
  }
  
  public String getPrunedHex() {
    return prunedHex;
  }
  
  public MoneroTx setPrunedHex(String prunedHex) {
    this.prunedHex = prunedHex;
    return this;
  }
  
  public String getPrunableHex() {
    return prunableHex;
  }
  
  public MoneroTx setPrunableHex(String prunableHex) {
    this.prunableHex = prunableHex;
    return this;
  }
  
  public String getPrunableHash() {
    return prunableHash;
  }
  
  public MoneroTx setPrunableHash(String prunableHash) {
    this.prunableHash = prunableHash;
    return this;
  }
  
  public Integer getSize() {
    return size;
  }
  
  public MoneroTx setSize(Integer size) {
    this.size = size;
    return this;
  }
  
  public Integer getWeight() {
    return weight;
  }
  
  public MoneroTx setWeight(Integer weight) {
    this.weight = weight;
    return this;
  }
  
  public List<MoneroOutput> getVins() {
    return vins;
  }
  
  public MoneroTx setVins(List<MoneroOutput> vins) {
    this.vins = vins;
    return this;
  }
  
  public List<MoneroOutput> getVouts() {
    return vouts;
  }
  
  public MoneroTx setVouts(List<MoneroOutput> vouts) {
    this.vouts = vouts;
    return this;
  }
  
  public List<Integer> getOutputIndices() {
    return outputIndices;
  }
  
  public MoneroTx setOutputIndices(List<Integer> outputIndices) {
    this.outputIndices = outputIndices;
    return this;
  }
  
  public String getMetadata() {
    return metadata;
  }
  
  public MoneroTx setMetadata(String metadata) {
    this.metadata = metadata;
    return this;
  }
  
  public String getCommonTxSets() {
    return commonTxSets;
  }
  
  public MoneroTx setCommonTxSets(String commonTxSets) {
    this.commonTxSets = commonTxSets;
    return this;
  }
  
  public int[] getExtra() {
    return extra;
  }
  
  public MoneroTx setExtra(int[] extra) {
    this.extra = extra;
    return this;
  }
  
  public Object getRctSignatures() {
    return rctSignatures;
  }
  
  public MoneroTx setRctSignatures(Object rctSignatures) {
    this.rctSignatures = rctSignatures;
    return this;
  }
  
  public Object getRctSigPrunable() {
    return rctSigPrunable;
  }
  
  public MoneroTx setRctSigPrunable(Object rctSigPrunable) {
    this.rctSigPrunable = rctSigPrunable;
    return this;
  }
  
  public Boolean getIsKeptByBlock() {
    return isKeptByBlock;
  }
  
  public MoneroTx setIsKeptByBlock(Boolean isKeptByBlock) {
    this.isKeptByBlock = isKeptByBlock;
    return this;
  }
  
  public Boolean getIsFailed() {
    return isFailed;
  }
  
  public MoneroTx setIsFailed(Boolean isFailed) {
    this.isFailed = isFailed;
    return this;
  }
  
  public Integer getLastFailedHeight() {
    return lastFailedHeight;
  }
  
  public MoneroTx setLastFailedHeight(Integer lastFailedHeight) {
    this.lastFailedHeight = lastFailedHeight;
    return this;
  }
  
  public String getLastFailedId() {
    return lastFailedId;
  }
  
  public MoneroTx setLastFailedId(String lastFailedId) {
    this.lastFailedId = lastFailedId;
    return this;
  }
  
  public Integer getMaxUsedBlockHeight() {
    return maxUsedBlockHeight;
  }
  
  public MoneroTx setMaxUsedBlockHeight(Integer maxUsedBlockHeight) {
    this.maxUsedBlockHeight = maxUsedBlockHeight;
    return this;
  }
  
  public String getMaxUsedBlockId() {
    return maxUsedBlockId;
  }
  
  public MoneroTx setMaxUsedBlockId(String maxUsedBlockId) {
    this.maxUsedBlockId = maxUsedBlockId;
    return this;
  }
  
  public List<String> getSignatures() {
    return signatures;
  }
  
  public MoneroTx setSignatures(List<String> signatures) {
    this.signatures = signatures;
    return this;
  }
  
  public MoneroTx merge(MoneroTx tx) {
    throw new RuntimeException("Not implemented");
  }
  
  public MoneroTx copy() {
    return new MoneroTx(this);
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.getIndent(indent) + "=== TX ===\n");
    sb.append(MoneroUtils.kvLine("Tx ID: ", getId(), indent));
    sb.append(MoneroUtils.kvLine("Version", getVersion(), indent));
    sb.append(MoneroUtils.kvLine("Is coinbase", getIsCoinbase(), indent));
    sb.append(MoneroUtils.kvLine("Payment ID", getPaymentId(), indent));
    sb.append(MoneroUtils.kvLine("Fee", getFee(), indent));
    sb.append(MoneroUtils.kvLine("Mixin", getMixin(), indent));
    sb.append(MoneroUtils.kvLine("Do not relay", getDoNotRelay(), indent));
    sb.append(MoneroUtils.kvLine("Is relayed", getIsRelayed(), indent));
    sb.append(MoneroUtils.kvLine("Is confirmed", getIsConfirmed(), indent));
    sb.append(MoneroUtils.kvLine("In tx pool", getInTxPool(), indent));
    sb.append(MoneroUtils.kvLine("Num confirmations", getNumConfirmations(), indent));
    sb.append(MoneroUtils.kvLine("Num estimated blocks until confirmed", getNumEstimatedBlocksUntilConfirmed(), indent));
    sb.append(MoneroUtils.kvLine("Unlock time", getUnlockTime(), indent));
    sb.append(MoneroUtils.kvLine("Last relayed time", getLastRelayedTimestamp(), indent));
    sb.append(MoneroUtils.kvLine("Received time", getReceivedTimestamp(), indent));
    sb.append(MoneroUtils.kvLine("Is double spend", getIsDoubleSpend(), indent));
    sb.append(MoneroUtils.kvLine("Key", getKey(), indent));
    sb.append(MoneroUtils.kvLine("Full Hex", getFullHex(), indent));
    sb.append(MoneroUtils.kvLine("Pruned hex", getPrunedHex(), indent));
    sb.append(MoneroUtils.kvLine("Prunable hex", getPrunableHex(), indent));
    sb.append(MoneroUtils.kvLine("Prunable hash", getPrunableHash(), indent));
    sb.append(MoneroUtils.kvLine("Size", getSize(), indent));
    sb.append(MoneroUtils.kvLine("Weight", getWeight(), indent));
    sb.append(MoneroUtils.kvLine("Output indices", getOutputIndices(), indent));
    sb.append(MoneroUtils.kvLine("Metadata", getMetadata(), indent));
    sb.append(MoneroUtils.kvLine("Common tx sets", getCommonTxSets(), indent));
    sb.append(MoneroUtils.kvLine("Extra", Arrays.toString(getExtra()), indent));
    sb.append(MoneroUtils.kvLine("RCT signatures", getRctSignatures(), indent));
    sb.append(MoneroUtils.kvLine("RCT sig prunable", getRctSigPrunable(), indent));
    sb.append(MoneroUtils.kvLine("Kept by block", getIsKeptByBlock(), indent));
    sb.append(MoneroUtils.kvLine("Is failed", getIsFailed(), indent));
    sb.append(MoneroUtils.kvLine("Last failed height", getLastFailedHeight(), indent));
    sb.append(MoneroUtils.kvLine("Last failed id", getLastFailedId(), indent));
    sb.append(MoneroUtils.kvLine("Max used block height", getMaxUsedBlockHeight(), indent));
    sb.append(MoneroUtils.kvLine("Max used block id", getMaxUsedBlockId(), indent));
    sb.append(MoneroUtils.kvLine("Signatures", getSignatures(), indent));
    if (getVins() != null) {
      sb.append(MoneroUtils.kvLine("Vins", "", indent));
      for (int i = 0; i < getVins().size(); i++) {
        sb.append(MoneroUtils.kvLine(i + 1, "", indent + 1));
        sb.append(getVins().get(i).toString(indent + 2));
        sb.append('\n');
      }
    }
    if (getVouts() != null) {
      sb.append(MoneroUtils.kvLine("Vouts", "", indent));
      for (int i = 0; i < getVouts().size(); i++) {
        sb.append(MoneroUtils.kvLine(i + 1, "", indent + 1));
        sb.append(getVouts().get(i).toString(indent + 2));
        sb.append('\n');
      }
    }
    String str = sb.toString();
    return str.substring(0, str.length() - 1);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((block == null) ? 0 : block.hashCode());
    result = prime * result + ((commonTxSets == null) ? 0 : commonTxSets.hashCode());
    result = prime * result + ((doNotRelay == null) ? 0 : doNotRelay.hashCode());
    result = prime * result + Arrays.hashCode(extra);
    result = prime * result + ((fee == null) ? 0 : fee.hashCode());
    result = prime * result + ((height == null) ? 0 : height.hashCode());
    result = prime * result + ((fullHex == null) ? 0 : fullHex.hashCode());
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    result = prime * result + ((inTxPool == null) ? 0 : inTxPool.hashCode());
    result = prime * result + ((isCoinbase == null) ? 0 : isCoinbase.hashCode());
    result = prime * result + ((isConfirmed == null) ? 0 : isConfirmed.hashCode());
    result = prime * result + ((isDoubleSpend == null) ? 0 : isDoubleSpend.hashCode());
    result = prime * result + ((isFailed == null) ? 0 : isFailed.hashCode());
    result = prime * result + ((isKeptByBlock == null) ? 0 : isKeptByBlock.hashCode());
    result = prime * result + ((isRelayed == null) ? 0 : isRelayed.hashCode());
    result = prime * result + ((key == null) ? 0 : key.hashCode());
    result = prime * result + ((lastFailedHeight == null) ? 0 : lastFailedHeight.hashCode());
    result = prime * result + ((lastFailedId == null) ? 0 : lastFailedId.hashCode());
    result = prime * result + ((lastRelayedTimestamp == null) ? 0 : lastRelayedTimestamp.hashCode());
    result = prime * result + ((maxUsedBlockHeight == null) ? 0 : maxUsedBlockHeight.hashCode());
    result = prime * result + ((maxUsedBlockId == null) ? 0 : maxUsedBlockId.hashCode());
    result = prime * result + ((metadata == null) ? 0 : metadata.hashCode());
    result = prime * result + ((mixin == null) ? 0 : mixin.hashCode());
    result = prime * result + ((numConfirmations == null) ? 0 : numConfirmations.hashCode());
    result = prime * result + ((numEstimatedBlocksUntilConfirmed == null) ? 0 : numEstimatedBlocksUntilConfirmed.hashCode());
    result = prime * result + ((outputIndices == null) ? 0 : outputIndices.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    result = prime * result + ((prunableHash == null) ? 0 : prunableHash.hashCode());
    result = prime * result + ((prunableHex == null) ? 0 : prunableHex.hashCode());
    result = prime * result + ((prunedHex == null) ? 0 : prunedHex.hashCode());
    result = prime * result + ((rctSigPrunable == null) ? 0 : rctSigPrunable.hashCode());
    result = prime * result + ((rctSignatures == null) ? 0 : rctSignatures.hashCode());
    result = prime * result + ((receivedTimestamp == null) ? 0 : receivedTimestamp.hashCode());
    result = prime * result + ((signatures == null) ? 0 : signatures.hashCode());
    result = prime * result + ((size == null) ? 0 : size.hashCode());
    result = prime * result + ((unlockTime == null) ? 0 : unlockTime.hashCode());
    result = prime * result + ((version == null) ? 0 : version.hashCode());
    result = prime * result + ((vins == null) ? 0 : vins.hashCode());
    result = prime * result + ((vouts == null) ? 0 : vouts.hashCode());
    result = prime * result + ((weight == null) ? 0 : weight.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTx other = (MoneroTx) obj;
    if (block == null) {
      if (other.block != null) return false;
    } else if (!block.equals(other.block)) return false;
    if (commonTxSets == null) {
      if (other.commonTxSets != null) return false;
    } else if (!commonTxSets.equals(other.commonTxSets)) return false;
    if (doNotRelay == null) {
      if (other.doNotRelay != null) return false;
    } else if (!doNotRelay.equals(other.doNotRelay)) return false;
    if (!Arrays.equals(extra, other.extra)) return false;
    if (fee == null) {
      if (other.fee != null) return false;
    } else if (!fee.equals(other.fee)) return false;
    if (height == null) {
      if (other.height != null) return false;
    } else if (!height.equals(other.height)) return false;
    if (fullHex == null) {
      if (other.fullHex != null) return false;
    } else if (!fullHex.equals(other.fullHex)) return false;
    if (id == null) {
      if (other.id != null) return false;
    } else if (!id.equals(other.id)) return false;
    if (inTxPool == null) {
      if (other.inTxPool != null) return false;
    } else if (!inTxPool.equals(other.inTxPool)) return false;
    if (isCoinbase == null) {
      if (other.isCoinbase != null) return false;
    } else if (!isCoinbase.equals(other.isCoinbase)) return false;
    if (isConfirmed == null) {
      if (other.isConfirmed != null) return false;
    } else if (!isConfirmed.equals(other.isConfirmed)) return false;
    if (isDoubleSpend == null) {
      if (other.isDoubleSpend != null) return false;
    } else if (!isDoubleSpend.equals(other.isDoubleSpend)) return false;
    if (isFailed == null) {
      if (other.isFailed != null) return false;
    } else if (!isFailed.equals(other.isFailed)) return false;
    if (isKeptByBlock == null) {
      if (other.isKeptByBlock != null) return false;
    } else if (!isKeptByBlock.equals(other.isKeptByBlock)) return false;
    if (isRelayed == null) {
      if (other.isRelayed != null) return false;
    } else if (!isRelayed.equals(other.isRelayed)) return false;
    if (key == null) {
      if (other.key != null) return false;
    } else if (!key.equals(other.key)) return false;
    if (lastFailedHeight == null) {
      if (other.lastFailedHeight != null) return false;
    } else if (!lastFailedHeight.equals(other.lastFailedHeight)) return false;
    if (lastFailedId == null) {
      if (other.lastFailedId != null) return false;
    } else if (!lastFailedId.equals(other.lastFailedId)) return false;
    if (lastRelayedTimestamp == null) {
      if (other.lastRelayedTimestamp != null) return false;
    } else if (!lastRelayedTimestamp.equals(other.lastRelayedTimestamp)) return false;
    if (maxUsedBlockHeight == null) {
      if (other.maxUsedBlockHeight != null) return false;
    } else if (!maxUsedBlockHeight.equals(other.maxUsedBlockHeight)) return false;
    if (maxUsedBlockId == null) {
      if (other.maxUsedBlockId != null) return false;
    } else if (!maxUsedBlockId.equals(other.maxUsedBlockId)) return false;
    if (metadata == null) {
      if (other.metadata != null) return false;
    } else if (!metadata.equals(other.metadata)) return false;
    if (mixin == null) {
      if (other.mixin != null) return false;
    } else if (!mixin.equals(other.mixin)) return false;
    if (numConfirmations == null) {
      if (other.numConfirmations != null) return false;
    } else if (!numConfirmations.equals(other.numConfirmations)) return false;
    if (numEstimatedBlocksUntilConfirmed == null) {
      if (other.numEstimatedBlocksUntilConfirmed != null) return false;
    } else if (!numEstimatedBlocksUntilConfirmed.equals(other.numEstimatedBlocksUntilConfirmed)) return false;
    if (outputIndices == null) {
      if (other.outputIndices != null) return false;
    } else if (!outputIndices.equals(other.outputIndices)) return false;
    if (paymentId == null) {
      if (other.paymentId != null) return false;
    } else if (!paymentId.equals(other.paymentId)) return false;
    if (prunableHash == null) {
      if (other.prunableHash != null) return false;
    } else if (!prunableHash.equals(other.prunableHash)) return false;
    if (prunableHex == null) {
      if (other.prunableHex != null) return false;
    } else if (!prunableHex.equals(other.prunableHex)) return false;
    if (prunedHex == null) {
      if (other.prunedHex != null) return false;
    } else if (!prunedHex.equals(other.prunedHex)) return false;
    if (rctSigPrunable == null) {
      if (other.rctSigPrunable != null) return false;
    } else if (!rctSigPrunable.equals(other.rctSigPrunable)) return false;
    if (rctSignatures == null) {
      if (other.rctSignatures != null) return false;
    } else if (!rctSignatures.equals(other.rctSignatures)) return false;
    if (receivedTimestamp == null) {
      if (other.receivedTimestamp != null) return false;
    } else if (!receivedTimestamp.equals(other.receivedTimestamp)) return false;
    if (signatures == null) {
      if (other.signatures != null) return false;
    } else if (!signatures.equals(other.signatures)) return false;
    if (size == null) {
      if (other.size != null) return false;
    } else if (!size.equals(other.size)) return false;
    if (unlockTime == null) {
      if (other.unlockTime != null) return false;
    } else if (!unlockTime.equals(other.unlockTime)) return false;
    if (version == null) {
      if (other.version != null) return false;
    } else if (!version.equals(other.version)) return false;
    if (vins == null) {
      if (other.vins != null) return false;
    } else if (!vins.equals(other.vins)) return false;
    if (vouts == null) {
      if (other.vouts != null) return false;
    } else if (!vouts.equals(other.vouts)) return false;
    if (weight == null) {
      if (other.weight != null) return false;
    } else if (!weight.equals(other.weight)) return false;
    return true;
  }
}
