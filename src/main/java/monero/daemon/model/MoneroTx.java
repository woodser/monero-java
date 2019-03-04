package monero.daemon.model;

import java.math.BigInteger;
import java.util.List;

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
  private String hex;
  private String prunedHex;
  private String prunableHex;
  private String prunableHash;
  private Integer size;
  private Integer weight;
  private List<MoneroOutput> vins;
  private List<? extends MoneroOutput> vouts;
  private List<Integer> outputIndices;
  private String metadata;
  private String commonTxSets;
  private int[] extra;
  private List<String> rctSignatures;
  private Object rctSigPrunable;
  private Boolean isKeptByBlock;
  private Boolean isFailed;
  private Integer lastFailedHeight;
  private String lastFailedId;
  private Integer maxUsedBlockHeight;
  private Integer maxUsedBlockId;
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
  
  public void setBlock(MoneroBlock block) {
    this.block = block;
  }
  
  public Integer getHeight() {
    return height;
  }
  
  public void setHeight(Integer height) {
    this.height = height;
  }
  
  public String getId() {
    return id;
  }
  
  public void setId(String id) {
    this.id = id;
  }
  
  public Integer getVersion() {
    return version;
  }
  
  public void setVersion(Integer version) {
    this.version = version;
  }
  
  public Boolean getIsCoinbase() {
    return isCoinbase;
  }
  
  public void setIsCoinbase(Boolean isCoinbase) {
    this.isCoinbase = isCoinbase;
  }
  
  public String getPaymentId() {
    return paymentId;
  }
  
  public void setPaymentId(String paymentId) {
    this.paymentId = paymentId;
  }
  
  public BigInteger getFee() {
    return fee;
  }
  
  public void setFee(BigInteger fee) {
    this.fee = fee;
  }
  
  public Integer getMixin() {
    return mixin;
  }
  
  public void setMixin(Integer mixin) {
    this.mixin = mixin;
  }
  
  public Boolean getDoNotRelay() {
    return doNotRelay;
  }
  
  public void setDoNotRelay(Boolean doNotRelay) {
    this.doNotRelay = doNotRelay;
  }
  
  public Boolean getIsRelayed() {
    return isRelayed;
  }
  
  public void setIsRelayed(Boolean isRelayed) {
    this.isRelayed = isRelayed;
  }
  
  public Boolean getIsConfirmed() {
    return isConfirmed;
  }
  
  public void setIsConfirmed(Boolean isConfirmed) {
    this.isConfirmed = isConfirmed;
  }
  
  public Boolean getInTxPool() {
    return inTxPool;
  }
  
  public void setInTxPool(Boolean inTxPool) {
    this.inTxPool = inTxPool;
  }
  
  public Integer getNumConfirmations() {
    return numConfirmations;
  }
  
  public void setNumConfirmations(Integer numConfirmations) {
    this.numConfirmations = numConfirmations;
  }
  
  public Integer getNumEstimatedBlocksUntilConfirmed() {
    return numEstimatedBlocksUntilConfirmed;
  }
  
  public void setNumEstimatedBlocksUntilConfirmed(Integer numEstimatedBlocksUntilConfirmed) {
    this.numEstimatedBlocksUntilConfirmed = numEstimatedBlocksUntilConfirmed;
  }
  
  public Integer getUnlockTime() {
    return unlockTime;
  }
  
  public void setUnlockTime(Integer unlockTime) {
    this.unlockTime = unlockTime;
  }
  
  public Long getLastRelayedTimestamp() {
    return lastRelayedTimestamp;
  }
  
  public void setLastRelayedTimestamp(Long lastRelayedTimestamp) {
    this.lastRelayedTimestamp = lastRelayedTimestamp;
  }
  
  public Long getReceivedTimestamp() {
    return receivedTimestamp;
  }
  
  public void setReceivedTimestamp(Long receivedTimestamp) {
    this.receivedTimestamp = receivedTimestamp;
  }
  
  public Boolean getIsDoubleSpend() {
    return isDoubleSpend;
  }
  
  public void setIsDoubleSpend(Boolean isDoubleSpend) {
    this.isDoubleSpend = isDoubleSpend;
  }
  
  public String getKey() {
    return key;
  }
  
  public void setKey(String key) {
    this.key = key;
  }
  
  public String getHex() {
    return hex;
  }
  
  public void setHex(String hex) {
    this.hex = hex;
  }
  
  public String getPrunedHex() {
    return prunedHex;
  }
  
  public void setPrunedHex(String prunedHex) {
    this.prunedHex = prunedHex;
  }
  
  public String getPrunableHex() {
    return prunableHex;
  }
  
  public void setPrunableHex(String prunableHex) {
    this.prunableHex = prunableHex;
  }
  
  public String getPrunableHash() {
    return prunableHash;
  }
  
  public void setPrunableHash(String prunableHash) {
    this.prunableHash = prunableHash;
  }
  
  public Integer getSize() {
    return size;
  }
  
  public void setSize(Integer size) {
    this.size = size;
  }
  
  public Integer getWeight() {
    return weight;
  }
  
  public void setWeight(Integer weight) {
    this.weight = weight;
  }
  
  public List<MoneroOutput> getVins() {
    return vins;
  }
  
  public void setVins(List<MoneroOutput> vins) {
    this.vins = vins;
  }
  
  public List<? extends MoneroOutput> getVouts() {
    return vouts;
  }
  
  public void setVouts(List<? extends MoneroOutput> vouts) {
    this.vouts = vouts;
  }
  
  public List<Integer> getOutputIndices() {
    return outputIndices;
  }
  
  public void setOutputIndices(List<Integer> outputIndices) {
    this.outputIndices = outputIndices;
  }
  
  public String getMetadata() {
    return metadata;
  }
  
  public void setMetadata(String metadata) {
    this.metadata = metadata;
  }
  
  public String getCommonTxSets() {
    return commonTxSets;
  }
  
  public void setCommonTxSets(String commonTxSets) {
    this.commonTxSets = commonTxSets;
  }
  
  public int[] getExtra() {
    return extra;
  }
  
  public void setExtra(int[] extra) {
    this.extra = extra;
  }
  
  public List<String> getRctSignatures() {
    return rctSignatures;
  }
  
  public void setRctSignatures(List<String> rctSignatures) {
    this.rctSignatures = rctSignatures;
  }
  
  public Object getRctSigPrunable() {
    return rctSigPrunable;
  }
  
  public void setRctSigPrunable(Object rctSigPrunable) {
    this.rctSigPrunable = rctSigPrunable;
  }
  
  public Boolean getIsKeptByBlock() {
    return isKeptByBlock;
  }
  
  public void setIsKeptByBlock(Boolean isKeptByBlock) {
    this.isKeptByBlock = isKeptByBlock;
  }
  
  public Boolean getIsFailed() {
    return isFailed;
  }
  
  public void setIsFailed(Boolean isFailed) {
    this.isFailed = isFailed;
  }
  
  public Integer getLastFailedHeight() {
    return lastFailedHeight;
  }
  
  public void setLastFailedHeight(Integer lastFailedHeight) {
    this.lastFailedHeight = lastFailedHeight;
  }
  
  public String getLastFailedId() {
    return lastFailedId;
  }
  
  public void setLastFailedId(String lastFailedId) {
    this.lastFailedId = lastFailedId;
  }
  
  public Integer getMaxUsedBlockHeight() {
    return maxUsedBlockHeight;
  }
  
  public void setMaxUsedBlockHeight(Integer maxUsedBlockHeight) {
    this.maxUsedBlockHeight = maxUsedBlockHeight;
  }
  
  public Integer getMaxUsedBlockId() {
    return maxUsedBlockId;
  }
  
  public void setMaxUsedBlockId(Integer maxUsedBlockId) {
    this.maxUsedBlockId = maxUsedBlockId;
  }
  
  public List<String> getSignatures() {
    return signatures;
  }
  
  public void setSignatures(List<String> signatures) {
    this.signatures = signatures;
  }
  
  public MoneroTx merge(MoneroTx tx) {
    throw new RuntimeException("Not implemented");
  }
  
  public MoneroTx copy() {
    return new MoneroTx(this);
  }
}
