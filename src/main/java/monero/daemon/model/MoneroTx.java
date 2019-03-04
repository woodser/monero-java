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
  
  public String getHex() {
    return hex;
  }
  
  public MoneroTx setHex(String hex) {
    this.hex = hex;
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
  
  public List<? extends MoneroOutput> getVouts() {
    return vouts;
  }
  
  public MoneroTx setVouts(List<? extends MoneroOutput> vouts) {
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
  
  public List<String> getRctSignatures() {
    return rctSignatures;
  }
  
  public MoneroTx setRctSignatures(List<String> rctSignatures) {
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
  
  public Integer getMaxUsedBlockId() {
    return maxUsedBlockId;
  }
  
  public MoneroTx setMaxUsedBlockId(Integer maxUsedBlockId) {
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
}
