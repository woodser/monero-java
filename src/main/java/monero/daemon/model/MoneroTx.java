package monero.daemon.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.GenUtils;

/**
 * Represents a transaction on the Monero network.
 */
public class MoneroTx {
  
  public static final String DEFAULT_PAYMENT_ID = "0000000000000000";

  private MoneroBlock block;
  private String hash;
  private Integer version;
  private Boolean isMinerTx;
  private String paymentId;
  private BigInteger fee;
  private Integer ringSize;
  private Boolean relay;
  private Boolean isRelayed;
  private Boolean isConfirmed;
  private Boolean inTxPool;
  private Long numConfirmations;
  private Long unlockHeight;
  private Long lastRelayedTimestamp;
  private Long receivedTimestamp;
  private Boolean isDoubleSpendSeen;
  private String key;
  private String fullHex;
  private String prunedHex;
  private String prunableHex;
  private String prunableHash;
  private Long size;
  private Long weight;
  private List<MoneroOutput> inputs;
  private List<MoneroOutput> outputs;
  private List<Long> outputIndices;
  private String metadata;
  private int[] extra;  // TODO: switch to string for consistency with MoneroTxWallet
  private Object rctSignatures; // TODO: implement
  private Object rctSigPrunable;  // TODO: implement
  private Boolean isKeptByBlock;
  private Boolean isFailed;
  private Long lastFailedHeight;
  private String lastFailedHash;
  private Long maxUsedBlockHeight;
  private String maxUsedBlockHash;
  private List<String> signatures;
  
  public MoneroTx() {
    // nothing to build
  }
  
  /**
   * Construct this transaction as a deep copy of the given transaction.
   * 
   * @param tx is the transaction to make a deep copy of
   */
  public MoneroTx(final MoneroTx tx) {
    this.hash = tx.hash;
    this.version = tx.version;
    this.isMinerTx = tx.isMinerTx;
    this.paymentId = tx.paymentId;
    this.fee = tx.fee;
    this.ringSize = tx.ringSize;
    this.relay = tx.relay;
    this.isRelayed = tx.isRelayed;
    this.isConfirmed = tx.isConfirmed;
    this.inTxPool = tx.inTxPool;
    this.numConfirmations = tx.numConfirmations;
    this.unlockHeight = tx.unlockHeight;
    this.lastRelayedTimestamp = tx.lastRelayedTimestamp;
    this.receivedTimestamp = tx.receivedTimestamp;
    this.isDoubleSpendSeen = tx.isDoubleSpendSeen;
    this.key = tx.key;
    this.fullHex = tx.fullHex;
    this.prunedHex = tx.prunedHex;
    this.prunableHex = tx.prunableHex;
    this.prunableHash = tx.prunableHash;
    this.size = tx.size;
    this.weight = tx.weight;
    if (tx.inputs != null) {
      this.inputs = new ArrayList<MoneroOutput>();
      for (MoneroOutput input : tx.inputs) inputs.add(input.copy().setTx(this));
    }
    if (tx.outputs != null) {
      this.outputs = new ArrayList<MoneroOutput>();
      for (MoneroOutput output : tx.outputs) outputs.add(output.copy().setTx(this));
    }
    if (tx.outputIndices != null) this.outputIndices = new ArrayList<Long>(tx.outputIndices);
    this.metadata = tx.metadata;
    if (tx.extra != null) this.extra = tx.extra.clone();
    this.rctSignatures = tx.rctSignatures;
    this.rctSigPrunable = tx.rctSigPrunable;
    this.isKeptByBlock = tx.isKeptByBlock;
    this.isFailed = tx.isFailed;
    this.lastFailedHeight = tx.lastFailedHeight;
    this.lastFailedHash = tx.lastFailedHash;
    this.maxUsedBlockHeight = tx.maxUsedBlockHeight;
    this.maxUsedBlockHash = tx.maxUsedBlockHash;
    if (tx.signatures != null) this.signatures = new ArrayList<String>(tx.signatures);
  }
  
  public MoneroTx copy() {
    return new MoneroTx(this);
  }
  
  @JsonBackReference("block_txs")
  public MoneroBlock getBlock() {
    return block;
  }
  
  public MoneroTx setBlock(MoneroBlock block) {
    this.block = block;
    return this;
  }
  
  public Long getHeight() {
    return this.getBlock() == null ? null : this.getBlock().getHeight();
  }
  
  public String getHash() {
    return hash;
  }
  
  public MoneroTx setHash(String hash) {
    this.hash = hash;
    return this;
  }
  
  public Integer getVersion() {
    return version;
  }
  
  public MoneroTx setVersion(Integer version) {
    this.version = version;
    return this;
  }
  
  @JsonProperty("isMinerTx")
  public Boolean isMinerTx() {
    return isMinerTx;
  }
  
  public MoneroTx setIsMinerTx(Boolean isMinerTx) {
    this.isMinerTx = isMinerTx;
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
  
  public Integer getRingSize() {
    return ringSize;
  }
  
  public MoneroTx setRingSize(Integer ringSize) {
    this.ringSize = ringSize;
    return this;
  }
  
  public Boolean getRelay() {
    return relay;
  }
  
  public MoneroTx setRelay(Boolean relay) {
    this.relay = relay;
    return this;
  }
  
  @JsonProperty("isRelayed")
  public Boolean isRelayed() {
    return isRelayed;
  }
  
  public MoneroTx setIsRelayed(Boolean isRelayed) {
    this.isRelayed = isRelayed;
    return this;
  }
  
  @JsonProperty("isConfirmed")
  public Boolean isConfirmed() {
    return isConfirmed;
  }
  
  public MoneroTx setIsConfirmed(Boolean isConfirmed) {
    this.isConfirmed = isConfirmed;
    return this;
  }
  
  @JsonProperty("inTxPool")
  public Boolean inTxPool() {
    return inTxPool;
  }
  
  public MoneroTx setInTxPool(Boolean inTxPool) {
    this.inTxPool = inTxPool;
    return this;
  }
  
  public Long getNumConfirmations() {
    return numConfirmations;
  }
  
  public MoneroTx setNumConfirmations(Long numConfirmations) {
    this.numConfirmations = numConfirmations;
    return this;
  }
  
  public Long getUnlockHeight() {
    return unlockHeight;
  }
  
  public MoneroTx setUnlockHeight(Long unlockHeight) {
    this.unlockHeight = unlockHeight;
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
  
  @JsonProperty("isDoubleSpendSeen")
  public Boolean isDoubleSpendSeen() {
    return isDoubleSpendSeen;
  }
  
  public MoneroTx setIsDoubleSpendSeen(Boolean isDoubleSpend) {
    this.isDoubleSpendSeen = isDoubleSpend;
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
  
  public Long getSize() {
    return size;
  }
  
  public MoneroTx setSize(Long size) {
    this.size = size;
    return this;
  }
  
  public Long getWeight() {
    return weight;
  }
  
  public MoneroTx setWeight(Long weight) {
    this.weight = weight;
    return this;
  }
  
  @JsonManagedReference
  public List<MoneroOutput> getInputs() {
    return inputs;
  }
  
  public MoneroTx setInputs(List<MoneroOutput> inputs) {
    this.inputs = inputs;
    return this;
  }
  
  public List<String> getInputKeyImages() {
    if (inputs == null) return null;
    List<String> keyImages = new ArrayList<String>();
    for (MoneroOutput input : inputs) keyImages.add(input.getKeyImage().getHex());
    return keyImages;
  }
  
  @JsonManagedReference
  public List<MoneroOutput> getOutputs() {
    return outputs;
  }
  
  public MoneroTx setOutputs(List<MoneroOutput> outputs) {
    this.outputs = outputs;
    return this;
  }
  
  public List<Long> getOutputIndices() {
    return outputIndices;
  }
  
  public MoneroTx setOutputIndices(List<Long> outputIndices) {
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
  
  @JsonProperty("isKeptByBlock")
  public Boolean isKeptByBlock() {
    return isKeptByBlock;
  }
  
  public MoneroTx setIsKeptByBlock(Boolean isKeptByBlock) {
    this.isKeptByBlock = isKeptByBlock;
    return this;
  }
  
  @JsonProperty("isFailed")
  public Boolean isFailed() {
    return isFailed;
  }
  
  public MoneroTx setIsFailed(Boolean isFailed) {
    this.isFailed = isFailed;
    return this;
  }
  
  public Long getLastFailedHeight() {
    return lastFailedHeight;
  }
  
  public MoneroTx setLastFailedHeight(Long lastFailedHeight) {
    this.lastFailedHeight = lastFailedHeight;
    return this;
  }
  
  public String getLastFailedHash() {
    return lastFailedHash;
  }
  
  public MoneroTx setLastFailedHash(String lastFailedHash) {
    this.lastFailedHash = lastFailedHash;
    return this;
  }
  
  public Long getMaxUsedBlockHeight() {
    return maxUsedBlockHeight;
  }
  
  public MoneroTx setMaxUsedBlockHeight(Long maxUsedBlockHeight) {
    this.maxUsedBlockHeight = maxUsedBlockHeight;
    return this;
  }
  
  public String getMaxUsedBlockHash() {
    return maxUsedBlockHash;
  }
  
  public MoneroTx setMaxUsedBlockHash(String maxUsedBlockHash) {
    this.maxUsedBlockHash = maxUsedBlockHash;
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
    if (this == tx) return this;
    
    // merge blocks if they're different
    if (this.getBlock() != tx.getBlock()) {
      if (this.getBlock() == null) {
        this.setBlock(tx.getBlock());
        this.getBlock().getTxs().set(this.getBlock().getTxs().indexOf(tx), this); // update block to point to this tx
      } else if (tx.getBlock() != null) {
        this.getBlock().merge(tx.getBlock()); // comes back to merging txs
        return this;
      }
    }
    
    // otherwise merge tx fields
    this.setHash(GenUtils.reconcile(this.getHash(), tx.getHash()));
    this.setVersion(GenUtils.reconcile(this.getVersion(), tx.getVersion()));
    this.setPaymentId(GenUtils.reconcile(this.getPaymentId(), tx.getPaymentId()));
    this.setFee(GenUtils.reconcile(this.getFee(), tx.getFee()));
    this.setRingSize(GenUtils.reconcile(this.getRingSize(), tx.getRingSize()));
    this.setIsConfirmed(GenUtils.reconcile(this.isConfirmed(), tx.isConfirmed(), null, true, null));  // tx can become confirmed
    this.setIsMinerTx(GenUtils.reconcile(this.isMinerTx(), tx.isMinerTx(), null, null, null));
    this.setRelay(GenUtils.reconcile(this.getRelay(), tx.getRelay(), null, true, null));        // tx can become relayed
    this.setIsRelayed(GenUtils.reconcile(this.isRelayed(), tx.isRelayed(), null, true, null));  // tx can become relayed
    this.setIsDoubleSpendSeen(GenUtils.reconcile(this.isDoubleSpendSeen(), tx.isDoubleSpendSeen(), null, true, null)); // double spend can become seen
    this.setKey(GenUtils.reconcile(this.getKey(), tx.getKey()));
    this.setFullHex(GenUtils.reconcile(this.getFullHex(), tx.getFullHex()));
    this.setPrunedHex(GenUtils.reconcile(this.getPrunedHex(), tx.getPrunedHex()));
    this.setPrunableHex(GenUtils.reconcile(this.getPrunableHex(), tx.getPrunableHex()));
    this.setPrunableHash(GenUtils.reconcile(this.getPrunableHash(), tx.getPrunableHash()));
    this.setSize(GenUtils.reconcile(this.getSize(), tx.getSize()));
    this.setWeight(GenUtils.reconcile(this.getWeight(), tx.getWeight()));
    this.setOutputIndices(GenUtils.reconcile(this.getOutputIndices(), tx.getOutputIndices()));
    this.setMetadata(GenUtils.reconcile(this.getMetadata(), tx.getMetadata()));
    this.setExtra(GenUtils.reconcileIntArrays(this.getExtra(), tx.getExtra()));
    this.setRctSignatures(GenUtils.reconcile(this.getRctSignatures(), tx.getRctSignatures()));
    this.setRctSigPrunable(GenUtils.reconcile(this.getRctSigPrunable(), tx.getRctSigPrunable()));
    this.setIsKeptByBlock(GenUtils.reconcile(this.isKeptByBlock(), tx.isKeptByBlock()));
    this.setIsFailed(GenUtils.reconcile(this.isFailed(), tx.isFailed()));
    this.setLastFailedHeight(GenUtils.reconcile(this.getLastFailedHeight(), tx.getLastFailedHeight()));
    this.setLastFailedHash(GenUtils.reconcile(this.getLastFailedHash(), tx.getLastFailedHash()));
    this.setMaxUsedBlockHeight(GenUtils.reconcile(this.getMaxUsedBlockHeight(), tx.getMaxUsedBlockHeight()));
    this.setMaxUsedBlockHash(GenUtils.reconcile(this.getMaxUsedBlockHash(), tx.getMaxUsedBlockHash()));
    this.setSignatures(GenUtils.reconcile(this.getSignatures(), tx.getSignatures()));
    this.setUnlockHeight(GenUtils.reconcile(this.getUnlockHeight(), tx.getUnlockHeight()));
    this.setNumConfirmations(GenUtils.reconcile(this.getNumConfirmations(), tx.getNumConfirmations(), null, null, true)); // num confirmations can increase
    
    // merge inputs
    if (tx.getInputs() != null) {
      for (MoneroOutput merger : tx.getInputs()) {
        boolean merged = false;
        merger.setTx(this);
        if (this.getInputs() == null) this.setInputs(new ArrayList<MoneroOutput>());
        for (MoneroOutput mergee : this.getInputs()) {
          if (mergee.getKeyImage().getHex().equals(merger.getKeyImage().getHex())) {
            mergee.merge(merger);
            merged = true;
            break;
          }
        }
        if (!merged) this.getInputs().add(merger);
      }
    }
    
    // merge outputs
    if (tx.getOutputs() != null) {
      for (MoneroOutput output : tx.getOutputs()) output.setTx(this);
      if (this.getOutputs() == null) this.setOutputs(tx.getOutputs());
      else {
        
        // merge outputs if key image or stealth public key present, otherwise append
        for (MoneroOutput merger : tx.getOutputs()) {
          boolean merged = false;
          merger.setTx(this);
          for (MoneroOutput mergee : this.getOutputs()) {
            if ((merger.getKeyImage() != null && mergee.getKeyImage().getHex().equals(merger.getKeyImage().getHex())) ||
                (merger.getStealthPublicKey() != null && mergee.getStealthPublicKey().equals(merger.getStealthPublicKey()))) {
             mergee.merge(merger);
             merged = true;
             break;
            }
          }
          if (!merged) this.getOutputs().add(merger); // append output
        }
      }
    }
    
    // handle unrelayed -> relayed -> confirmed
    if (this.isConfirmed()) {
      this.setInTxPool(false);
      this.setReceivedTimestamp(null);
      this.setLastRelayedTimestamp(null);
    } else {
      this.setInTxPool(GenUtils.reconcile(this.inTxPool(), tx.inTxPool(), null, true, null)); // unrelayed -> tx pool
      this.setReceivedTimestamp(GenUtils.reconcile(this.getReceivedTimestamp(), tx.getReceivedTimestamp(), null, null, false)); // take earliest receive time
      this.setLastRelayedTimestamp(GenUtils.reconcile(this.getLastRelayedTimestamp(), tx.getLastRelayedTimestamp(), null, null, true));  // take latest relay time
    }
    
    return this;  // for chaining
  }
  
  @Override
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.getIndent(indent) + "=== TX ===\n");
    sb.append(GenUtils.kvLine("Tx hash", getHash(), indent));
    sb.append(GenUtils.kvLine("Height", getHeight(), indent));
    sb.append(GenUtils.kvLine("Version", getVersion(), indent));
    sb.append(GenUtils.kvLine("Is miner tx", isMinerTx(), indent));
    sb.append(GenUtils.kvLine("Payment ID", getPaymentId(), indent));
    sb.append(GenUtils.kvLine("Fee", getFee(), indent));
    sb.append(GenUtils.kvLine("Ring size", getRingSize(), indent));
    sb.append(GenUtils.kvLine("Relay", getRelay(), indent));
    sb.append(GenUtils.kvLine("Is relayed", isRelayed(), indent));
    sb.append(GenUtils.kvLine("Is confirmed", isConfirmed(), indent));
    sb.append(GenUtils.kvLine("In tx pool", inTxPool(), indent));
    sb.append(GenUtils.kvLine("Num confirmations", getNumConfirmations(), indent));
    sb.append(GenUtils.kvLine("Unlock height", getUnlockHeight(), indent));
    sb.append(GenUtils.kvLine("Last relayed time", getLastRelayedTimestamp(), indent));
    sb.append(GenUtils.kvLine("Received time", getReceivedTimestamp(), indent));
    sb.append(GenUtils.kvLine("Is double spend", isDoubleSpendSeen(), indent));
    sb.append(GenUtils.kvLine("Key", getKey(), indent));
    sb.append(GenUtils.kvLine("Full hex", getFullHex(), indent));
    sb.append(GenUtils.kvLine("Pruned hex", getPrunedHex(), indent));
    sb.append(GenUtils.kvLine("Prunable hex", getPrunableHex(), indent));
    sb.append(GenUtils.kvLine("Prunable hash", getPrunableHash(), indent));
    sb.append(GenUtils.kvLine("Size", getSize(), indent));
    sb.append(GenUtils.kvLine("Weight", getWeight(), indent));
    sb.append(GenUtils.kvLine("Output indices", getOutputIndices(), indent));
    sb.append(GenUtils.kvLine("Metadata", getMetadata(), indent));
    sb.append(GenUtils.kvLine("Extra", Arrays.toString(getExtra()), indent));
    sb.append(GenUtils.kvLine("RCT signatures", getRctSignatures(), indent));
    sb.append(GenUtils.kvLine("RCT sig prunable", getRctSigPrunable(), indent));
    sb.append(GenUtils.kvLine("Kept by block", isKeptByBlock(), indent));
    sb.append(GenUtils.kvLine("Is failed", isFailed(), indent));
    sb.append(GenUtils.kvLine("Last failed height", getLastFailedHeight(), indent));
    sb.append(GenUtils.kvLine("Last failed hash", getLastFailedHash(), indent));
    sb.append(GenUtils.kvLine("Max used block height", getMaxUsedBlockHeight(), indent));
    sb.append(GenUtils.kvLine("Max used block hash", getMaxUsedBlockHash(), indent));
    sb.append(GenUtils.kvLine("Signatures", getSignatures(), indent));
    if (getInputs() != null) {
      sb.append(GenUtils.kvLine("Inputs", "", indent));
      for (int i = 0; i < getInputs().size(); i++) {
        sb.append(GenUtils.kvLine(i + 1, "", indent + 1));
        sb.append(getInputs().get(i).toString(indent + 2));
        sb.append('\n');
      }
    }
    if (getOutputs() != null) {
      sb.append(GenUtils.kvLine("Outputs", "", indent));
      for (int i = 0; i < getOutputs().size(); i++) {
        sb.append(GenUtils.kvLine(i + 1, "", indent + 1));
        sb.append(getOutputs().get(i).toString(indent + 2));
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
    result = prime * result + ((relay == null) ? 0 : relay.hashCode());
    result = prime * result + Arrays.hashCode(extra);
    result = prime * result + ((fee == null) ? 0 : fee.hashCode());
    result = prime * result + ((fullHex == null) ? 0 : fullHex.hashCode());
    result = prime * result + ((hash == null) ? 0 : hash.hashCode());
    result = prime * result + ((inTxPool == null) ? 0 : inTxPool.hashCode());
    result = prime * result + ((isMinerTx == null) ? 0 : isMinerTx.hashCode());
    result = prime * result + ((isConfirmed == null) ? 0 : isConfirmed.hashCode());
    result = prime * result + ((isDoubleSpendSeen == null) ? 0 : isDoubleSpendSeen.hashCode());
    result = prime * result + ((isFailed == null) ? 0 : isFailed.hashCode());
    result = prime * result + ((isKeptByBlock == null) ? 0 : isKeptByBlock.hashCode());
    result = prime * result + ((isRelayed == null) ? 0 : isRelayed.hashCode());
    result = prime * result + ((key == null) ? 0 : key.hashCode());
    result = prime * result + ((lastFailedHeight == null) ? 0 : lastFailedHeight.hashCode());
    result = prime * result + ((lastFailedHash == null) ? 0 : lastFailedHash.hashCode());
    result = prime * result + ((lastRelayedTimestamp == null) ? 0 : lastRelayedTimestamp.hashCode());
    result = prime * result + ((maxUsedBlockHeight == null) ? 0 : maxUsedBlockHeight.hashCode());
    result = prime * result + ((maxUsedBlockHash == null) ? 0 : maxUsedBlockHash.hashCode());
    result = prime * result + ((metadata == null) ? 0 : metadata.hashCode());
    result = prime * result + ((ringSize == null) ? 0 : ringSize.hashCode());
    result = prime * result + ((numConfirmations == null) ? 0 : numConfirmations.hashCode());
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
    result = prime * result + ((unlockHeight == null) ? 0 : unlockHeight.hashCode());
    result = prime * result + ((version == null) ? 0 : version.hashCode());
    result = prime * result + ((inputs == null) ? 0 : inputs.hashCode());
    result = prime * result + ((outputs == null) ? 0 : outputs.hashCode());
    result = prime * result + ((weight == null) ? 0 : weight.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTx other = (MoneroTx) obj;
    if (relay == null) {
      if (other.relay != null) return false;
    } else if (!relay.equals(other.relay)) return false;
    if (!Arrays.equals(extra, other.extra)) return false;
    if (fee == null) {
      if (other.fee != null) return false;
    } else if (!fee.equals(other.fee)) return false;
    if (fullHex == null) {
      if (other.fullHex != null) return false;
    } else if (!fullHex.equals(other.fullHex)) return false;
    if (hash == null) {
      if (other.hash != null) return false;
    } else if (!hash.equals(other.hash)) return false;
    if (inTxPool == null) {
      if (other.inTxPool != null) return false;
    } else if (!inTxPool.equals(other.inTxPool)) return false;
    if (isMinerTx == null) {
      if (other.isMinerTx != null) return false;
    } else if (!isMinerTx.equals(other.isMinerTx)) return false;
    if (isConfirmed == null) {
      if (other.isConfirmed != null) return false;
    } else if (!isConfirmed.equals(other.isConfirmed)) return false;
    if (isDoubleSpendSeen == null) {
      if (other.isDoubleSpendSeen != null) return false;
    } else if (!isDoubleSpendSeen.equals(other.isDoubleSpendSeen)) return false;
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
    if (lastFailedHash == null) {
      if (other.lastFailedHash != null) return false;
    } else if (!lastFailedHash.equals(other.lastFailedHash)) return false;
    if (lastRelayedTimestamp == null) {
      if (other.lastRelayedTimestamp != null) return false;
    } else if (!lastRelayedTimestamp.equals(other.lastRelayedTimestamp)) return false;
    if (maxUsedBlockHeight == null) {
      if (other.maxUsedBlockHeight != null) return false;
    } else if (!maxUsedBlockHeight.equals(other.maxUsedBlockHeight)) return false;
    if (maxUsedBlockHash == null) {
      if (other.maxUsedBlockHash != null) return false;
    } else if (!maxUsedBlockHash.equals(other.maxUsedBlockHash)) return false;
    if (metadata == null) {
      if (other.metadata != null) return false;
    } else if (!metadata.equals(other.metadata)) return false;
    if (ringSize == null) {
      if (other.ringSize != null) return false;
    } else if (!ringSize.equals(other.ringSize)) return false;
    if (numConfirmations == null) {
      if (other.numConfirmations != null) return false;
    } else if (!numConfirmations.equals(other.numConfirmations)) return false;
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
    if (unlockHeight == null) {
      if (other.unlockHeight != null) return false;
    } else if (!unlockHeight.equals(other.unlockHeight)) return false;
    if (version == null) {
      if (other.version != null) return false;
    } else if (!version.equals(other.version)) return false;
    if (inputs == null) {
      if (other.inputs != null) return false;
    } else if (!inputs.equals(other.inputs)) return false;
    if (outputs == null) {
      if (other.outputs != null) return false;
    } else if (!outputs.equals(other.outputs)) return false;
    if (weight == null) {
      if (other.weight != null) return false;
    } else if (!weight.equals(other.weight)) return false;
    return true;
  }
}
