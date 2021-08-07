package monero.wallet.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import common.types.Filter;
import common.utils.GenUtils;
import monero.common.MoneroError;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroOutput;

/**
 * Configures a query to retrieve transactions.
 * 
 * All transactions are returned except those that do not meet the criteria defined in this query.
 */
public class MoneroTxQuery extends MoneroTxWallet implements Filter<MoneroTxWallet> {
  
  private Boolean isOutgoing;
  private Boolean isIncoming;
  private List<String> hashes;
  private Boolean hasPaymentId;
  private List<String> paymentIds;
  private Long height;
  private Long minHeight;
  private Long maxHeight;
  private Boolean includeOutputs;
  protected MoneroTransferQuery transferQuery;
  protected MoneroOutputQuery inputQuery;
  protected MoneroOutputQuery outputQuery;
  
  public MoneroTxQuery() {
    
  }
  
  public MoneroTxQuery(final MoneroTxQuery query) {
    super(query);
    this.isOutgoing = query.isOutgoing;
    this.isIncoming = query.isIncoming;
    if (query.hashes != null) this.hashes = new ArrayList<String>(query.hashes);
    this.hasPaymentId = query.hasPaymentId;
    if (query.paymentIds != null) this.paymentIds = new ArrayList<String>(query.paymentIds);
    this.height = query.height;
    this.minHeight = query.minHeight;
    this.maxHeight = query.maxHeight;
    this.includeOutputs = query.includeOutputs;
    if (query.transferQuery != null) this.setTransferQuery(new MoneroTransferQuery(query.transferQuery));
    if (query.inputQuery != null) this.setInputQuery(new MoneroOutputQuery(query.inputQuery));
    if (query.outputQuery != null) this.setOutputQuery(new MoneroOutputQuery(query.outputQuery));
  }
  
  @Override
  public MoneroTxQuery copy() {
    return new MoneroTxQuery(this);
  }
  
  @Override
  @JsonProperty("isOutgoing")
  public Boolean isOutgoing() {
    return isOutgoing;
  }

  @Override
  public MoneroTxQuery setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }

  @Override
  @JsonProperty("isIncoming")
  public Boolean isIncoming() {
    return isIncoming;
  }

  @Override
  public MoneroTxQuery setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  @Override
  public MoneroTxQuery setHash(String hash) {
    super.setHash(hash);
    return setHashes(Arrays.asList(hash));
  }

  public List<String> getHashes() {
    return hashes;
  }

  public MoneroTxQuery setHashes(List<String> hashes) {
    this.hashes = hashes;
    return this;
  }
  
  public MoneroTxQuery setHashes(String... hashes) {
    this.hashes = GenUtils.arrayToList(hashes);
    return this;
  }
  
  @JsonProperty("hasPaymentId")
  public Boolean hasPaymentId() {
    return hasPaymentId;
  }

  public MoneroTxQuery setHasPaymentId(Boolean hasPaymentId) {
    this.hasPaymentId = hasPaymentId;
    return this;
  }

  public List<String> getPaymentIds() {
    return paymentIds;
  }

  public MoneroTxQuery setPaymentIds(List<String> paymentIds) {
    this.paymentIds = paymentIds;
    return this;
  }
  
  @Override
  public MoneroTxQuery setPaymentId(String paymentId) {
    return setPaymentIds(Arrays.asList(paymentId));
  }
  
  @Override
  public Long getHeight() {
    return height;
  }

  public MoneroTxQuery setHeight(Long height) {
    this.height = height;
    return this;
  }

  public Long getMinHeight() {
    return minHeight;
  }

  public MoneroTxQuery setMinHeight(Long minHeight) {
    this.minHeight = minHeight;
    return this;
  }

  public Long getMaxHeight() {
    return maxHeight;
  }

  public MoneroTxQuery setMaxHeight(Long maxHeight) {
    this.maxHeight = maxHeight;
    return this;
  }

  public Boolean getIncludeOutputs() {
    return includeOutputs;
  }

  public MoneroTxQuery setIncludeOutputs(Boolean includeOutputs) {
    this.includeOutputs = includeOutputs;
    return this;
  }

  public MoneroTransferQuery getTransferQuery() {
    return transferQuery;
  }

  public MoneroTxQuery setTransferQuery(MoneroTransferQuery transferQuery) {
    this.transferQuery = transferQuery;
    if (transferQuery != null) transferQuery.txQuery = this;
    return this;
  }
  
  public MoneroOutputQuery getInputQuery() {
    return inputQuery;
  }
  
  public MoneroTxQuery setInputQuery(MoneroOutputQuery inputQuery) {
    this.inputQuery = inputQuery;
    if (inputQuery != null) inputQuery.txQuery = this;
    return this;
  }
  
  public MoneroOutputQuery getOutputQuery() {
    return outputQuery;
  }

  public MoneroTxQuery setOutputQuery(MoneroOutputQuery outputQuery) {
    this.outputQuery = outputQuery;
    if (outputQuery != null) outputQuery.txQuery = this;
    return this;
  }
  
  @Override
  public boolean meetsCriteria(MoneroTxWallet tx) {
    return meetsCriteria(tx, true);
  }
  
  protected boolean meetsCriteria(MoneroTxWallet tx, boolean queryChildren) {
    if (tx == null) throw new MoneroError("No tx given to MoneroTxQuery.meetsCriteria()");
    
    // filter on tx
    if (this.getHash() != null && !this.getHash().equals(tx.getHash())) return false;
    if (this.getPaymentId() != null && !this.getPaymentId().equals(tx.getPaymentId())) return false;
    if (this.isConfirmed() != null && this.isConfirmed() != tx.isConfirmed()) return false;
    if (this.inTxPool() != null && this.inTxPool() != tx.inTxPool()) return false;
    if (this.getRelay() != null && this.getRelay() != tx.getRelay()) return false;
    if (this.isRelayed() != null && this.isRelayed() != tx.isRelayed()) return false;
    if (this.isFailed() != null && this.isFailed() != tx.isFailed()) return false;
    if (this.isMinerTx() != null && this.isMinerTx() != tx.isMinerTx()) return false;
    if (this.isLocked() != null && this.isLocked() != tx.isLocked()) return false;
    
    // filter on having a payment id
    if (this.hasPaymentId() != null) {
      if (this.hasPaymentId() && tx.getPaymentId() == null) return false;
      if (!this.hasPaymentId() && tx.getPaymentId() != null) return false;
    }
    
    // filter on incoming
    if (this.isIncoming() != null && this.isIncoming() != Boolean.TRUE.equals(tx.isIncoming())) return false;
    
    // filter on outgoing
    if (this.isOutgoing() != null && this.isOutgoing() != Boolean.TRUE.equals(tx.isOutgoing())) return false;
    
    // filter on remaining fields
    Long txHeight = tx.getBlock() == null ? null : tx.getBlock().getHeight();
    if (this.getHashes() != null && !this.getHashes().contains(tx.getHash())) return false;
    if (this.getPaymentIds() != null && !this.getPaymentIds().contains(tx.getPaymentId())) return false;
    if (this.getHeight() != null && !this.getHeight().equals(txHeight)) return false;
    if (this.getMinHeight() != null && (txHeight == null || txHeight < this.getMinHeight())) return false;
    if (this.getMaxHeight() != null && (txHeight == null || txHeight > this.getMaxHeight())) return false;
    
    // done if not querying transfers or outputs
    if (!queryChildren) return true;
    
    // at least one transfer must meet transfer query if defined
    if (this.getTransferQuery() != null) {
      boolean matchFound = false;
      if (tx.getOutgoingTransfer() != null && this.getTransferQuery().meetsCriteria(tx.getOutgoingTransfer(), false)) matchFound = true;
      else if (tx.getIncomingTransfers() != null) {
        for (MoneroTransfer incomingTransfer : tx.getIncomingTransfers()) {
          if (this.getTransferQuery().meetsCriteria(incomingTransfer, false)) {
            matchFound = true;
            break;
          }
        }
      }
      if (!matchFound) return false;
    }
    
    // at least one input must meet input query if defined
    if (this.getInputQuery() != null) {
      if (tx.getInputs() == null || tx.getInputs().isEmpty()) return false;
      boolean matchFound = false;
      for (MoneroOutputWallet input : tx.getInputsWallet()) {
        if (this.getInputQuery().meetsCriteria(input, false)) {
          matchFound = true;
          break;
        }
      }
      if (!matchFound) return false;
    }
    
    // at least one output must meet output query if defined
    if (this.getOutputQuery() != null) {
      if (tx.getOutputs() == null || tx.getOutputs().isEmpty()) return false;
      boolean matchFound = false;
      for (MoneroOutputWallet output : tx.getOutputsWallet()) {
        if (this.getOutputQuery().meetsCriteria(output, false)) {
          matchFound = true;
          break;
        }
      }
      if (!matchFound) return false;
    }
    
    return true;  // transaction meets query criteria
  }
  
  @Override
  public String toString() {
    throw new RuntimeException("MoneroTxQuery.toString() not implemented");
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTxQuery setIncomingTransfers(List<MoneroIncomingTransfer> incomingTransfers) {
    super.setIncomingTransfers(incomingTransfers);
    return this;
  }

  @Override
  public MoneroTxQuery setOutgoingTransfer(MoneroOutgoingTransfer outgoingTransfer) {
    super.setOutgoingTransfer(outgoingTransfer);
    return this;
  }

  @Override
  public MoneroTxQuery setOutputs(List<MoneroOutput> outputs) {
    super.setOutputs(outputs);
    return this;
  }

  @Override
  public MoneroTxQuery setNote(String note) {
    super.setNote(note);
    return this;
  }
  
  @Override
  public MoneroTxQuery setIsLocked(Boolean isLocked) {
    super.setIsLocked(isLocked);
    return this;
  }

  @Override
  public MoneroTxQuery setBlock(MoneroBlock block) {
    super.setBlock(block);
    return this;
  }
  
  @Override
  public MoneroTxQuery setVersion(Integer version) {
    super.setVersion(version);
    return this;
  }

  @Override
  public MoneroTxQuery setIsMinerTx(Boolean isMinerTx) {
    super.setIsMinerTx(isMinerTx);
    return this;
  }

  @Override
  public MoneroTxQuery setFee(BigInteger fee) {
    super.setFee(fee);
    return this;
  }

  @Override
  public MoneroTxQuery setRingSize(Integer ringSize) {
    super.setRingSize(ringSize);
    return this;
  }

  @Override
  public MoneroTxQuery setRelay(Boolean relay) {
    super.setRelay(relay);
    return this;
  }

  @Override
  public MoneroTxQuery setIsRelayed(Boolean isRelayed) {
    super.setIsRelayed(isRelayed);
    return this;
  }

  @Override
  public MoneroTxQuery setIsConfirmed(Boolean isConfirmed) {
    super.setIsConfirmed(isConfirmed);
    return this;
  }

  @Override
  public MoneroTxQuery setInTxPool(Boolean inTxPool) {
    super.setInTxPool(inTxPool);
    return this;
  }

  @Override
  public MoneroTxQuery setNumConfirmations(Long numConfirmations) {
    super.setNumConfirmations(numConfirmations);
    return this;
  }

  @Override
  public MoneroTxQuery setUnlockHeight(Long unlockHeight) {
    super.setUnlockHeight(unlockHeight);
    return this;
  }

  @Override
  public MoneroTxQuery setLastRelayedTimestamp(Long lastRelayedTimestamp) {
    super.setLastRelayedTimestamp(lastRelayedTimestamp);
    return this;
  }

  @Override
  public MoneroTxQuery setReceivedTimestamp(Long receivedTimestamp) {
    super.setReceivedTimestamp(receivedTimestamp);
    return this;
  }

  @Override
  public MoneroTxQuery setIsDoubleSpendSeen(Boolean isDoubleSpend) {
    super.setIsDoubleSpendSeen(isDoubleSpend);
    return this;
  }

  @Override
  public MoneroTxQuery setKey(String key) {
    super.setKey(key);
    return this;
  }

  @Override
  public MoneroTxQuery setFullHex(String hex) {
    super.setFullHex(hex);
    return this;
  }

  @Override
  public MoneroTxQuery setPrunedHex(String prunedHex) {
    super.setPrunedHex(prunedHex);
    return this;
  }

  @Override
  public MoneroTxQuery setPrunableHex(String prunableHex) {
    super.setPrunableHex(prunableHex);
    return this;
  }

  @Override
  public MoneroTxQuery setPrunableHash(String prunableHash) {
    super.setPrunableHash(prunableHash);
    return this;
  }

  @Override
  public MoneroTxQuery setSize(Long size) {
    super.setSize(size);
    return this;
  }

  @Override
  public MoneroTxQuery setWeight(Long weight) {
    super.setWeight(weight);
    return this;
  }

  @Override
  public MoneroTxQuery setInputs(List<MoneroOutput> inputs) {
    super.setInputs(inputs);
    return this;
  }

  @Override
  public MoneroTxQuery setOutputIndices(List<Long> outputIndices) {
    super.setOutputIndices(outputIndices);
    return this;
  }

  @Override
  public MoneroTxQuery setMetadata(String metadata) {
    super.setMetadata(metadata);
    return this;
  }

  @Override
  public MoneroTxQuery setTxSet(MoneroTxSet commonTxSets) {
    super.setTxSet(commonTxSets);
    return this;
  }

  @Override
  public MoneroTxQuery setExtra(int[] extra) {
    super.setExtra(extra);
    return this;
  }

  @Override
  public MoneroTxQuery setRctSignatures(Object rctSignatures) {
    super.setRctSignatures(rctSignatures);
    return this;
  }

  @Override
  public MoneroTxQuery setRctSigPrunable(Object rctSigPrunable) {
    super.setRctSigPrunable(rctSigPrunable);
    return this;
  }

  @Override
  public MoneroTxQuery setIsKeptByBlock(Boolean isKeptByBlock) {
    super.setIsKeptByBlock(isKeptByBlock);
    return this;
  }

  @Override
  public MoneroTxQuery setIsFailed(Boolean isFailed) {
    super.setIsFailed(isFailed);
    return this;
  }
  
  @Override
  public MoneroTxQuery setLastFailedHeight(Long lastFailedHeight) {
    super.setLastFailedHeight(lastFailedHeight);
    return this;
  }

  @Override
  public MoneroTxQuery setLastFailedHash(String lastFailedId) {
    super.setLastFailedHash(lastFailedId);
    return this;
  }

  @Override
  public MoneroTxQuery setMaxUsedBlockHeight(Long maxUsedBlockHeight) {
    super.setMaxUsedBlockHeight(maxUsedBlockHeight);
    return this;
  }

  @Override
  public MoneroTxQuery setMaxUsedBlockHash(String maxUsedBlockId) {
    super.setMaxUsedBlockHash(maxUsedBlockId);
    return this;
  }

  @Override
  public MoneroTxQuery setSignatures(List<String> signatures) {
    super.setSignatures(signatures);
    return this;
  }
}
