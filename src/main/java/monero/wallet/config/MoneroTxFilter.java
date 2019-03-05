package monero.wallet.config;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import common.types.Filter;
import common.utils.JsonUtils;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroOutput;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;

/**
 * Filters transactions that don't meet initialized filter criteria.
 */
public class MoneroTxFilter extends MoneroTxWallet implements Filter<MoneroTxWallet> {
  
  private Boolean isOutgoing;
  private Boolean isIncoming;
  private List<String> txIds;
  private Boolean hasPaymentId;
  private List<String> paymentIds;
  private Integer minHeight;
  private Integer maxHeight;
  private Boolean includeVouts;
  private MoneroTransferFilter transferFilter;
  
  public Boolean getIsOutgoing() {
    return isOutgoing;
  }

  public MoneroTxFilter setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }

  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public MoneroTxFilter setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }

  public List<String> getTxIds() {
    return txIds;
  }

  public MoneroTxFilter setTxIds(List<String> txIds) {
    this.txIds = txIds;
    return this;
  }
  
  public MoneroTxFilter setTxId(String txId) {
    return setTxIds(Arrays.asList(txId));
  }

  public Boolean getHasPaymentId() {
    return hasPaymentId;
  }

  public MoneroTxFilter setHasPaymentId(Boolean hasPaymentId) {
    this.hasPaymentId = hasPaymentId;
    return this;
  }

  public List<String> getPaymentIds() {
    return paymentIds;
  }

  public MoneroTxFilter setPaymentIds(List<String> paymentIds) {
    this.paymentIds = paymentIds;
    return this;
  }
  
  public MoneroTxFilter setPaymentId(String paymentId) {
    return setPaymentIds(Arrays.asList(paymentId));
  }

  public Integer getMinHeight() {
    return minHeight;
  }

  public MoneroTxFilter setMinHeight(Integer minHeight) {
    this.minHeight = minHeight;
    return this;
  }

  public Integer getMaxHeight() {
    return maxHeight;
  }

  public MoneroTxFilter setMaxHeight(Integer maxHeight) {
    this.maxHeight = maxHeight;
    return this;
  }

  public Boolean getIncludeVouts() {
    return includeVouts;
  }

  public MoneroTxFilter setIncludeVouts(Boolean includeVouts) {
    this.includeVouts = includeVouts;
    return this;
  }

  public MoneroTransferFilter getTransferFilter() {
    return transferFilter;
  }

  public MoneroTxFilter setTransferFilter(MoneroTransferFilter transferFilter) {
    this.transferFilter = transferFilter;
    return this;
  }
  
  @Override
  public boolean meetsCriteria(MoneroTxWallet item) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public String toString() {
    return JsonUtils.serialize(this);
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTxFilter setIncomingAmount(BigInteger incomingAmount) {
    super.setIncomingAmount(incomingAmount);
    return this;
  }

  @Override
  public MoneroTxFilter setOutgoingAmount(BigInteger outgoingAmount) {
    super.setOutgoingAmount(outgoingAmount);
    return this;
  }

  @Override
  public MoneroTxFilter setIncomingTransfers(List<MoneroTransfer> incomingTransfers) {
    super.setIncomingTransfers(incomingTransfers);
    return this;
  }

  @Override
  public MoneroTxFilter setOutgoingTransfer(MoneroTransfer outgoingTransfer) {
    super.setOutgoingTransfer(outgoingTransfer);
    return this;
  }

  @Override
  public MoneroTxFilter setVouts(List<? extends MoneroOutput> vouts) {
    super.setVouts(vouts);
    return this;
  }

  @Override
  public MoneroTxFilter setNote(String note) {
    super.setNote(note);
    return this;
  }

  @Override
  public MoneroTxFilter setBlock(MoneroBlock block) {
    super.setBlock(block);
    return this;
  }

  @Override
  public MoneroTxFilter setHeight(Integer height) {
    super.setHeight(height);
    return this;
  }

  @Override
  public MoneroTxFilter setId(String id) {
    super.setId(id);
    return this;
  }

  @Override
  public MoneroTxFilter setVersion(Integer version) {
    super.setVersion(version);
    return this;
  }

  @Override
  public MoneroTxFilter setIsCoinbase(Boolean isCoinbase) {
    super.setIsCoinbase(isCoinbase);
    return this;
  }

  @Override
  public MoneroTxFilter setFee(BigInteger fee) {
    super.setFee(fee);
    return this;
  }

  @Override
  public MoneroTxFilter setMixin(Integer mixin) {
    super.setMixin(mixin);
    return this;
  }

  @Override
  public MoneroTxFilter setDoNotRelay(Boolean doNotRelay) {
    super.setDoNotRelay(doNotRelay);
    return this;
  }

  @Override
  public MoneroTxFilter setIsRelayed(Boolean isRelayed) {
    super.setIsRelayed(isRelayed);
    return this;
  }

  @Override
  public MoneroTxFilter setIsConfirmed(Boolean isConfirmed) {
    super.setIsConfirmed(isConfirmed);
    return this;
  }

  @Override
  public MoneroTxFilter setInTxPool(Boolean inTxPool) {
    super.setInTxPool(inTxPool);
    return this;
  }

  @Override
  public MoneroTxFilter setNumConfirmations(Integer numConfirmations) {
    super.setNumConfirmations(numConfirmations);
    return this;
  }

  @Override
  public MoneroTxFilter setNumEstimatedBlocksUntilConfirmed(Integer numEstimatedBlocksUntilConfirmed) {
    super.setNumEstimatedBlocksUntilConfirmed(numEstimatedBlocksUntilConfirmed);
    return this;
  }

  @Override
  public MoneroTxFilter setUnlockTime(Integer unlockTime) {
    super.setUnlockTime(unlockTime);
    return this;
  }

  @Override
  public MoneroTxFilter setLastRelayedTimestamp(Long lastRelayedTimestamp) {
    super.setLastRelayedTimestamp(lastRelayedTimestamp);
    return this;
  }

  @Override
  public MoneroTxFilter setReceivedTimestamp(Long receivedTimestamp) {
    super.setReceivedTimestamp(receivedTimestamp);
    return this;
  }

  @Override
  public MoneroTxFilter setIsDoubleSpend(Boolean isDoubleSpend) {
    super.setIsDoubleSpend(isDoubleSpend);
    return this;
  }

  @Override
  public MoneroTxFilter setKey(String key) {
    super.setKey(key);
    return this;
  }

  @Override
  public MoneroTxFilter setHex(String hex) {
    super.setHex(hex);
    return this;
  }

  @Override
  public MoneroTxFilter setPrunedHex(String prunedHex) {
    super.setPrunedHex(prunedHex);
    return this;
  }

  @Override
  public MoneroTxFilter setPrunableHex(String prunableHex) {
    super.setPrunableHex(prunableHex);
    return this;
  }

  @Override
  public MoneroTxFilter setPrunableHash(String prunableHash) {
    super.setPrunableHash(prunableHash);
    return this;
  }

  @Override
  public MoneroTxFilter setSize(Integer size) {
    super.setSize(size);
    return this;
  }

  @Override
  public MoneroTxFilter setWeight(Integer weight) {
    super.setWeight(weight);
    return this;
  }

  @Override
  public MoneroTxFilter setVins(List<MoneroOutput> vins) {
    super.setVins(vins);
    return this;
  }

  @Override
  public MoneroTxFilter setOutputIndices(List<Integer> outputIndices) {
    super.setOutputIndices(outputIndices);
    return this;
  }

  @Override
  public MoneroTxFilter setMetadata(String metadata) {
    super.setMetadata(metadata);
    return this;
  }

  @Override
  public MoneroTxFilter setCommonTxSets(String commonTxSets) {
    super.setCommonTxSets(commonTxSets);
    return this;
  }

  @Override
  public MoneroTxFilter setExtra(int[] extra) {
    super.setExtra(extra);
    return this;
  }

  @Override
  public MoneroTxFilter setRctSignatures(List<String> rctSignatures) {
    super.setRctSignatures(rctSignatures);
    return this;
  }

  @Override
  public MoneroTxFilter setRctSigPrunable(Object rctSigPrunable) {
    super.setRctSigPrunable(rctSigPrunable);
    return this;
  }

  @Override
  public MoneroTxFilter setIsKeptByBlock(Boolean isKeptByBlock) {
    super.setIsKeptByBlock(isKeptByBlock);
    return this;
  }

  @Override
  public MoneroTxFilter setIsFailed(Boolean isFailed) {
    super.setIsFailed(isFailed);
    return this;
  }

  @Override
  public MoneroTxFilter setLastFailedHeight(Integer lastFailedHeight) {
    super.setLastFailedHeight(lastFailedHeight);
    return this;
  }

  @Override
  public MoneroTxFilter setLastFailedId(String lastFailedId) {
    super.setLastFailedId(lastFailedId);
    return this;
  }

  @Override
  public MoneroTxFilter setMaxUsedBlockHeight(Integer maxUsedBlockHeight) {
    super.setMaxUsedBlockHeight(maxUsedBlockHeight);
    return this;
  }

  @Override
  public MoneroTxFilter setMaxUsedBlockId(Integer maxUsedBlockId) {
    super.setMaxUsedBlockId(maxUsedBlockId);
    return this;
  }

  @Override
  public MoneroTxFilter setSignatures(List<String> signatures) {
    super.setSignatures(signatures);
    return this;
  }
}
