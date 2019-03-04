package monero.wallet.model;

import java.math.BigInteger;
import java.util.List;

import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;

/**
 * Models a Monero transaction with wallet extensions.
 */
public class MoneroWalletTx extends MoneroTx {

  private BigInteger incomingAmount;
  private BigInteger outgoingAmount;
  private List<MoneroTransfer> incomingTransfers;
  private MoneroTransfer outgoingTransfer;
  private String note;
  
  // ----------------------------- WALLET-SPECIFIC ----------------------------
  
  public boolean getIsOutgoing() {
    return getOutgoingTransfer() != null;
  }
  
  public boolean getIsIncoming() {
    return getIncomingTransfers() != null;
  }
  
  public BigInteger getIncomingAmount() {
    return incomingAmount;
  }
  
  public MoneroWalletTx setIncomingAmount(BigInteger incomingAmount) {
    this.incomingAmount = incomingAmount;
    return this;
  }
  
  public BigInteger getOutgoingAmount() {
    return outgoingAmount;
  }
  
  public MoneroWalletTx setOutgoingAmount(BigInteger outgoingAmount) {
    this.outgoingAmount = outgoingAmount;
    return this;
  }
  
  public List<MoneroTransfer> getIncomingTransfers() {
    return incomingTransfers;
  }
  
  public MoneroWalletTx setIncomingTransfers(List<MoneroTransfer> incomingTransfers) {
    this.incomingTransfers = incomingTransfers;
    return this;
  }
  
  public MoneroTransfer getOutgoingTransfer() {
    return outgoingTransfer;
  }
  
  public MoneroWalletTx setOutgoingTransfer(MoneroTransfer outgoingTransfer) {
    this.outgoingTransfer = outgoingTransfer;
    return this;
  }
  
  @SuppressWarnings("unchecked")
  public List<MoneroWalletOutput> getVouts() {
    return (List<MoneroWalletOutput>) super.getVouts();
  }
  
  public MoneroWalletTx setVouts(List<? extends MoneroOutput> vouts) {
    
    // validate that all vouts are wallet outputs
    if (vouts != null) {
      for (MoneroOutput vout : vouts) {
        if (!(vout instanceof MoneroWalletOutput)) throw new MoneroException("Wallet transaction vouts must be of type MoneroWalletOutput");
      }
    }
    super.setVouts(vouts);
    return this;
  }
  
  public String getNote() {
    return note;
  }
  
  public MoneroWalletTx setNote(String note) {
    this.note = note;
    return this;
  }
  
  public MoneroWalletTx copy() {
    throw new RuntimeException("Not implemented");
  }
  
  public MoneroWalletTx merge(MoneroTx tx) {
    if (tx != null && !(tx instanceof MoneroWalletTx)) throw new MoneroException("Wallet transaction must be merged with type MoneroWalletTx");
    return merge((MoneroWalletTx) tx);
  }
  
  public MoneroWalletTx merge(MoneroWalletTx tx) {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroWalletTx setBlock(MoneroBlock block) {
    super.setBlock(block);
    return this;
  }

  @Override
  public MoneroWalletTx setHeight(Integer height) {
    super.setHeight(height);
    return this;
  }

  @Override
  public MoneroWalletTx setId(String id) {
    super.setId(id);
    return this;
  }

  @Override
  public MoneroWalletTx setVersion(Integer version) {
    super.setVersion(version);
    return this;
  }

  @Override
  public MoneroWalletTx setIsCoinbase(Boolean isCoinbase) {
    super.setIsCoinbase(isCoinbase);
    return this;
  }

  @Override
  public MoneroWalletTx setPaymentId(String paymentId) {
    super.setPaymentId(paymentId);
    return this;
  }

  @Override
  public MoneroWalletTx setFee(BigInteger fee) {
    super.setFee(fee);
    return this;
  }

  @Override
  public MoneroWalletTx setMixin(Integer mixin) {
    super.setMixin(mixin);
    return this;
  }

  @Override
  public MoneroWalletTx setDoNotRelay(Boolean doNotRelay) {
    super.setDoNotRelay(doNotRelay);
    return this;
  }

  @Override
  public MoneroWalletTx setIsRelayed(Boolean isRelayed) {
    super.setIsRelayed(isRelayed);
    return this;
  }

  @Override
  public MoneroWalletTx setIsConfirmed(Boolean isConfirmed) {
    super.setIsConfirmed(isConfirmed);
    return this;
  }

  @Override
  public MoneroWalletTx setInTxPool(Boolean inTxPool) {
    super.setInTxPool(inTxPool);
    return this;
  }

  @Override
  public MoneroWalletTx setNumConfirmations(Integer numConfirmations) {
    super.setNumConfirmations(numConfirmations);
    return this;
  }

  @Override
  public MoneroWalletTx setNumEstimatedBlocksUntilConfirmed(Integer numEstimatedBlocksUntilConfirmed) {
    super.setNumEstimatedBlocksUntilConfirmed(numEstimatedBlocksUntilConfirmed);
    return this;
  }

  @Override
  public MoneroWalletTx setUnlockTime(Integer unlockTime) {
    super.setUnlockTime(unlockTime);
    return this;
  }

  @Override
  public MoneroWalletTx setLastRelayedTimestamp(Long lastRelayedTimestamp) {
    super.setLastRelayedTimestamp(lastRelayedTimestamp);
    return this;
  }

  @Override
  public MoneroWalletTx setReceivedTimestamp(Long receivedTimestamp) {
    super.setReceivedTimestamp(receivedTimestamp);
    return this;
  }

  @Override
  public MoneroWalletTx setIsDoubleSpend(Boolean isDoubleSpend) {
    super.setIsDoubleSpend(isDoubleSpend);
    return this;
  }

  @Override
  public MoneroWalletTx setKey(String key) {
    super.setKey(key);
    return this;
  }

  @Override
  public MoneroWalletTx setHex(String hex) {
    super.setHex(hex);
    return this;
  }

  @Override
  public MoneroWalletTx setPrunedHex(String prunedHex) {
    super.setPrunedHex(prunedHex);
    return this;
  }

  @Override
  public MoneroWalletTx setPrunableHex(String prunableHex) {
    super.setPrunableHex(prunableHex);
    return this;
  }

  @Override
  public MoneroWalletTx setPrunableHash(String prunableHash) {
    super.setPrunableHash(prunableHash);
    return this;
  }

  @Override
  public MoneroWalletTx setSize(Integer size) {
    super.setSize(size);
    return this;
  }

  @Override
  public MoneroWalletTx setWeight(Integer weight) {
    super.setWeight(weight);
    return this;
  }

  @Override
  public MoneroWalletTx setVins(List<MoneroOutput> vins) {
    super.setVins(vins);
    return this;
  }

  @Override
  public MoneroWalletTx setOutputIndices(List<Integer> outputIndices) {
    super.setOutputIndices(outputIndices);
    return this;
  }

  @Override
  public MoneroWalletTx setMetadata(String metadata) {
    super.setMetadata(metadata);
    return this;
  }

  @Override
  public MoneroWalletTx setCommonTxSets(String commonTxSets) {
    super.setCommonTxSets(commonTxSets);
    return this;
  }

  @Override
  public MoneroWalletTx setExtra(int[] extra) {
    super.setExtra(extra);
    return this;
  }

  @Override
  public MoneroWalletTx setRctSignatures(List<String> rctSignatures) {
    super.setRctSignatures(rctSignatures);
    return this;
  }

  @Override
  public MoneroWalletTx setRctSigPrunable(Object rctSigPrunable) {
    super.setRctSigPrunable(rctSigPrunable);
    return this;
  }

  @Override
  public MoneroWalletTx setIsKeptByBlock(Boolean isKeptByBlock) {
    super.setIsKeptByBlock(isKeptByBlock);
    return this;
  }

  @Override
  public MoneroWalletTx setIsFailed(Boolean isFailed) {
    super.setIsFailed(isFailed);
    return this;
  }

  @Override
  public MoneroWalletTx setLastFailedHeight(Integer lastFailedHeight) {
    super.setLastFailedHeight(lastFailedHeight);
    return this;
  }

  @Override
  public MoneroWalletTx setLastFailedId(String lastFailedId) {
    super.setLastFailedId(lastFailedId);
    return this;
  }

  @Override
  public MoneroWalletTx setMaxUsedBlockHeight(Integer maxUsedBlockHeight) {
    super.setMaxUsedBlockHeight(maxUsedBlockHeight);
    return this;
  }

  @Override
  public MoneroWalletTx setMaxUsedBlockId(Integer maxUsedBlockId) {
    super.setMaxUsedBlockId(maxUsedBlockId);
    return this;
  }

  @Override
  public MoneroWalletTx setSignatures(List<String> signatures) {
    super.setSignatures(signatures);
    return this;
  }
}
