package monero.wallet.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;

/**
 * Models a Monero transaction with wallet extensions.
 */
public class MoneroTxWallet extends MoneroTx {

  private BigInteger incomingAmount;
  private BigInteger outgoingAmount;
  private List<MoneroTransfer> incomingTransfers;
  private MoneroTransfer outgoingTransfer;
  private String note;
  
  public MoneroTxWallet() {
    // nothing to initialize
  }
  
  public MoneroTxWallet(MoneroTxWallet tx) {
    super(tx);
    this.incomingAmount = tx.incomingAmount;
    this.outgoingAmount = tx.outgoingAmount;
    if (tx.incomingTransfers != null) {
      this.incomingTransfers = new ArrayList<MoneroTransfer>();
      for (MoneroTransfer transfer : tx.incomingTransfers) {
        this.incomingTransfers.add(transfer.copy().setTx(this));
      }
    }
    if (tx.outgoingTransfer != null) this.outgoingTransfer = tx.outgoingTransfer.copy().setTx(this);
    this.note = tx.note;
  }
  
  // ----------------------------- WALLET-SPECIFIC ----------------------------
  
  public Boolean getIsOutgoing() {
    return getOutgoingTransfer() != null;
  }
  
  public Boolean getIsIncoming() {
    return getIncomingTransfers() != null && !getIncomingTransfers().isEmpty();
  }
  
  public BigInteger getIncomingAmount() {
    return incomingAmount;
  }
  
  public MoneroTxWallet setIncomingAmount(BigInteger incomingAmount) {
    this.incomingAmount = incomingAmount;
    return this;
  }
  
  public BigInteger getOutgoingAmount() {
    return outgoingAmount;
  }
  
  public MoneroTxWallet setOutgoingAmount(BigInteger outgoingAmount) {
    this.outgoingAmount = outgoingAmount;
    return this;
  }
  
  public List<MoneroTransfer> getIncomingTransfers() {
    return incomingTransfers;
  }
  
  public MoneroTxWallet setIncomingTransfers(List<MoneroTransfer> incomingTransfers) {
    this.incomingTransfers = incomingTransfers;
    return this;
  }
  
  public MoneroTransfer getOutgoingTransfer() {
    return outgoingTransfer;
  }
  
  public MoneroTxWallet setOutgoingTransfer(MoneroTransfer outgoingTransfer) {
    this.outgoingTransfer = outgoingTransfer;
    return this;
  }
  
  /**
   * Returns vouts as List<MoneroOutputWallet>.
   * 
   * @return vouts of type MoneroOutputWallet
   */
  public List<MoneroOutputWallet> getVoutsWallet() {
    List<MoneroOutputWallet> voutsWallet = new ArrayList<MoneroOutputWallet>();
    for (MoneroOutput vout : getVouts()) voutsWallet.add((MoneroOutputWallet) vout);
    return voutsWallet;
  }
  
  public MoneroTxWallet setVouts(List<MoneroOutput> vouts) {
    
    // validate that all vouts are wallet outputs
    if (vouts != null) {
      for (MoneroOutput vout : vouts) {
        if (!(vout instanceof MoneroOutputWallet)) throw new MoneroException("Wallet transaction vouts must be of type MoneroOutputWallet");
      }
    }
    super.setVouts(vouts);
    return this;
  }
  
  public String getNote() {
    return note;
  }
  
  public MoneroTxWallet setNote(String note) {
    this.note = note;
    return this;
  }
  
  public MoneroTxWallet copy() {
    return new MoneroTxWallet(this);
  }
  
  public MoneroTxWallet merge(MoneroTx tx) {
    if (tx != null && !(tx instanceof MoneroTxWallet)) throw new MoneroException("Wallet transaction must be merged with type MoneroTxWallet");
    return merge((MoneroTxWallet) tx);
  }
  
  public MoneroTxWallet merge(MoneroTxWallet tx) {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTxWallet setBlock(MoneroBlock block) {
    super.setBlock(block);
    return this;
  }

  @Override
  public MoneroTxWallet setHeight(Integer height) {
    super.setHeight(height);
    return this;
  }

  @Override
  public MoneroTxWallet setId(String id) {
    super.setId(id);
    return this;
  }

  @Override
  public MoneroTxWallet setVersion(Integer version) {
    super.setVersion(version);
    return this;
  }

  @Override
  public MoneroTxWallet setIsCoinbase(Boolean isCoinbase) {
    super.setIsCoinbase(isCoinbase);
    return this;
  }

  @Override
  public MoneroTxWallet setPaymentId(String paymentId) {
    super.setPaymentId(paymentId);
    return this;
  }

  @Override
  public MoneroTxWallet setFee(BigInteger fee) {
    super.setFee(fee);
    return this;
  }

  @Override
  public MoneroTxWallet setMixin(Integer mixin) {
    super.setMixin(mixin);
    return this;
  }

  @Override
  public MoneroTxWallet setDoNotRelay(Boolean doNotRelay) {
    super.setDoNotRelay(doNotRelay);
    return this;
  }

  @Override
  public MoneroTxWallet setIsRelayed(Boolean isRelayed) {
    super.setIsRelayed(isRelayed);
    return this;
  }

  @Override
  public MoneroTxWallet setIsConfirmed(Boolean isConfirmed) {
    super.setIsConfirmed(isConfirmed);
    return this;
  }

  @Override
  public MoneroTxWallet setInTxPool(Boolean inTxPool) {
    super.setInTxPool(inTxPool);
    return this;
  }

  @Override
  public MoneroTxWallet setNumConfirmations(Integer numConfirmations) {
    super.setNumConfirmations(numConfirmations);
    return this;
  }

  @Override
  public MoneroTxWallet setNumEstimatedBlocksUntilConfirmed(Integer numEstimatedBlocksUntilConfirmed) {
    super.setNumEstimatedBlocksUntilConfirmed(numEstimatedBlocksUntilConfirmed);
    return this;
  }

  @Override
  public MoneroTxWallet setUnlockTime(Integer unlockTime) {
    super.setUnlockTime(unlockTime);
    return this;
  }

  @Override
  public MoneroTxWallet setLastRelayedTimestamp(Long lastRelayedTimestamp) {
    super.setLastRelayedTimestamp(lastRelayedTimestamp);
    return this;
  }

  @Override
  public MoneroTxWallet setReceivedTimestamp(Long receivedTimestamp) {
    super.setReceivedTimestamp(receivedTimestamp);
    return this;
  }

  @Override
  public MoneroTxWallet setIsDoubleSpend(Boolean isDoubleSpend) {
    super.setIsDoubleSpend(isDoubleSpend);
    return this;
  }

  @Override
  public MoneroTxWallet setKey(String key) {
    super.setKey(key);
    return this;
  }

  @Override
  public MoneroTxWallet setFullHex(String hex) {
    super.setFullHex(hex);
    return this;
  }

  @Override
  public MoneroTxWallet setPrunedHex(String prunedHex) {
    super.setPrunedHex(prunedHex);
    return this;
  }

  @Override
  public MoneroTxWallet setPrunableHex(String prunableHex) {
    super.setPrunableHex(prunableHex);
    return this;
  }

  @Override
  public MoneroTxWallet setPrunableHash(String prunableHash) {
    super.setPrunableHash(prunableHash);
    return this;
  }

  @Override
  public MoneroTxWallet setSize(Integer size) {
    super.setSize(size);
    return this;
  }

  @Override
  public MoneroTxWallet setWeight(Integer weight) {
    super.setWeight(weight);
    return this;
  }

  @Override
  public MoneroTxWallet setVins(List<MoneroOutput> vins) {
    super.setVins(vins);
    return this;
  }

  @Override
  public MoneroTxWallet setOutputIndices(List<Integer> outputIndices) {
    super.setOutputIndices(outputIndices);
    return this;
  }

  @Override
  public MoneroTxWallet setMetadata(String metadata) {
    super.setMetadata(metadata);
    return this;
  }

  @Override
  public MoneroTxWallet setCommonTxSets(String commonTxSets) {
    super.setCommonTxSets(commonTxSets);
    return this;
  }

  @Override
  public MoneroTxWallet setExtra(int[] extra) {
    super.setExtra(extra);
    return this;
  }

  @Override
  public MoneroTxWallet setRctSignatures(Object rctSignatures) {
    super.setRctSignatures(rctSignatures);
    return this;
  }

  @Override
  public MoneroTxWallet setRctSigPrunable(Object rctSigPrunable) {
    super.setRctSigPrunable(rctSigPrunable);
    return this;
  }

  @Override
  public MoneroTxWallet setIsKeptByBlock(Boolean isKeptByBlock) {
    super.setIsKeptByBlock(isKeptByBlock);
    return this;
  }

  @Override
  public MoneroTxWallet setIsFailed(Boolean isFailed) {
    super.setIsFailed(isFailed);
    return this;
  }

  @Override
  public MoneroTxWallet setLastFailedHeight(Integer lastFailedHeight) {
    super.setLastFailedHeight(lastFailedHeight);
    return this;
  }

  @Override
  public MoneroTxWallet setLastFailedId(String lastFailedId) {
    super.setLastFailedId(lastFailedId);
    return this;
  }

  @Override
  public MoneroTxWallet setMaxUsedBlockHeight(Integer maxUsedBlockHeight) {
    super.setMaxUsedBlockHeight(maxUsedBlockHeight);
    return this;
  }

  @Override
  public MoneroTxWallet setMaxUsedBlockId(String maxUsedBlockId) {
    super.setMaxUsedBlockId(maxUsedBlockId);
    return this;
  }

  @Override
  public MoneroTxWallet setSignatures(List<String> signatures) {
    super.setSignatures(signatures);
    return this;
  }
}
