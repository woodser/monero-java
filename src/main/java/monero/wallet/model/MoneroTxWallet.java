package monero.wallet.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.GenUtils;
import monero.common.MoneroError;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;

/**
 * Models a Monero transaction with wallet extensions.
 */
public class MoneroTxWallet extends MoneroTx {

  private MoneroTxSet txSet;
  private Boolean isIncoming;
  private Boolean isOutgoing;
  private List<MoneroIncomingTransfer> incomingTransfers;
  private MoneroOutgoingTransfer outgoingTransfer;
  private String note;
  private Boolean isLocked;
  private BigInteger inputSum;
  private BigInteger outputSum;
  private String changeAddress;
  private BigInteger changeAmount;
  private Integer numDummyOutputs;
  private String extraHex;  // TODO: refactor MoneroTx to only use extra as hex string
  
  public MoneroTxWallet() {
    // nothing to initialize
  }
  
  public MoneroTxWallet(final MoneroTxWallet tx) {
    super(tx);
    this.txSet = tx.txSet;
    this.isIncoming = tx.isIncoming;
    this.isOutgoing = tx.isOutgoing;
    if (tx.incomingTransfers != null) {
      this.incomingTransfers = new ArrayList<MoneroIncomingTransfer>();
      for (MoneroIncomingTransfer transfer : tx.incomingTransfers) {
        this.incomingTransfers.add(transfer.copy().setTx(this));
      }
    }
    if (tx.outgoingTransfer != null) this.outgoingTransfer = tx.outgoingTransfer.copy().setTx(this);
    this.note = tx.note;
    this.isLocked = tx.isLocked;
    this.inputSum = tx.inputSum;
    this.outputSum = tx.outputSum;
    this.changeAddress = tx.changeAddress;
    this.changeAmount = tx.changeAmount;
    this.numDummyOutputs = tx.numDummyOutputs;
    this.extraHex = tx.extraHex;
  }
  
  @Override
  public MoneroTxWallet copy() {
    return new MoneroTxWallet(this);
  }
  
  @JsonBackReference("tx_set")
  public MoneroTxSet getTxSet() {
    return txSet;
  }
  
  public MoneroTxWallet setTxSet(MoneroTxSet txSet) {
    this.txSet = txSet;
    return this;
  }
  
  @JsonProperty("isIncoming")
  public Boolean isIncoming() {
    return isIncoming;
  }
  
  public MoneroTxWallet setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  @JsonProperty("isOutgoing")
  public Boolean isOutgoing() {
    return isOutgoing;
  }
  
  public MoneroTxWallet setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }
  
  public BigInteger getIncomingAmount() {
    if (getIncomingTransfers() == null) return null;
    BigInteger incomingAmt = BigInteger.valueOf(0);
    for (MoneroTransfer transfer : this.getIncomingTransfers()) incomingAmt = incomingAmt.add(transfer.getAmount());
    return incomingAmt;
  }
  
  public BigInteger getOutgoingAmount() {
    return getOutgoingTransfer() != null ? getOutgoingTransfer().getAmount() : null;
  }
  
  @JsonIgnore
  public List<MoneroTransfer> getTransfers() {
    return getTransfers(null);
  }
  
  public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    if (getOutgoingTransfer() != null && (query == null || query.meetsCriteria(getOutgoingTransfer()))) transfers.add(getOutgoingTransfer());
    if (getIncomingTransfers() != null) {
      for (MoneroTransfer transfer : getIncomingTransfers()) {
        if (query == null || query.meetsCriteria(transfer)) transfers.add(transfer);
      }
    }
    return transfers;
  }
  
  public List<MoneroTransfer> filterTransfers(MoneroTransferQuery query) {
    List<MoneroTransfer> transfers = new ArrayList<MoneroTransfer>();
    
    // collect outgoing transfer or erase if filtered
    if (getOutgoingTransfer() != null && (query == null || query.meetsCriteria(getOutgoingTransfer()))) transfers.add(getOutgoingTransfer());
    else setOutgoingTransfer(null);
    
    // collect incoming transfers or erase if filtered
    if (getIncomingTransfers() != null) {
      List<MoneroTransfer> toRemoves = new ArrayList<MoneroTransfer>();
      for (MoneroTransfer transfer : getIncomingTransfers()) {
        if (query == null || query.meetsCriteria(transfer)) transfers.add(transfer);
        else toRemoves.add(transfer);
      }
      getIncomingTransfers().removeAll(toRemoves);
      if (getIncomingTransfers().isEmpty()) setIncomingTransfers(null);
    }
    
    return transfers;
  }
  
  @JsonManagedReference
  public List<MoneroIncomingTransfer> getIncomingTransfers() {
    return incomingTransfers;
  }
  
  public MoneroTxWallet setIncomingTransfers(List<MoneroIncomingTransfer> incomingTransfers) {
    this.incomingTransfers = incomingTransfers;
    return this;
  }
  
  @JsonManagedReference
  public MoneroOutgoingTransfer getOutgoingTransfer() {
    return outgoingTransfer;
  }
  
  public MoneroTxWallet setOutgoingTransfer(MoneroOutgoingTransfer outgoingTransfer) {
    this.outgoingTransfer = outgoingTransfer;
    return this;
  }
  
  /**
   * Set the tx's inputs (MoneroOutputWallet) which contain information relative
   * to a wallet.
   * 
   * Callers must cast to extended type (MoneroOutput) because Java
   * paramaterized types do not recognize inheritance.
   * 
   * @param inputs are MoneroOutputWallets to set for the wallet tx
   * @return MoneroTxWallet is a reference to this tx for chaining
   */
  @Override
  public MoneroTxWallet setInputs(List<MoneroOutput> inputs) {
    
    // validate that all inputs are wallet inputs
    if (inputs != null) {
      for (MoneroOutput input : inputs) {
        if (!(input instanceof MoneroOutputWallet)) throw new MoneroError("Wallet transaction inputs must be of type MoneroOutputWallet");
      }
    }
    super.setInputs(inputs);
    return this;
  }
  
  /**
   * Set inputs with compile-time binding to MoneroOutputWallet for deserialization.
   * 
   * @param inputs are the tx's inputs
   * @return MoneroTxWallet is a reference to this tx for chaining
   */
  @JsonProperty("inputs")
  public MoneroTxWallet setInputsWallet(List<MoneroOutputWallet> inputs) {
    return setInputs(new ArrayList<MoneroOutput>(inputs));
  }
  
  /**
   * Returns a copy of this model's inputs as a list of type MoneroOutputWallet.
   * 
   * @return inputs of type MoneroOutputWallet
   */
  public List<MoneroOutputWallet> getInputsWallet() {
    return getInputsWallet(null);
  }
  
  public List<MoneroOutputWallet> getInputsWallet(MoneroOutputQuery query) {
    List<MoneroOutputWallet> inputsWallet = new ArrayList<MoneroOutputWallet>();
    List<MoneroOutput> inputs = getInputs();
    if (inputs == null) return inputsWallet;
    for (MoneroOutput output : inputs) {
      if (query == null || query.meetsCriteria((MoneroOutputWallet) output)) inputsWallet.add((MoneroOutputWallet) output);
    }
    return inputsWallet;
  }
  
  /**
   * Set the tx's outputs (MoneroOutputWallet) which contain information relative
   * to a wallet.
   * 
   * Callers must cast to extended type (MoneroOutput) because Java
   * paramaterized types do not recognize inheritance.
   * 
   * @param outputs are MoneroOutputWallets to set for the wallet tx
   * @return MoneroTxWallet is a reference to this tx for chaining
   */
  @Override
  public MoneroTxWallet setOutputs(List<MoneroOutput> outputs) {
    
    // validate that all outputs are wallet outputs
    if (outputs != null) {
      for (MoneroOutput output : outputs) {
        if (!(output instanceof MoneroOutputWallet)) throw new MoneroError("Wallet transaction outputs must be of type MoneroOutputWallet");
      }
    }
    super.setOutputs(outputs);
    return this;
  }
  
  /**
   * Set outputs with compile-time binding to MoneroOutputWallet for deserialization.
   * 
   * @param outputs are the tx's outputs
   * @return MoneroTxWallet is a reference to this tx for chaining
   */
  @JsonProperty("outputs")
  public MoneroTxWallet setOutputsWallet(List<MoneroOutputWallet> outputs) {
    return setOutputs(new ArrayList<MoneroOutput>(outputs));
  }
  
  /**
   * Returns a copy of this model's outputs as a list of type MoneroOutputWallet.
   * 
   * @return outputs of type MoneroOutputWallet
   */
  public List<MoneroOutputWallet> getOutputsWallet() {
    return getOutputsWallet(null);
  }
  
  public List<MoneroOutputWallet> getOutputsWallet(MoneroOutputQuery query) {
    List<MoneroOutputWallet> outputsWallet = new ArrayList<MoneroOutputWallet>();
    List<MoneroOutput> outputs = getOutputs();
    if (outputs == null) return outputsWallet;
    for (MoneroOutput output : outputs) {
      if (query == null || query.meetsCriteria((MoneroOutputWallet) output)) outputsWallet.add((MoneroOutputWallet) output);
    }
    return outputsWallet;
  }
  
  public List<MoneroOutputWallet> filterOutputsWallet(MoneroOutputQuery query) {
    List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
    if (getOutputs() != null) {
      List<MoneroOutput> toRemoves = new ArrayList<MoneroOutput>();
      for (MoneroOutput output : getOutputs()) {
        if (query == null || query.meetsCriteria((MoneroOutputWallet) output)) outputs.add((MoneroOutputWallet) output);
        else toRemoves.add(output);
      }
      getOutputs().removeAll(toRemoves);
      if (getOutputs().isEmpty()) setOutputs(null);
    }
    return outputs;
  }
  
  public String getNote() {
    return note;
  }
  
  public MoneroTxWallet setNote(String note) {
    this.note = note;
    return this;
  }
  
  @JsonProperty("isLocked")
  public Boolean isLocked() {
    return isLocked;
  }
  
  public MoneroTxWallet setIsLocked(Boolean isLocked) {
    this.isLocked = isLocked;
    return this;
  }

  public BigInteger getInputSum() {
    return inputSum;
  }

  public MoneroTxWallet setInputSum(BigInteger inputSum) {
    this.inputSum = inputSum;
    return this;
  }

  public BigInteger getOutputSum() {
    return outputSum;
  }

  public MoneroTxWallet setOutputSum(BigInteger outputSum) {
    this.outputSum = outputSum;
    return this;
  }

  public String getChangeAddress() {
    return changeAddress;
  }

  public MoneroTxWallet setChangeAddress(String changeAddress) {
    this.changeAddress = changeAddress;
    return this;
  }

  public BigInteger getChangeAmount() {
    return changeAmount;
  }

  public MoneroTxWallet setChangeAmount(BigInteger changeAmount) {
    this.changeAmount = changeAmount;
    return this;
  }

  public Integer getNumDummyOutputs() {
    return numDummyOutputs;
  }

  public MoneroTxWallet setNumDummyOutputs(Integer numDummyOutputs) {
    this.numDummyOutputs = numDummyOutputs;
    return this;
  }

  public String getExtraHex() {
    return extraHex;
  }

  public MoneroTxWallet setExtraHex(String extraHex) {
    this.extraHex = extraHex;
    return this;
  }
  
  @Override
  public MoneroTxWallet merge(MoneroTx tx) {
    if (tx != null && !(tx instanceof MoneroTxWallet)) throw new MoneroError("Wallet transaction must be merged with type MoneroTxWallet");
    return merge((MoneroTxWallet) tx);
  }
  
  /**
   * Updates this transaction by merging the latest information from the given
   * transaction.
   * 
   * Merging can modify or build references to the transaction given so it
   * should not be re-used or it should be copied before calling this method.
   * 
   * @param tx is the transaction to merge into this transaction
   * @return this tx for chaining
   */
  public MoneroTxWallet merge(MoneroTxWallet tx) {
    if (!(tx instanceof MoneroTxWallet)) throw new MoneroError("Wallet transaction must be merged with type MoneroTxWallet");
    if (this == tx) return this;
    
    // merge base classes
    super.merge(tx);
    
    // merge tx set if they're different which comes back to merging txs
    if (txSet != tx.getTxSet()) {
      if (txSet == null) {
        txSet = new MoneroTxSet();
        txSet.setTxs(this);
      }
      if (tx.getTxSet() == null) {
        tx.setTxSet(new MoneroTxSet());
        tx.getTxSet().setTxs(tx);
      }
      txSet.merge(tx.getTxSet());
      return this;
    }
    
    // merge incoming transfers
    if (tx.getIncomingTransfers() != null) {
      if (this.getIncomingTransfers() == null) this.setIncomingTransfers(new ArrayList<MoneroIncomingTransfer>());
      for (MoneroIncomingTransfer transfer : tx.getIncomingTransfers()) {
        transfer.setTx(this);
        mergeIncomingTransfer(this.getIncomingTransfers(), transfer);
      }
    }
    
    // merge outgoing transfer
    if (tx.getOutgoingTransfer() != null) {
      tx.getOutgoingTransfer().setTx(this);
      if (this.getOutgoingTransfer() == null) this.setOutgoingTransfer(tx.getOutgoingTransfer());
      else this.getOutgoingTransfer().merge(tx.getOutgoingTransfer());
    }
    
    // merge simple extensions
    this.setIsIncoming(GenUtils.reconcile(this.isIncoming(), tx.isIncoming()));
    this.setIsOutgoing(GenUtils.reconcile(this.isOutgoing(), tx.isOutgoing()));
    this.setNote(GenUtils.reconcile(this.getNote(), tx.getNote()));
    this.setIsLocked(GenUtils.reconcile(this.isLocked(), tx.isLocked(), null, false, null));  // tx can become unlocked
    this.setInputSum(GenUtils.reconcile(this.getInputSum(), tx.getInputSum()));
    this.setOutputSum(GenUtils.reconcile(this.getOutputSum(), tx.getOutputSum()));
    this.setChangeAddress(GenUtils.reconcile(this.getChangeAddress(), tx.getChangeAddress()));
    this.setChangeAmount(GenUtils.reconcile(this.getChangeAmount(), tx.getChangeAmount()));
    this.setNumDummyOutputs(GenUtils.reconcile(this.getNumDummyOutputs(), tx.getNumDummyOutputs()));
    this.setExtraHex(GenUtils.reconcile(this.getExtraHex(), tx.getExtraHex()));
    
    return this;  // for chaining
  }
  
  @Override
  public String toString() {
    return toString(0, false);
  }
  
  @Override
  public String toString(int indent) {
    return toString(indent, false);
  }
  
  public String toString(int indent, boolean oneLine) {
    StringBuilder sb = new StringBuilder();
    
    // represent tx with one line string
    // TODO: proper csv export
    if (oneLine) {
      sb.append(this.getHash() + ", ");
      sb.append((this.isConfirmed() ? this.getBlock().getTimestamp() : this.getReceivedTimestamp()) + ", ");
      sb.append(this.isConfirmed() + ", ");
      sb.append((this.getOutgoingAmount() != null? this.getOutgoingAmount().toString() : "") + ", ");
      sb.append(this.getIncomingAmount() != null ? this.getIncomingAmount().toString() : "");
      return sb.toString();
    }
    
    // otherwise stringify all fields
    sb.append(super.toString(indent) + "\n");
    sb.append(GenUtils.kvLine("Is incoming", this.isIncoming(), indent));
    sb.append(GenUtils.kvLine("Incoming amount", this.getIncomingAmount(), indent));
    if (this.getIncomingTransfers() != null) {
      sb.append(GenUtils.kvLine("Incoming transfers", "", indent));
      for (int i = 0; i < this.getIncomingTransfers().size(); i++) {
        sb.append(GenUtils.kvLine(i + 1, "", indent + 1));
        sb.append(this.getIncomingTransfers().get(i).toString(indent + 2) + "\n");
      }
    }
    sb.append(GenUtils.kvLine("Is outgoing", this.isOutgoing(), indent));
    sb.append(GenUtils.kvLine("Outgoing amount", this.getOutgoingAmount(), indent));
    if (this.getOutgoingTransfer() != null) {
      sb.append(GenUtils.kvLine("Outgoing transfer", "", indent));
      sb.append(this.getOutgoingTransfer().toString(indent + 1) + "\n");
    }
    sb.append(GenUtils.kvLine("Note", this.getNote(), indent));
    sb.append(GenUtils.kvLine("Is locked", this.isLocked(), indent));
    sb.append(GenUtils.kvLine("Input sum", this.getInputSum(), indent));
    sb.append(GenUtils.kvLine("Output sum", this.getOutputSum(), indent));
    sb.append(GenUtils.kvLine("Change address", this.getChangeAddress(), indent));
    sb.append(GenUtils.kvLine("Change amount", this.getChangeAmount(), indent));
    sb.append(GenUtils.kvLine("Num dummy outputs", this.getNumDummyOutputs(), indent));
    sb.append(GenUtils.kvLine("Extra hex", this.getExtraHex(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);  // strip last newline
  }
  
  // private helper to merge transfers
  private static void mergeIncomingTransfer(List<MoneroIncomingTransfer> transfers, MoneroIncomingTransfer transfer) {
    for (MoneroIncomingTransfer aTransfer : transfers) {
      if (aTransfer.getAccountIndex() == transfer.getAccountIndex() && aTransfer.getSubaddressIndex() == transfer.getSubaddressIndex()) {
        aTransfer.merge(transfer);
        return;
      }
    }
    transfers.add(transfer);
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((changeAddress == null) ? 0 : changeAddress.hashCode());
    result = prime * result + ((changeAmount == null) ? 0 : changeAmount.hashCode());
    result = prime * result + ((extraHex == null) ? 0 : extraHex.hashCode());
    result = prime * result + ((incomingTransfers == null) ? 0 : incomingTransfers.hashCode());
    result = prime * result + ((inputSum == null) ? 0 : inputSum.hashCode());
    result = prime * result + ((isIncoming == null) ? 0 : isIncoming.hashCode());
    result = prime * result + ((isOutgoing == null) ? 0 : isOutgoing.hashCode());
    result = prime * result + ((isLocked == null) ? 0 : isLocked.hashCode());
    result = prime * result + ((note == null) ? 0 : note.hashCode());
    result = prime * result + ((numDummyOutputs == null) ? 0 : numDummyOutputs.hashCode());
    result = prime * result + ((outgoingTransfer == null) ? 0 : outgoingTransfer.hashCode());
    result = prime * result + ((outputSum == null) ? 0 : outputSum.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!super.equals(obj)) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTxWallet other = (MoneroTxWallet) obj;
    if (changeAddress == null) {
      if (other.changeAddress != null) return false;
    } else if (!changeAddress.equals(other.changeAddress)) return false;
    if (changeAmount == null) {
      if (other.changeAmount != null) return false;
    } else if (!changeAmount.equals(other.changeAmount)) return false;
    if (extraHex == null) {
      if (other.extraHex != null) return false;
    } else if (!extraHex.equals(other.extraHex)) return false;
    if (incomingTransfers == null) {
      if (other.incomingTransfers != null) return false;
    } else if (!incomingTransfers.equals(other.incomingTransfers)) return false;
    if (inputSum == null) {
      if (other.inputSum != null) return false;
    } else if (!inputSum.equals(other.inputSum)) return false;
    if (isIncoming == null) {
      if (other.isIncoming != null) return false;
    } else if (!isIncoming.equals(other.isIncoming)) return false;
    if (isOutgoing == null) {
      if (other.isOutgoing != null) return false;
    } else if (!isOutgoing.equals(other.isOutgoing)) return false;
    if (isLocked == null) {
      if (other.isLocked != null) return false;
    } else if (!isLocked.equals(other.isLocked)) return false;
    if (note == null) {
      if (other.note != null) return false;
    } else if (!note.equals(other.note)) return false;
    if (numDummyOutputs == null) {
      if (other.numDummyOutputs != null) return false;
    } else if (!numDummyOutputs.equals(other.numDummyOutputs)) return false;
    if (outgoingTransfer == null) {
      if (other.outgoingTransfer != null) return false;
    } else if (!outgoingTransfer.equals(other.outgoingTransfer)) return false;
    if (outputSum == null) {
      if (other.outputSum != null) return false;
    } else if (!outputSum.equals(other.outputSum)) return false;
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTxWallet setBlock(MoneroBlock block) {
    super.setBlock(block);
    return this;
  }

  @Override
  public MoneroTxWallet setHash(String hash) {
    super.setHash(hash);
    return this;
  }

  @Override
  public MoneroTxWallet setVersion(Integer version) {
    super.setVersion(version);
    return this;
  }

  @Override
  public MoneroTxWallet setIsMinerTx(Boolean isMinerTx) {
    super.setIsMinerTx(isMinerTx);
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
  public MoneroTxWallet setRingSize(Integer ringSize) {
    super.setRingSize(ringSize);
    return this;
  }

  @Override
  public MoneroTxWallet setRelay(Boolean relay) {
    super.setRelay(relay);
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
  public MoneroTxWallet setNumConfirmations(Long numConfirmations) {
    super.setNumConfirmations(numConfirmations);
    return this;
  }

  @Override
  public MoneroTxWallet setUnlockHeight(Long unlockHeight) {
    super.setUnlockHeight(unlockHeight);
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
  public MoneroTxWallet setIsDoubleSpendSeen(Boolean isDoubleSpend) {
    super.setIsDoubleSpendSeen(isDoubleSpend);
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
  public MoneroTxWallet setSize(Long size) {
    super.setSize(size);
    return this;
  }

  @Override
  public MoneroTxWallet setWeight(Long weight) {
    super.setWeight(weight);
    return this;
  }

  @Override
  public MoneroTxWallet setOutputIndices(List<Long> outputIndices) {
    super.setOutputIndices(outputIndices);
    return this;
  }

  @Override
  public MoneroTxWallet setMetadata(String metadata) {
    super.setMetadata(metadata);
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
  public MoneroTxWallet setLastFailedHeight(Long lastFailedHeight) {
    super.setLastFailedHeight(lastFailedHeight);
    return this;
  }

  @Override
  public MoneroTxWallet setLastFailedHash(String lastFailedId) {
    super.setLastFailedHash(lastFailedId);
    return this;
  }

  @Override
  public MoneroTxWallet setMaxUsedBlockHeight(Long maxUsedBlockHeight) {
    super.setMaxUsedBlockHeight(maxUsedBlockHeight);
    return this;
  }

  @Override
  public MoneroTxWallet setMaxUsedBlockHash(String maxUsedBlockId) {
    super.setMaxUsedBlockHash(maxUsedBlockId);
    return this;
  }

  @Override
  public MoneroTxWallet setSignatures(List<String> signatures) {
    super.setSignatures(signatures);
    return this;
  }
}
