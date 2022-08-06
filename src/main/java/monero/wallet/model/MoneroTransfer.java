package monero.wallet.model;

import java.math.BigInteger;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.GenUtils;
import monero.common.MoneroError;

/**
 * Models a base transfer of funds to or from the wallet.
 * 
 * Transfers are either of type MoneroIncomingTransfer or MoneroOutgoingTransfer so this class is abstract.
 */
public abstract class MoneroTransfer {

  private MoneroTxWallet tx;
  private BigInteger amount;
  private Integer accountIndex;
  
  public MoneroTransfer() {
    // nothing to initialize
  }
  
  public MoneroTransfer(final MoneroTransfer transfer) {
    this.amount = transfer.amount;
    this.accountIndex = transfer.accountIndex;
    validate();
  }
  
  public abstract MoneroTransfer copy();
  
  @JsonBackReference
  public MoneroTxWallet getTx() {
    return tx;
  }
  
  public MoneroTransfer setTx(MoneroTxWallet tx) {
    this.tx = tx;
    return this;
  }
  
  @JsonIgnore
  public Boolean isOutgoing() {
    return !isIncoming();
  }
  
  @JsonProperty("isIncoming")
  public abstract Boolean isIncoming();
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public MoneroTransfer setAmount(BigInteger amount) {
    this.amount = amount;
    return this;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }

  public MoneroTransfer setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
    validate();
    return this;
  }
  
  /**
   * Updates this transfer by merging the latest information from the given
   * transfer.
   * 
   * Merging can modify or build references to the transfer given so it
   * should not be re-used or it should be copied before calling this method.
   * 
   * @param transfer is the transfer to merge into this one
   * @return the merged transfer
   */
  public MoneroTransfer merge(MoneroTransfer transfer) {
    assert(transfer instanceof MoneroTransfer);
    if (this == transfer) return this;
    
    // merge txs if they're different which comes back to merging transfers
    if (this.getTx() != transfer.getTx()) {
      this.getTx().merge(transfer.getTx());
      return this;
    }
    
    // otherwise merge transfer fields
    this.setAccountIndex(GenUtils.reconcile(this.getAccountIndex(), transfer.getAccountIndex()));
    
    // TODO monero-project: failed tx in pool (after testUpdateLockedDifferentAccounts()) causes non-originating saved wallets to return duplicate incoming transfers but one has amount of 0
    if (this.getAmount() != null && transfer.getAmount() != null && !this.getAmount().equals(transfer.getAmount()) && (BigInteger.valueOf(0).equals(this.getAmount()) || BigInteger.valueOf(0).equals(transfer.getAmount()))) {
      System.err.println("WARNING: monero-project returning transfers with 0 amount/numSuggestedConfirmations");
    } else {
      this.setAmount(GenUtils.reconcile(this.getAmount(), transfer.getAmount()));
    }
    
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(GenUtils.kvLine("Is incoming", this.isIncoming(), indent));
    sb.append(GenUtils.kvLine("Amount", this.getAmount() != null ? this.getAmount().toString() : null, indent));
    sb.append(GenUtils.kvLine("Account index", this.getAccountIndex(), indent));
    String str = sb.toString();
    return str.isEmpty() ? str :  str.substring(0, str.length() - 1);	  // strip last newline
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((amount == null) ? 0 : amount.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTransfer other = (MoneroTransfer) obj;
    if (accountIndex == null) {
      if (other.accountIndex != null) return false;
    } else if (!accountIndex.equals(other.accountIndex)) return false;
    if (amount == null) {
      if (other.amount != null) return false;
    } else if (!amount.equals(other.amount)) return false;
    return true;
  }
  
  private void validate() {
    if (accountIndex != null && accountIndex < 0) throw new MoneroError("Account index must be >= 0");
  }
}
