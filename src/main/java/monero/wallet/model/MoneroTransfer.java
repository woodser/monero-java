package monero.wallet.model;

import java.math.BigInteger;

import com.fasterxml.jackson.annotation.JsonBackReference;

import monero.utils.MoneroUtils;

/**
 * Models a base transfer of funds to or from the wallet.
 */
public class MoneroTransfer {

  private MoneroTxWallet tx;
  private BigInteger amount;
  private Integer accountIndex;
  
  public MoneroTransfer() {
    // nothing to initialize
  }
  
  public MoneroTransfer(MoneroTransfer transfer) {
    this.amount = transfer.amount;
    this.accountIndex = transfer.accountIndex;
  }
  
  @JsonBackReference
  public MoneroTxWallet getTx() {
    return tx;
  }
  
  public MoneroTransfer setTx(MoneroTxWallet tx) {
    this.tx = tx;
    return this;
  }
  
  public Boolean getIsOutgoing() {
    return !getIsIncoming();
  }
  
  public Boolean getIsIncoming() {
    throw new RuntimeException("Subclass must implement");
  }
  
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
    return this;
  }
  
  public MoneroTransfer copy() {
    return new MoneroTransfer(this);
  }
  
  /**
   * Updates this transaction by merging the latest information from the given
   * transaction.
   * 
   * Merging can modify or build references to the transfer given so it
   * should not be re-used or it should be copied before calling this method.
   * 
   * @param transfer is the transfer to merge into this one
   */
  public MoneroTransfer merge(MoneroTransfer transfer) {
    assert(transfer instanceof MoneroTransfer);
    if (this == transfer) return this;
    
    // merge txs if they're different which comes back to merging transfers
    if (this.getTx() != transfer.getTx()) this.getTx().merge(transfer.getTx());
    
    // otherwise merge transfer fields
    else {
      this.setAmount(MoneroUtils.reconcile(this.getAmount(), transfer.getAmount()));
      this.setAccountIndex(MoneroUtils.reconcile(this.getAccountIndex(), transfer.getAccountIndex()));
    }
    
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(MoneroUtils.kvLine("Amount", this.getAmount() != null ? this.getAmount().toString() : null, indent));
    sb.append(MoneroUtils.kvLine("Account index", this.getAccountIndex(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);
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
}
