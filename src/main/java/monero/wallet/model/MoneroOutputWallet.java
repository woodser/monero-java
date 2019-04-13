package monero.wallet.model;

import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;

/**
 * Models a Monero output with wallet extensions.
 */
public class MoneroOutputWallet extends MoneroOutput {

  private Integer accountIndex;
  private Integer subaddressIndex;
  private Boolean isSpent;
  
  public MoneroOutputWallet() {
    // nothing to construct
  }
  
  /**
   * Deep copy constructor.
   * 
   * @param output is the output to initialize from
   */
  public MoneroOutputWallet(MoneroOutputWallet output) {
    super(output);
    accountIndex = output.accountIndex;
    subaddressIndex = output.subaddressIndex;
    isSpent = output.isSpent;
  }
  
  public MoneroTxWallet getTx() {
    return (MoneroTxWallet) super.getTx();
  }
  
  public MoneroOutputWallet setTx(MoneroTx tx) {
    if (tx != null && !(tx instanceof MoneroTxWallet)) throw new MoneroException("Wallet output's transaction must be of type MoneroTxWallet");
    super.setTx(tx);
    return this;
  }
  
  public MoneroOutputWallet setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public MoneroOutputWallet setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
    return this;
  }
  
  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }
  
  public MoneroOutputWallet setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    return this;
  }
  
  public Boolean getIsSpent() {
    return isSpent;
  }
  
  public MoneroOutputWallet setIsSpent(Boolean isSpent) {
    this.isSpent = isSpent;
    return this;
  }
  
  public MoneroOutputWallet copy() {
    return new MoneroOutputWallet(this);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((isSpent == null) ? 0 : isSpent.hashCode());
    result = prime * result + ((subaddressIndex == null) ? 0 : subaddressIndex.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!super.equals(obj)) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroOutputWallet other = (MoneroOutputWallet) obj;
    if (accountIndex == null) {
      if (other.accountIndex != null) return false;
    } else if (!accountIndex.equals(other.accountIndex)) return false;
    if (isSpent == null) {
      if (other.isSpent != null) return false;
    } else if (!isSpent.equals(other.isSpent)) return false;
    if (subaddressIndex == null) {
      if (other.subaddressIndex != null) return false;
    } else if (!subaddressIndex.equals(other.subaddressIndex)) return false;
    return true;
  }
}
