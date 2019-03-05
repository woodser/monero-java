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
    throw new RuntimeException("Not implemented");
  }
}
