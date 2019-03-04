package monero.wallet.model;

import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;

/**
 * Models a Monero output with wallet extensions.
 */
public class MoneroWalletOutput extends MoneroOutput {

  private Integer accountIndex;
  private Integer subaddressIndex;
  private Boolean isSpent;
  
  public MoneroWalletTx getTx() {
    return (MoneroWalletTx) super.getTx();
  }
  
  public MoneroWalletOutput setTx(MoneroTx tx) {
    if (tx != null && !(tx instanceof MoneroWalletTx)) throw new MoneroException("Wallet output's transaction must be of type MoneroWalletTx");
    super.setTx(tx);
    return this;
  }
  
  public MoneroWalletOutput setTx(MoneroWalletTx tx) {
    super.setTx(tx);
    return this;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public MoneroWalletOutput setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
    return this;
  }
  
  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }
  
  public MoneroWalletOutput setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    return this;
  }
  
  public Boolean getIsSpent() {
    return isSpent;
  }
  
  public MoneroWalletOutput setIsSpent(Boolean isSpent) {
    this.isSpent = isSpent;
    return this;
  }
  
  public MoneroWalletOutput copy() {
    throw new RuntimeException("Not implemented");
  }
}
