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
    return (MoneroWalletTx) getTx();
  }
  
  public void setTx(MoneroTx tx) {
    if (!(tx instanceof MoneroWalletTx)) throw new MoneroException("Wallet output's transaction must be of type MoneroWalletTx");
    super.setTx(tx);
  }
  
  public void setTx(MoneroWalletTx tx) {
    super.setTx(tx);
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public void setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
  }
  
  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }
  
  public void setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
  }
  
  public Boolean getIsSpent() {
    return isSpent;
  }
  
  public void setIsSpent(Boolean isSpent) {
    this.isSpent = isSpent;
  }
}
