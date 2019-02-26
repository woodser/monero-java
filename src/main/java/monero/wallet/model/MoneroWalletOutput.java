package monero.wallet.model;

import monero.daemon.model.MoneroOutput;

/**
 * Models a Monero output with wallet extensions.
 */
public class MoneroWalletOutput extends MoneroOutput {

  private Integer accountIndex;
  private Integer subaddressIndex;
  private Boolean isSpent;
  
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
