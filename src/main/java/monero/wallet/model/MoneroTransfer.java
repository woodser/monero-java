package monero.wallet.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Models a directional transfer of funds from or to a wallet.
 */
public class MoneroTransfer {

  private MoneroWalletTx tx;
  private Boolean isOutgoing;
  private Boolean isIncoming;
  private String address;
  private Integer accountIndex;
  private Integer subaddressIndex;
  private BigInteger amount;
  private List<MoneroDestination> destinations;
  
  public MoneroWalletTx getTx() {
    return tx;
  }
  
  public void setTx(MoneroWalletTx tx) {
    this.tx = tx;
  }
  
  public Boolean getIsOutgoing() {
    return isOutgoing;
  }
  
  public void setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
  }
  
  public Boolean getIsIncoming() {
    return isIncoming;
  }
  
  public void setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
  }
  
  public String getAddress() {
    return address;
  }
  
  public void setAddress(String address) {
    this.address = address;
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
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public void setAmount(BigInteger amount) {
    this.amount = amount;
  }
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  public void setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
  }
}
