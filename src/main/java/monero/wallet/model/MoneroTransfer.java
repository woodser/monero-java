package monero.wallet.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Models a directional transfer of funds from or to a wallet.
 */
public class MoneroTransfer {

  private MoneroTxWallet tx;
  private Boolean isOutgoing;
  private Boolean isIncoming;
  private String address;
  private Integer accountIndex;
  private Integer subaddressIndex;
  private BigInteger amount;
  private List<MoneroDestination> destinations;
  
  public MoneroTxWallet getTx() {
    return tx;
  }
  
  public MoneroTransfer setTx(MoneroTxWallet tx) {
    this.tx = tx;
    return this;
  }
  
  public Boolean getIsOutgoing() {
    return isOutgoing;
  }
  
  public MoneroTransfer setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }
  
  public Boolean getIsIncoming() {
    return isIncoming;
  }
  
  public MoneroTransfer setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  public String getAddress() {
    return address;
  }
  
  public MoneroTransfer setAddress(String address) {
    this.address = address;
    return this;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public MoneroTransfer setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
    return this;
  }
  
  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }
  
  public MoneroTransfer setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    return this;
  }
  
  public BigInteger getAmount() {
    return amount;
  }
  
  public MoneroTransfer setAmount(BigInteger amount) {
    this.amount = amount;
    return this;
  }
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  public MoneroTransfer setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }
}
