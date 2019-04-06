package monero.wallet.model;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.List;

import monero.utils.MoneroUtils;

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
    
    // merge transactions if they're different which comes back to merging transfers
    if (this.getTx() != transfer.getTx()) this.getTx().merge(transfer.getTx());
    
    // otherwise merge transfer fields
    else {
      this.setAddress(MoneroUtils.reconcile(this.getAddress(), transfer.getAddress()));
      this.setAccountIndex(MoneroUtils.reconcile(this.getAccountIndex(), transfer.getAccountIndex()));
      this.setSubaddressIndex(MoneroUtils.reconcile(this.getSubaddressIndex(), transfer.getSubaddressIndex()));
      this.setAmount(MoneroUtils.reconcile(this.getAmount(), transfer.getAmount()));
      
      // merge destinations
      if (this.getDestinations() == null) this.setDestinations(transfer.getDestinations());
      else if (transfer.getDestinations() != null) {
        assertEquals("Cannot merge transfer because destinations are different", this.getDestinations(), transfer.getDestinations());
      }
    }
    
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(MoneroUtils.kvLine("Is outgoing", this.getIsOutgoing(), indent));
    sb.append(MoneroUtils.kvLine("Address", this.getAddress(), indent));
    sb.append(MoneroUtils.kvLine("Account index", this.getAccountIndex(), indent));
    sb.append(MoneroUtils.kvLine("Subaddress index", this.getSubaddressIndex(), indent));
    sb.append(MoneroUtils.kvLine("Amount", this.getAmount() != null ? this.getAmount().toString() : null, indent));
    if (this.getDestinations() != null) {
      sb.append(MoneroUtils.kvLine("Destinations", "", indent));
      for (int i = 0; i < this.getDestinations().size(); i++) {
        sb.append(MoneroUtils.kvLine(i + 1, "", indent + 1));
        sb.append(getDestinations().get(i).toString(indent + 2) + "\n");
      }
    }
    String str = sb.toString();
    return str.substring(0, str.length() - 1);
  }
}
