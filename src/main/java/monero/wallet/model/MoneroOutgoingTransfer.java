package monero.wallet.model;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import monero.utils.MoneroUtils;

/**
 * Models an outgoing transfer of funds from the wallet.
 */
public class MoneroOutgoingTransfer extends MoneroTransfer {

  private List<Integer> subaddressIndices;
  private List<MoneroDestination> destinations;
  
  public MoneroOutgoingTransfer() {
    // nothing to initialize
  }
  
  public MoneroOutgoingTransfer(MoneroOutgoingTransfer transfer) {
    super(transfer);
    this.subaddressIndices = new ArrayList<Integer>(transfer.subaddressIndices);
    if (transfer.destinations != null) {
      this.destinations = new ArrayList<MoneroDestination>();
      for (MoneroDestination destination : transfer.getDestinations()) {
        this.destinations.add(destination.copy()); 
      }
    }
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroOutgoingTransfer setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  public MoneroOutgoingTransfer setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }
  
  public MoneroOutgoingTransfer copy() {
    return new MoneroOutgoingTransfer(this);
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
  public MoneroOutgoingTransfer merge(MoneroOutgoingTransfer transfer) {
    super.merge(transfer);
    assert(transfer instanceof MoneroOutgoingTransfer);
    if (this == transfer) return this;
    
    // merge subaddress indices
    this.setSubaddressIndices(MoneroUtils.reconcile(this.getSubaddressIndices(), transfer.getSubaddressIndices()));
    
    // merge destinations
    if (this.getDestinations() == null) this.setDestinations(transfer.getDestinations());
    else if (transfer.getDestinations() != null) {
      assertEquals("Cannot merge transfer because destinations are different", this.getDestinations(), transfer.getDestinations());
    }
    
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(super.toString(indent));
    sb.append(MoneroUtils.kvLine("Subaddress indices", this.getSubaddressIndices(), indent));
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

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((destinations == null) ? 0 : destinations.hashCode());
    result = prime * result + ((subaddressIndices == null) ? 0 : subaddressIndices.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!super.equals(obj)) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroOutgoingTransfer other = (MoneroOutgoingTransfer) obj;
    if (destinations == null) {
      if (other.destinations != null) return false;
    } else if (!destinations.equals(other.destinations)) return false;
    if (subaddressIndices == null) {
      if (other.subaddressIndices != null) return false;
    } else if (!subaddressIndices.equals(other.subaddressIndices)) return false;
    return true;
  }
}
