package monero.wallet.config;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroOutgoingTransfer;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;

/**
 * Filters transfers that don't meet initialized filter criteria.
 */
public class MoneroTransferFilter extends MoneroTransfer implements Filter<MoneroTransfer> {

  private Boolean isIncoming;
  private String address;
  private List<String> addresses;
  private Integer subaddressIndex;
  private List<Integer> subaddressIndices;
  private List<MoneroDestination> destinations;
  private Boolean hasDestinations;
  private MoneroTxFilter txFilter;

  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public MoneroTransferFilter setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  public Boolean getIsOutgoing() {
    return isIncoming == null ? null : !isIncoming;
  }
  
  public MoneroTransferFilter setIsOutgoing(Boolean isOutgoing) {
    isIncoming = isOutgoing == null ? null : !isOutgoing;
    return this;
  }

  public String getAddress() {
    return address;
  }

  public MoneroTransferFilter setAddress(String address) {
    this.address = address;
    return this;
  }

  public List<String> getAddresses() {
    return addresses;
  }

  public MoneroTransferFilter setAddresses(List<String> addresses) {
    this.addresses = addresses;
    return this;
  }

  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }

  public MoneroTransferFilter setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    return this;
  }

  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroTransferFilter setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }

  public List<MoneroDestination> getDestinations() {
    return destinations;
  }

  public MoneroTransferFilter setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }

  public Boolean getHasDestinations() {
    return hasDestinations;
  }

  public MoneroTransferFilter setHasDestinations(Boolean hasDestinations) {
    this.hasDestinations = hasDestinations;
    return this;
  }

  public MoneroTxFilter getTxFilter() {
    return txFilter;
  }

  public MoneroTransferFilter setTxFilter(MoneroTxFilter txFilter) {
    this.txFilter = txFilter;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroTransfer transfer) {
    if (transfer == null) return false;
    
    // filter on common fields
    if (this.getIsIncoming() != null && this.getIsIncoming() != transfer.getIsIncoming()) return false;
    if (this.getIsOutgoing() != null && this.getIsOutgoing() != transfer.getIsOutgoing()) return false;
    if (this.getAmount() != null && this.getAmount().compareTo(transfer.getAmount()) != 0) return false;
    if (this.getAccountIndex() != null && !this.getAccountIndex().equals(transfer.getAccountIndex())) return false;
    
    // filter on incoming fields
    if (transfer instanceof MoneroIncomingTransfer) {
      if (Boolean.TRUE.equals(this.getHasDestinations())) return false;
      MoneroIncomingTransfer inTransfer = (MoneroIncomingTransfer) transfer;
      if (this.getAddress() != null && !this.getAddress().equals(inTransfer.getAddress())) return false;
      if (this.getAddresses() != null && !this.getAddresses().contains(inTransfer.getAddress())) return false;
      if (this.getSubaddressIndex() != null && !this.getSubaddressIndex().equals(inTransfer.getSubaddressIndex())) return false;
      if (this.getSubaddressIndices() != null && !this.getSubaddressIndices().contains(inTransfer.getSubaddressIndex())) return false;
    }

    // filter on outgoing fields
    else if (transfer instanceof MoneroOutgoingTransfer) {
      MoneroOutgoingTransfer outTransfer = (MoneroOutgoingTransfer) transfer;
      
      // filter on addresses
      if (this.getAddress() != null && (outTransfer.getAddresses() == null || !outTransfer.getAddresses().contains(this.getAddress()))) return false;   // TODO: will filter all transfers if they don't contain addresses
      if (this.getAddresses() != null) {
        List<String> intersections = new ArrayList<String>(this.getAddresses());
        intersections.retainAll(outTransfer.getAddresses());
        if (intersections.isEmpty()) return false;  // must have overlapping addresses
      }
      
      // filter on subaddress indices
      if (this.getSubaddressIndex() != null && (outTransfer.getSubaddressIndices() == null || !outTransfer.getSubaddressIndices().contains(this.getSubaddressIndex()))) return false;
      if (this.getSubaddressIndices() != null) {
        List<Integer> intersections = new ArrayList<Integer>(this.getSubaddressIndices());
        intersections.retainAll(outTransfer.getSubaddressIndices());
        if (intersections.isEmpty()) return false;  // must have overlapping subaddress indices
      }
      
      // filter on having destinations
      if (this.getHasDestinations() != null) {
        if (this.getHasDestinations() && outTransfer.getDestinations() == null) return false;
        if (!this.getHasDestinations() && outTransfer.getDestinations() != null) return false;
      }
      
      // filter on destinations TODO: start with test for this
//    if (this.getDestionations() != null && this.getDestionations() != transfer.getDestionations()) return false;
    }
    
    // otherwise invalid type
    else throw new RuntimeException("Transfer must be MoneroIncomingTransfer or MoneroOutgoingTransfer");
    
    // filter with tx filter
    if (this.getTxFilter() != null && !this.getTxFilter().meetsCriteria(transfer.getTx())) return false;    
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTransferFilter setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroTransferFilter setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroTransferFilter setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }
}
