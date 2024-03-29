package monero.wallet.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import common.types.Filter;
import common.utils.GenUtils;
import monero.common.MoneroError;

/**
 * Configures a query to retrieve transfers.
 * 
 * All transfers are returned except those that do not meet the criteria defined in this query.
 */
public class MoneroTransferQuery extends MoneroTransfer implements Filter<MoneroTransfer> {

  protected MoneroTxQuery txQuery;
  private Boolean isIncoming;
  private String address;
  private List<String> addresses;
  private Integer subaddressIndex;
  private List<Integer> subaddressIndices;
  private List<MoneroDestination> destinations;
  private Boolean hasDestinations;
  
  public MoneroTransferQuery() {
    
  }
  
  public MoneroTransferQuery(final MoneroTransferQuery query) {
    super(query);
    this.isIncoming = query.isIncoming;
    this.address = query.address;
    if (query.addresses != null) this.addresses = new ArrayList<String>(query.addresses);
    this.subaddressIndex = query.subaddressIndex;
    if (query.subaddressIndices != null) this.subaddressIndices = new ArrayList<Integer>(query.subaddressIndices);
    if (query.destinations != null) {
      this.destinations = new ArrayList<MoneroDestination>();
      for (MoneroDestination destination : query.getDestinations()) this.destinations.add(destination.copy());
    }
    this.hasDestinations = query.hasDestinations;
    this.txQuery = query.txQuery; // reference original by default, MoneroTxQuery's deep copy will set this to itself
    validate();
  }
  
  @Override
  public MoneroTransferQuery copy() {
    return new MoneroTransferQuery(this);
  }
  
  @JsonIgnore
  public MoneroTxQuery getTxQuery() {
    return txQuery;
  }

  public MoneroTransferQuery setTxQuery(MoneroTxQuery txQuery) {
    this.txQuery = txQuery;
    if (txQuery != null) txQuery.transferQuery = this;
    return this;
  }

  @Override
  public Boolean isIncoming() {
    return isIncoming;
  }

  public MoneroTransferQuery setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  @Override
  public Boolean isOutgoing() {
    return isIncoming == null ? null : !isIncoming;
  }
  
  public MoneroTransferQuery setIsOutgoing(Boolean isOutgoing) {
    isIncoming = isOutgoing == null ? null : !isOutgoing;
    return this;
  }

  public String getAddress() {
    return address;
  }

  public MoneroTransferQuery setAddress(String address) {
    this.address = address;
    return this;
  }

  public List<String> getAddresses() {
    return addresses;
  }

  public MoneroTransferQuery setAddresses(List<String> addresses) {
    this.addresses = addresses;
    return this;
  }
  
  public MoneroTransferQuery setAddresses(String... addresses) {
    this.addresses = GenUtils.arrayToList(addresses);
    return this;
  }

  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }

  public MoneroTransferQuery setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    validate();
    return this;
  }

  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroTransferQuery setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    validate();
    return this;
  }
  
  public MoneroTransferQuery setSubaddressIndices(Integer... subaddressIndices) {
    this.subaddressIndices = GenUtils.arrayToList(subaddressIndices);
    validate();
    return this;
  }

  public List<MoneroDestination> getDestinations() {
    return destinations;
  }

  public MoneroTransferQuery setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }

  @JsonProperty("hasDestinations")
  public Boolean hasDestinations() {
    return hasDestinations;
  }

  public MoneroTransferQuery setHasDestinations(Boolean hasDestinations) {
    this.hasDestinations = hasDestinations;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroTransfer transfer) {
    return meetsCriteria(transfer, true);
  }

  protected boolean meetsCriteria(MoneroTransfer transfer, boolean queryParent) {
    GenUtils.assertNotNull("transfer is null", transfer);
    
    // filter on common fields
    if (this.isIncoming() != null && this.isIncoming() != transfer.isIncoming()) return false;
    if (this.isOutgoing() != null && this.isOutgoing() != transfer.isOutgoing()) return false;
    if (this.getAmount() != null && this.getAmount().compareTo(transfer.getAmount()) != 0) return false;
    if (this.getAccountIndex() != null && !this.getAccountIndex().equals(transfer.getAccountIndex())) return false;
    
    // filter on incoming fields
    if (transfer instanceof MoneroIncomingTransfer) {
      if (Boolean.TRUE.equals(this.hasDestinations())) return false;
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
      if (getAddress() != null && (outTransfer.getAddresses() == null || !outTransfer.getAddresses().contains(this.getAddress()))) return false;   // TODO: will filter all transfers if they don't contain addresses
      if (getAddresses() != null) {
        List<String> intersections = new ArrayList<String>(this.getAddresses());
        intersections.retainAll(outTransfer.getAddresses());
        if (intersections.isEmpty()) return false;  // must have overlapping addresses
      }
      
      // filter on subaddress indices
      if (getSubaddressIndex() != null && (outTransfer.getSubaddressIndices() == null || !outTransfer.getSubaddressIndices().contains(this.getSubaddressIndex()))) return false;
      if (getSubaddressIndices() != null) {
        List<Integer> intersections = new ArrayList<Integer>(this.getSubaddressIndices());
        intersections.retainAll(outTransfer.getSubaddressIndices());
        if (intersections.isEmpty()) return false;  // must have overlapping subaddress indices
      }
      
      // filter on having destinations
      if (hasDestinations() != null) {
        if (hasDestinations() && outTransfer.getDestinations() == null) return false;
        if (!hasDestinations() && outTransfer.getDestinations() != null) return false;
      }
      
      // filter on destinations TODO: start with test for this
//    if (this.getDestionations() != null && this.getDestionations() != transfer.getDestionations()) return false;
    }
    
    // otherwise invalid type
    else throw new RuntimeException("Transfer must be MoneroIncomingTransfer or MoneroOutgoingTransfer");
    
    // filter with tx filter
    if (queryParent && getTxQuery() != null && !getTxQuery().meetsCriteria(transfer.getTx(), false)) return false;
    return true;
  }
  
  private void validate() {
    if (subaddressIndex != null && subaddressIndex < 0) throw new MoneroError("Subaddress index must be >= 0");
    if (subaddressIndices != null) for (Integer subaddressIdx : subaddressIndices) if (subaddressIdx < 0) throw new MoneroError("Subaddress indices must be >= 0");
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTransferQuery setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroTransferQuery setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroTransferQuery setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }
}
