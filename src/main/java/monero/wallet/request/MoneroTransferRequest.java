package monero.wallet.request;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import common.types.Filter;
import common.utils.GenUtils;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroIncomingTransfer;
import monero.wallet.model.MoneroOutgoingTransfer;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;

/**
 * Configures a request to retrieve transfers.
 * 
 * All transfers are returned except those that do not meet the criteria defined in this request.
 */
public class MoneroTransferRequest extends MoneroTransfer implements Filter<MoneroTransfer> {

  private Boolean isIncoming;
  private String address;
  private List<String> addresses;
  private Integer subaddressIndex;
  private List<Integer> subaddressIndices;
  private List<MoneroDestination> destinations;
  private Boolean hasDestinations;
  private MoneroTxRequest txRequest;

  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public MoneroTransferRequest setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  public Boolean getIsOutgoing() {
    return isIncoming == null ? null : !isIncoming;
  }
  
  public MoneroTransferRequest setIsOutgoing(Boolean isOutgoing) {
    isIncoming = isOutgoing == null ? null : !isOutgoing;
    return this;
  }

  public String getAddress() {
    return address;
  }

  public MoneroTransferRequest setAddress(String address) {
    this.address = address;
    return this;
  }

  public List<String> getAddresses() {
    return addresses;
  }

  public MoneroTransferRequest setAddresses(List<String> addresses) {
    this.addresses = addresses;
    return this;
  }
  
  public MoneroTransferRequest setAddresses(String... addresses) {
    this.addresses = GenUtils.arrayToList(addresses);
    return this;
  }

  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }

  public MoneroTransferRequest setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    return this;
  }

  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroTransferRequest setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public MoneroTransferRequest setSubaddressIndices(Integer... subaddressIndices) {
    this.subaddressIndices = GenUtils.arrayToList(subaddressIndices);
    return this;
  }

  public List<MoneroDestination> getDestinations() {
    return destinations;
  }

  public MoneroTransferRequest setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }

  public Boolean getHasDestinations() {
    return hasDestinations;
  }

  public MoneroTransferRequest setHasDestinations(Boolean hasDestinations) {
    this.hasDestinations = hasDestinations;
    return this;
  }

  public MoneroTxRequest getTxRequest() {
    return txRequest;
  }

  public MoneroTransferRequest setTxRequest(MoneroTxRequest txRequest) {
    this.txRequest = txRequest;
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
    if (this.getTxRequest() != null && !this.getTxRequest().meetsCriteria(transfer.getTx())) return false;    
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTransferRequest setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroTransferRequest setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroTransferRequest setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }
}
