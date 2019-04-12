package monero.wallet.config;

import java.math.BigInteger;
import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;

/**
 * Filters transfers that don't meet initialized filter criteria.
 */
public class MoneroTransferFilter extends MoneroTransfer implements Filter<MoneroTransfer> {

  private Boolean isOutgoing;
  private Boolean isIncoming;
  private List<Integer> subaddressIndices;
  private Boolean hasDestinations;
  private MoneroTxFilter txFilter;
  
  public Boolean getIsOutgoing() {
    return isOutgoing;
  }
  
  public MoneroTransferFilter setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }
  
  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public MoneroTransferFilter setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  public Boolean getHasDestinations() {
    return hasDestinations;
  }
  
  public MoneroTransferFilter setHasDestinations(Boolean hasDestinations) {
    this.hasDestinations = hasDestinations;
    return this;
  }

  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }
  
  public MoneroTransferFilter setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
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
    
    // filter on transfer fields
    if (this.getAddress() != null && !this.getAddress().equals(transfer.getAddress())) return false;
    if (this.getAccountIndex() != null && !this.getAccountIndex().equals(transfer.getAccountIndex())) return false;
    if (this.getSubaddressIndex() != null && !transfer.getIsOutgoing() && this.getSubaddressIndex() != transfer.getSubaddressIndex()) return false; // outgoing subaddresses are always 0 TODO monero-wallet-rpc: possible to return correct subaddress?
    if (this.getAmount() != null && this.getAmount().compareTo(transfer.getAmount()) != 0) return false;
    
    // filter extensions
    if (this.getIsIncoming() != null && this.getIsIncoming() != transfer.getIsIncoming()) return false;
    if (this.getIsOutgoing() != null && this.getIsOutgoing() != transfer.getIsOutgoing()) return false;
    if (this.getSubaddressIndices() != null && !this.getSubaddressIndices().contains(transfer.getSubaddressIndex())) return false;
    if (this.getHasDestinations() != null) {
      if (this.getHasDestinations() && transfer.getDestinations() == null) return false;
      if (!this.getHasDestinations() && transfer.getDestinations() != null) return false;
    }
    
    // filter with transaction filter
    if (this.getTxFilter() != null && !this.getTxFilter().meetsCriteria(transfer.getTx())) return false;
    
    // filter on destinations TODO: start with test for this
//  if (this.getDestionations() != null && this.getDestionations() != transfer.getDestionations()) return false;
    
    // transfer meets filter criteria
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTransferFilter setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroTransferFilter setAddress(String address) {
    super.setAddress(address);
    return this;
  }

  @Override
  public MoneroTransferFilter setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }

  @Override
  public MoneroTransferFilter setSubaddressIndex(Integer subaddressIndex) {
    super.setSubaddressIndex(subaddressIndex);
    return this;
  }

  @Override
  public MoneroTransferFilter setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroTransferFilter setDestinations(List<MoneroDestination> destinations) {
    super.setDestinations(destinations);
    return this;
  }
}
