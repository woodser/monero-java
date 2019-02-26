package monero.wallet.config;

import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroTransfer;

/**
 * Filters transfers that don't match initialized filter criteria.
 */
public class MoneroTransferFilter extends Filter<MoneroTransfer> {

  private Boolean isOutgoing;
  private Boolean isIncoming;
  private Boolean hasDestinations;
  private List<Integer> subaddressIndices;
  private MoneroTxFilter txFilter;
  private MoneroTransfer transfer;
  
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
  
  public Boolean getHasDestinations() {
    return hasDestinations;
  }
  
  public void setHasDestinations(Boolean hasDestinations) {
    this.hasDestinations = hasDestinations;
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }
  
  public void setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
  }
  
  public MoneroTxFilter getTxFilter() {
    return txFilter;
  }
  
  public void setTxFilter(MoneroTxFilter txFilter) {
    this.txFilter = txFilter;
  }
  
  public MoneroTransfer getTransfer() {
    return transfer;
  }
  
  public void setTransfer(MoneroTransfer transfer) {
    this.transfer = transfer;
  }

  @Override
  public boolean meetsCriteria(MoneroTransfer item) {
    throw new RuntimeException("Not implemented");
  }
}
