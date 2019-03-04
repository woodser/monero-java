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
  private Integer accountIndex;
  private List<Integer> subaddressIndices;
  private MoneroTxFilter txFilter;
  private MoneroTransfer transfer;
  
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
  
  public Integer getAccountIndex() {
    return accountIndex;
  }

  public MoneroTransferFilter setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
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
  
  public MoneroTransfer getTransfer() {
    return transfer;
  }
  
  public MoneroTransferFilter setTransfer(MoneroTransfer transfer) {
    this.transfer = transfer;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroTransfer item) {
    throw new RuntimeException("Not implemented");
  }
}
