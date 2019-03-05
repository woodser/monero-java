package monero.wallet.config;

import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroTransfer;

/**
 * Filters transfers that don't meet initialized filter criteria.
 */
public class MoneroTransferFilter extends MoneroTransfer implements Filter<MoneroTransfer> {

  private List<Integer> subaddressIndices;
  private Boolean hasDestinations;
  private MoneroTxFilter txFilter;
  
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
  public boolean meetsCriteria(MoneroTransfer item) {
    throw new RuntimeException("Not implemented");
  }
}
