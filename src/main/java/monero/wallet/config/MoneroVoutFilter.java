package monero.wallet.config;

import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroOutputWallet;

/**
 * Filters transfers that don't match initialized filter criteria.
 */
public class MoneroVoutFilter extends MoneroOutputWallet implements Filter<MoneroOutputWallet> {

  private List<Integer> subaddressIndices;
  private MoneroTxFilter txFilter;
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroVoutFilter setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }

  public MoneroTxFilter getTxFilter() {
    return txFilter;
  }

  public MoneroVoutFilter setTxFilter(MoneroTxFilter txFilter) {
    this.txFilter = txFilter;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroOutputWallet item) {
    throw new RuntimeException("Not implemented");
  }
}
