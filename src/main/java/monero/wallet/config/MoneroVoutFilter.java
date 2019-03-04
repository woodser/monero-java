package monero.wallet.config;

import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroWalletOutput;

/**
 * Filters transfers that don't match initialized filter criteria.
 */
public class MoneroVoutFilter extends Filter<MoneroWalletOutput> {

  private List<Integer> subaddressIndices;
  private MoneroTxFilter txFilter;
  private MoneroWalletOutput vout;
  
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

  public MoneroWalletOutput getVout() {
    return vout;
  }

  public MoneroVoutFilter setVout(MoneroWalletOutput vout) {
    this.vout = vout;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroWalletOutput item) {
    throw new RuntimeException("Not implemented");
  }
}
