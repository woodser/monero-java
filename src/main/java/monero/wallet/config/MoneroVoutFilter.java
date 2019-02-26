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

  public void setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
  }

  public MoneroTxFilter getTxFilter() {
    return txFilter;
  }

  public void setTxFilter(MoneroTxFilter txFilter) {
    this.txFilter = txFilter;
  }

  public MoneroWalletOutput getVout() {
    return vout;
  }

  public void setVout(MoneroWalletOutput vout) {
    this.vout = vout;
  }

  @Override
  public boolean meetsCriteria(MoneroWalletOutput item) {
    throw new RuntimeException("Not implemented");
  }
}
