package monero.wallet.config;

import java.util.Arrays;
import java.util.List;

import common.types.Filter;
import common.utils.JsonUtils;
import monero.wallet.model.MoneroTxWallet;

/**
 * Filters transactions that don't meet initialized filter criteria.
 */
public class MoneroTxFilter extends MoneroTxWallet implements Filter<MoneroTxWallet> {
  
  private Boolean isOutgoing;
  private Boolean isIncoming;
  private List<String> txIds;
  private Boolean hasPaymentId;
  private List<String> paymentIds;
  private Integer minHeight;
  private Integer maxHeight;
  private Boolean includeVouts;
  private MoneroTransferFilter transferFilter;
  
  public Boolean getIsOutgoing() {
    return isOutgoing;
  }

  public MoneroTxFilter setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }

  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public MoneroTxFilter setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }

  public List<String> getTxIds() {
    return txIds;
  }

  public MoneroTxFilter setTxIds(List<String> txIds) {
    this.txIds = txIds;
    return this;
  }
  
  public MoneroTxFilter setTxId(String txId) {
    return setTxIds(Arrays.asList(txId));
  }

  public Boolean getHasPaymentId() {
    return hasPaymentId;
  }

  public MoneroTxFilter setHasPaymentId(Boolean hasPaymentId) {
    this.hasPaymentId = hasPaymentId;
    return this;
  }

  public List<String> getPaymentIds() {
    return paymentIds;
  }

  public MoneroTxFilter setPaymentIds(List<String> paymentIds) {
    this.paymentIds = paymentIds;
    return this;
  }
  
  public MoneroTxFilter setPaymentId(String paymentId) {
    return setPaymentIds(Arrays.asList(paymentId));
  }

  public Integer getMinHeight() {
    return minHeight;
  }

  public MoneroTxFilter setMinHeight(Integer minHeight) {
    this.minHeight = minHeight;
    return this;
  }

  public Integer getMaxHeight() {
    return maxHeight;
  }

  public MoneroTxFilter setMaxHeight(Integer maxHeight) {
    this.maxHeight = maxHeight;
    return this;
  }

  public Boolean getIncludeVouts() {
    return includeVouts;
  }

  public MoneroTxFilter setIncludeVouts(Boolean includeVouts) {
    this.includeVouts = includeVouts;
    return this;
  }

  public MoneroTransferFilter getTransferFilter() {
    return transferFilter;
  }

  public MoneroTxFilter setTransferFilter(MoneroTransferFilter transferFilter) {
    this.transferFilter = transferFilter;
    return this;
  }
  
  @Override
  public boolean meetsCriteria(MoneroTxWallet item) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public String toString() {
    return JsonUtils.serialize(this);
  }
}
