package monero.wallet.config;

import java.util.Arrays;
import java.util.List;

import common.types.Filter;
import common.utils.JsonUtils;
import monero.wallet.model.MoneroWalletTx;

/**
 * Filters transactions that don't match initialized filter criteria.
 */
public class MoneroTxFilter extends Filter<MoneroWalletTx> {
  
  private List<String> txIds;
  private Boolean hasPaymentId;
  private List<String> paymentIds;
  private Integer height;
  private Integer minHeight;
  private Integer maxHeight;
  private Boolean isOutgoing;
  private Boolean isIncoming;
  private MoneroTransferFilter transferFilter;
  private MoneroWalletTx tx;

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

  public Integer getHeight() {
    return height;
  }

  public MoneroTxFilter setHeight(Integer height) {
    this.height = height;
    return this;
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

  public MoneroTransferFilter getTransferFilter() {
    return transferFilter;
  }

  public MoneroTxFilter setTransferFilter(MoneroTransferFilter transferFilter) {
    this.transferFilter = transferFilter;
    return this;
  }

  public MoneroWalletTx getTx() {
    return tx;
  }

  public MoneroTxFilter setTx(MoneroWalletTx tx) {
    this.tx = tx;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroWalletTx item) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public String toString() {
    return JsonUtils.serialize(this);
  }
}
