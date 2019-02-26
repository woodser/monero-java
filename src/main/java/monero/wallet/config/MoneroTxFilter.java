package monero.wallet.config;

import java.util.List;

import common.types.Filter;
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

  public void setTxIds(List<String> txIds) {
    this.txIds = txIds;
  }

  public Boolean getHasPaymentId() {
    return hasPaymentId;
  }

  public void setHasPaymentId(Boolean hasPaymentId) {
    this.hasPaymentId = hasPaymentId;
  }

  public List<String> getPaymentIds() {
    return paymentIds;
  }

  public void setPaymentIds(List<String> paymentIds) {
    this.paymentIds = paymentIds;
  }

  public Integer getHeight() {
    return height;
  }

  public void setHeight(Integer height) {
    this.height = height;
  }

  public Integer getMinHeight() {
    return minHeight;
  }

  public void setMinHeight(Integer minHeight) {
    this.minHeight = minHeight;
  }

  public Integer getMaxHeight() {
    return maxHeight;
  }

  public void setMaxHeight(Integer maxHeight) {
    this.maxHeight = maxHeight;
  }

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

  public MoneroTransferFilter getTransferFilter() {
    return transferFilter;
  }

  public void setTransferFilter(MoneroTransferFilter transferFilter) {
    this.transferFilter = transferFilter;
  }

  public MoneroWalletTx getTx() {
    return tx;
  }

  public void setTx(MoneroWalletTx tx) {
    this.tx = tx;
  }

  @Override
  public boolean meetsCriteria(MoneroWalletTx item) {
    throw new RuntimeException("Not implemented");
  }
}
