package model;

import java.util.Collection;

/**
 * Specifies filter options when querying transactions.
 */
public class MoneroTxFilter {
  
  private boolean incoming;
  private boolean outgoing;
  private boolean pending;
  private boolean failed;
  private boolean memPool;
  private Integer minHeight;
  private Integer maxHeight;
  private Integer accountIdx;
  private Collection<Integer> subaddressIndices;
  private Collection<Integer> txIds;
  private Collection<String> paymentIds;
  
  public MoneroTxFilter(boolean incoming, boolean outgoing, boolean pending, boolean failed, boolean memPool, Integer minHeight, Integer maxHeight, Integer accountIdx, Collection<Integer> subaddressIndices, Collection<Integer> txIds, Collection<String> paymentIds) {
    super();
    this.incoming = incoming;
    this.outgoing = outgoing;
    this.pending = pending;
    this.failed = failed;
    this.memPool = memPool;
    this.minHeight = minHeight;
    this.maxHeight = maxHeight;
    this.accountIdx = accountIdx;
    this.subaddressIndices = subaddressIndices;
    this.txIds = txIds;
    this.paymentIds = paymentIds;
  }

  public boolean isIncoming() {
    return incoming;
  }

  public void setIncoming(boolean incoming) {
    this.incoming = incoming;
  }

  public boolean isOutgoing() {
    return outgoing;
  }

  public void setOutgoing(boolean outgoing) {
    this.outgoing = outgoing;
  }

  public boolean isPending() {
    return pending;
  }

  public void setPending(boolean pending) {
    this.pending = pending;
  }

  public boolean isFailed() {
    return failed;
  }

  public void setFailed(boolean failed) {
    this.failed = failed;
  }

  public boolean isMemPool() {
    return memPool;
  }

  public void setMemPool(boolean memPool) {
    this.memPool = memPool;
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

  public Integer getAccountIdx() {
    return accountIdx;
  }

  public void setAccountIdx(Integer accountIdx) {
    this.accountIdx = accountIdx;
  }

  public Collection<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public void setSubaddressIndices(Collection<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
  }

  public Collection<Integer> getTxIds() {
    return txIds;
  }

  public void setTxIds(Collection<Integer> txIds) {
    this.txIds = txIds;
  }

  public Collection<String> getPaymentIds() {
    return paymentIds;
  }

  public void setPaymentIds(Collection<String> paymentIds) {
    this.paymentIds = paymentIds;
  }
}
