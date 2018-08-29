package model;

import java.math.BigInteger;
import java.util.Collection;

import model.MoneroTx.MoneroTxPriority;

/**
 * Configuration for sending a transaction.
 */
public class MoneroTxConfig {

  private Collection<MoneroPayment> destinations;
  private Integer accountIdx;
  private Collection<Integer> subaddressIndices;
  private BigInteger fee;
  private Integer mixin;
  private Integer unlockTime;
  private String paymentId;
  private MoneroTxPriority priority;
  private Boolean doNotRelay;
  
  public MoneroTxConfig() {
    super();
    destinations = null;
    accountIdx = null;
    subaddressIndices = null;
    fee = null;
    mixin = null;
    unlockTime = null;
    paymentId = null;
    priority = null;
    doNotRelay = null;
  }

  public MoneroTxConfig(Collection<MoneroPayment> destinations, Integer accountIdx, Collection<Integer> subaddressIndices, BigInteger fee, Integer mixin, Integer unlockTime, String paymentId, MoneroTxPriority priority, Boolean doNotRelay) {
    super();
    this.destinations = destinations;
    this.accountIdx = accountIdx;
    this.subaddressIndices = subaddressIndices;
    this.fee = fee;
    this.mixin = mixin;
    this.unlockTime = unlockTime;
    this.paymentId = paymentId;
    this.priority = priority;
    this.doNotRelay = doNotRelay;
  }

  public Collection<MoneroPayment> getDestinations() {
    return destinations;
  }

  public void setDestinations(Collection<MoneroPayment> destinations) {
    this.destinations = destinations;
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

  public BigInteger getFee() {
    return fee;
  }

  public void setFee(BigInteger fee) {
    this.fee = fee;
  }

  public Integer getMixin() {
    return mixin;
  }

  public void setMixin(Integer mixin) {
    this.mixin = mixin;
  }

  public Integer getUnlockTime() {
    return unlockTime;
  }

  public void setUnlockTime(Integer unlockTime) {
    this.unlockTime = unlockTime;
  }

  public String getPaymentId() {
    return paymentId;
  }

  public void setPaymentId(String paymentId) {
    this.paymentId = paymentId;
  }

  public MoneroTxPriority getPriority() {
    return priority;
  }

  public void setPriority(MoneroTxPriority priority) {
    this.priority = priority;
  }

  public Boolean getDoNotRelay() {
    return doNotRelay;
  }

  public void setDoNotRelay(Boolean doNotRelay) {
    this.doNotRelay = doNotRelay;
  }
}
