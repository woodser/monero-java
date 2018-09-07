package model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import model.MoneroTx.MoneroTxPriority;

/**
 * Configuration for sending a transaction.
 */
public class MoneroTxConfig {

  private List<MoneroPayment> destinations;
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
    unlockTime = 0;
    paymentId = null;
    priority = null;
    doNotRelay = null;
  }
  
  /**
   * Convenience constructor to specify transaction destination and amount with defaults.
   * 
   * @param address is the destination address
   * @param paymentId is the destination payment id
   * @param amount is the amount to send
   */
  public MoneroTxConfig(String address, String paymentId, BigInteger amount) {
    destinations = new ArrayList<MoneroPayment>();
    destinations.add(new MoneroPayment(address, paymentId, amount));
    
  }

  public MoneroTxConfig(List<MoneroPayment> destinations, Integer accountIdx, Collection<Integer> subaddressIndices, BigInteger fee, Integer mixin, Integer unlockTime, String paymentId, MoneroTxPriority priority, Boolean doNotRelay) {
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

  public List<MoneroPayment> getDestinations() {
    return destinations;
  }

  public void setDestinations(List<MoneroPayment> destinations) {
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
