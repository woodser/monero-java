package monero.wallet.config;

import java.math.BigInteger;
import java.util.List;

import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroSendPriority;

/**
 * Common configuration for sending, sweeping, and creation of payment URIs.
 * 
 * TODO: allow setAddress(), setAmount() for default destination?
 */
public class MoneroSendConfig {

  private List<MoneroDestination> destinations;
  private String paymentId;
  private MoneroSendPriority priority;
  private Integer mixin;
  private BigInteger fee;
  private Integer accountIndex;
  private List<Integer> subaddressIndices;
  private Integer unlockTime;
  private Boolean canSplit;
  private Boolean doNotRelay;
  private String note;
  private String recipientName;
  private BigInteger belowAmount;
  private Boolean sweepEachSubaddress;
  private String keyImage;
  
  public MoneroSendConfig() {
    this(null, null);
  }
  
  public MoneroSendConfig(String address) {
    this(address, null);
  }
  
  public MoneroSendConfig(String address, BigInteger amount) {
    this(address, amount, null);
  }
  
  public MoneroSendConfig(String address, BigInteger amount, MoneroSendPriority priority) {
    throw new Error("Not implemented");
  }
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  public MoneroSendConfig setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }
  
  public String getPaymentId() {
    return paymentId;
  }
  
  public MoneroSendConfig setPaymentId(String paymentId) {
    this.paymentId = paymentId;
    return this;
  }
  
  public MoneroSendPriority getPriority() {
    return priority;
  }
  
  public MoneroSendConfig setPriority(MoneroSendPriority priority) {
    this.priority = priority;
    return this;
  }
  
  public Integer getMixin() {
    return mixin;
  }
  
  public MoneroSendConfig setMixin(Integer mixin) {
    this.mixin = mixin;
    return this;
  }
  
  public BigInteger getFee() {
    return fee;
  }
  
  public MoneroSendConfig setFee(BigInteger fee) {
    this.fee = fee;
    return this;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public MoneroSendConfig setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
    return this;
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }
  
  public MoneroSendConfig setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public Integer getUnlockTime() {
    return unlockTime;
  }

  public MoneroSendConfig setUnlockTime(Integer unlockTime) {
    this.unlockTime = unlockTime;
    return this;
  }

  public Boolean getCanSplit() {
    return canSplit;
  }
  
  public MoneroSendConfig setCanSplit(Boolean canSplit) {
    this.canSplit = canSplit;
    return this;
  }
  
  public Boolean getDoNotRelay() {
    return doNotRelay;
  }
  
  public MoneroSendConfig setDoNotRelay(Boolean doNotRelay) {
    this.doNotRelay = doNotRelay;
    return this;
  }
  
  public String getNote() {
    return note;
  }
  
  public MoneroSendConfig setNote(String note) {
    this.note = note;
    return this;
  }
  
  public String getRecipientName() {
    return recipientName;
  }
  
  public MoneroSendConfig setRecipientName(String recipientName) {
    this.recipientName = recipientName;
    return this;
  }
  
  public BigInteger getBelowAmount() {
    return belowAmount;
  }
  
  public MoneroSendConfig setBelowAmount(BigInteger belowAmount) {
    this.belowAmount = belowAmount;
    return this;
  }
  
  public Boolean getSweepEachSubaddress() {
    return sweepEachSubaddress;
  }
  
  public MoneroSendConfig setSweepEachSubaddress(Boolean sweepEachSubaddress) {
    this.sweepEachSubaddress = sweepEachSubaddress;
    return this;
  }
  
  public String getKeyImage() {
    return keyImage;
  }
  
  public MoneroSendConfig setKeyImage(String keyImage) {
    this.keyImage = keyImage;
    return this;
  }
}
