package monero.wallet.config;

import java.math.BigInteger;
import java.util.List;

import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroSendPriority;

/**
 * Common configuration for sending, sweeping, and creation of payment URIs.
 */
public class MoneroSendConfig {

  private List<MoneroDestination> destinations;
  private String paymentId;
  private MoneroSendPriority priority;
  private Integer mixin;
  private BigInteger fee;
  private Integer accountIndex;
  private List<Integer> subaddressIndices;
  private Boolean doNotRelay;
  private Boolean canSplit;
  private String note;
  private String recipientName;
  private BigInteger belowAmount;
  private Boolean sweepEachSubaddress;
  private String keyImage;
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  public void setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
  }
  
  public String getPaymentId() {
    return paymentId;
  }
  
  public void setPaymentId(String paymentId) {
    this.paymentId = paymentId;
  }
  
  public MoneroSendPriority getPriority() {
    return priority;
  }
  
  public void setPriority(MoneroSendPriority priority) {
    this.priority = priority;
  }
  
  public Integer getMixin() {
    return mixin;
  }
  
  public void setMixin(Integer mixin) {
    this.mixin = mixin;
  }
  
  public BigInteger getFee() {
    return fee;
  }
  
  public void setFee(BigInteger fee) {
    this.fee = fee;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public void setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }
  
  public void setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
  }
  
  public Boolean getDoNotRelay() {
    return doNotRelay;
  }
  
  public void setDoNotRelay(Boolean doNotRelay) {
    this.doNotRelay = doNotRelay;
  }
  
  public Boolean getCanSplit() {
    return canSplit;
  }
  
  public void setCanSplit(Boolean canSplit) {
    this.canSplit = canSplit;
  }
  
  public String getNote() {
    return note;
  }
  
  public void setNote(String note) {
    this.note = note;
  }
  
  public String getRecipientName() {
    return recipientName;
  }
  
  public void setRecipientName(String recipientName) {
    this.recipientName = recipientName;
  }
  
  public BigInteger getBelowAmount() {
    return belowAmount;
  }
  
  public void setBelowAmount(BigInteger belowAmount) {
    this.belowAmount = belowAmount;
  }
  
  public Boolean getSweepEachSubaddress() {
    return sweepEachSubaddress;
  }
  
  public void setSweepEachSubaddress(Boolean sweepEachSubaddress) {
    this.sweepEachSubaddress = sweepEachSubaddress;
  }
  
  public String getKeyImage() {
    return keyImage;
  }
  
  public void setKeyImage(String keyImage) {
    this.keyImage = keyImage;
  }
}
