package monero.wallet.config;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import common.utils.GenUtils;
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
  private Integer ringSize;
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
    // map address and amount to single destination
    if (address != null || amount != null) {
      this.destinations = Arrays.asList(new MoneroDestination(address, amount));
    }
    this.priority = priority;
  }
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  public MoneroSendConfig setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }
  
  public MoneroSendConfig setDestinations(MoneroDestination... destinations) {
    this.destinations = GenUtils.arrayToList(destinations);
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
  
  public Integer getRingSize() {
    return ringSize;
  }
  
  public MoneroSendConfig setRingSize(Integer ringSize) {
    this.ringSize = ringSize;
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
  
  public MoneroSendConfig setSubaddressIndices(Integer... subaddressIndices) {
    this.subaddressIndices = GenUtils.arrayToList(subaddressIndices);
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

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((belowAmount == null) ? 0 : belowAmount.hashCode());
    result = prime * result + ((canSplit == null) ? 0 : canSplit.hashCode());
    result = prime * result + ((destinations == null) ? 0 : destinations.hashCode());
    result = prime * result + ((doNotRelay == null) ? 0 : doNotRelay.hashCode());
    result = prime * result + ((fee == null) ? 0 : fee.hashCode());
    result = prime * result + ((keyImage == null) ? 0 : keyImage.hashCode());
    result = prime * result + ((mixin == null) ? 0 : mixin.hashCode());
    result = prime * result + ((note == null) ? 0 : note.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    result = prime * result + ((priority == null) ? 0 : priority.hashCode());
    result = prime * result + ((recipientName == null) ? 0 : recipientName.hashCode());
    result = prime * result + ((ringSize == null) ? 0 : ringSize.hashCode());
    result = prime * result + ((subaddressIndices == null) ? 0 : subaddressIndices.hashCode());
    result = prime * result + ((sweepEachSubaddress == null) ? 0 : sweepEachSubaddress.hashCode());
    result = prime * result + ((unlockTime == null) ? 0 : unlockTime.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroSendConfig other = (MoneroSendConfig) obj;
    if (accountIndex == null) {
      if (other.accountIndex != null) return false;
    } else if (!accountIndex.equals(other.accountIndex)) return false;
    if (belowAmount == null) {
      if (other.belowAmount != null) return false;
    } else if (!belowAmount.equals(other.belowAmount)) return false;
    if (canSplit == null) {
      if (other.canSplit != null) return false;
    } else if (!canSplit.equals(other.canSplit)) return false;
    if (destinations == null) {
      if (other.destinations != null) return false;
    } else if (!destinations.equals(other.destinations)) return false;
    if (doNotRelay == null) {
      if (other.doNotRelay != null) return false;
    } else if (!doNotRelay.equals(other.doNotRelay)) return false;
    if (fee == null) {
      if (other.fee != null) return false;
    } else if (!fee.equals(other.fee)) return false;
    if (keyImage == null) {
      if (other.keyImage != null) return false;
    } else if (!keyImage.equals(other.keyImage)) return false;
    if (mixin == null) {
      if (other.mixin != null) return false;
    } else if (!mixin.equals(other.mixin)) return false;
    if (note == null) {
      if (other.note != null) return false;
    } else if (!note.equals(other.note)) return false;
    if (paymentId == null) {
      if (other.paymentId != null) return false;
    } else if (!paymentId.equals(other.paymentId)) return false;
    if (priority != other.priority) return false;
    if (recipientName == null) {
      if (other.recipientName != null) return false;
    } else if (!recipientName.equals(other.recipientName)) return false;
    if (ringSize == null) {
      if (other.ringSize != null) return false;
    } else if (!ringSize.equals(other.ringSize)) return false;
    if (subaddressIndices == null) {
      if (other.subaddressIndices != null) return false;
    } else if (!subaddressIndices.equals(other.subaddressIndices)) return false;
    if (sweepEachSubaddress == null) {
      if (other.sweepEachSubaddress != null) return false;
    } else if (!sweepEachSubaddress.equals(other.sweepEachSubaddress)) return false;
    if (unlockTime == null) {
      if (other.unlockTime != null) return false;
    } else if (!unlockTime.equals(other.unlockTime)) return false;
    return true;
  }
}
