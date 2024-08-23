package monero.wallet.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroError;

/**
 * Configures a transaction to send, sweep, or create a payment URI.
 */
public class MoneroTxConfig {

  private List<MoneroDestination> destinations;
  private List<Integer> subtractFeeFrom;
  private String paymentId;
  private MoneroTxPriority priority;
  private BigInteger fee;
  private Integer accountIndex;
  private List<Integer> subaddressIndices;
  private Boolean canSplit;
  private Boolean relay;
  private String note;
  private String recipientName;
  private BigInteger belowAmount;
  private Boolean sweepEachSubaddress;
  private String keyImage;
  
  public MoneroTxConfig() {
    // necessary for deserialization
  }
  
  MoneroTxConfig(final MoneroTxConfig config) {
    if (config.destinations != null) {
      this.destinations = new ArrayList<MoneroDestination>();
      for (MoneroDestination destination : config.getDestinations()) this.destinations.add(destination.copy());
    }
    this.subtractFeeFrom = config.subtractFeeFrom;
    this.paymentId = config.paymentId;
    this.priority = config.priority;
    this.fee = config.fee;
    this.accountIndex = config.accountIndex;
    if (config.subaddressIndices != null) this.subaddressIndices = new ArrayList<Integer>(config.subaddressIndices);
    this.canSplit = config.canSplit;
    this.relay = config.relay;
    this.note = config.note;
    this.recipientName = config.recipientName;
    this.belowAmount = config.belowAmount;
    this.sweepEachSubaddress = config.sweepEachSubaddress;
    this.keyImage = config.keyImage;
  }
  
  public MoneroTxConfig copy() {
    return new MoneroTxConfig(this);
  }
  
  /**
   * Set the address of a single-destination configuration.
   * 
   * @param address - the address to set for the single destination
   * @return MoneroTxConfig this configuration for chaining
   */
  public MoneroTxConfig setAddress(String address) {
    if (this.destinations != null && this.destinations.size() > 1) throw new MoneroError("Cannot set address because MoneroTxConfig already has multiple destinations");
    if (this.destinations == null || this.destinations.isEmpty()) addDestination(new MoneroDestination(address));
    else this.destinations.get(0).setAddress(address);
    return this;
  }
  
  /**
   * Get the address of a single-destination configuration.
   * 
   * @return String the address of the single destination
   */
  @JsonIgnore
  public String getAddress() {
    if (this.destinations == null || this.destinations.size() != 1) throw new MoneroError("Cannot get address because MoneroTxConfig does not have exactly one destination");
    return this.destinations.get(0).getAddress();
  }
  
  /**
   * Set the amount of a single-destination configuration.
   * 
   * @param amount - the amount to set for the single destination
   * @return MoneroTxConfig this configuration for chaining
   */
  public MoneroTxConfig setAmount(BigInteger amount) {
    if (this.destinations != null && this.destinations.size() > 1) throw new MoneroError("Cannot set amount because MoneroTxConfig already has multiple destinations");
    if (this.destinations == null || this.destinations.isEmpty()) addDestination(new MoneroDestination(null, amount));
    else this.destinations.get(0).setAmount(amount);
    return this;
  }
  
  /**
   * Set the amount of a single-destination configuration.
   * 
   * @param amount - the amount to set for the single destination in atomic units as a string
   * @return MoneroTxConfig this configuration for chaining
   */
  public MoneroTxConfig setAmount(String amount) {
    return setAmount(new BigInteger(amount));
  }
  
  /**
   * Get the amount of a single-destination configuration.
   * 
   * @return BigInteger the amount of the single destination
   */
  @JsonIgnore
  public BigInteger getAmount() {
    if (this.destinations == null || this.destinations.size() != 1) throw new MoneroError("Cannot get amount because MoneroTxConfig does not have exactly one destination");
    return this.destinations.get(0).getAmount();
  }
  
  public MoneroTxConfig addDestination(String address, BigInteger amount) {
    return addDestination(new MoneroDestination(address, amount));
  }
  
  public MoneroTxConfig addDestination(MoneroDestination destination) {
    if (this.destinations == null) this.destinations = new ArrayList<MoneroDestination>();
    this.destinations.add(destination);
    return this;
  }
  
  public List<MoneroDestination> getDestinations() {
    return destinations;
  }
  
  @JsonProperty("destinations")
  public MoneroTxConfig setDestinations(List<MoneroDestination> destinations) {
    this.destinations = destinations;
    return this;
  }
  
  public MoneroTxConfig setDestinations(MoneroDestination... destinations) {
    this.destinations = GenUtils.arrayToList(destinations);
    return this;
  }
  
  public List<Integer> getSubtractFeeFrom() {
    return subtractFeeFrom;
  }

  @JsonProperty("subtractFeeFrom")
  public MoneroTxConfig setSubtractFeeFrom(List<Integer> destinationIndices) {
    this.subtractFeeFrom = destinationIndices;
    return this;
  }
  
  public MoneroTxConfig setSubtractFeeFrom(Integer... destinationIndices) {
    return setSubtractFeeFrom(GenUtils.arrayToList(destinationIndices));
  }
  
  public String getPaymentId() {
    return paymentId;
  }
  
  public MoneroTxConfig setPaymentId(String paymentId) {
    this.paymentId = paymentId;
    return this;
  }
  
  public MoneroTxPriority getPriority() {
    return priority;
  }
  
  public MoneroTxConfig setPriority(MoneroTxPriority priority) {
    this.priority = priority;
    return this;
  }
  
  public BigInteger getFee() {
    return fee;
  }
  
  public MoneroTxConfig setFee(BigInteger fee) {
    this.fee = fee;
    return this;
  }
  
  public Integer getAccountIndex() {
    return accountIndex;
  }
  
  public MoneroTxConfig setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
    return this;
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }
  
  public MoneroTxConfig setSubaddressIndex(Integer subaddressIndex) {
    setSubaddressIndices(subaddressIndex);
    return this;
  }
  
  @JsonProperty("subaddressIndices")
  public MoneroTxConfig setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public MoneroTxConfig setSubaddressIndices(Integer... subaddressIndices) {
    List<Integer> asList = subaddressIndices == null ? null : GenUtils.arrayToList(subaddressIndices);
    if (asList.size() == 1 && asList.get(0) == null) asList = null; // ...null becomes [null] so change to null
    return setSubaddressIndices(asList);
  }

  public Boolean getCanSplit() {
    return canSplit;
  }
  
  public MoneroTxConfig setCanSplit(Boolean canSplit) {
    this.canSplit = canSplit;
    return this;
  }
  
  public Boolean getRelay() {
    return relay;
  }
  
  public MoneroTxConfig setRelay(Boolean relay) {
    this.relay = relay;
    return this;
  }
  
  public String getNote() {
    return note;
  }
  
  public MoneroTxConfig setNote(String note) {
    this.note = note;
    return this;
  }
  
  public String getRecipientName() {
    return recipientName;
  }
  
  public MoneroTxConfig setRecipientName(String recipientName) {
    this.recipientName = recipientName;
    return this;
  }
  
  public BigInteger getBelowAmount() {
    return belowAmount;
  }
  
  public MoneroTxConfig setBelowAmount(BigInteger belowAmount) {
    this.belowAmount = belowAmount;
    return this;
  }
  
  public Boolean getSweepEachSubaddress() {
    return sweepEachSubaddress;
  }
  
  public MoneroTxConfig setSweepEachSubaddress(Boolean sweepEachSubaddress) {
    this.sweepEachSubaddress = sweepEachSubaddress;
    return this;
  }
  
  public String getKeyImage() {
    return keyImage;
  }
  
  public MoneroTxConfig setKeyImage(String keyImage) {
    this.keyImage = keyImage;
    return this;
  }
  
  @Override
  public String toString() {
    return JsonUtils.serialize(this);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((belowAmount == null) ? 0 : belowAmount.hashCode());
    result = prime * result + ((canSplit == null) ? 0 : canSplit.hashCode());
    result = prime * result + ((destinations == null) ? 0 : destinations.hashCode());
    result = prime * result + ((fee == null) ? 0 : fee.hashCode());
    result = prime * result + ((keyImage == null) ? 0 : keyImage.hashCode());
    result = prime * result + ((note == null) ? 0 : note.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    result = prime * result + ((priority == null) ? 0 : priority.hashCode());
    result = prime * result + ((recipientName == null) ? 0 : recipientName.hashCode());
    result = prime * result + ((relay == null) ? 0 : relay.hashCode());
    result = prime * result + ((subaddressIndices == null) ? 0 : subaddressIndices.hashCode());
    result = prime * result + ((subtractFeeFrom == null) ? 0 : subtractFeeFrom.hashCode());
    result = prime * result + ((sweepEachSubaddress == null) ? 0 : sweepEachSubaddress.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTxConfig other = (MoneroTxConfig) obj;
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
    if (fee == null) {
      if (other.fee != null) return false;
    } else if (!fee.equals(other.fee)) return false;
    if (keyImage == null) {
      if (other.keyImage != null) return false;
    } else if (!keyImage.equals(other.keyImage)) return false;
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
    if (relay == null) {
      if (other.relay != null) return false;
    } else if (!relay.equals(other.relay)) return false;
    if (subaddressIndices == null) {
      if (other.subaddressIndices != null) return false;
    } else if (!subaddressIndices.equals(other.subaddressIndices)) return false;
    if (subtractFeeFrom == null) {
      if (other.subtractFeeFrom != null) return false;
    } else if (!subtractFeeFrom.equals(other.subtractFeeFrom)) return false;
    if (sweepEachSubaddress == null) {
      if (other.sweepEachSubaddress != null) return false;
    } else if (!sweepEachSubaddress.equals(other.sweepEachSubaddress)) return false;
    return true;
  }
}
