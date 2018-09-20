
package monero.wallet.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Represents a transaction on the Monero network.
 */
public class MoneroTx {
  
  /**
   * Default payment id.
   */
  public static final String DEFAULT_PAYMENT_ID = "0000000000000000";
  
  /**
   * Enumerates Monero transaction priorities.
   */
  public enum MoneroTxPriority {
    DEFAULT,
    UNIMPORTANT,
    NORMAL,
    ELEVATED
  }
  
  /**
   * Enumerates Monero transaction types.
   */
  public enum MoneroTxType {
    INCOMING,
    OUTGOING,
    PENDING,
    FAILED,
    MEMPOOL
  }
  
  private String id; 
  private String address;
  private Integer accountIndex;
  private Integer subaddressIndex;
	private List<MoneroPayment> payments;
	private List<MoneroOutput> outputs;
	private String paymentId;
  private BigInteger amount;
	private BigInteger fee;
	private Integer mixin;
	private String key;
	private Integer size;
	private MoneroTxType type;
  private Integer height;
  private String note;
  private Long timestamp;
  private Integer unlockTime;
  private Boolean isDoubleSpend;
  private String blob;
  private String metadata;
	
	public MoneroTx() {
	  super();
	}
	
	public String getId() {
	  return id;
	}
	
	public void setId(String id) {
	  this.id = id;
	}

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public Integer getAccountIndex() {
    return accountIndex;
  }

  public void setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
  }

  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }

  public void setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
  }

  public List<MoneroPayment> getPayments() {
    return payments;
  }

  public void setPayments(List<MoneroPayment> payments) {
    this.payments = payments;
    if (payments != null) {
      for (MoneroPayment payment : payments) {
        payment.setTransaction(this);
      }
    }
  }
  
  public List<MoneroOutput> getOutputs() {
    return outputs;
  }

  public void setOutputs(List<MoneroOutput> outputs) {
    this.outputs = outputs;
    if (outputs != null) {
      for (MoneroOutput output : outputs) {
        output.setTransaction(this);
      }
    }
  }
  
  public String getPaymentId() {
    return paymentId;
  }

  public void setPaymentId(String paymentId) {
    this.paymentId = paymentId;
  }
  
  public BigInteger getAmount() {
    return amount;
  }

  public void setAmount(BigInteger amount) {
    this.amount = amount;
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

  public String getKey() {
    return key;
  }

  public void setKey(String key) {
    this.key = key;
  }

  public Integer getSize() {
    return size;
  }

  public void setSize(Integer size) {
    this.size = size;
  }

  public MoneroTxType getType() {
    return type;
  }

  public void setType(MoneroTxType type) {
    this.type = type;
  }
  
  public Integer getHeight() {
    return height;
  }

  public void setHeight(Integer height) {
    this.height = height;
  }

  public String getNote() {
    return note;
  }

  public void setNote(String note) {
    this.note = note;
  }

  public Long getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(Long timestamp) {
    this.timestamp = timestamp;
  }

  public Integer getUnlockTime() {
    return unlockTime;
  }

  public void setUnlockTime(Integer unlockTime) {
    this.unlockTime = unlockTime;
  }

  public Boolean getIsDoubleSpend() {
    return isDoubleSpend;
  }

  public void setIsDoubleSpend(Boolean isDoubleSpend) {
    this.isDoubleSpend = isDoubleSpend;
  }

  public String getBlob() {
    return blob;
  }

  public void setBlob(String blob) {
    this.blob = blob;
  }

  public String getMetadata() {
    return metadata;
  }

  public void setMetadata(String metadata) {
    this.metadata = metadata;
  }

  /**
   * Merges the given transaction into this transaction.
   * 
   * Appends payments and outputs. Sets uninitialized fields to the given transaction. Validates initialized fields are equal.
   * 
   * @param tx is the transaction to merge into this one
   */
  public void merge(MoneroTx tx) {
    if (id == null) id = tx.getId();
    else if (tx.getId() != null) validateEquals("IDs", id, tx.getId());
    if (address == null) address = tx.getAddress();
    else if (tx.getAddress() != null) validateEquals("Addresses", address, tx.getAddress());
    if (accountIndex == null) accountIndex = tx.getAccountIndex();
    else if (tx.getAccountIndex() != null) validateEquals("Account indices", accountIndex, tx.getAccountIndex());
    if (subaddressIndex == null) subaddressIndex = tx.getSubaddressIndex();
    else if (tx.getSubaddressIndex() != null) validateEquals("Subaddress indices", subaddressIndex, tx.getSubaddressIndex());
    if (payments == null) payments = tx.getPayments();
    else if (tx.getPayments() != null) payments.addAll(tx.getPayments());
    if (outputs == null) outputs = tx.getOutputs();
    else if (tx.getOutputs() != null) outputs.addAll(tx.getOutputs());
    if (paymentId == null) paymentId = tx.getPaymentId();
    else if (tx.getPaymentId() != null) validateEquals("Payment ids", paymentId, tx.getPaymentId());
    if (amount == null) amount = tx.getAmount();
    else if (tx.getAmount() != null) validateEquals("Amounts", amount, tx.getAmount());
    if (fee == null) fee = tx.getFee();
    else if (tx.getFee() != null) validateEquals("Fees", fee, tx.getFee());
    if (mixin == null) mixin = tx.getMixin();
    else if (tx.getMixin() != null) validateEquals("Mixins", mixin, tx.getMixin());
    if (key == null) key = tx.getKey();
    else if (tx.getKey() != null) validateEquals("Keys", key, tx.getKey());
    if (size == null) size = tx.getSize();
    else if (tx.getSize() != null) validateEquals("Sizes", size, tx.getSize());
    if (type == null) type = tx.getType();
    else if (tx.getType() != null) validateEquals("Types", type, tx.getType());
    if (height == null) height = tx.getHeight();
    else if (tx.getHeight() != null) validateEquals("Heights", height, tx.getHeight());
    if (note == null) note = tx.getNote();
    else if (tx.getNote() != null) validateEquals("Notes", note, tx.getNote());
    if (timestamp == null) timestamp = tx.getTimestamp();
    else if (tx.getTimestamp() != null) validateEquals("Timestamps", timestamp, tx.getTimestamp());
    if (unlockTime == null) unlockTime = tx.getUnlockTime();
    else if (tx.getUnlockTime() != null) validateEquals("Unlock times", unlockTime, tx.getUnlockTime());
    if (isDoubleSpend == null) isDoubleSpend = tx.getIsDoubleSpend();
    else if (tx.getIsDoubleSpend() != null) validateEquals("Is double spend", isDoubleSpend, tx.getIsDoubleSpend());
    if (blob == null) blob = tx.getBlob();
    else if (tx.getBlob() != null) validateEquals("Blobs", blob, tx.getBlob());
    if (metadata == null) metadata = tx.getMetadata();
    else if (tx.getMetadata() != null) validateEquals("Metadatas", metadata, tx.getMetadata());
  }
  
  private static void validateEquals(String fieldName, Object obj1, Object obj2) {
    if (!obj1.equals(obj2)) throw new MoneroException(fieldName + " are not equal: " + obj1 + " vs " + obj2);
  }
  
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ID: " + id + "\n");
    sb.append("Address: " + address + "\n");
    sb.append("Account index: " + accountIndex + "\n");
    sb.append("Subaddress index: " + subaddressIndex + "\n");
    sb.append("Key: " + key + "\n");
    if (payments != null) {
      sb.append("Payments:\n");
      for (int i = 0; i < payments.size(); i++) {
        sb.append("\t" + (i + 1) + ":\n");
        sb.append("\t\tAddress: " + payments.get(i).getAddress() + "\n");
        sb.append("\t\tAmount: " + payments.get(i).getAmount() + "\n");
      }
    }
    if (outputs != null) {
      sb.append("Outputs:\n");
      for (int i = 0; i < outputs.size(); i++) {
        sb.append("\t" + (i + 1) + ":\n");
        sb.append("\t\tAmount: " + outputs.get(i).getAmount() + "\n");
        sb.append("\t\tIs spent: " + outputs.get(i).isSpent() + "\n");
      }
    }
    sb.append("Payment ID: " + paymentId + "\n");
    sb.append("Amount: " + amount + "\n");
    sb.append("Fee: " + fee + "\n");
    sb.append("Mixin: " + mixin + "\n");
    sb.append("Size: " + size + "\n");
    sb.append("Type: " + type + "\n");
    sb.append("Height: " + height + "\n");
    sb.append("Note: " + note + "\n");
    sb.append("Timestamp: " + timestamp + "\n");
    sb.append("Unlock time: " + unlockTime + "\n");
    sb.append("Is double spend: " + isDoubleSpend + "\n");
    sb.append("Blob: " + blob + "\n");
    sb.append("Metadata: " + metadata + "\n");
    return sb.toString();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((address == null) ? 0 : address.hashCode());
    result = prime * result + ((amount == null) ? 0 : amount.hashCode());
    result = prime * result + ((blob == null) ? 0 : blob.hashCode());
    result = prime * result + ((fee == null) ? 0 : fee.hashCode());
    result = prime * result + ((height == null) ? 0 : height.hashCode());
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    result = prime * result + ((isDoubleSpend == null) ? 0 : isDoubleSpend.hashCode());
    result = prime * result + ((key == null) ? 0 : key.hashCode());
    result = prime * result + ((metadata == null) ? 0 : metadata.hashCode());
    result = prime * result + ((mixin == null) ? 0 : mixin.hashCode());
    result = prime * result + ((note == null) ? 0 : note.hashCode());
    result = prime * result + ((outputs == null) ? 0 : outputs.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    result = prime * result + ((payments == null) ? 0 : payments.hashCode());
    result = prime * result + ((size == null) ? 0 : size.hashCode());
    result = prime * result + ((subaddressIndex == null) ? 0 : subaddressIndex.hashCode());
    result = prime * result + ((timestamp == null) ? 0 : timestamp.hashCode());
    result = prime * result + ((type == null) ? 0 : type.hashCode());
    result = prime * result + ((unlockTime == null) ? 0 : unlockTime.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTx other = (MoneroTx) obj;
    if (accountIndex == null) {
      if (other.accountIndex != null) return false;
    } else if (!accountIndex.equals(other.accountIndex)) return false;
    if (address == null) {
      if (other.address != null) return false;
    } else if (!address.equals(other.address)) return false;
    if (amount == null) {
      if (other.amount != null) return false;
    } else if (!amount.equals(other.amount)) return false;
    if (blob == null) {
      if (other.blob != null) return false;
    } else if (!blob.equals(other.blob)) return false;
    if (fee == null) {
      if (other.fee != null) return false;
    } else if (!fee.equals(other.fee)) return false;
    if (height == null) {
      if (other.height != null) return false;
    } else if (!height.equals(other.height)) return false;
    if (id == null) {
      if (other.id != null) return false;
    } else if (!id.equals(other.id)) return false;
    if (isDoubleSpend == null) {
      if (other.isDoubleSpend != null) return false;
    } else if (!isDoubleSpend.equals(other.isDoubleSpend)) return false;
    if (key == null) {
      if (other.key != null) return false;
    } else if (!key.equals(other.key)) return false;
    if (metadata == null) {
      if (other.metadata != null) return false;
    } else if (!metadata.equals(other.metadata)) return false;
    if (mixin == null) {
      if (other.mixin != null) return false;
    } else if (!mixin.equals(other.mixin)) return false;
    if (note == null) {
      if (other.note != null) return false;
    } else if (!note.equals(other.note)) return false;
    if (outputs == null) {
      if (other.outputs != null) return false;
    } else if (!outputs.equals(other.outputs)) return false;
    if (paymentId == null) {
      if (other.paymentId != null) return false;
    } else if (!paymentId.equals(other.paymentId)) return false;
    if (payments == null) {
      if (other.payments != null) return false;
    } else if (!payments.equals(other.payments)) return false;
    if (size == null) {
      if (other.size != null) return false;
    } else if (!size.equals(other.size)) return false;
    if (subaddressIndex == null) {
      if (other.subaddressIndex != null) return false;
    } else if (!subaddressIndex.equals(other.subaddressIndex)) return false;
    if (timestamp == null) {
      if (other.timestamp != null) return false;
    } else if (!timestamp.equals(other.timestamp)) return false;
    if (type != other.type) return false;
    if (unlockTime == null) {
      if (other.unlockTime != null) return false;
    } else if (!unlockTime.equals(other.unlockTime)) return false;
    return true;
  }
}
