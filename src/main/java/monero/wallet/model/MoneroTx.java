
package monero.wallet.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.List;

import org.apache.log4j.Logger;

/**
 * Represents a transaction on the Monero network.
 */
public class MoneroTx {
  
  //logger
 private static final Logger LOGGER = Logger.getLogger(MoneroTx.class);
  
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
  private String srcAddress;
  private Integer srcAccountIdx;
  private Integer srcSubaddressIdx; // TODO (monero-wallet-rpc): transactions may originate from multiple subaddresses but querying only provides subaddress 0
  private BigInteger totalAmount;
	private List<MoneroPayment> payments;
	private String paymentId;
	private BigInteger fee;
	private Integer mixin;
	private Integer size;
	private MoneroTxType type;
  private Integer height;
  private String note;
  private Long timestamp;
  private Integer unlockTime;
  private Boolean isDoubleSpend;
  private String key;
  private String blob;
  private String metadata;
  private Integer numConfirmations;
  private Integer numEstimatedBlocksUntilConfirmed;
	
	public MoneroTx() {
	  super();
	}
	
	public String getId() {
	  return id;
	}
	
	public void setId(String id) {
	  this.id = id;
	}

  public String getSrcAddress() {
    return srcAddress;
  }

  public void setSrcAddress(String srcAddress) {
    this.srcAddress = srcAddress;
  }

  public Integer getSrcAccountIdx() {
    return srcAccountIdx;
  }

  public void setSrcAccountIdx(Integer srcAccountIdx) {
    this.srcAccountIdx = srcAccountIdx;
  }

  public Integer getSrcSubaddressIdx() {
    return srcSubaddressIdx;
  }

  public void setSrcSubaddressIdx(Integer srcSubaddressIdx) {
    this.srcSubaddressIdx = srcSubaddressIdx;
  }

  public BigInteger getTotalAmount() {
    return totalAmount;
  }

  public void setTotalAmount(BigInteger totalAmount) {
    this.totalAmount = totalAmount;
  }

  public List<MoneroPayment> getPayments() {
    return payments;
  }

  public void setPayments(List<MoneroPayment> payments) {
    this.payments = payments;
    if (payments != null) {
      for (MoneroPayment payment : payments) {
        payment.setTx(this);
      }
    }
  }
  
  public String getPaymentId() {
    return paymentId;
  }

  public void setPaymentId(String paymentId) {
    this.paymentId = paymentId;
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

  public String getKey() {
    return key;
  }

  public void setKey(String key) {
    this.key = key;
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

  public Integer getNumConfirmations() {
    return numConfirmations;
  }

  public void setNumConfirmations(Integer numConfirmations) {
    this.numConfirmations = numConfirmations;
  }

  public Integer getNumEstimatedBlocksUntilConfirmed() {
    return numEstimatedBlocksUntilConfirmed;
  }

  public void setNumEstimatedBlocksUntilConfirmed(Integer numEstimatedBlocksUntilConfirmed) {
    this.numEstimatedBlocksUntilConfirmed = numEstimatedBlocksUntilConfirmed;
  }

  /**
   * Merges the given transaction into this transaction.
   * 
   * Appends payments and outputs. Sets uninitialized fields to the given transaction. Validates initialized fields are equal.
   * 
   * @param tx is the transaction to merge into this one
   * @param addPayments specifies if payments should be appended or merged with existing payments
   */
  public void merge(MoneroTx tx, boolean addPayments) {
    if (id == null) id = tx.getId();
    else if (tx.getId() != null) assertEquals("IDs", id, tx.getId());
    if (srcAddress == null) srcAddress = tx.getSrcAddress();
    else if (tx.getSrcAddress() != null) assertEquals("Addresses", srcAddress, tx.getSrcAddress());
    if (srcAccountIdx == null) srcAccountIdx = tx.getSrcAccountIdx();
    else if (tx.getSrcAccountIdx() != null) assertEquals("Account indices", srcAccountIdx, tx.getSrcAccountIdx());
    if (srcSubaddressIdx == null) srcSubaddressIdx = tx.getSrcSubaddressIdx();
    else if (tx.getSrcSubaddressIdx() != null) assertEquals("Subaddress indices", srcSubaddressIdx, tx.getSrcSubaddressIdx());
    if (totalAmount == null) totalAmount = tx.getTotalAmount();
    else if (tx.getTotalAmount() != null) {
      if (addPayments) totalAmount = totalAmount.add(tx.getTotalAmount());
      else assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
    }
    if (payments == null) setPayments(tx.getPayments());
    else if (tx.getPayments() != null) {
      if (addPayments) {
        for (MoneroPayment payment : tx.getPayments()) {
          payment.setTx(this);
          payments.add(payment);
        }
      } else {
        assertEquals("Tx " + tx.getId() + " cannot be merged because payments are different sizes", payments.size(), tx.getPayments().size());
        for (int i = 0; i < payments.size(); i++) {
          payments.get(i).merge(tx.getPayments().get(i));
        }
      }
    }
    if (paymentId == null) paymentId = tx.getPaymentId();
    else if (tx.getPaymentId() != null) assertEquals("Payment ids", paymentId, tx.getPaymentId());
    if (fee == null) fee = tx.getFee();
    else if (tx.getFee() != null) assertEquals("Fees", fee, tx.getFee());
    if (mixin == null) mixin = tx.getMixin();
    else if (tx.getMixin() != null) assertEquals("Mixins", mixin, tx.getMixin());
    if (key == null) key = tx.getKey();
    else if (tx.getKey() != null) assertEquals("Keys", key, tx.getKey());
    if (size == null) size = tx.getSize();
    else if (tx.getSize() != null) assertEquals("Sizes", size, tx.getSize());
    if (type == null) type = tx.getType();
    else if (tx.getType() != null) assertEquals("Types", type, tx.getType());
    if (height == null) height = tx.getHeight();
    else if (tx.getHeight() != null) assertEquals("Heights", height, tx.getHeight());
    if (note == null) note = tx.getNote();
    else if (tx.getNote() != null) assertEquals("Notes", note, tx.getNote());
    if (timestamp == null) timestamp = tx.getTimestamp();
    else if (tx.getTimestamp() != null) {
      //assertEquals("Timestamps", timestamp, tx.getTimestamp()); // TODO (monero-wallet-rpc): timestamps for id can vary (e.g. faa8a4aecb4de18c91dcedb00be76f4f9d3d79839b5bf9cab87f2e3632fc574c)
      if (timestamp.equals(tx.getTimestamp())) LOGGER.warn("Timestamps for tx " + tx.getId() + " are different, using first one");
      timestamp = Math.min(timestamp, tx.getTimestamp());
    }
    if (unlockTime == null) unlockTime = tx.getUnlockTime();
    else if (tx.getUnlockTime() != null) assertEquals(unlockTime, tx.getUnlockTime());
    if (isDoubleSpend == null) isDoubleSpend = tx.getIsDoubleSpend();
    else if (tx.getIsDoubleSpend() != null) assertEquals(isDoubleSpend, tx.getIsDoubleSpend());
    if (blob == null) blob = tx.getBlob();
    else if (tx.getBlob() != null) assertEquals(blob, tx.getBlob());
    if (metadata == null) metadata = tx.getMetadata();
    else if (tx.getMetadata() != null) assertEquals(metadata, tx.getMetadata());
    if (numConfirmations == null) numConfirmations = tx.getNumConfirmations();  // TODO: confirmations can change
    else if (tx.getNumConfirmations() != null) assertEquals(numConfirmations, tx.getNumConfirmations());
    if (numEstimatedBlocksUntilConfirmed == null) numEstimatedBlocksUntilConfirmed = tx.getNumEstimatedBlocksUntilConfirmed();
    else if (tx.getNumEstimatedBlocksUntilConfirmed() != null) assertEquals(numEstimatedBlocksUntilConfirmed, tx.getNumEstimatedBlocksUntilConfirmed());
  }
  
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ID: " + id + "\n");
    sb.append("Source address: " + srcAddress + "\n");
    sb.append("Source account index: " + srcAccountIdx + "\n");
    sb.append("Source subaddress index: " + srcSubaddressIdx + "\n");
    sb.append("Total amount: " + totalAmount + "\n");
    if (payments != null) {
      sb.append("Payments:\n");
      for (int i = 0; i < payments.size(); i++) {
        sb.append("\t" + (i + 1) + ":\n");
        sb.append("\t\tAddress: " + payments.get(i).getAddress() + "\n");
        sb.append("\t\tAmount: " + payments.get(i).getAmount() + "\n");
        sb.append("\t\tAccount idx: " + payments.get(i).getAccountIdx() + "\n");
        sb.append("\t\tSubaddress idx: " + payments.get(i).getSubaddressIdx() + "\n");
        sb.append("\t\tIs spent: " + payments.get(i).getIsSpent() + "\n");
      }
    } else {
      sb.append("Payments: null\n");
    }
    sb.append("Payment ID: " + paymentId + "\n");
    sb.append("Fee: " + fee + "\n");
    sb.append("Mixin: " + mixin + "\n");
    sb.append("Size: " + size + "\n");
    sb.append("Type: " + type + "\n");
    sb.append("Height: " + height + "\n");
    sb.append("Note: " + note + "\n");
    sb.append("Timestamp: " + timestamp + "\n");
    sb.append("Unlock time: " + unlockTime + "\n");
    sb.append("Is double spend: " + isDoubleSpend + "\n");
    sb.append("Key: " + key + "\n");
    sb.append("Blob: " + blob + "\n");
    sb.append("Metadata: " + metadata + "\n");
    sb.append("Num confirmations: " + numConfirmations + "\n");
    sb.append("Num estimated blocks until confirmed: " + numEstimatedBlocksUntilConfirmed + "\n");
    return sb.toString();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((blob == null) ? 0 : blob.hashCode());
    result = prime * result + ((fee == null) ? 0 : fee.hashCode());
    result = prime * result + ((height == null) ? 0 : height.hashCode());
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    result = prime * result + ((isDoubleSpend == null) ? 0 : isDoubleSpend.hashCode());
    result = prime * result + ((key == null) ? 0 : key.hashCode());
    result = prime * result + ((metadata == null) ? 0 : metadata.hashCode());
    result = prime * result + ((mixin == null) ? 0 : mixin.hashCode());
    result = prime * result + ((note == null) ? 0 : note.hashCode());
    result = prime * result + ((numConfirmations == null) ? 0 : numConfirmations.hashCode());
    result = prime * result + ((numEstimatedBlocksUntilConfirmed == null) ? 0 : numEstimatedBlocksUntilConfirmed.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    result = prime * result + ((payments == null) ? 0 : payments.hashCode());
    result = prime * result + ((size == null) ? 0 : size.hashCode());
    result = prime * result + ((srcAccountIdx == null) ? 0 : srcAccountIdx.hashCode());
    result = prime * result + ((srcAddress == null) ? 0 : srcAddress.hashCode());
    result = prime * result + ((srcSubaddressIdx == null) ? 0 : srcSubaddressIdx.hashCode());
    result = prime * result + ((timestamp == null) ? 0 : timestamp.hashCode());
    result = prime * result + ((totalAmount == null) ? 0 : totalAmount.hashCode());
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
    if (numConfirmations == null) {
      if (other.numConfirmations != null) return false;
    } else if (!numConfirmations.equals(other.numConfirmations)) return false;
    if (numEstimatedBlocksUntilConfirmed == null) {
      if (other.numEstimatedBlocksUntilConfirmed != null) return false;
    } else if (!numEstimatedBlocksUntilConfirmed.equals(other.numEstimatedBlocksUntilConfirmed)) return false;
    if (paymentId == null) {
      if (other.paymentId != null) return false;
    } else if (!paymentId.equals(other.paymentId)) return false;
    if (payments == null) {
      if (other.payments != null) return false;
    } else if (!payments.equals(other.payments)) return false;
    if (size == null) {
      if (other.size != null) return false;
    } else if (!size.equals(other.size)) return false;
    if (srcAccountIdx == null) {
      if (other.srcAccountIdx != null) return false;
    } else if (!srcAccountIdx.equals(other.srcAccountIdx)) return false;
    if (srcAddress == null) {
      if (other.srcAddress != null) return false;
    } else if (!srcAddress.equals(other.srcAddress)) return false;
    if (srcSubaddressIdx == null) {
      if (other.srcSubaddressIdx != null) return false;
    } else if (!srcSubaddressIdx.equals(other.srcSubaddressIdx)) return false;
    if (timestamp == null) {
      if (other.timestamp != null) return false;
    } else if (!timestamp.equals(other.timestamp)) return false;
    if (totalAmount == null) {
      if (other.totalAmount != null) return false;
    } else if (!totalAmount.equals(other.totalAmount)) return false;
    if (type != other.type) return false;
    if (unlockTime == null) {
      if (other.unlockTime != null) return false;
    } else if (!unlockTime.equals(other.unlockTime)) return false;
    return true;
  }
}
