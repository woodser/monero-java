
package monero.wallet.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.List;

import monero.utils.MoneroUtils;

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
    MEMPOOL,
    NOT_RELAYED
  }
  
  private String id;
  private MoneroSubaddress srcSubaddress; // TODO (monero-wallet-rpc): transactions may originate from multiple subaddresses but querying only provides subaddress 0
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

  public MoneroSubaddress getSrcSubaddress() {
    return srcSubaddress;
  }

  public void setSrcSubaddress(MoneroSubaddress srcSubaddress) {
    this.srcSubaddress = srcSubaddress;
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
   * Merges the given transaction into this transaction.  Does not merge payments or total amount.
   * 
   * @param tx is the transaction to merge into this transaction
   */
  public void merge(MoneroTx tx) {
    if (id == null) id = tx.getId();
    else if (tx.getId() != null) assertEquals("IDs", id, tx.getId());
    if (srcSubaddress == null) srcSubaddress = tx.getSrcSubaddress();
    else if (tx.getSrcSubaddress() != null) srcSubaddress.merge(tx.getSrcSubaddress());
    if (paymentId == null) paymentId = tx.getPaymentId();
    else if (tx.getPaymentId() != null) assertEquals(tx.getId(), paymentId, tx.getPaymentId());
    if (fee == null) fee = tx.getFee();
    else if (tx.getFee() != null) assertEquals(tx.getId(), fee, tx.getFee());
    if (mixin == null) mixin = tx.getMixin();
    else if (tx.getMixin() != null) assertEquals(tx.getId(), mixin, tx.getMixin());
    if (key == null) key = tx.getKey();
    else if (tx.getKey() != null) assertEquals(tx.getId(), key, tx.getKey());
    if (size == null) size = tx.getSize();
    else if (tx.getSize() != null) assertEquals(tx.getId(), size, tx.getSize());
    if (type == null) type = tx.getType();
    else if (tx.getType() != null) assertEquals(tx.getId(), type, tx.getType());
    if (height == null) height = tx.getHeight();
    else if (tx.getHeight() != null) assertEquals(tx.getId(), height, tx.getHeight());
    if (note == null) note = tx.getNote();
    else if (tx.getNote() != null) assertEquals(tx.getId(), note, tx.getNote());
    if (timestamp == null) timestamp = tx.getTimestamp();
    else if (tx.getTimestamp() != null) {
      if (tx.getType() == MoneroTxType.PENDING || tx.getType() == MoneroTxType.MEMPOOL) {
        timestamp = Math.min(timestamp, tx.getTimestamp()); // mempool timestamps can vary so use first timestamp
      } else {
        assertEquals(tx.getId(), timestamp, tx.getTimestamp());
      }
    }
    if (unlockTime == null) unlockTime = tx.getUnlockTime();
    else if (tx.getUnlockTime() != null) assertEquals(unlockTime, tx.getUnlockTime());
    if (isDoubleSpend == null) isDoubleSpend = tx.getIsDoubleSpend();
    else if (tx.getIsDoubleSpend() != null) assertEquals(isDoubleSpend, tx.getIsDoubleSpend());
    if (blob == null) blob = tx.getBlob();
    else if (tx.getBlob() != null) assertEquals(blob, tx.getBlob());
    if (metadata == null) metadata = tx.getMetadata();
    else if (tx.getMetadata() != null) assertEquals(metadata, tx.getMetadata());
    if (numConfirmations == null) numConfirmations = tx.getNumConfirmations();
    else if (tx.getNumConfirmations() != null) {
      assertTrue(Math.abs(numConfirmations - tx.getNumConfirmations()) <= 1); // num confirmations can change, take the latest (max)
      numConfirmations = Math.max(numConfirmations, tx.getNumConfirmations());
    }
    if (numEstimatedBlocksUntilConfirmed != null) {
      if (tx.getNumEstimatedBlocksUntilConfirmed() == null) numEstimatedBlocksUntilConfirmed = null;  // becomes null when confirmed
      else {
        assertTrue(Math.abs(numEstimatedBlocksUntilConfirmed - tx.getNumEstimatedBlocksUntilConfirmed()) <= 1); // num estimated blocks can change, take the latest (min)
        numEstimatedBlocksUntilConfirmed = Math.min(numEstimatedBlocksUntilConfirmed, tx.getNumEstimatedBlocksUntilConfirmed());
      }
    }
  }

  /**
   * Merges the given transaction into this transaction.
   * 
   * @param tx is the transaction to merge into this one
   * @param mergePayments specifies if payments should be merged with xor appended to existing payments
   */
  public void merge(MoneroTx tx, boolean mergePayments) {
    merge(tx);
    if (totalAmount == null) totalAmount = tx.getTotalAmount();
    else if (tx.getTotalAmount() != null) {
      if (mergePayments) assertTrue(totalAmount.compareTo(tx.getTotalAmount()) == 0);
      else totalAmount = totalAmount.add(tx.getTotalAmount());
    }
    if (payments == null) setPayments(tx.getPayments());
    else if (tx.getPayments() != null) {
      if (mergePayments) {
        assertEquals("Tx " + tx.getId() + " cannot be merged because payments are different sizes", payments.size(), tx.getPayments().size());
        for (int i = 0; i < payments.size(); i++) {
          payments.get(i).merge(tx.getPayments().get(i));
        }
      } else {
        for (MoneroPayment payment : tx.getPayments()) {
          payment.setTx(this);
          payments.add(payment);
        }
      }
    }
  }
  
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ID: " + id + "\n");
    sb.append("Source subaddress: " + srcSubaddress + "\n");
    sb.append("Total amount: " + totalAmount + "\n");
    if (payments != null) {
      sb.append("Payments:\n");
      for (int i = 0; i < payments.size(); i++) {
        sb.append("\t" + (i + 1) + ":\n");
        sb.append("\t\tSubaddress: " + payments.get(i).getDestination());
        sb.append("\t\tAmount: " + payments.get(i).getAmount() + "\n");
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
    result = prime * result + ((srcSubaddress == null) ? 0 : srcSubaddress.hashCode());
    result = prime * result + ((timestamp == null || !MoneroUtils.isConfirmed(type)) ? 0 : timestamp.hashCode()); // ignore timestamps if not confirmed
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
    if (srcSubaddress == null) {
      if (other.srcSubaddress != null) return false;
    } else if (!srcSubaddress.equals(other.srcSubaddress)) return false;
    if (timestamp == null) {
      if (other.timestamp != null) return false;
    } else if (MoneroUtils.isConfirmed(type) && !timestamp.equals(other.timestamp)) return false;  // only must be the same if confirmed
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
