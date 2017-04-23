
package wallet;

import java.math.BigInteger;
import java.util.List;

/**
 * Represents a transaction on the Monero network.
 * 
 * @author woodser
 */
public class MoneroTransaction {
  
  /**
   * Enumerates possible transaction types.
   */
  public enum MoneroTransactionType {
    INCOMING,
    OUTGOING,
    PENDING,
    FAILED,
    MEMPOOL
  }

  private BigInteger amount;
	private List<MoneroPayment> payments;
	private List<MoneroOutput> outputs;
	private String paymentId;
	private BigInteger fee;
	private Integer mixin;
	private String hash;
	private String key;
	private Integer size;
	private MoneroTransactionType type;
  private Integer height;
  private String note;
  private Long timestamp;
  private Integer unlockTime;
	
	public MoneroTransaction() {
	  super();
	}

  public BigInteger getAmount() {
    return amount;
  }

  public void setAmount(BigInteger amount) {
    this.amount = amount;
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
  
  public String getHash() {
    return hash;
  }

  public void setHash(String hash) {
    this.hash = hash;
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

  public MoneroTransactionType getType() {
    return type;
  }

  public void setType(MoneroTransactionType type) {
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
}
