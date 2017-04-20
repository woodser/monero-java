
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


	private List<MoneroPayment> payments;
	private BigInteger fee;
	private Integer mixin;
	private String txKey;
	private String txHash;
	private Integer size;
	private MoneroTransactionType type;
  private Integer blockHeight;
	
	public MoneroTransaction() {
	  super();
	}

  public MoneroTransaction(List<MoneroPayment> payments, BigInteger fee, Integer mixin, String txKey, String txHash, Integer size, MoneroTransactionType type, Integer blockHeight) {
    this();
    this.payments = payments;
    this.fee = fee;
    this.mixin = mixin;
    this.txKey = txKey;
    this.txHash = txHash;
    this.size = size;
    this.type = type;
    this.blockHeight = blockHeight;
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

  public String getTxKey() {
    return txKey;
  }

  public void setTxKey(String txKey) {
    this.txKey = txKey;
  }

  public String getTxHash() {
    return txHash;
  }

  public void setTxHash(String txHash) {
    this.txHash = txHash;
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
  
  public Integer getBlockHeight() {
    return blockHeight;
  }

  public void setBlockHeight(Integer blockHeight) {
    this.blockHeight = blockHeight;
  }
}
