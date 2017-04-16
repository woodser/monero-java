package wallet;

import java.math.BigInteger;
import java.util.List;

/**
 * Represents a transaction on the Monero network.
 * 
 * @author woodser
 */
public class MoneroTransaction {

	private List<MoneroPayment> payments;
	private BigInteger fee;
	private int mixin;
	private String txKey;
	private String txHash;
	private int size;
	private MoneroTransactionType type;
	
  public MoneroTransaction(List<MoneroPayment> payments, BigInteger fee, int mixin, String txKey, String txHash, int size, MoneroTransactionType type) {
    super();
    this.payments = payments;
    this.fee = fee;
    this.mixin = mixin;
    this.txKey = txKey;
    this.txHash = txHash;
    this.size = size;
    this.type = type;
  }

  public List<MoneroPayment> getPayments() {
    return payments;
  }

  public void setPayments(List<MoneroPayment> payments) {
    this.payments = payments;
  }

  public BigInteger getFee() {
    return fee;
  }

  public void setFee(BigInteger fee) {
    this.fee = fee;
  }

  public int getMixin() {
    return mixin;
  }

  public void setMixin(int mixin) {
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

  public int getSize() {
    return size;
  }

  public void setSize(int size) {
    this.size = size;
  }

  public MoneroTransactionType getType() {
    return type;
  }

  public void setType(MoneroTransactionType type) {
    this.type = type;
  }
}
