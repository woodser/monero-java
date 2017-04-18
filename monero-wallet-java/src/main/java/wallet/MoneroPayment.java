package wallet;

import java.math.BigInteger;

/**
 * Represents a payment on the Monero network.
 * 
 * @author woodser
 */
public class MoneroPayment {

	private String address;
	private BigInteger amount;
	private Boolean isSpent;
	private MoneroTransaction transaction;
	
	public MoneroPayment(String address, BigInteger amount) {
	  this(address, amount, null, null);
	}
	
  public MoneroPayment(String address, BigInteger amount, Boolean isSpent, MoneroTransaction transaction) {
    super();
    this.address = address;
    this.amount = amount;
    this.isSpent = isSpent;
    this.transaction = transaction;
  }

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public BigInteger getAmount() {
    return amount;
  }

  public void setAmount(BigInteger amount) {
    this.amount = amount;
  }

  public Boolean isSpent() {
    return isSpent;
  }

  public void setSpent(boolean isSpent) {
    this.isSpent = isSpent;
  }

  public MoneroTransaction getTransaction() {
    return transaction;
  }

  public void setTransaction(MoneroTransaction transaction) {
    this.transaction = transaction;
  }
}
