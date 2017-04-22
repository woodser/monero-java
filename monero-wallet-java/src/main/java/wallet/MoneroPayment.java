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
	private MoneroTransaction transaction;
	
	public MoneroPayment() {
	  super();
	}
	
	public MoneroPayment(String address, BigInteger amount) {
	  this(address, amount, null);
	}
	
  public MoneroPayment(String address, BigInteger amount, MoneroTransaction transaction) {
    super();
    this.address = address;
    this.amount = amount;
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

  public MoneroTransaction getTransaction() {
    return transaction;
  }

  public void setTransaction(MoneroTransaction transaction) {
    this.transaction = transaction;
  }
}
