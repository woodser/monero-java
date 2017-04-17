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
	private Integer blockHeight;
	private MoneroTransaction transaction;
	private Boolean isAvailableToSpend;
	
	public MoneroPayment(String address, BigInteger amount) {
	  this(address, amount, null, null, null);
	}
	
  public MoneroPayment(String address, BigInteger amount, Integer blockHeight, MoneroTransaction transaction, Boolean isAvailableToSpend) {
    super();
    this.address = address;
    this.amount = amount;
    this.blockHeight = blockHeight;
    this.transaction = transaction;
    this.isAvailableToSpend = isAvailableToSpend;
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

  public Integer getBlockHeight() {
    return blockHeight;
  }

  public void setBlockHeight(int blockHeight) {
    this.blockHeight = blockHeight;
  }

  public MoneroTransaction getTransaction() {
    return transaction;
  }

  public void setTransaction(MoneroTransaction transaction) {
    this.transaction = transaction;
  }

  public Boolean isAvailableToSpend() {
    return isAvailableToSpend;
  }

  public void setAvailableToSpend(boolean isAvailableToSpend) {
    this.isAvailableToSpend = isAvailableToSpend;
  }
}
