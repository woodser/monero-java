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
	private int blockHeight;
	private MoneroTransaction transaction;
	private boolean isAvailableToSpend;
	
  public MoneroPayment(String address, BigInteger amount, int blockHeight, MoneroTransaction transaction, boolean isAvailableToSpend) {
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

  public int getBlockHeight() {
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

  public boolean isAvailableToSpend() {
    return isAvailableToSpend;
  }

  public void setAvailableToSpend(boolean isAvailableToSpend) {
    this.isAvailableToSpend = isAvailableToSpend;
  }
}
