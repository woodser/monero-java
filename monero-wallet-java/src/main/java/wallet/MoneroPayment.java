package wallet;

import java.math.BigInteger;

/**
 * Represents a payment on the Monero network to an address.
 * 
 * A transaction may have one or more payments.
 * 
 * Each payment may be fulfilled by multiple transaction outputs.
 * 
 * @author woodser
 */
public class MoneroPayment {

  private MoneroTransaction transaction;
  private String address;
  private BigInteger amount;
  
  public MoneroPayment() {
    super();
  }
  
  public MoneroPayment(String address, BigInteger amount) {
    this(null, address, amount);
  }
  
  public MoneroPayment(MoneroTransaction transaction, String address, BigInteger amount) {
    super();
    this.transaction = transaction;
    this.address = address;
    this.amount = amount;
  }

  public MoneroTransaction getTransaction() {
    return transaction;
  }

  public void setTransaction(MoneroTransaction transaction) {
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
}
