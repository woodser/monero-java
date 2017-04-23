package wallet;

import java.math.BigInteger;

/**
 * Models an address an amount which can represent a payment or a transaction output.
 * 
 * @author woodser
 */
public class MoneroAddressAmount {

  private MoneroTransaction transaction;
  private String address;
  private BigInteger amount;
  
  public MoneroAddressAmount(MoneroTransaction transaction, String address, BigInteger amount) {
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
