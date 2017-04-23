package wallet;

import java.math.BigInteger;

/**
 * Represents a Monero transaction output.
 * 
 * @author woodser
 */
public class MoneroOutput {

  private MoneroTransaction transaction;
  private BigInteger amount;
  private Boolean isAvailableToSpend;
  
  public MoneroOutput() {
    super();
  }
  
  public MoneroOutput(MoneroTransaction transaction, BigInteger amount, Boolean isAvailableToSpend) {
    super();
    this.transaction = transaction;
    this.amount = amount;
    this.isAvailableToSpend = isAvailableToSpend;
  }

  public MoneroTransaction getTransaction() {
    return transaction;
  }

  public void setTransaction(MoneroTransaction transaction) {
    this.transaction = transaction;
  }

  public BigInteger getAmount() {
    return amount;
  }

  public void setAmount(BigInteger amount) {
    this.amount = amount;
  }

  public Boolean getIsAvailableToSpend() {
    return isAvailableToSpend;
  }

  public void setIsAvailableToSpend(Boolean isAvailableToSpend) {
    this.isAvailableToSpend = isAvailableToSpend;
  }
}
