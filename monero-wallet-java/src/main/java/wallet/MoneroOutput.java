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
  private Boolean isSpent;
  
  public MoneroOutput() {
    super();
  }
  
  public MoneroOutput(MoneroTransaction transaction, BigInteger amount, Boolean isSpent) {
    super();
    this.transaction = transaction;
    this.amount = amount;
    this.isSpent = isSpent;
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

  public Boolean getIsSpent() {
    return isSpent;
  }

  public void setIsSpent(Boolean isSpent) {
    this.isSpent = isSpent;
  }
}
