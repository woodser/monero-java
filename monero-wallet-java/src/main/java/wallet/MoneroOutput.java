package wallet;

import java.math.BigInteger;

/**
 * Represents a Monero transaction output.
 * 
 * @author woodser
 */
public class MoneroOutput extends MoneroAddressAmount {

  private Boolean isSpent;
  
  public MoneroOutput(MoneroTransaction tx, String address, BigInteger amount, Boolean isSpent) {
    super(tx, address, amount);
    this.isSpent = isSpent;
  }

  public Boolean getIsSpent() {
    return isSpent;
  }

  public void setIsSpent(Boolean isSpent) {
    this.isSpent = isSpent;
  }
}
