package wallet;

import java.math.BigInteger;

/**
 * Represents a Monero transaction output.
 * 
 * @author woodser
 */
public class MoneroOutput extends MoneroAddressAmount {

  private Boolean isAvailableToSpend;
  
  public MoneroOutput() {
    super();
  }
  
  public MoneroOutput(MoneroTransaction tx, String address, BigInteger amount, Boolean isAvailableToSpend) {
    super(tx, address, amount);
    this.isAvailableToSpend = isAvailableToSpend;
  }

  public Boolean getIsAvailableToSpend() {
    return isAvailableToSpend;
  }

  public void setIsAvailableToSpend(Boolean isAvailableToSpend) {
    this.isAvailableToSpend = isAvailableToSpend;
  }
}
