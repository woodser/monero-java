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
public class MoneroPayment extends MoneroAddressAmount {
  
  public MoneroPayment() {
    super();
  }
  
  public MoneroPayment(String address, BigInteger amount) {
    this(null, address, amount);
  }
  
  public MoneroPayment(MoneroTransaction tx, String address, BigInteger amount) {
    super(tx, address, amount);
  }
}
