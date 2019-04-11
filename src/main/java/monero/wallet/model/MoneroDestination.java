package monero.wallet.model;

import java.math.BigInteger;

import monero.utils.MoneroUtils;

/**
 * Models an outgoing transfer destination.
 */
public class MoneroDestination {

  private String address;
  private BigInteger amount;
  
  public MoneroDestination() {
    // nothing to construct
  }
  
  public MoneroDestination(String address, BigInteger amount) {
    super();
    this.address = address;
    this.amount = amount;
  }
  
  public MoneroDestination(MoneroDestination destination) {
    this.address = destination.address;
    this.amount = destination.amount;
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
  
  public MoneroDestination copy() {
    return new MoneroDestination(this);
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(MoneroUtils.kvLine("Address", this.getAddress(), indent));
    sb.append(MoneroUtils.kvLine("Amount", this.getAmount() != null ? this.getAmount().toString() : null, indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);
  }
}
