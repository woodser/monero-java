package monero.wallet.model;

import java.math.BigInteger;

/**
 * Represents a payment on the Monero network to an address.
 * 
 * A transaction may have one or more payments.
 */
public class MoneroPayment {

  private MoneroTx tx;
  private String address;
  private BigInteger amount;
  private Integer accountIdx;
  private Integer subaddressIdx;
  private Boolean isSpent;
  
  public MoneroPayment() {
    super();
  }
  
  public MoneroPayment(MoneroTx transaction, String address, BigInteger amount) {
    super();
    this.tx = transaction;
    this.address = address;
    this.amount = amount;
  }

  public MoneroPayment(MoneroTx tx, String address, BigInteger amount, Integer accountIdx, Integer subaddressIdx) {
    super();
    this.tx = tx;
    this.address = address;
    this.amount = amount;
    this.accountIdx = accountIdx;
    this.subaddressIdx = subaddressIdx;
  }

  public Integer getAccountIdx() {
    return accountIdx;
  }

  public void setAccountIdx(Integer accountIdx) {
    this.accountIdx = accountIdx;
  }

  public Integer getSubaddressIdx() {
    return subaddressIdx;
  }

  public void setSubaddressIdx(Integer subaddressIdx) {
    this.subaddressIdx = subaddressIdx;
  }

  public MoneroTx getTx() {
    return tx;
  }

  public void setTx(MoneroTx transaction) {
    this.tx = transaction;
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

  public Boolean getIsSpent() {
    return isSpent;
  }

  public void setIsSpent(Boolean isSpent) {
    this.isSpent = isSpent;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIdx == null) ? 0 : accountIdx.hashCode());
    result = prime * result + ((address == null) ? 0 : address.hashCode());
    result = prime * result + ((amount == null) ? 0 : amount.hashCode());
    result = prime * result + ((isSpent == null) ? 0 : isSpent.hashCode());
    result = prime * result + ((subaddressIdx == null) ? 0 : subaddressIdx.hashCode());
    result = prime * result + ((tx == null) ? 0 : tx.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroPayment other = (MoneroPayment) obj;
    if (accountIdx == null) {
      if (other.accountIdx != null) return false;
    } else if (!accountIdx.equals(other.accountIdx)) return false;
    if (address == null) {
      if (other.address != null) return false;
    } else if (!address.equals(other.address)) return false;
    if (amount == null) {
      if (other.amount != null) return false;
    } else if (!amount.equals(other.amount)) return false;
    if (isSpent == null) {
      if (other.isSpent != null) return false;
    } else if (!isSpent.equals(other.isSpent)) return false;
    if (subaddressIdx == null) {
      if (other.subaddressIdx != null) return false;
    } else if (!subaddressIdx.equals(other.subaddressIdx)) return false;
    if (tx == null) {
      if (other.tx != null) return false;
    } else if (!tx.equals(other.tx)) return false;
    return true;
  }
}
