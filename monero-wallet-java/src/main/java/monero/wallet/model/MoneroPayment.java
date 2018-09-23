package monero.wallet.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

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
  
  /**
   * Merges the given payment into this payment.
   * 
   * Sets uninitialized fields to the given payent. Validates initialized fields are equal.
   * 
   * @param tx is the transaction to merge into this one
   */
  public void merge(MoneroPayment payment) {
    assertNotNull(accountIdx);
    assertEquals(accountIdx, payment.getAccountIdx());
    assertNotNull(subaddressIdx);
    assertEquals(subaddressIdx, payment.getSubaddressIdx());
    assertEquals(tx.getId(), payment.getTx().getId());
    if (address == null) address = payment.getAddress();
    else if (payment.getAddress() != null) assertEquals("Address", address, payment.getAddress());
    if (amount == null) amount = payment.getAmount();
    else if (payment.getAmount() != null) assertEquals("Amounts", amount, payment.getAmount());
    if (accountIdx == null) accountIdx = payment.getAccountIdx();
    else if (payment.getAccountIdx() != null) assertEquals("Account indices", accountIdx, payment.getAccountIdx());
    if (subaddressIdx == null) subaddressIdx = payment.getSubaddressIdx();
    else if (payment.getSubaddressIdx() != null) assertEquals("Subaddress indices", subaddressIdx, payment.getSubaddressIdx());
    if (isSpent == null) isSpent = payment.getIsSpent();
    else if (payment.getIsSpent() != null) assertEquals("Is spents", isSpent, payment.getIsSpent());
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
