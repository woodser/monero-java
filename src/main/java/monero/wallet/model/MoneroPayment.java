package monero.wallet.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;

import common.utils.StringUtils;

/**
 * Represents a payment on the Monero network to an address.
 * 
 * A transaction may have one or more payments.
 */
public class MoneroPayment {

  private MoneroTx tx;
  private String address;
  private Integer accountIndex;
  private Integer subaddrIndex;
  private BigInteger amount;
  private Boolean isSpent;
  private String keyImage;
  
  public MoneroPayment() {
    super();
  }

  public MoneroTx getTx() {
    return tx;
  }

  public void setTx(MoneroTx tx) {
    this.tx = tx;
  }
  
  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public Integer getAccountIndex() {
    return accountIndex;
  }

  public void setAccountIndex(Integer accountIndex) {
    this.accountIndex = accountIndex;
  }

  public Integer getSubaddrIndex() {
    return subaddrIndex;
  }

  public void setSubaddrIndex(Integer subaddrIndex) {
    this.subaddrIndex = subaddrIndex;
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

  public String getKeyImage() {
    return keyImage;
  }

  public void setKeyImage(String keyImage) {
    this.keyImage = keyImage;
  }

  /**
   * Merges the given payment into this payment.
   * 
   * Sets uninitialized fields to the given payent. Validates initialized fields are equal.
   * 
   * @param tx is the transaction to merge into this one
   */
  public void merge(MoneroPayment payment) {
    if (tx == null) tx = payment.getTx();
    else if (payment.getTx() != null) tx.merge(payment.getTx());
    if (address == null) address = payment.getAddress();
    else if (payment.getAddress() != null) assertEquals(address, payment.getAddress());
    if (accountIndex == null) accountIndex = payment.getAccountIndex();
    else if (payment.getAccountIndex() != null) assertEquals(accountIndex, payment.getAccountIndex());
    if (subaddrIndex == null) subaddrIndex = payment.getSubaddrIndex();
    else if (payment.getSubaddrIndex() != null) assertEquals(subaddrIndex, payment.getSubaddrIndex());
    if (amount == null) amount = payment.getAmount();
    else if (payment.getAmount() != null) assertTrue("Amounts", amount.compareTo(payment.getAmount()) == 0);
    if (isSpent == null) isSpent = payment.getIsSpent();
    else if (payment.getIsSpent() != null) assertEquals("Is spents", isSpent, payment.getIsSpent());
    if (keyImage == null) keyImage = payment.getKeyImage();
    else if (payment.getKeyImage() != null) assertEquals("Key images", keyImage, payment.getKeyImage());
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int offset) {
    StringBuilder sb = new StringBuilder();
    sb.append(StringUtils.getTabs(offset) + "Address: " + getAddress() + "\n");
    sb.append(StringUtils.getTabs(offset) + "Account idx: " + getAccountIndex() + "\n");
    sb.append(StringUtils.getTabs(offset) + "Subaddr idx: " + getSubaddrIndex() + "\n");
    sb.append(StringUtils.getTabs(offset) + "Amount: " + getAmount() + "\n");
    sb.append(StringUtils.getTabs(offset) + "Is spent: " + getIsSpent() + "\n");
    sb.append(StringUtils.getTabs(offset) + "Key image: " + getKeyImage());
    return sb.toString();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((accountIndex == null) ? 0 : accountIndex.hashCode());
    result = prime * result + ((address == null) ? 0 : address.hashCode());
    result = prime * result + ((amount == null) ? 0 : amount.hashCode());
    result = prime * result + ((isSpent == null) ? 0 : isSpent.hashCode());
    result = prime * result + ((keyImage == null) ? 0 : keyImage.hashCode());
    result = prime * result + ((subaddrIndex == null) ? 0 : subaddrIndex.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroPayment other = (MoneroPayment) obj;
    if (accountIndex == null) {
      if (other.accountIndex != null) return false;
    } else if (!accountIndex.equals(other.accountIndex)) return false;
    if (address == null) {
      if (other.address != null) return false;
    } else if (!address.equals(other.address)) return false;
    if (amount == null) {
      if (other.amount != null) return false;
    } else if (!amount.equals(other.amount)) return false;
    if (isSpent == null) {
      if (other.isSpent != null) return false;
    } else if (!isSpent.equals(other.isSpent)) return false;
    if (keyImage == null) {
      if (other.keyImage != null) return false;
    } else if (!keyImage.equals(other.keyImage)) return false;
    if (subaddrIndex == null) {
      if (other.subaddrIndex != null) return false;
    } else if (!subaddrIndex.equals(other.subaddrIndex)) return false;
    return true;
  }
}
