package monero.wallet.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Monero account model.
 */
public class MoneroAccount {

  private Integer index;
  private String primaryAddress;
  private String label;
  private BigInteger balance;
  private BigInteger unlockedBalance;
  private String tag;
  private List<MoneroSubaddress> subaddresses;
  
  public MoneroAccount() {
    super();
  }
  
  public MoneroAccount(int index, String primaryAddress, String label, BigInteger balance, BigInteger unlockedBalance, List<MoneroSubaddress> subaddresses) {
    super();
    this.index = index;
    this.primaryAddress = primaryAddress;
    this.label = label;
    this.balance = balance;
    this.unlockedBalance = unlockedBalance;
    this.subaddresses = subaddresses;
  }
  
  public Integer getIndex() {
    return index;
  }
  
  public void setIndex(Integer index) {
    this.index = index;
  }
  
  public String getPrimaryAddress() {
    return primaryAddress;
  }

  public void setPrimaryAddress(String primaryAddress) {
    this.primaryAddress = primaryAddress;
  }

  public String getLabel() {
    return label;
  }
  
  public void setLabel(String label) {
    this.label = label;
  }
  
  public BigInteger getBalance() {
    return balance;
  }
  
  public void setBalance(BigInteger balance) {
    this.balance = balance;
  }
  
  public BigInteger getUnlockedBalance() {
    return unlockedBalance;
  }
  
  public void setUnlockedBalance(BigInteger unlockedBalance) {
    this.unlockedBalance = unlockedBalance;
  }
  
  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }
  
  public List<MoneroSubaddress> getSubaddresses() {
    return subaddresses;
  }
  
  public void setSubaddresses(List<MoneroSubaddress> subaddresses) {
    this.subaddresses = subaddresses;
    if (subaddresses != null) {
      for (MoneroSubaddress subaddress : subaddresses) {
        subaddress.setAccountIndex(index);
      }
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((balance == null) ? 0 : balance.hashCode());
    result = prime * result + ((index == null) ? 0 : index.hashCode());
    result = prime * result + ((label == null) ? 0 : label.hashCode());
    result = prime * result + ((primaryAddress == null) ? 0 : primaryAddress.hashCode());
    result = prime * result + ((subaddresses == null) ? 0 : subaddresses.hashCode());
    result = prime * result + ((tag == null) ? 0 : tag.hashCode());
    result = prime * result + ((unlockedBalance == null) ? 0 : unlockedBalance.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroAccount other = (MoneroAccount) obj;
    if (balance == null) {
      if (other.balance != null) return false;
    } else if (!balance.equals(other.balance)) return false;
    if (index == null) {
      if (other.index != null) return false;
    } else if (!index.equals(other.index)) return false;
    if (label == null) {
      if (other.label != null) return false;
    } else if (!label.equals(other.label)) return false;
    if (primaryAddress == null) {
      if (other.primaryAddress != null) return false;
    } else if (!primaryAddress.equals(other.primaryAddress)) return false;
    if (subaddresses == null) {
      if (other.subaddresses != null) return false;
    } else if (!subaddresses.equals(other.subaddresses)) return false;
    if (tag == null) {
      if (other.tag != null) return false;
    } else if (!tag.equals(other.tag)) return false;
    if (unlockedBalance == null) {
      if (other.unlockedBalance != null) return false;
    } else if (!unlockedBalance.equals(other.unlockedBalance)) return false;
    return true;
  }
}
