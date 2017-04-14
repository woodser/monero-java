package wallet;

import com.google.common.primitives.UnsignedInteger;

public class MoneroUri {

	private String address;
	private UnsignedInteger amount;
	private String paymentId;
	private String recipientName;
	private String txDescription;
	
	public MoneroUri() {
	  super();
    setAddress(null);
    setAmount(null);
    setPaymentId(null);
    setRecipientName(null);
    setTxDescription(null);
	}
	
  public MoneroUri(String address, UnsignedInteger amount, String paymentId, String recipientName, String txDescription) {
    super();
    setAddress(address);
    setAmount(amount);
    setPaymentId(paymentId);
    setRecipientName(recipientName);
    setTxDescription(txDescription);
  }

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public UnsignedInteger getAmount() {
    return amount;
  }

  public void setAmount(UnsignedInteger amount) {
    this.amount = amount == null ? UnsignedInteger.valueOf(0) : amount;
  }

  public String getPaymentId() {
    return paymentId;
  }

  public void setPaymentId(String paymentId) {
    this.paymentId = paymentId;
  }

  public String getRecipientName() {
    return recipientName;
  }

  public void setRecipientName(String recipientName) {
    this.recipientName = recipientName;
  }

  public String getTxDescription() {
    return txDescription;
  }

  public void setTxDescription(String txDescription) {
    this.txDescription = txDescription;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((address == null) ? 0 : address.hashCode());
    result = prime * result + ((amount == null) ? 0 : amount.hashCode());
    result = prime * result + ((paymentId == null) ? 0 : paymentId.hashCode());
    result = prime * result + ((recipientName == null) ? 0 : recipientName.hashCode());
    result = prime * result + ((txDescription == null) ? 0 : txDescription.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroUri other = (MoneroUri) obj;
    if (address == null) {
      if (other.address != null) return false;
    } else if (!address.equals(other.address)) return false;
    if (amount == null) {
      if (other.amount != null) return false;
    } else if (!amount.equals(other.amount)) return false;
    if (paymentId == null) {
      if (other.paymentId != null) return false;
    } else if (!paymentId.equals(other.paymentId)) return false;
    if (recipientName == null) {
      if (other.recipientName != null) return false;
    } else if (!recipientName.equals(other.recipientName)) return false;
    if (txDescription == null) {
      if (other.txDescription != null) return false;
    } else if (!txDescription.equals(other.txDescription)) return false;
    return true;
  }

  @Override
  public String toString() {
    return "MoneroUri [address=" + address + ", amount=" + amount + ", paymentId=" + paymentId + ", recipientName=" + recipientName + ", txDescription=" + txDescription + "]";
  }
}
