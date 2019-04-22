package monero.wallet.model;

import monero.utils.MoneroUtils;

/**
 * Models an incoming transfer of funds to the wallet.
 */
public class MoneroIncomingTransfer extends MoneroTransfer {

  private Integer subaddressIndex;
  
  public MoneroIncomingTransfer() {
    // nothing to initialize
  }
  
  public MoneroIncomingTransfer(MoneroIncomingTransfer transfer) {
    super(transfer);
    this.subaddressIndex = transfer.subaddressIndex;
  }
  
  public Integer getSubaddressIndex() {
    return subaddressIndex;
  }
  
  public MoneroIncomingTransfer setSubaddressIndex(Integer subaddressIndex) {
    this.subaddressIndex = subaddressIndex;
    return this;
  }
  
  public MoneroIncomingTransfer copy() {
    return new MoneroIncomingTransfer(this);
  }
  
  /**
   * Updates this transaction by merging the latest information from the given
   * transaction.
   * 
   * Merging can modify or build references to the transfer given so it
   * should not be re-used or it should be copied before calling this method.
   * 
   * @param transfer is the transfer to merge into this one
   */
  public MoneroIncomingTransfer merge(MoneroIncomingTransfer transfer) {
    super.merge(transfer);
    assert(transfer instanceof MoneroIncomingTransfer);
    if (this == transfer) return this;
    this.setSubaddressIndex(MoneroUtils.reconcile(this.getSubaddressIndex(), transfer.getSubaddressIndex()));
    return this;
  }
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(MoneroUtils.kvLine("Subaddress index", this.getSubaddressIndex(), indent));
    String str = sb.toString();
    return str.substring(0, str.length() - 1);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((subaddressIndex == null) ? 0 : subaddressIndex.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!super.equals(obj)) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroIncomingTransfer other = (MoneroIncomingTransfer) obj;
    if (subaddressIndex == null) {
      if (other.subaddressIndex != null) return false;
    } else if (!subaddressIndex.equals(other.subaddressIndex)) return false;
    return true;
  }
}
