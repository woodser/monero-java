package monero.wallet.model;

/**
 * Common tx sets across transactions from initiating a send.
 */
public class MoneroTxSets {

  private String multisigTxSet;
  private String unsignedTxSet;
  private String signedTxSet;
  
  public String getMultisigTxSet() {
    return multisigTxSet;
  }
  
  public void setMultisigTxSet(String multisigTxSet) {
    this.multisigTxSet = multisigTxSet;
  }
  
  public String getUnsignedTxSet() {
    return unsignedTxSet;
  }
  
  public void setUnsignedTxSet(String unsignedTxSet) {
    this.unsignedTxSet = unsignedTxSet;
  }

  public String getSignedTxSet() {
    return signedTxSet;
  }

  public void setSignedTxSet(String signedTxSet) {
    this.signedTxSet = signedTxSet;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((multisigTxSet == null) ? 0 : multisigTxSet.hashCode());
    result = prime * result + ((signedTxSet == null) ? 0 : signedTxSet.hashCode());
    result = prime * result + ((unsignedTxSet == null) ? 0 : unsignedTxSet.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTxSets other = (MoneroTxSets) obj;
    if (multisigTxSet == null) {
      if (other.multisigTxSet != null) return false;
    } else if (!multisigTxSet.equals(other.multisigTxSet)) return false;
    if (signedTxSet == null) {
      if (other.signedTxSet != null) return false;
    } else if (!signedTxSet.equals(other.signedTxSet)) return false;
    if (unsignedTxSet == null) {
      if (other.unsignedTxSet != null) return false;
    } else if (!unsignedTxSet.equals(other.unsignedTxSet)) return false;
    return true;
  }
}
