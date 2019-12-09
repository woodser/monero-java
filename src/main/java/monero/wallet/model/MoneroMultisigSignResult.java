package monero.wallet.model;

import java.util.List;

/**
 * Models the result of signing multisig tx hex.
 */
public class MoneroMultisigSignResult {
  
  private String signedMultisigTxHex;
  private List<String> txHashes;

  public String getSignedMultisigTxHex() {
    return signedMultisigTxHex;
  }

  public void setSignedMultisigTxHex(String signedTxMultisigHex) {
    this.signedMultisigTxHex = signedTxMultisigHex;
  }

  public List<String> getTxIds() {
    return txHashes;
  }

  public void setTxIds(List<String> txHashes) {
    this.txHashes = txHashes;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((signedMultisigTxHex == null) ? 0 : signedMultisigTxHex.hashCode());
    result = prime * result + ((txHashes == null) ? 0 : txHashes.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroMultisigSignResult other = (MoneroMultisigSignResult) obj;
    if (signedMultisigTxHex == null) {
      if (other.signedMultisigTxHex != null) return false;
    } else if (!signedMultisigTxHex.equals(other.signedMultisigTxHex)) return false;
    if (txHashes == null) {
      if (other.txHashes != null) return false;
    } else if (!txHashes.equals(other.txHashes)) return false;
    return true;
  }
}
