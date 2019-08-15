package monero.wallet.model;

import static org.junit.Assert.assertNotNull;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.GenUtils;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroUtils;

/**
 * Retains common information for a group of transactions which belong to a set.
 * 
 * For example, transactions returned from sendSplit() belong to a set whose
 * common information is needed in order to sign and submit a multisig tx.
 */
public class MoneroTxSet {

  private List<MoneroTx> txs;
  private String signedTxSet;
  private String unsignedTxSet;
  private String multisigTxSet;
  
  @JsonManagedReference
  public List<MoneroTx> getTxs() {
    return txs;
  }

  @JsonProperty("txs")
  public MoneroTxSet setTxs(List<MoneroTx> txs) {
    this.txs = txs;
    return this;
  }
  
  @JsonIgnore
  public MoneroTxSet setTxs(MoneroTx... txs) {
    this.txs = GenUtils.arrayToList(txs);
    return this;
  }

  public String getSignedTxSet() {
    return signedTxSet;
  }
  
  public MoneroTxSet setSignedTxSet(String signedTxSet) {
    this.signedTxSet = signedTxSet;
    return this;
  }
  
  public String getUnsignedTxSet() {
    return unsignedTxSet;
  }
  
  public MoneroTxSet setUnsignedTxSet(String unsignedTxSet) {
    this.unsignedTxSet = unsignedTxSet;
    return this;
  }
  
  public String getMultisigTxSet() {
    return multisigTxSet;
  }
  
  public MoneroTxSet setMultisigTxSet(String multisigTxSet) {
    this.multisigTxSet = multisigTxSet;
    return this;
  }
  
  public MoneroTxSet merge(MoneroTxSet txSet) {
    assertNotNull(txSet);
    if (this == txSet) return this;
    
    // merge sets
    this.setSignedTxSet(MoneroUtils.reconcile(this.getSignedTxSet(), txSet.getSignedTxSet()));
    this.setUnsignedTxSet(MoneroUtils.reconcile(this.getUnsignedTxSet(), txSet.getUnsignedTxSet()));
    this.setMultisigTxSet(MoneroUtils.reconcile(this.getMultisigTxSet(), txSet.getMultisigTxSet()));
    
    // merge txs
    if (txSet.getTxs() != null) {
      for (MoneroTx tx : txSet.getTxs()) {
        tx.setTxSet(this);
        MoneroUtils.mergeTx(txs, tx);
      }
    }

    return this;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((multisigTxSet == null) ? 0 : multisigTxSet.hashCode());
    result = prime * result + ((signedTxSet == null) ? 0 : signedTxSet.hashCode());
    result = prime * result + ((txs == null) ? 0 : txs.hashCode());
    result = prime * result + ((unsignedTxSet == null) ? 0 : unsignedTxSet.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroTxSet other = (MoneroTxSet) obj;
    if (multisigTxSet == null) {
      if (other.multisigTxSet != null) return false;
    } else if (!multisigTxSet.equals(other.multisigTxSet)) return false;
    if (signedTxSet == null) {
      if (other.signedTxSet != null) return false;
    } else if (!signedTxSet.equals(other.signedTxSet)) return false;
    if (txs == null) {
      if (other.txs != null) return false;
    } else if (!txs.equals(other.txs)) return false;
    if (unsignedTxSet == null) {
      if (other.unsignedTxSet != null) return false;
    } else if (!unsignedTxSet.equals(other.unsignedTxSet)) return false;
    return true;
  }
}
