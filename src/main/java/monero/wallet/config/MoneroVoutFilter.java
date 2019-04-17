package monero.wallet.config;

import java.math.BigInteger;
import java.util.List;

import common.types.Filter;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroTx;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxWallet;

/**
 * Filters outputs that don't match initialized filter criteria.
 */
public class MoneroVoutFilter extends MoneroOutputWallet implements Filter<MoneroOutputWallet> {

  private List<Integer> subaddressIndices;
  private MoneroTxFilter txFilter;
  
  public MoneroVoutFilter() {
    super();
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroVoutFilter setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }

  public MoneroTxFilter getTxFilter() {
    return txFilter;
  }

  public MoneroVoutFilter setTxFilter(MoneroTxFilter txFilter) {
    this.txFilter = txFilter;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroOutputWallet vout) {
    if (!(vout instanceof MoneroOutputWallet)) return false;
    
    // filter on vout
    if (this.getAccountIndex() != null && !this.getAccountIndex().equals(vout.getAccountIndex())) return false;
    if (this.getSubaddressIndex() != null && !this.getSubaddressIndex().equals(vout.getSubaddressIndex())) return false;
    if (this.getAmount() != null && this.getAmount().compareTo(vout.getAmount()) != 0) return false;
    if (this.getIsSpent() != null && !this.getIsSpent().equals(vout.getIsSpent())) return false;
    
    // filter on vout's key image
    if (this.getKeyImage() != null) {
      if (vout.getKeyImage() == null) return false;
      if (this.getKeyImage().getHex() != null && !this.getKeyImage().getHex().equals(vout.getKeyImage().getHex())) return false;
      if (this.getKeyImage().getSignature() != null && !this.getKeyImage().getSignature().equals(vout.getKeyImage().getSignature())) return false;
    }
    
    // filter extensions
    if (this.getSubaddressIndices() != null && !this.getSubaddressIndices().contains(vout.getSubaddressIndex())) return false;
    
    // filter with transaction filter
    if (this.getTxFilter() != null && !this.getTxFilter().meetsCriteria(vout.getTx())) return false;
    
    // vout meets filter criteria
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroVoutFilter setTx(MoneroTx tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroVoutFilter setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroVoutFilter setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }

  @Override
  public MoneroVoutFilter setSubaddressIndex(Integer subaddressIndex) {
    super.setSubaddressIndex(subaddressIndex);
    return this;
  }

  @Override
  public MoneroVoutFilter setIsSpent(Boolean isSpent) {
    super.setIsSpent(isSpent);
    return this;
  }

  @Override
  public MoneroVoutFilter setKeyImage(MoneroKeyImage keyImage) {
    super.setKeyImage(keyImage);
    return this;
  }

  @Override
  public MoneroVoutFilter setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroVoutFilter setIndex(Integer index) {
    super.setIndex(index);
    return this;
  }

  @Override
  public MoneroVoutFilter setRingOutputIndices(List<Integer> ringOutputIndices) {
    super.setRingOutputIndices(ringOutputIndices);
    return this;
  }

  @Override
  public MoneroVoutFilter setStealthPublicKey(String stealthPublicKey) {
    super.setStealthPublicKey(stealthPublicKey);
    return this;
  }
}
