package monero.wallet.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import common.types.Filter;
import common.utils.GenUtils;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroTx;

/**
 * Configures a query to retrieve wallet outputs (i.e. outputs that the wallet has or had the
 * ability to spend).
 * 
 * All outputs are returned except those that do not meet the criteria defined in this query.
 */
public class MoneroOutputQuery extends MoneroOutputWallet implements Filter<MoneroOutputWallet> {

  protected MoneroTxQuery txQuery;
  private List<Integer> subaddressIndices;
  private BigInteger minAmount;
  private BigInteger maxAmount;
  
  public MoneroOutputQuery() {
    super();
  }
  
  public MoneroOutputQuery(final MoneroOutputQuery query) {
    super(query);
    if (query.getMinAmount() != null) this.minAmount = query.getMinAmount();
    if (query.getMaxAmount() != null) this.maxAmount = query.getMaxAmount();
    if (query.subaddressIndices != null) this.subaddressIndices = new ArrayList<Integer>(query.subaddressIndices);
    this.txQuery = query.txQuery;  // reference original by default, MoneroTxQuery's deep copy will set this to itself
  }
  
  @Override
  public MoneroOutputQuery copy() {
    return new MoneroOutputQuery(this);
  }
  
  public BigInteger getMinAmount() {
    return minAmount;
  }

  public MoneroOutputQuery setMinAmount(BigInteger minAmount) {
    this.minAmount = minAmount;
    return this;
  }

  public BigInteger getMaxAmount() {
    return maxAmount;
  }

  public MoneroOutputQuery setMaxAmount(BigInteger maxAmount) {
    this.maxAmount = maxAmount;
    return this;
  }

  @JsonIgnore
  public MoneroTxQuery getTxQuery() {
    return txQuery;
  }

  public MoneroOutputQuery setTxQuery(MoneroTxQuery txQuery) {
    this.txQuery = txQuery;
    if (txQuery != null) txQuery.outputQuery = this;
    return this;
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroOutputQuery setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public MoneroOutputQuery setSubaddressIndices(Integer... subaddressIndices) {
    this.subaddressIndices = GenUtils.arrayToList(subaddressIndices);
    return this;
  }
  
  @Override
  public boolean meetsCriteria(MoneroOutputWallet output) {
    return meetsCriteria(output, true);
  }
    
  protected boolean meetsCriteria(MoneroOutputWallet output, boolean queryParent) {

    if (!(output instanceof MoneroOutputWallet)) return false;
    
    // filter on output
    if (this.getAccountIndex() != null && !this.getAccountIndex().equals(output.getAccountIndex())) return false;
    if (this.getSubaddressIndex() != null && !this.getSubaddressIndex().equals(output.getSubaddressIndex())) return false;
    if (this.getAmount() != null && this.getAmount().compareTo(output.getAmount()) != 0) return false;
    if (this.isSpent() != null && !this.isSpent().equals(output.isSpent())) return false;
    if (this.isFrozen() != null && !this.isFrozen().equals(output.isFrozen())) return false;
    
    // filter on output key image
    if (this.getKeyImage() != null) {
      if (output.getKeyImage() == null) return false;
      if (this.getKeyImage().getHex() != null && !this.getKeyImage().getHex().equals(output.getKeyImage().getHex())) return false;
      if (this.getKeyImage().getSignature() != null && !this.getKeyImage().getSignature().equals(output.getKeyImage().getSignature())) return false;
    }
    
    // filter on extensions
    if (this.getSubaddressIndices() != null && !this.getSubaddressIndices().contains(output.getSubaddressIndex())) return false;
    
    // filter with tx query
    if (this.getTxQuery() != null && !this.getTxQuery().meetsCriteria(output.getTx(), false)) return false;
    
    // filter on remaining fields
    if (this.getMinAmount() != null && (output.getAmount() == null || output.getAmount().compareTo(this.getMinAmount()) < 0)) return false;
    if (this.getMaxAmount() != null && (output.getAmount() == null || output.getAmount().compareTo(this.getMaxAmount()) > 0)) return false;
    
    // output meets query
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroOutputQuery setTx(MoneroTx tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroOutputQuery setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroOutputQuery setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }

  @Override
  public MoneroOutputQuery setSubaddressIndex(Integer subaddressIndex) {
    super.setSubaddressIndex(subaddressIndex);
    return this;
  }

  @Override
  public MoneroOutputQuery setIsSpent(Boolean isSpent) {
    super.setIsSpent(isSpent);
    return this;
  }
  
  @Override
  public MoneroOutputQuery setIsFrozen(Boolean isFrozen) {
    super.setIsFrozen(isFrozen);
    return this;
  }
  
  @Override
  public MoneroOutputQuery setKeyImage(MoneroKeyImage keyImage) {
    super.setKeyImage(keyImage);
    return this;
  }

  @Override
  public MoneroOutputQuery setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroOutputQuery setIndex(Long index) {
    super.setIndex(index);
    return this;
  }

  @Override
  public MoneroOutputQuery setRingOutputIndices(List<Long> ringOutputIndices) {
    super.setRingOutputIndices(ringOutputIndices);
    return this;
  }

  @Override
  public MoneroOutputQuery setStealthPublicKey(String stealthPublicKey) {
    super.setStealthPublicKey(stealthPublicKey);
    return this;
  }
}
