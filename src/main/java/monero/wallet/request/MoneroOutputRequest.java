package monero.wallet.request;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;

import common.types.Filter;
import common.utils.GenUtils;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroTx;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxWallet;

/**
 * Configures a request to retrieve wallet outputs (i.e. outputs that the wallet has or had the
 * ability to spend).
 * 
 * All outputs are returned except those that do not meet the criteria defined in this request.
 */
public class MoneroOutputRequest extends MoneroOutputWallet implements Filter<MoneroOutputWallet> {

  private MoneroTxRequest txRequest;
  private List<Integer> subaddressIndices;
  
  public MoneroOutputRequest() {
    super();
  }
  
  public MoneroOutputRequest(final MoneroOutputRequest req) {
    super(req);
    if (req.subaddressIndices != null) this.subaddressIndices = new ArrayList<Integer>(req.subaddressIndices);
    this.txRequest = req.txRequest;  // reference original by default, MoneroTxRequest's deep copy will set this to itself
  }
  
  public MoneroOutputRequest copy() {
    return new MoneroOutputRequest(this);
  }
  
  @JsonIgnore
  public MoneroTxRequest getTxRequest() {
    return txRequest;
  }

  public MoneroOutputRequest setTxRequest(MoneroTxRequest txRequest) {
    this.txRequest = txRequest;
    return this;
  }
  
  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }

  public MoneroOutputRequest setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public MoneroOutputRequest setSubaddressIndices(Integer... subaddressIndices) {
    this.subaddressIndices = GenUtils.arrayToList(subaddressIndices);
    return this;
  }
  
  @Override
  public boolean meetsCriteria(MoneroOutputWallet output) {
    if (!(output instanceof MoneroOutputWallet)) return false;
    
    // filter on output
    if (this.getAccountIndex() != null && !this.getAccountIndex().equals(output.getAccountIndex())) return false;
    if (this.getSubaddressIndex() != null && !this.getSubaddressIndex().equals(output.getSubaddressIndex())) return false;
    if (this.getAmount() != null && this.getAmount().compareTo(output.getAmount()) != 0) return false;
    if (this.isSpent() != null && !this.isSpent().equals(output.isSpent())) return false;
    if (this.isUnlocked() != null && !this.isUnlocked().equals(output.isUnlocked())) return false;
    
    // filter on output key image
    if (this.getKeyImage() != null) {
      if (output.getKeyImage() == null) return false;
      if (this.getKeyImage().getHex() != null && !this.getKeyImage().getHex().equals(output.getKeyImage().getHex())) return false;
      if (this.getKeyImage().getSignature() != null && !this.getKeyImage().getSignature().equals(output.getKeyImage().getSignature())) return false;
    }
    
    // filter on extensions
    if (this.getSubaddressIndices() != null && !this.getSubaddressIndices().contains(output.getSubaddressIndex())) return false;
    
    // filter with tx request
    if (this.getTxRequest() != null && !this.getTxRequest().meetsCriteria(output.getTx())) return false;
    
    // output meets request
    return true;
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroOutputRequest setTx(MoneroTx tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroOutputRequest setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroOutputRequest setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }

  @Override
  public MoneroOutputRequest setSubaddressIndex(Integer subaddressIndex) {
    super.setSubaddressIndex(subaddressIndex);
    return this;
  }

  @Override
  public MoneroOutputRequest setIsSpent(Boolean isSpent) {
    super.setIsSpent(isSpent);
    return this;
  }
  
  @Override
  public MoneroOutputRequest setIsUnlocked(Boolean isUnlocked) {
    super.setIsUnlocked(isUnlocked);
    return this;
  }


  @Override
  public MoneroOutputRequest setKeyImage(MoneroKeyImage keyImage) {
    super.setKeyImage(keyImage);
    return this;
  }

  @Override
  public MoneroOutputRequest setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroOutputRequest setIndex(Integer index) {
    super.setIndex(index);
    return this;
  }

  @Override
  public MoneroOutputRequest setRingOutputIndices(List<Integer> ringOutputIndices) {
    super.setRingOutputIndices(ringOutputIndices);
    return this;
  }

  @Override
  public MoneroOutputRequest setStealthPublicKey(String stealthPublicKey) {
    super.setStealthPublicKey(stealthPublicKey);
    return this;
  }
}
