package monero.wallet.config;

import java.math.BigInteger;
import java.util.List;

import common.types.Filter;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;

/**
 * Filters transfers that don't meet initialized filter criteria.
 */
public class MoneroTransferFilter extends MoneroTransfer implements Filter<MoneroTransfer> {

  private boolean isOutgoing;
  private boolean isIncoming;
  private List<Integer> subaddressIndices;
  private Boolean hasDestinations;
  private MoneroTxFilter txFilter;
  
  public Boolean getIsOutgoing() {
    return isOutgoing;
  }
  
  public MoneroTransferFilter setIsOutgoing(Boolean isOutgoing) {
    this.isOutgoing = isOutgoing;
    return this;
  }
  
  public Boolean getIsIncoming() {
    return isIncoming;
  }

  public MoneroTransferFilter setIsIncoming(Boolean isIncoming) {
    this.isIncoming = isIncoming;
    return this;
  }
  
  public Boolean getHasDestinations() {
    return hasDestinations;
  }
  
  public MoneroTransferFilter setHasDestinations(Boolean hasDestinations) {
    this.hasDestinations = hasDestinations;
    return this;
  }

  public List<Integer> getSubaddressIndices() {
    return subaddressIndices;
  }
  
  public MoneroTransferFilter setSubaddressIndices(List<Integer> subaddressIndices) {
    this.subaddressIndices = subaddressIndices;
    return this;
  }
  
  public MoneroTxFilter getTxFilter() {
    return txFilter;
  }
  
  public MoneroTransferFilter setTxFilter(MoneroTxFilter txFilter) {
    this.txFilter = txFilter;
    return this;
  }

  @Override
  public boolean meetsCriteria(MoneroTransfer item) {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------- OVERRIDE CO-VARIANT RETURN TYPES ---------------------

  @Override
  public MoneroTransferFilter setTx(MoneroTxWallet tx) {
    super.setTx(tx);
    return this;
  }

  @Override
  public MoneroTransferFilter setAddress(String address) {
    super.setAddress(address);
    return this;
  }

  @Override
  public MoneroTransferFilter setAccountIndex(Integer accountIndex) {
    super.setAccountIndex(accountIndex);
    return this;
  }

  @Override
  public MoneroTransferFilter setSubaddressIndex(Integer subaddressIndex) {
    super.setSubaddressIndex(subaddressIndex);
    return this;
  }

  @Override
  public MoneroTransferFilter setAmount(BigInteger amount) {
    super.setAmount(amount);
    return this;
  }

  @Override
  public MoneroTransferFilter setDestinations(List<MoneroDestination> destinations) {
    super.setDestinations(destinations);
    return this;
  }
}
