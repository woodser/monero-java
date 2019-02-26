package monero.wallet.model;

/**
 * Result from syncing a Monero wallet.
 */
public class MoneroSyncResult {

  private Integer numBlocksFetched;
  private Boolean receivedAmount;
  
  public Integer getNumBlocksFetched() {
    return numBlocksFetched;
  }
  
  public void setNumBlocksFetched(Integer numBlocksFetched) {
    this.numBlocksFetched = numBlocksFetched;
  }
  
  public Boolean getReceivedAmount() {
    return receivedAmount;
  }
  
  public void setReceivedAmount(Boolean receivedAmount) {
    this.receivedAmount = receivedAmount;
  }
}
