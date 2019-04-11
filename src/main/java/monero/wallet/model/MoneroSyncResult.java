package monero.wallet.model;

/**
 * Result from syncing a Monero wallet.
 */
public class MoneroSyncResult {

  private Integer numBlocksFetched;
  private Boolean receivedMoney;
  
  public MoneroSyncResult() {
    this(null, null);
  }
  
  public MoneroSyncResult(Integer numBlocksFetched, Boolean receivedMoney) {
    this.numBlocksFetched = numBlocksFetched;
    this.receivedMoney = receivedMoney;
  }
  
  public Integer getNumBlocksFetched() {
    return numBlocksFetched;
  }
  
  public void setNumBlocksFetched(Integer numBlocksFetched) {
    this.numBlocksFetched = numBlocksFetched;
  }
  
  public Boolean getReceivedMoney() {
    return receivedMoney;
  }
  
  public void setReceivedMoney(Boolean receivedMoney) {
    this.receivedMoney = receivedMoney;
  }
}
