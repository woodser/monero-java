package monero.daemon.model;

/**
 * Models transaction pool statistics.
 */
public class MoneroTxPoolStats {

  private Integer numTxs;
  private Integer numNotRelayed;
  private Integer numFailing;
  private Integer numDoubleSpends;
  private Integer num10m;
  private Integer feeTotal;
  private Integer bytesMax;
  private Integer bytesMed;
  private Integer bytesMin;
  private Integer bytesTotal;
  private Object histo;
  private Integer histo98pc;
  private Integer oldestTimestamp;
  
  public Integer getNumTxs() {
    return numTxs;
  }
  
  public void setNumTxs(Integer numTxs) {
    this.numTxs = numTxs;
  }
  
  public Integer getNumNotRelayed() {
    return numNotRelayed;
  }
  
  public void setNumNotRelayed(Integer numNotRelayed) {
    this.numNotRelayed = numNotRelayed;
  }
  
  public Integer getNumFailing() {
    return numFailing;
  }
  
  public void setNumFailing(Integer numFailing) {
    this.numFailing = numFailing;
  }
  
  public Integer getNumDoubleSpends() {
    return numDoubleSpends;
  }
  
  public void setNumDoubleSpends(Integer numDoubleSpends) {
    this.numDoubleSpends = numDoubleSpends;
  }
  
  public Integer getNum10m() {
    return num10m;
  }
  
  public void setNum10m(Integer num10m) {
    this.num10m = num10m;
  }
  
  public Integer getFeeTotal() {
    return feeTotal;
  }
  
  public void setFeeTotal(Integer feeTotal) {
    this.feeTotal = feeTotal;
  }
  
  public Integer getBytesMax() {
    return bytesMax;
  }
  
  public void setBytesMax(Integer bytesMax) {
    this.bytesMax = bytesMax;
  }
  
  public Integer getBytesMed() {
    return bytesMed;
  }
  
  public void setBytesMed(Integer bytesMed) {
    this.bytesMed = bytesMed;
  }
  
  public Integer getBytesMin() {
    return bytesMin;
  }
  
  public void setBytesMin(Integer bytesMin) {
    this.bytesMin = bytesMin;
  }
  
  public Integer getBytesTotal() {
    return bytesTotal;
  }
  
  public void setBytesTotal(Integer bytesTotal) {
    this.bytesTotal = bytesTotal;
  }
  
  public Object getHisto() {
    return histo;
  }
  
  public void setHisto(Object histo) {
    this.histo = histo;
  }
  
  public Integer getHisto98pc() {
    return histo98pc;
  }
  
  public void setHisto98pc(Integer histo98pc) {
    this.histo98pc = histo98pc;
  }
  
  public Integer getOldestTimestamp() {
    return oldestTimestamp;
  }
  
  public void setOldestTimestamp(Integer oldestTimestamp) {
    this.oldestTimestamp = oldestTimestamp;
  }
}
