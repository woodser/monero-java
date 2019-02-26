package monero.daemon.model;

/**
 * Models the result from submitting a tx to a daemon.
 */
public class MoneroSubmitTxResult {

  private Boolean isGood;
  private Boolean isRelayed;
  private Boolean isDoubleSpend;
  private Boolean isFeeTooLow;
  private Boolean isMixinTooLow;
  private Boolean hasInvalidInput;
  private Boolean hasInvalidOutput;
  private Boolean isRct;
  private Boolean isOverspend;
  private Boolean isTooBig;
  private String reason;
  
  public Boolean getIsGood() {
    return isGood;
  }
  
  public void setIsGood(Boolean isGood) {
    this.isGood = isGood;
  }
  
  public Boolean getIsRelayed() {
    return isRelayed;
  }
  
  public void setIsRelayed(Boolean isRelayed) {
    this.isRelayed = isRelayed;
  }
  
  public Boolean getIsDoubleSpend() {
    return isDoubleSpend;
  }
  
  public void setIsDoubleSpend(Boolean isDoubleSpend) {
    this.isDoubleSpend = isDoubleSpend;
  }
  
  public Boolean getIsFeeTooLow() {
    return isFeeTooLow;
  }
  
  public void setIsFeeTooLow(Boolean isFeeTooLow) {
    this.isFeeTooLow = isFeeTooLow;
  }
  
  public Boolean getIsMixinTooLow() {
    return isMixinTooLow;
  }
  
  public void setIsMixinTooLow(Boolean isMixinTooLow) {
    this.isMixinTooLow = isMixinTooLow;
  }
  
  public Boolean getHasInvalidInput() {
    return hasInvalidInput;
  }
  
  public void setHasInvalidInput(Boolean hasInvalidInput) {
    this.hasInvalidInput = hasInvalidInput;
  }
  
  public Boolean getHasInvalidOutput() {
    return hasInvalidOutput;
  }
  
  public void setHasInvalidOutput(Boolean hasInvalidOutput) {
    this.hasInvalidOutput = hasInvalidOutput;
  }
  
  public Boolean getIsRct() {
    return isRct;
  }
  
  public void setIsRct(Boolean isRct) {
    this.isRct = isRct;
  }
  
  public Boolean getIsOverspend() {
    return isOverspend;
  }
  
  public void setIsOverspend(Boolean isOverspend) {
    this.isOverspend = isOverspend;
  }
  
  public Boolean getIsTooBig() {
    return isTooBig;
  }
  
  public void setIsTooBig(Boolean isTooBig) {
    this.isTooBig = isTooBig;
  }
  
  public String getReason() {
    return reason;
  }
  
  public void setReason(String reason) {
    this.reason = reason;
  }
}
