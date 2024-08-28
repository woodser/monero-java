package monero.daemon.model;

import java.math.BigInteger;

import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.JsonUtils;

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
  private Boolean hasTooFewOutputs;
  private Boolean isOverspend;
  private Boolean isTooBig;
  private Boolean sanityCheckFailed;
  private String reason;
  private BigInteger credits;
  private String topBlockHash;
  private Boolean isTxExtraTooBig;
  private Boolean isNonzeroUnlockTime;
  
  @JsonProperty("isGood")
  public Boolean isGood() {
    return isGood;
  }
  
  public void setIsGood(Boolean isGood) {
    this.isGood = isGood;
  }
  
  @JsonProperty("isRelayed")
  public Boolean isRelayed() {
    return isRelayed;
  }
  
  public void setIsRelayed(Boolean isRelayed) {
    this.isRelayed = isRelayed;
  }
  
  @JsonProperty("isDoubleSpend")
  public Boolean isDoubleSpend() {
    return isDoubleSpend;
  }
  
  public void setIsDoubleSpend(Boolean isDoubleSpend) {
    this.isDoubleSpend = isDoubleSpend;
  }
  
  @JsonProperty("isFeeTooLow")
  public Boolean isFeeTooLow() {
    return isFeeTooLow;
  }
  
  public void setIsFeeTooLow(Boolean isFeeTooLow) {
    this.isFeeTooLow = isFeeTooLow;
  }
  
  @JsonProperty("isMixinTooLow")
  public Boolean isMixinTooLow() {
    return isMixinTooLow;
  }
  
  public void setIsMixinTooLow(Boolean isMixinTooLow) {
    this.isMixinTooLow = isMixinTooLow;
  }
  
  @JsonProperty("hasInvalidInput")
  public Boolean hasInvalidInput() {
    return hasInvalidInput;
  }
  
  public void setHasInvalidInput(Boolean hasInvalidInput) {
    this.hasInvalidInput = hasInvalidInput;
  }
  
  public Boolean hasInvalidOutput() {
    return hasInvalidOutput;
  }
  
  public void setHasInvalidOutput(Boolean hasInvalidOutput) {
    this.hasInvalidOutput = hasInvalidOutput;
  }
  
  public Boolean hasTooFewOutputs() {
    return hasTooFewOutputs;
  }
  
  public void setHasTooFewOutputs(Boolean hasTooFewOutputs) {
    this.hasTooFewOutputs = hasTooFewOutputs;
  }
  
  @JsonProperty("isOverspend")
  public Boolean isOverspend() {
    return isOverspend;
  }
  
  public void setIsOverspend(Boolean isOverspend) {
    this.isOverspend = isOverspend;
  }
  
  @JsonProperty("isTooBig")
  public Boolean isTooBig() {
    return isTooBig;
  }
  
  public void setIsTooBig(Boolean isTooBig) {
    this.isTooBig = isTooBig;
  }
  
  public Boolean getSanityCheckFailed() {
    return sanityCheckFailed;
  }
  
  public void setSanityCheckFailed(Boolean sanityCheckFailed) {
    this.sanityCheckFailed = sanityCheckFailed;
  }
  
  public String getReason() {
    return reason;
  }
  
  public void setReason(String reason) {
    this.reason = reason;
  }
  
  public BigInteger getCredits() {
    return credits;
  }
  
  public void setCredits(BigInteger credits) {
    this.credits = credits;
  }

  public String getTopBlockHash() {
    return topBlockHash;
  }

  public void setTopBlockHash(String topBlockHash) {
    this.topBlockHash = topBlockHash;
  }

  @JsonProperty("isTxExtraTooBig")
  public Boolean isTxExtraTooBig() {
    return isTxExtraTooBig;
  }
  
  public void setIsTxExtraTooBig(Boolean isTxExtraTooBig) {
    this.isTxExtraTooBig = isTxExtraTooBig;
  }

  @JsonProperty("isNonzeroUnlockTime")
  public Boolean isNonzeroUnlockTime() {
    return isNonzeroUnlockTime;
  }
  
  public void setIsNonzeroUnlockTime(Boolean isNonzeroUnlockTime) {
    this.isNonzeroUnlockTime = isNonzeroUnlockTime;
  }
  
  public String toString() {
    return JsonUtils.serialize(this);
  }
}
