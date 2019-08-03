package monero.wallet.model;

/**
 * Base class for results from checking a transaction or reserve proof.
 */
public class MoneroCheck {

  public Boolean isGood;

  public Boolean isGood() {
    return isGood;
  }

  public void setIsGood(Boolean isGood) {
    this.isGood = isGood;
  }
}
