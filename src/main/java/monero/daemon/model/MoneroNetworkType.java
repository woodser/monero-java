package monero.daemon.model;

/**
 * Enumerates daemon networks.
 */
public enum MoneroNetworkType {
  MAINNET(18, 19, 42),
  TESTNET(53, 54, 63),
  STAGENET(24, 25, 36);

  private final int primaryAddressCode;
  private final int integratedAddressCode;
  private final int subaddressCode;

  private MoneroNetworkType(int primaryAddressCode, int integratedAddressCode, int subaddressCode) {
    this.primaryAddressCode = primaryAddressCode;
    this.integratedAddressCode = integratedAddressCode;
    this.subaddressCode = subaddressCode;
  }

  public int getPrimaryAddressCode() {
    return primaryAddressCode;
  }

  public int getIntegratedAddressCode() {
    return integratedAddressCode;
  }
  
  public int getSubaddressCode() {
    return subaddressCode;
  }
}
