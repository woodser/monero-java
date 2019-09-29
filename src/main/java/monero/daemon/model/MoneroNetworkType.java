package monero.daemon.model;

/**
 * Enumerates daemon networks.
 */
public enum MoneroNetworkType {
  MAINNET(18, 19),
  TESTNET(53, 54),
  STAGENET(53, 54);

  private final int standardAddressCode;
  private final int integratedAddressCode;

  private MoneroNetworkType(int standardAddressCode, int integratedAddressCode) {
    this.standardAddressCode = standardAddressCode;
    this.integratedAddressCode = integratedAddressCode;
  }

  public int getStandardAddressCode() {
    return this.standardAddressCode;
  }

  public int getIntegratedAddressCode() {
    return this.integratedAddressCode;
  }
}
