package monero.daemon.model;

/**
 * Enumerates daemon networks.
 */
public enum MoneroNetworkType {
  MAINNET(18, 19),
  TESTNET(53, 54),
  STAGENET(53, 54);

  private final int codeForRegularAddress;
  private final int codeForIntegratedAddress;

  private MoneroNetworkType(int codeForRegularAddress, int codeForIntegratedAddress) {
    this.codeForRegularAddress = codeForRegularAddress;
    this.codeForIntegratedAddress = codeForIntegratedAddress;
  }

  public int getCodeForRegularAddress() {
    return this.codeForRegularAddress;
  }

  public int getCodeForIntegratedAddress() {
    return this.codeForIntegratedAddress;
  }
}
