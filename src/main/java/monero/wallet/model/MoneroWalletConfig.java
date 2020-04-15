package monero.wallet.model;

import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpcConnection;

/**
 * Configuration to create new Monero wallets.
 */
public class MoneroWalletConfig {
  
  private String path;
  private String password;
  private MoneroNetworkType networkType;
  private String serverUri;
  private String serverUsername;
  private String serverPassword;
  private String mnemonic;
  private String seedOffset;
  private String primaryAddress;
  private String privateViewKey;
  private String privateSpendKey;
  private Long restoreHeight;
  private String language;
  private Boolean saveCurrent;
  
  public String getPath() {
    return path;
  }
  
  public MoneroWalletConfig setPath(String path) {
    this.path = path;
    return this;
  }
  
  public String getPassword() {
    return password;
  }
  
  public MoneroWalletConfig setPassword(String password) {
    this.password = password;
    return this;
  }
  
  public MoneroNetworkType getNetworkType() {
    return networkType;
  }
  
  public MoneroWalletConfig setNetworkType(MoneroNetworkType networkType) {
    this.networkType = networkType;
    return this;
  }
  
  public MoneroWalletConfig setNetworkType(String networkTypeStr) {
    return setNetworkType(MoneroNetworkType.parse(networkTypeStr));
  }
  
  public MoneroRpcConnection getServer() {
    return new MoneroRpcConnection(this.serverUri, this.serverUsername, this.serverPassword);
  }
  
  public MoneroWalletConfig setServer(MoneroRpcConnection server) {
    this.serverUri = server == null ? null : server.getUri();
    this.serverUsername = server == null ? null : server.getUsername();
    this.serverPassword = server == null ? null : server.getPassword();
    return this;
  }
  
  public String getServerUri() {
    return serverUri;
  }
  
  public MoneroWalletConfig setServerUri(String serverUri) {
    this.serverUri = serverUri;
    return this;
  }
  
  public String getServerUsername() {
    return serverUsername;
  }
  
  public MoneroWalletConfig setServerUsername(String serverUsername) {
    this.serverUsername = serverUsername;
    return this;
  }
  
  public String getServerPassword() {
    return serverPassword;
  }
  
  public MoneroWalletConfig setServerPassword(String serverPassword) {
    this.serverPassword = serverPassword;
    return this;
  }
  
  public String getMnemonic() {
    return mnemonic;
  }
  
  public MoneroWalletConfig setMnemonic(String mnemonic) {
    this.mnemonic = mnemonic;
    return this;
  }
  
  public String getSeedOffset() {
    return seedOffset;
  }
  
  public MoneroWalletConfig setSeedOffset(String seedOffset) {
    this.seedOffset = seedOffset;
    return this;
  }
  
  public String getPrimaryAddress() {
    return primaryAddress;
  }
  
  public MoneroWalletConfig setPrimaryAddress(String primaryAddress) {
    this.primaryAddress = primaryAddress;
    return this;
  }
  
  public String getPrivateViewKey() {
    return privateViewKey;
  }
  
  public MoneroWalletConfig setPrivateViewKey(String privateViewKey) {
    this.privateViewKey = privateViewKey;
    return this;
  }
  
  public String getPrivateSpendKey() {
    return privateSpendKey;
  }
  
  public MoneroWalletConfig setPrivateSpendKey(String privateSpendKey) {
    this.privateSpendKey = privateSpendKey;
    return this;
  }
  
  public Long getRestoreHeight() {
    return restoreHeight;
  }
  
  public MoneroWalletConfig setRestoreHeight(Long restoreHeight) {
    this.restoreHeight = restoreHeight;
    return this;
  }
  
  public String getLanguage() {
    return language;
  }
  
  public MoneroWalletConfig setLanguage(String language) {
    this.language = language;
    return this;
  }
  
  public Boolean getSaveCurrent() {
    return saveCurrent;
  }
  
  public MoneroWalletConfig setSaveCurrent(Boolean saveCurrent) {
    this.saveCurrent = saveCurrent;
    return this;
  }
}