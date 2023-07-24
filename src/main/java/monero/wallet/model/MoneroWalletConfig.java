package monero.wallet.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import common.utils.JsonUtils;
import monero.common.MoneroRpcConnection;
import monero.daemon.model.MoneroNetworkType;

/**
 * Configuration to create a Monero wallet.
 */
public class MoneroWalletConfig {
  
  private String path;
  private String password;
  private MoneroNetworkType networkType;
  private String serverUri;
  private String serverUsername;
  private String serverPassword;
  private String seed;
  private String seedOffset;
  private String primaryAddress;
  private String privateViewKey;
  private String privateSpendKey;
  private Long restoreHeight;
  private String language;
  private Boolean saveCurrent;
  private Integer accountLookahead;     // number of accounts to scan
  private Integer subaddressLookahead;  // number of subaddresses to scan per account
  private byte[] keysData;
  private byte[] cacheData;
  private Boolean isMultisig;

  public MoneroWalletConfig() {
    // default constructor
  }

  public MoneroWalletConfig(MoneroWalletConfig config) {
    path = config.getPath();
    password = config.getPassword();
    networkType = config.getNetworkType();
    serverUri = config.getServerUri();
    serverUsername = config.getServerUsername();
    serverPassword = config.getServerPassword();
    seed = config.getSeed();
    seedOffset = config.getSeedOffset();
    primaryAddress = config.getPrimaryAddress();
    privateViewKey = config.getPrivateViewKey();
    privateSpendKey = config.getPrivateSpendKey();
    restoreHeight = config.getRestoreHeight();
    language = config.getLanguage();
    saveCurrent = config.getSaveCurrent();
    accountLookahead = config.getAccountLookahead();
    subaddressLookahead = config.getSubaddressLookahead();
    keysData = config.getKeysData();
    cacheData = config.getCacheData();
    isMultisig = config.isMultisig();
  }

  public MoneroWalletConfig copy() {
    return new MoneroWalletConfig(this);
  }
  
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
    return this.serverUri == null || this.serverUri.isEmpty() ? null : new MoneroRpcConnection(this.serverUri, this.serverUsername, this.serverPassword);
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
  
  public String getSeed() {
    return seed;
  }
  
  public MoneroWalletConfig setSeed(String seed) {
    this.seed = seed;
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
  
  /**
   * Set the number of accounts of scan.
   * 
   * @param accountLookahead the number of accounts to scan
   * @return this config for convenience
   */
  public MoneroWalletConfig setAccountLookahead(Integer accountLookahead) {
    this.accountLookahead = accountLookahead;
    return this;
  }
  
  public Integer getAccountLookahead() {
    return accountLookahead;
  }
  
  /**
   * Set the number of subaddresses to scan per account.
   * 
   * @param subaddressLookahead the number of subaddresses to scan per account
   * @return this config for convenience
   */
  public MoneroWalletConfig setSubaddressLookahead(Integer subaddressLookahead) {
    this.subaddressLookahead = subaddressLookahead;
    return this;
  }
  
  public Integer getSubaddressLookahead() {
    return subaddressLookahead;
  }

  public byte[] getKeysData() {
    return keysData;
  }

  public MoneroWalletConfig setKeysData(byte[] keysData) {
    this.keysData = keysData;
    return this;
  }

  public byte[] getCacheData() {
    return cacheData;
  }

  public MoneroWalletConfig setCacheData(byte[] cacheData) {
    this.cacheData = cacheData;
    return this;
  }

  @JsonProperty("isMultisig")
  public Boolean isMultisig() {
    return isMultisig;
  }
  
  public MoneroWalletConfig setIsMultisig(Boolean isMultisig) {
    this.isMultisig = isMultisig;
    return this;
  }

  @Override
  public String toString() {
    return JsonUtils.serialize(this);
  }
}