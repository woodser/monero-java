package monero.common;

import java.util.List;

/**
 * SSL options for remote endpoints.
 */
public class SslOptions {

  private String privateKeyPath;
  private String certificatePath;
  private String certificateAuthorityFile;
  private List<String> allowedFingerprints;
  private Boolean allowAnyCert;
  
  public String getPrivateKeyPath() {
    return privateKeyPath;
  }
  
  public void setPrivateKeyPath(String privateKeyPath) {
    this.privateKeyPath = privateKeyPath;
  }
  
  public String getCertificatePath() {
    return certificatePath;
  }
  
  public void setCertificatePath(String certificatePath) {
    this.certificatePath = certificatePath;
  }
  
  public String getCertificateAuthorityFile() {
    return certificateAuthorityFile;
  }
  
  public void setCertificateAuthorityFile(String certificateAuthorityFile) {
    this.certificateAuthorityFile = certificateAuthorityFile;
  }
  
  public List<String> getAllowedFingerprints() {
    return allowedFingerprints;
  }
  
  public void setAllowedFingerprints(List<String> allowedFingerprints) {
    this.allowedFingerprints = allowedFingerprints;
  }
  
  public Boolean getAllowAnyCert() {
    return allowAnyCert;
  }
  
  public void setAllowAnyCert(Boolean allowAnyCert) {
    this.allowAnyCert = allowAnyCert;
  }
}
