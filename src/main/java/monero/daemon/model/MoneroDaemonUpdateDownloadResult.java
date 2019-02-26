package monero.daemon.model;

/**
 * Models the result of downloading an update.
 */
public class MoneroDaemonUpdateDownloadResult extends MoneroDaemonUpdateCheckResult {

  private String downloadPath;

  public String getDownloadPath() {
    return downloadPath;
  }

  public void setDownloadPath(String downloadPath) {
    this.downloadPath = downloadPath;
  }
}
