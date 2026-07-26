package monero.wallet.model;

import java.util.List;

import monero.daemon.model.MoneroKeyImage;

/**
 * Models results from exporting signed key images.
 */
public class MoneroKeyImageExportResult {

  private Long offset;
  private List<MoneroKeyImage> keyImages;

  public Long getOffset() {
    return offset;
  }

  public void setOffset(Long offset) {
    this.offset = offset;
  }

  public List<MoneroKeyImage> getKeyImages() {
    return keyImages;
  }

  public void setKeyImages(List<MoneroKeyImage> keyImages) {
    this.keyImages = keyImages;
  }
}
