package monero.daemon.model;

import java.util.List;

/**
 * Monero block.
 */
public class MoneroBlock extends MoneroDaemonModel {

  private String blob;
  private MoneroBlockHeader header;
  private MoneroMinerTx minerTx;
  private List<String> txHashes;
  
  public String getBlob() {
    return blob;
  }
  
  public void setBlob(String blob) {
    this.blob = blob;
  }
  
  public MoneroBlockHeader getHeader() {
    return header;
  }
  
  public void setHeader(MoneroBlockHeader header) {
    this.header = header;
  }
  
  public MoneroMinerTx getMinerTx() {
    return minerTx;
  }
  
  public void setMinerTx(MoneroMinerTx minerTx) {
    this.minerTx = minerTx;
  }
  
  public List<String> getTxHashes() {
    return txHashes;
  }
  
  public void setTxHashes(List<String> txHashes) {
    this.txHashes = txHashes;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((blob == null) ? 0 : blob.hashCode());
    result = prime * result + ((header == null) ? 0 : header.hashCode());
    result = prime * result + ((minerTx == null) ? 0 : minerTx.hashCode());
    result = prime * result + ((txHashes == null) ? 0 : txHashes.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroBlock other = (MoneroBlock) obj;
    if (blob == null) {
      if (other.blob != null) return false;
    } else if (!blob.equals(other.blob)) return false;
    if (header == null) {
      if (other.header != null) return false;
    } else if (!header.equals(other.header)) return false;
    if (minerTx == null) {
      if (other.minerTx != null) return false;
    } else if (!minerTx.equals(other.minerTx)) return false;
    if (txHashes == null) {
      if (other.txHashes != null) return false;
    } else if (!txHashes.equals(other.txHashes)) return false;
    return true;
  }
}
