package daemon.model;

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
}
