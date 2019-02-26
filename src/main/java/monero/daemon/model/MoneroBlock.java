package monero.daemon.model;

import java.util.List;

import monero.daemon.model.MoneroTx;

/**
 * Monero block.
 */
public class MoneroBlock {

  private MoneroBlockHeader header;
  private String hex;
  private MoneroTx coinbaseTx;
  private List<MoneroTx> txs;
  private List<String> txIds;
  
  public MoneroBlockHeader getHeader() {
    return header;
  }
  
  public void setHeader(MoneroBlockHeader header) {
    this.header = header;
  }
  
  public String getHex() {
    return hex;
  }
  
  public void setHex(String hex) {
    this.hex = hex;
  }
  
  public MoneroTx getCoinbaseTx() {
    return coinbaseTx;
  }
  
  public void setCoinbaseTx(MoneroTx coinbaseTx) {
    this.coinbaseTx = coinbaseTx;
  }
  
  public List<MoneroTx> getTxs() {
    return txs;
  }
  
  public void setTxs(List<MoneroTx> txs) {
    this.txs = txs;
  }
  
  public List<String> getTxIds() {
    return txIds;
  }
  
  public void setTxIds(List<String> txIds) {
    this.txIds = txIds;
  }
}
