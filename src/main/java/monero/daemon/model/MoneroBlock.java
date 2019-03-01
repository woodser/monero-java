package monero.daemon.model;

import java.util.List;

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
  
  public MoneroBlock setHeader(MoneroBlockHeader header) {
    this.header = header;
    return this;
  }
  
  public String getHex() {
    return hex;
  }
  
  public MoneroBlock setHex(String hex) {
    this.hex = hex;
    return this;
  }
  
  public MoneroTx getCoinbaseTx() {
    return coinbaseTx;
  }
  
  public MoneroBlock setCoinbaseTx(MoneroTx coinbaseTx) {
    this.coinbaseTx = coinbaseTx;
    return this;
  }
  
  public List<MoneroTx> getTxs() {
    return txs;
  }
  
  public MoneroBlock setTxs(List<MoneroTx> txs) {
    this.txs = txs;
    return this;
  }
  
  public List<String> getTxIds() {
    return txIds;
  }
  
  public MoneroBlock setTxIds(List<String> txIds) {
    this.txIds = txIds;
    return this;
  }
  
  public MoneroBlock copy() {
    throw new RuntimeException("Not implemented");
  }
}
