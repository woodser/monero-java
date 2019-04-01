package monero.daemon.model;

import java.util.List;

import monero.utils.MoneroUtils;

/**
 * Monero block.
 */
public class MoneroBlock extends MoneroBlockHeader {

  private String hex;
  private MoneroTx coinbaseTx;
  private List<MoneroTx> txs;
  private List<String> txIds;
  
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
  
  public String toString() {
    return toString(0);
  }
  
  public String toString(int indent) {
    StringBuilder sb = new StringBuilder();
    sb.append(super.toString(indent));
    sb.append("\n");
    sb.append(MoneroUtils.kvLine("Hex", getHex(), indent));
    sb.append(MoneroUtils.kvLine("Txs ids", getTxIds(), indent));
    if (getCoinbaseTx() != null) {
      sb.append(MoneroUtils.kvLine("Coinbase tx", "", indent));
      sb.append(getCoinbaseTx().toString(indent + 1) + "\n");
    }
    if (getTxs() != null) {
      sb.append(MoneroUtils.kvLine("Txs", "", indent));
      for (MoneroTx tx : getTxs()) {
        sb.append(tx.toString(indent + 1) + "\n");
      }
    }
    String str = sb.toString();
    return str.charAt(str.length() - 1) == '\n' ? str.substring(0, str.length() - 1) : str; // strip newline
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((coinbaseTx == null) ? 0 : coinbaseTx.hashCode());
    result = prime * result + ((hex == null) ? 0 : hex.hashCode());
    result = prime * result + ((txIds == null) ? 0 : txIds.hashCode());
    result = prime * result + ((txs == null) ? 0 : txs.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!super.equals(obj)) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroBlock other = (MoneroBlock) obj;
    if (coinbaseTx == null) {
      if (other.coinbaseTx != null) return false;
    } else if (!coinbaseTx.equals(other.coinbaseTx)) return false;
    if (hex == null) {
      if (other.hex != null) return false;
    } else if (!hex.equals(other.hex)) return false;
    if (txIds == null) {
      if (other.txIds != null) return false;
    } else if (!txIds.equals(other.txIds)) return false;
    if (txs == null) {
      if (other.txs != null) return false;
    } else if (!txs.equals(other.txs)) return false;
    return true;
  }
}
