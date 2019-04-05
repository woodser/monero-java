package monero.daemon.model;

import java.util.ArrayList;
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
  
  public MoneroBlock() {
    super();
  }
  
  public MoneroBlock(MoneroBlock block) {
    super(block);
    this.hex = block.getHex();
    this.coinbaseTx = block.coinbaseTx.copy().setBlock(this);
    if (block.txs != null) {
      this.txs = new ArrayList<MoneroTx>();
      for (MoneroTx tx : block.txs) txs.add(tx.copy().setBlock(this));
    }
    if (block.getTxIds() != null) this.txIds = new ArrayList<String>(block.getTxIds());
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
    return new MoneroBlock(this);
  }
  
  public MoneroBlock merge(MoneroBlock block) {
    throw new RuntimeException("Not implemented");
//    assert(block instanceof MoneroBlock);
//    if (this === block) return;
//    
//    // merge header fields
//    super.merge(block);
//    
//    // merge coinbase tx
//    if (!this.getCoinbaseTx()) this.setCoinbaseTx(block.getCoinbaseTx());
//    else if (block.getCoinbaseTx()) this.getCoinbaseTx().merge(block.getCoinbaseTx());
//    
//    // merge non-coinbase txs
//    if (!this.getTxs()) this.setTxs(block.getTxs());
//    else if (block.getTxs()) {
//      for (let thatTx of block.getTxs()) {
//        let found = false;
//        for (let thisTx of this.getTxs()) {
//          if (thatTx.getId() === thisTx.getId()) {
//            thisTx.merge(thatTx);
//            found = true;
//            break;
//          }
//        }
//        if (!found) this.getTxs().push(thatTx);
//      }
//    }
//    if (this.getTxs()) for (let tx of this.getTxs()) tx.setBlock(this);
//    
//    // merge other fields
//    this.setHex(MoneroUtils.reconcile(this.getHex(), block.getHex()));
//    this.setTxIds(MoneroUtils.reconcile(this.getTxIds(), block.getTxIds()));
//    return this;
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
