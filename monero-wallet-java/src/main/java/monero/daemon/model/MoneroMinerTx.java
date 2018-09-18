package monero.daemon.model;

import java.util.Arrays;

import monero.wallet.model.MoneroTx;

/**
 * Represents a miner tx in a block.
 */
public class MoneroMinerTx extends MoneroTx {

  private Integer version;
  private int[] extra;
  
  public Integer getVersion() {
    return version;
  }
  
  public void setVersion(Integer version) {
    this.version = version;
  }

  public int[] getExtra() {
    return extra;
  }

  public void setExtra(int[] extra) {
    this.extra = extra;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + Arrays.hashCode(extra);
    result = prime * result + ((version == null) ? 0 : version.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    MoneroMinerTx other = (MoneroMinerTx) obj;
    if (!Arrays.equals(extra, other.extra)) return false;
    if (version == null) {
      if (other.version != null) return false;
    } else if (!version.equals(other.version)) return false;
    return true;
  }
}
