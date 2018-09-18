package monero.daemon.model;

/**
 * Number of blocks in the longest chain known to the node.
 */
public class MoneroBlockCount extends MoneroDaemonModel {

  private Integer count;

  public Integer getCount() {
    return count;
  }

  public void setCount(Integer count) {
    this.count = count;
  }
}
