package daemon.model;

/**
 * Models a chain seen by the network.
 */
public class MoneroChain extends MoneroDaemonStatus {

  private String blockHash;
  private Integer difficulty;
  private Integer height;
  private Integer length;
}
