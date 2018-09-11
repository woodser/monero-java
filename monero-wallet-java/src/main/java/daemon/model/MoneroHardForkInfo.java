package daemon.model;

/**
 * Monero hard fork info.
 */
public class MoneroHardForkInfo extends MoneroDaemonModel {

  private Integer earliestHeight;
  private Boolean isEnabled;
  private Integer state;
  private Integer threshold;
  private Integer version;
  private Integer votes;
  private Integer voting;
  private Integer window;
}
