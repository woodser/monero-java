package monero.daemon.model;

/**
 * Monero daemon mining status.
 */
public class MoneroMiningStatus extends MoneroDaemonModel {
  
  private String address;
  private Integer numThreads;
  private Boolean active;
  private Boolean isBackgroundMining;
  private Integer speed;
}
