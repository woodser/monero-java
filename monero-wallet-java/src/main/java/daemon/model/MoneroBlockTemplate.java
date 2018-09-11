package daemon.model;

/**
 * Monero block template to mine.
 */
public class MoneroBlockTemplate extends MoneroDaemonModel {

  private String templateBlob;
  private String hashBlob;
  private Integer difficulty;
  private Integer expectedReward;
  private Integer height;
  private String prevHash;
  private Integer reservedOffset;
}
