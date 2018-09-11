package daemon.model;

import java.math.BigInteger;

/**
 * Monero block header information.
 */
public class MoneroBlockHeader extends MoneroDaemonModel {
  
  private Integer size;
  private Integer depth;
  private BigInteger difficulty;
  private String hash;
  private Integer height;
  private Integer majorVersion;
  private Integer minorVersion;
  private BigInteger nonce;
  private Integer numTxs;
  private Boolean orphanStatus;
  private String prevHash;
  private BigInteger reward;
  private Long timestamp;
}
