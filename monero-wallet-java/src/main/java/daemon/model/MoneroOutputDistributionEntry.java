package daemon.model;

import java.math.BigInteger;
import java.util.List;

/**
 * Monero output distribution entry.
 */
public class MoneroOutputDistributionEntry extends MoneroDaemonStatus {

  private BigInteger amount;
  private Integer base;
  private List<Integer> distribution;
  private Integer startHeight;
}
