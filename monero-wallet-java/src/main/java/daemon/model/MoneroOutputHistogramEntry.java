package daemon.model;

import java.math.BigInteger;

/**
 * Entry in a Monero output histogram (see get_output_histogram of Daemon RPC documentation).
 */
public class MoneroOutputHistogramEntry {

  private BigInteger amount;
  private Integer numTotalInstances;
  private Integer numUnlockedInstances;
  private Integer numRecentInstances;
}
