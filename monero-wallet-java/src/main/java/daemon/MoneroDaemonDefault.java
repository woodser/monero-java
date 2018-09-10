package daemon;

import daemon.model.MoneroBan;

/**
 * Default Monero daemon implementation.
 */
public abstract class MoneroDaemonDefault implements MoneroDaemon {

  @Override
  public String setBan(MoneroBan ban) {
    throw new RuntimeException("Not implemented");
  }
}
