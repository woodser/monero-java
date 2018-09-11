package daemon;

import java.util.ArrayList;
import java.util.Collection;

import daemon.model.MoneroBan;
import daemon.model.MoneroDaemonModel;

/**
 * Default Monero daemon implementation.
 */
public abstract class MoneroDaemonDefault implements MoneroDaemon {

  @Override
  public MoneroDaemonModel setBan(MoneroBan ban) {
    throw new RuntimeException("Not implemented");
  }
  
  @Override
  public MoneroDaemonModel relayTx(String txId) {
    Collection<String> txIds = new ArrayList<>();
    txIds.add(txId);
    return relayTxs(txIds);
  }
}
