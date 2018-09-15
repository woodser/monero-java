package daemon;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import daemon.model.MoneroBan;
import daemon.model.MoneroCoinbaseTxSum;
import daemon.model.MoneroDaemonModel;

/**
 * Default Monero daemon implementation.
 */
public abstract class MoneroDaemonDefault implements MoneroDaemon {
  
  /**
   * Daemon networks.
   */
  public enum MoneroNetworkType {
    MAINNET,
    STAGENET,
    TESTNET
  }

  @Override
  public MoneroDaemonModel setBan(MoneroBan ban) {
    return setBans(Arrays.asList(ban));
  }
  
  @Override
  public MoneroDaemonModel relayTx(String txId) {
    Collection<String> txIds = new ArrayList<>();
    txIds.add(txId);
    return relayTxs(txIds);
  }
  
  @Override
  public MoneroDaemonModel flushTxPool() {
    return flushTxPool(null);
  }
  
  @Override
  public MoneroCoinbaseTxSum getCoinbaseTxSum() {
    return getCoinbaseTxSum(0, getInfo().getHeight());
  }
}
