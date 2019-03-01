package monero.daemon;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroTx;

/**
 * Default Monero daemon implementation.
 */
public abstract class MoneroDaemonDefault implements MoneroDaemon {
  
  @Override
  public MoneroTx getTx(String txId) {
    return getTx(txId, null);
  }
  
  @Override
  public List<MoneroTx> getTxs(List<String> txIds) {
    return getTxs(txIds, null);
  }
  
  public String getTxHex(String txId) {
    return getTxHex(txId, false);
  }
  
  public List<String> getTxHexes(List<String> txIds) {
    return getTxHexes(txIds, null);
  }
  
  public BigInteger getFeeEstimate() {
    return getFeeEstimate(null);
  }
  
  @Override
  public void relayTxById(String txId) {
    relayTxsById(Arrays.asList(txId));
  }
  
  @Override
  public MoneroKeyImageSpentStatus getSpentStatus(String keyImage) {
    return getSpentStatuses(Arrays.asList(keyImage)).get(0);
  }
  
  @Override
  public void setPeerBan(MoneroBan ban) {
    setPeerBans(Arrays.asList(ban));
  }
  
  @Override
  public void submitBlock(String blockBlob) {
    submitBlocks(Arrays.asList(blockBlob));
  }
}
