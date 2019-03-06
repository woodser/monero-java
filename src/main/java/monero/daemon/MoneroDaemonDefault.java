package monero.daemon;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;

/**
 * Default Monero daemon implementation.
 */
public abstract class MoneroDaemonDefault implements MoneroDaemon {
  
  @Override
  public MoneroBlockTemplate getBlockTemplate(String walletAddress) {
    return getBlockTemplate(walletAddress, null);
  }
  
  @Override
  public MoneroTx getTx(String txId) {
    return getTx(txId, null);
  }
  
  @Override
  public MoneroTx getTx(String txId, Boolean prune) {
    return getTxs(Arrays.asList(txId), prune).get(0);
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
  
  public MoneroSubmitTxResult submitTxHex(String txHex) {
    return submitTxHex(txHex, false);
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
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts) {
    return getOutputDistribution(amounts, null, null, null);
  }
  
  @Override
  public void setPeerBan(MoneroBan ban) {
    setPeerBans(Arrays.asList(ban));
  }
  
  @Override
  public void submitBlock(String blockBlob) {
    submitBlocks(Arrays.asList(blockBlob));
  }
  
  @Override
  public MoneroDaemonUpdateCheckResult checkForUpdate() {
    return checkForUpdate(null);
  }
  
  @Override
  public MoneroDaemonUpdateDownloadResult downloadUpdate() {
    return downloadUpdate(null);
  }
}
