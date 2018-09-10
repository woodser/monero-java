package daemon;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import daemon.model.MoneroBan;
import daemon.model.MoneroBlock;
import daemon.model.MoneroBlockCount;
import daemon.model.MoneroBlockHeader;
import daemon.model.MoneroBlockTemplate;
import daemon.model.MoneroChain;
import daemon.model.MoneroCoinbaseTxSum;
import daemon.model.MoneroDaemonConnection;
import daemon.model.MoneroDaemonInfo;
import daemon.model.MoneroDaemonStatus;
import daemon.model.MoneroFeeEstimate;
import daemon.model.MoneroHardForkInfo;
import daemon.model.MoneroOutputDistributionEntry;
import daemon.model.MoneroOutputHistogramEntry;
import daemon.model.MoneroSyncInfo;
import daemon.model.MoneroTxPoolBacklog;

/**
 * Monero daemon interface.
 */
public interface MoneroDaemon {

  public MoneroBlockCount getBlockCount();
  
  public String getBlockHash(int height);
  
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize);
  
  public MoneroDaemonStatus submitBlock(String blockBlob);
  
  public MoneroBlockHeader getLastBlockHeader();  
  
  public MoneroBlockHeader getBlockHeader(String hash);
  
  public MoneroBlockHeader getBlockHeader(int height);
  
  public List<MoneroBlockHeader> getBlockHeaders(int startHeight, int endHeight);
  
  public MoneroBlock getBlock(String hash);
  
  public MoneroBlock getBlock(int height);
  
  public List<MoneroDaemonConnection> getConnections();
  
  public MoneroDaemonInfo getInfo();
  
  public MoneroSyncInfo getSyncInfo();
  
  public MoneroHardForkInfo getHardForkInfo();
  
  public MoneroDaemonStatus setBan(MoneroBan ban );
  
  public MoneroDaemonStatus setBans(Collection<MoneroBan> bans);
  
  /**
   * Flush all transactions from the transaction pool.
   * 
   * @return String is the resulting RPC error code. "OK" means everything looks good
   */
  public MoneroDaemonStatus flushTxPool();
  
  /**
   * Flush specific transactions from the transaction pool or all if none specified.
   * 
   * @param txIds are transactions to flush from the pool, or all if none provided
   * @return 
   */
  public MoneroDaemonStatus flushTxPool(Collection<String> txIds);
  
  public List<MoneroOutputHistogramEntry> getOutputHistogram(List<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff);
  
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts, Boolean cumulative, Integer startHeight, Integer endHeight);
  
  public MoneroCoinbaseTxSum getCoinbaseTxSum(Integer height, Integer count);
  
  public MoneroFeeEstimate getFeeEstimate(Integer graceBlocks);
  
  public List<MoneroChain> getAlternativeChains();
  
  public MoneroDaemonStatus relayTx(String txId);
  
  public MoneroDaemonStatus relayTxs(Collection<String> txIds);
  
  public MoneroTxPoolBacklog getTxPoolBacklog();
}
