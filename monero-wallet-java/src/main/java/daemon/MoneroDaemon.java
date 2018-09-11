package daemon;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import daemon.model.MoneroBan;
import daemon.model.MoneroBlock;
import daemon.model.MoneroBlockCount;
import daemon.model.MoneroBlockHashes;
import daemon.model.MoneroBlockHeader;
import daemon.model.MoneroBlockTemplate;
import daemon.model.MoneroChain;
import daemon.model.MoneroCoinbaseTxSum;
import daemon.model.MoneroDaemonBandwidth;
import daemon.model.MoneroDaemonConnection;
import daemon.model.MoneroDaemonInfo;
import daemon.model.MoneroDaemonModel;
import daemon.model.MoneroFeeEstimate;
import daemon.model.MoneroHardForkInfo;
import daemon.model.MoneroMiningStatus;
import daemon.model.MoneroOutputDistributionEntry;
import daemon.model.MoneroOutputHistogramEntry;
import daemon.model.MoneroSyncInfo;
import daemon.model.MoneroTxPoolBacklog;
import wallet.model.MoneroKeyImage;
import wallet.model.MoneroTx;

/**
 * Monero daemon interface.
 */
public interface MoneroDaemon {

  /**
   * Get how many blocks are in the longest chain known to the node.
   * 
   * @return MoneroBlockCount contains the block count and response status
   */
  public MoneroBlockCount getBlockCount();
  
  /**
   * Get a block's hash by its height.
   * 
   * @param height is the height of the block hash to get
   * @return String is the block hash at the given height
   */
  public String getBlockHash(int height);
  
  /**
   * Get a block template for mining a new block.
   * 
   * @param walletAddress is the address of the wallet to receive coinbase transactions if block is successfully mined
   * @param reserveSize is the reserve size
   * @return MoneroBlockTemplate is a block template for mining a new block
   */
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize);
  
  /**
   * Submit a mined block to the network.
   * 
   * @param blockBlob is the mined block to submit
   * @return MoneroDaemonModel contains response status
   */
  public MoneroDaemonModel submitBlock(String blockBlob);
  
  /**
   * Get the last block's header.
   * 
   * @return MoneroBlockHeader is the last block's header
   */
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
  
  public MoneroDaemonModel setBan(MoneroBan ban );
  
  public MoneroDaemonModel setBans(Collection<MoneroBan> bans);
  
  /**
   * Flush all transactions from the transaction pool.
   * 
   * @return MoneroDaemonModel contains response information
   */
  public MoneroDaemonModel flushTxPool();
  
  /**
   * Flush specific transactions from the transaction pool or all if none specified.
   * 
   * @param txIds are transactions to flush from the pool, or all if none provided
   * @return MoneroDaemonModel contains response information
   */
  public MoneroDaemonModel flushTxPool(Collection<String> txIds);
  
  public List<MoneroOutputHistogramEntry> getOutputHistogram(List<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff);
  
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts, Boolean cumulative, Integer startHeight, Integer endHeight);
  
  public MoneroCoinbaseTxSum getCoinbaseTxSum(Integer height, Integer count);
  
  public MoneroFeeEstimate getFeeEstimate(Integer graceBlocks);
  
  public List<MoneroChain> getAlternativeChains();
  
  public MoneroDaemonModel relayTx(String txId);
  
  public MoneroDaemonModel relayTxs(Collection<String> txIds);
  
  public MoneroTxPoolBacklog getTxPoolBacklog();
  
  public MoneroBlockHashes getAltBlockHashes();
  
  public List<MoneroKeyImage> isKeyImageSpent(Collection<String> keyImageHexes);
  
  public List<MoneroTx> getTxs(Collection<String> hashes, Boolean prune);
  
  public MoneroDaemonModel startMining(String address, Integer numThreads, Boolean backgroundMining, Boolean ignoreBattery);
  
  public MoneroDaemonModel stopMining();
  
  public MoneroMiningStatus getMiningStatus();
  
  public MoneroDaemonModel setBandwidthLimit(Integer limitDown, Integer limitUp);
  
  public MoneroDaemonBandwidth getBandwidthLimit();
  
  public MoneroDaemonModel setNumOutgoingLimit(int limit);
  
  public MoneroDaemonModel setNumIncomingLimit(int limit);
}
