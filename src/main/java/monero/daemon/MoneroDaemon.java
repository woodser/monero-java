package monero.daemon;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockCount;
import monero.daemon.model.MoneroBlockHashes;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroChain;
import monero.daemon.model.MoneroCoinbaseTxSum;
import monero.daemon.model.MoneroDaemonBandwidth;
import monero.daemon.model.MoneroDaemonConnection;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonModel;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroFeeEstimate;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroOutputHistogramEntry;
import monero.daemon.model.MoneroTxPoolBacklog;
import monero.wallet.model.MoneroKeyImage;
import monero.wallet.model.MoneroTx;

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
  
  public MoneroDaemonSyncInfo getSyncInfo();
  
  public MoneroHardForkInfo getHardForkInfo();
  
  public MoneroDaemonModel setBan(MoneroBan ban );
  
  public MoneroDaemonModel setBans(Collection<MoneroBan> bans);
  
  public Collection<MoneroBan> getBans();
  
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
  
  /**
   * Gets the total emissions and fees from the genesis block to the current height.
   * 
   * @return MoneroCoinbaseTxSum encapsulates the total emissions and fees since the genesis block
   */
  public MoneroCoinbaseTxSum getCoinbaseTxSum();
  
  public MoneroCoinbaseTxSum getCoinbaseTxSum(int height, int count);
  
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
