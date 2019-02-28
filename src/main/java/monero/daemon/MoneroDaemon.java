/**
 * MIT License
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package monero.daemon;

import java.math.BigInteger;
import java.util.List;

import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockListener;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroCoinbaseTxSum;
import monero.daemon.model.MoneroDaemonConnection;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonPeer;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroOutputHistogramEntry;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxBacklogEntry;
import monero.daemon.model.MoneroTxPoolStats;

/**
 * Monero daemon interface.
 */
public interface MoneroDaemon {
  
  /**
   * Indicates if the daemon is trusted xor untrusted.
   * 
   * @return true if the daemon is trusted, false otherwise
   */
  public boolean getIsTrusted();
  
  /**
   * Get the number of blocks in the longest chain known to the node.
   * 
   * @return the number of blocks
   */
  public int getHeight();
  
  /**
   * Get a block's id by its height.
   * 
   * @param height is the height of the block id to get
   * @return the block's id at the given height
   */
  public String getBlockId(int height);
  
  /**
   * Get a block template for mining a new block.
   * 
   * @param walletAddress is the address of the wallet to receive coinbase transactions if block is successfully mined
   * @param reserveSize is the reserve size
   * @return a block template for mining a new block
   */
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize);
  
  /**
   * Get the last block's header.
   * 
   * @return the last block's header
   */
  public MoneroBlockHeader getLastBlockHeader();
  
  /**
   * Get a block header by its id.
   * 
   * @param blockId is the id of the block to get the header of
   * @return the block's header
   */
  public MoneroBlockHeader getBlockHeaderById(String blockId);
  
  /**
   * Get a block header by its height.
   * 
   * @param height is the height of the block to get the header of
   * @return the block's header
   */
  public MoneroBlockHeader getBlockHeaderByHeight(int height);
  
  /**
   * Get block headers for the given range.
   * 
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return block headers in the given range
   */
  public List<MoneroBlockHeader> getBlockHeadersByRange(Integer startHeight, Integer endHeight);
  
  /**
   * Get a block by id.
   * 
   * @param blockId is the id of the block to get
   * @return the block with the given id
   */
  public MoneroBlock getBlockById(String blockId);
  
  /**
   * Get blocks by id.
   * 
   * @param blockIds are array of hashes; first 10 blocks id goes sequential,
   *        next goes in pow(2,n) offset, like 2, 4, 8, 16, 32, 64 and so on,
   *        and the last one is always genesis block
   * @param startHeight is the start height to get blocks by id
   * @param prune specifies if returned blocks should be pruned (defaults to false)  // TODO: test default
   * @return the retrieved blocks
   */
  public List<MoneroBlock> getBlocksById(List<String> blockIds, Integer startHeight, Boolean prune);
  
  /**
   * Get a block by height.
   * 
   * @param height is the height of the block to get
   * @return the block at the given height
   */
  public MoneroBlock getBlockByHeight(int height);
  
  /**
   * Get blocks at the given heights.
   * 
   * @param heights are the heights of the blocks to get
   * @return blocks at the given heights
   */
  public List<MoneroBlock> getBlocksByHeight(List<Integer> heights);
  
  /**
   * Get blocks in the given height range.
   * 
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return blocks in the given height range
   */
  public List<MoneroBlock> getBlocksByRange(Integer startHeight, Integer endHeight);
  
  /**
   * Get block ids as a binary request to the daemon.
   * 
   * @param blockIds specify block ids to fetch; first 10 blocks id goes
   *        sequential, next goes in pow(2,n) offset, like 2, 4, 8, 16, 32, 64
   *        and so on, and the last one is always genesis block
   * @param startHeight is the starting height of block ids to return
   * @return the requested block ids     
   */
  public List<String> getBlockIds(List<String> blockIds, Integer startHeight);
  
  /**
   * Get a transaction by id.
   * 
   * @param txId is the id of the transaction to get
   * @return the transaction with the given id
   */
  public MoneroTx getTx(String txId);
  
  /**
   * Get a transaction by id.
   * 
   * @param txId is the id of the transaction to get
   * @param prune specifies if the returned tx should be pruned (defaults to false)
   * @return the transaction with the given id
   */
  public MoneroTx getTx(String txId, Boolean prune);
  
  /**
   * Get transactions by ids.
   * 
   * @param txIds are ids of transactions to get
   * @return the transactions with the given ids
   */
  public List<MoneroTx> getTxs(List<String> txIds);
  
  /**
   * Get transactions by ids.
   * 
   * @param txIds are ids of transactions to get
   * @param prune specifies if the returned txs should be pruned (defaults to false)
   * @return the transactions with the given ids
   */
  public List<MoneroTx> getTxs(List<String> txIds, Boolean prune);
  
  /**
   * Get a transaction hex by id.
   * 
   * @param txId is the id of the transaction to get hex from
   * @param prune specifies if the returned tx hex should be pruned (defaults to false)
   * @return the tx hex with the given id
   */
  public String getTxHex(String txId, Boolean prune);
  
  /**
   * Get transaction hexes by ids.
   * 
   * @param txIds are ids of transactions to get hexes from
   * @return are the tx hexes
   */
  public List<String> getTxHexes(String txIds);
  
  /**
   * Get transaction hexes by ids.
   * 
   * @param txIds are ids of transactions to get hexes from
   * @param prune specifies if the returned tx hexes should be pruned (defaults to false)
   * @return are the tx hexes
   */
  public List<String> getTxHexes(String txIds, Boolean prune);
  
  /**
   * Gets the total emissions and fees from the genesis block to the current height.
   * 
   * @param height is the height to start computing the coinbase sum
   * @param numBlocks are the number of blocks to include in the sum
   * @return the sum emission and fees since the geneis block
   */
  public MoneroCoinbaseTxSum getCoinbaseTxSum(int height, int numBlocks);
  
  /**
   * Get the fee estimate per kB.
   * 
   * @param graceBlocks TODO
   * @return is the fee estimate per kB.
   */
  public BigInteger getFeeEstimate(int graceBlocks);
  
  /**
   * Submits a transaction to the daemon's pool.
   * 
   * @param txHex is the raw transaction hex to submit
   * @param doNotRelay specifies if the tx should be relayed (optional)
   * @return the submission results
   */
  public MoneroSubmitTxResult submitTxHex(String txHex, Boolean doNotRelay);
  
  /**
   * Relays a transaction by id.
   * 
   * @param txId identifies the transaction to relay
   */
  public void relayTxById(String txId);
  
  /**
   * Relays transactions by id.
   * 
   * @param txIds identify the transactions to relay
   */
  public void relayTxsById(List<String> txIds);
  
  /**
   * Get valid transactions seen by the node but not yet mined into a block, as well
   * as spent key image information for the tx pool.
   * 
   * @return transactions in the transaction pool
   */
  public List<MoneroTx> getTxPool();
  
  /**
   * Get ids of transactions in the transaction pool.
   * 
   * @return ids of transactions in the transaction pool
   */
  public List<String> getTxPoolIds();
  
  /**
   * Get all transaction pool backlog.
   * 
   * @return transaction pool backlog entries
   */
  public List<MoneroTxBacklogEntry> getTxPoolBacklog();
  
  /**
   * Get transaction pool statistics.
   * 
   * @return statistics about the transaction pool
   */
  public MoneroTxPoolStats getTxPoolStats();
  
  /**
   * Flushes all transactions from the tx pool.
   */
  public void flushTxPool();
  
  /**
   * Flushes a transaction from the tx pool.
   * 
   * @param id is the id of the transaction to flush
   */
  public void flushTxPool(String id);
  
  /**
   * Flush transactions from the tx pool.
   * 
   * @paramids are ids of transactions to flush
   */
  public void flushTxPool(List<String> ids);
  
  /**
   * Get the spent status of the given key image.
   * 
   * @param keyImage is key image hex to get the status of
   * @return the status of the key image
   */
  public MoneroKeyImageSpentStatus getSpentStatus(String keyImage);
  
  /**
   * Get the spent status of each given key image.
   * 
   * @param keyImages are hex key images to get the statuses of
   * @return the spent status for each key image
   */
  public List<MoneroKeyImageSpentStatus> getSpentStatuses(List<String> keyImages);
  
  /**
   * Get outputs identified by a list of output amounts and indices as a binary
   * request.
   * 
   * @param identify each output by amount and index
   * @return the identified outputs
   */
  public List<MoneroOutput> getOutputs(List<MoneroOutput> outputs);
  
  /**
   * Get a histogram of output amounts. For all amounts (possibly filtered by
   * parameters), gives the number of outputs on the chain for that amount.
   * RingCT outputs counts as 0 amount.
   * 
   * @param amounts are amounts of outputs to make the histogram with
   * @param minCount TODO
   * @param maxCount TODO
   * @param isUnlocked makes a histogram with outputs with the specified lock state
   * @param recentCutoff TODO
   * @return output histogram entries meeting the parameters
   */
  public List<MoneroOutputHistogramEntry> getOutputHistogram(List<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff);
  
  /**
   * Creates an output distribution.
   * 
   * @param amounts are amounts of outputs to make the distribution with
   * @param cumulative specifies if the results should be cumulative (defaults to TODO)
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return output distribution entries meeting the parameters
   */
  public List<MoneroOutputDistributionEntry> getOutputDistribution(List<BigInteger> amounts, Boolean isCumulative, Integer startHeight, Integer endHeight);
  
  /**
   * Get general information about the state of the node and the network.
   * 
   * @return general information about the node and network
   */
  public MoneroDaemonInfo getInfo();
  
  /**
   * Get synchronization information.
   * 
   * @return contains sync information
   */
  public MoneroDaemonSyncInfo getSyncInfo();
  
  /**
   * Look up information regarding hard fork voting and readiness.
   * 
   * @return hard fork information
   */
  public MoneroHardForkInfo getHardForkInfo();
  
  /**
   * Get alternative chains seen by the node.
   * 
   * @return alternative chains seen by the node
   */
  public List<MoneroAltChain> getAltChains();
  
  /**
   * Get known block ids which are not on the main chain.
   * 
   * @return known block ids which are not on the main chain
   */
  public List<String> getAltBlockIds();
  
  /**
   * Get the download bandwidth limit.
   * 
   * @return is the download bandwidth limit
   */
  public int getDownloadLimit();
  
  /**
   * Set the download bandwidth limit.
   * 
   * @param limit is the download bandwidth limit to set
   */
  public void setDownloadLimit(int limit);
  
  /**
   * Reset the download bandwidth limit.
   * 
   * @return the download bandwidth limit after resetting
   */
  public int resetDownloadLimit();
  
  /**
   * Get the upload bandwidth limit.
   * 
   * @return is the upload bandwidth limit
   */
  public int getUploadLimit();
  
  /**
   * Set the upload bandwidth limit.
   * 
   * @param limit is the upload bandwidth limit to set
   */
  public void setUploadLimit(int limit);
  
  /**
   * Reset the upload bandwidth limit.
   * 
   * @return the upload bandwidth limit after resetting
   */
  public int resetUploadLimit();
  
  /**
   * Get known peers including their last known online status.
   * 
   * @return known peers
   */
  public List<MoneroDaemonPeer> getKnownPeers();
  
  /**
   * Get incoming and outgoing connections to the node.
   * 
   * @return the daemon's peer connections
   */
  public List<MoneroDaemonConnection> getConnections();
  
  /**
   * Limit number of outgoing peers.
   * 
   * @param limit is the maximum number of outgoing peers
   */
  public void setOutgoingPeerLimit(int limit);
  
  /**
   * Limit number of incoming peers.
   * 
   * @param limit is the maximum number of incoming peers
   */
  public void setIncomingPeerLimit(int limit);
  
  /**
   * Get peer bans.
   * 
   * @return entries about banned peers
   */
  public List<MoneroBan> getPeerBans();

  /**
   * Ban a peer node.
   * 
   * @param ban contains information about a node to ban
   */
  public void setPeerBan(MoneroBan ban);
  
  /**
   * Ban peers nodes.
   * 
   * @param bans are bans to apply against peer nodes
   */
  public void setPeerBans(List<MoneroBan> bans);
  
  /**
   * Start mining.
   * 
   * @param address is the address given miner rewards if the daemon mines a block
   * @param numThreads is the number of mining threads to run
   * @param isBackground specifies if the miner should run in the background or not
   * @param ignoreBattery specifies if the battery state (e.g. on laptop) should be ignored or not
   */
  public void startMining(String address, Integer numThreads, Boolean isBackground, Boolean ignoreBattery);
  
  /**
   * Stop mining.
   */
  public void stopMining();
  
  /**
   * Get the daemon's mining status.
   * 
   * @return the daemon's mining status
   */
  public MoneroMiningStatus getMiningStatus();
  
  /**
   * Submit a mined block to the network.
   * 
   * @param blockBlob is the mined block to submit
   */
  public void submitBlock(String blockBlob);
  
  /**
   * Submit mined blocks to the network.
   * 
   * @param blockBlobs are the mined blocks to submit
   */
  public void submitBlocks(List<String> blockBlobs);
  
  /**
   * Check for update.
   * 
   * @param path is the path to check for an update (optional)
   * @return the result of the update check
   */
  public MoneroDaemonUpdateCheckResult checkForUpdate(String path);
  
  /**
   * Download an update.
   * 
   * @param path is the path to download the update (optional)
   * @return the result of the update download
   */
  public MoneroDaemonUpdateDownloadResult downloadUpdate(String path);
  
  /**
   * Safely disconnect and shut down the daemon.
   */
  public void stop();
  
  /**
   * Get the header of the next block added to the chain.
   * 
   * @return the header of the next block added to the chain
   */
  public MoneroBlockHeader getNextBlockHeader();
  
  /**
   * Register a listener to be notified when blocks are added to the chain.
   * 
   * @param listener is invoked when blocks are added to the chain
   */
  public void addBlockListener(MoneroBlockListener listener);
  
  /**
   * Unregister a listener to be notified when blocks are added to the chain.
   * 
   * @param listener is a previously registered listener to be unregistered
   */
  public void removeBlockListener(MoneroBlockListener listener);
  
  // ----------------------------- STATIC UTILITIES ---------------------------
  
  /**
   * Parses a network string to an enumerated type.
   * 
   * @param network is the network string to parse
   * @return the enumerated network type
   */
  public static MoneroNetworkType parseNetworkType(String network) {
    if ("mainnet".equals(network)) return MoneroNetworkType.MAINNET;
    if ("testnet".equals(network)) return MoneroNetworkType.TESTNET;
    if ("stagenet".equals(network)) return MoneroNetworkType.STAGENET;
    throw new Error("Invalid network type to parse: " + network);
  }
}