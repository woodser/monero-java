/**
 * Copyright (c) woodser
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
import java.util.Collection;
import java.util.List;

import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonListener;
import monero.daemon.model.MoneroPeer;
import monero.daemon.model.MoneroPruneResult;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroFeeEstimate;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroMinerTxSum;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroOutputHistogramEntry;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxBacklogEntry;
import monero.daemon.model.MoneroTxPoolStats;
import monero.daemon.model.MoneroVersion;

/**
 * Monero daemon interface.
 */
public interface MoneroDaemon {
  
  /**
   * Register a listener to receive daemon notifications.
   * 
   * @param listener the listener to unregister
   */
  public void addListener(MoneroDaemonListener listener);
  
  /**
   * Unregister a listener to receive daemon notifications.
   * 
   * @param listener a previously registered listener to be unregistered
   */
  public void removeListener(MoneroDaemonListener listener);
  
  /**
   * Get the listeners registered with the daemon.
   * 
   * @return the registered listeners
   */
  public List<MoneroDaemonListener> getListeners();
  
  /**
   * Gets the version of the daemon.
   * 
   * @return the version of the daemon
   */
  public MoneroVersion getVersion();
  
  /**
   * Indicates if the daemon is trusted or untrusted.
   * 
   * @return true if the daemon is trusted, false otherwise
   */
  public boolean isTrusted();
  
  /**
   * Get the number of blocks in the longest chain known to the node.
   * 
   * @return the number of blocks
   */
  public long getHeight();
  
  /**
   * Get a block's hash by its height.
   * 
   * @param height is the height of the block hash to get
   * @return the block's hash at the given height
   */
  public String getBlockHash(long height);
  
  /**
   * Get a block template for mining a new block.
   * 
   * @param walletAddress is the address of the wallet to receive miner transactions if block is successfully mined
   * @return a block template for mining a new block
   */
  public MoneroBlockTemplate getBlockTemplate(String walletAddress);
  
  /**
   * Get a block template for mining a new block.
   * 
   * @param walletAddress is the address of the wallet to receive miner transactions if block is successfully mined
   * @param reserveSize is the reserve size (optional)
   * @return a block template for mining a new block
   */
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, Integer reserveSize);
  
  /**
   * Get the last block's header.
   * 
   * @return the last block's header
   */
  public MoneroBlockHeader getLastBlockHeader();
  
  /**
   * Get a block header by its hash.
   * 
   * @param blockHash is the hash of the block to get the header of
   * @return the block's header
   */
  public MoneroBlockHeader getBlockHeaderByHash(String blockHash);
  
  /**
   * Get a block header by its height.
   * 
   * @param height is the height of the block to get the header of
   * @return the block's header
   */
  public MoneroBlockHeader getBlockHeaderByHeight(long height);
  
  /**
   * Get block headers for the given range.
   * 
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return block headers in the given range
   */
  public List<MoneroBlockHeader> getBlockHeadersByRange(Long startHeight, Long endHeight);
  
  /**
   * Get a block by hash.
   * 
   * @param blockHash is the hash of the block to get
   * @return the block with the given hash
   */
  public MoneroBlock getBlockByHash(String blockHash);
  
  /**
   * Get blocks by hash.
   * 
   * @param blockHashes are array of hashes; first 10 blocks hash goes sequential,
   *        next goes in pow(2,n) offset, like 2, 4, 8, 16, 32, 64 and so on,
   *        and the last one is always genesis block
   * @param startHeight is the start height to get blocks by hash
   * @param prune specifies if returned blocks should be pruned (defaults to false)  // TODO: test default
   * @return the retrieved blocks
   */
  public List<MoneroBlock> getBlocksByHash(List<String> blockHashes, Long startHeight, Boolean prune);
  
  /**
   * Get a block by height.
   * 
   * @param height is the height of the block to get
   * @return the block at the given height
   */
  public MoneroBlock getBlockByHeight(long height);
  
  /**
   * Get blocks at the given heights.
   * 
   * @param heights are the heights of the blocks to get
   * @return blocks at the given heights
   */
  public List<MoneroBlock> getBlocksByHeight(List<Long> heights);
  
  /**
   * Get blocks in the given height range.
   * 
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return blocks in the given height range
   */
  public List<MoneroBlock> getBlocksByRange(Long startHeight, Long endHeight);
  
  /**
   * Get blocks in the given height range as chunked requests so that each request is
   * not too big.
   * 
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return blocks in the given height range
   */
  public List<MoneroBlock> getBlocksByRangeChunked(Long startHeight, Long endHeight);
  
  /**
   * Get blocks in the given height range as chunked requests so that each request is
   * not too big.
   * 
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @param maxChunkSize is the maximum chunk size in any one request (default 3,000,000 bytes)
   * @return blocks in the given height range
   */
  public List<MoneroBlock> getBlocksByRangeChunked(Long startHeight, Long endHeight, Long maxChunkSize);
  
  /**
   * Get block hashes as a binary request to the daemon.
   * 
   * @param blockHashes specify block hashes to fetch; first 10 blocks hash goes
   *        sequential, next goes in pow(2,n) offset, like 2, 4, 8, 16, 32, 64
   *        and so on, and the last one is always genesis block
   * @param startHeight is the starting height of block hashes to return
   * @return the requested block hashes
   */
  public List<String> getBlockHashes(List<String> blockHashes, Long startHeight);
  
  /**
   * Get a transaction by hash.
   * 
   * @param txHash is the hash of the transaction to get
   * @return the transaction with the given hash or null if not found
   */
  public MoneroTx getTx(String txHash);
  
  /**
   * Get a transaction by hash.
   * 
   * @param txHash is the hash of the transaction to get
   * @param prune specifies if the returned tx should be pruned (defaults to false)
   * @return the transaction with the given hash or null if not found
   */
  public MoneroTx getTx(String txHash, Boolean prune);
  
  /**
   * Get transactions by hashes.
   * 
   * @param txHashes are hashes of transactions to get
   * @return found transactions with the given hashes
   */
  public List<MoneroTx> getTxs(Collection<String> txHashes);
  
  /**
   * Get transactions by hashes.
   * 
   * @param txHashes are hashes of transactions to get
   * @param prune specifies if the returned txs should be pruned (defaults to false)
   * @return found transactions with the given hashes
   */
  public List<MoneroTx> getTxs(Collection<String> txHashes, Boolean prune);
  
  /**
   * Get a transaction hex by hash.
   * 
   * @param txHash is the hash of the transaction to get hex from
   * @return the tx hex with the given hash
   */
  public String getTxHex(String txHash);
  
  /**
   * Get a transaction hex by hash.
   * 
   * @param txHash is the hash of the transaction to get hex from
   * @param prune specifies if the returned tx hex should be pruned (defaults to false)
   * @return the tx hex with the given hash
   */
  public String getTxHex(String txHash, Boolean prune);
  
  /**
   * Get transaction hexes by hashes.
   * 
   * @param txHashes are hashes of transactions to get hexes from
   * @return are the tx hexes
   */
  public List<String> getTxHexes(Collection<String> txHashes);
  
  /**
   * Get transaction hexes by hashes.
   * 
   * @param txHashes are hashes of transactions to get hexes from
   * @param prune specifies if the returned tx hexes should be pruned (defaults to false)
   * @return are the tx hexes
   */
  public List<String> getTxHexes(Collection<String> txHashes, Boolean prune);
  
  /**
   * Gets the total emissions and fees from the genesis block to the current height.
   * 
   * @param height is the height to start computing the miner sum
   * @param numBlocks are the number of blocks to include in the sum
   * @return the sum emission and fees since the geneis block
   */
  public MoneroMinerTxSum getMinerTxSum(long height, Long numBlocks);
  
  /**
   * Get mining fee estimates per kB.
   * 
   * @return mining fee estimates per kB
   */
  public MoneroFeeEstimate getFeeEstimate();
  
  /**
   * Get mining fee estimates per kB.
   * 
   * @param graceBlocks TODO
   * @return mining fee estimates per kB
   */
  public MoneroFeeEstimate getFeeEstimate(Integer graceBlocks);
  
  /**
   * Submits a transaction to the daemon's pool.
   * 
   * @param txHex is the raw transaction hex to submit
   * @return the submission results
   */
  public MoneroSubmitTxResult submitTxHex(String txHex);
  
  /**
   * Submits a transaction to the daemon's pool.
   * 
   * @param txHex is the raw transaction hex to submit
   * @param doNotRelay specifies if the tx should be relayed (optional)
   * @return the submission results
   */
  public MoneroSubmitTxResult submitTxHex(String txHex, Boolean doNotRelay);
  
  /**
   * Relays a transaction by hash.
   * 
   * @param txHash identifies the transaction to relay
   */
  public void relayTxByHash(String txHash);
  
  /**
   * Relays transactions by hash.
   * 
   * @param txHashes identify the transactions to relay
   */
  public void relayTxsByHash(Collection<String> txHashes);
  
  /**
   * Get valid transactions seen by the node but not yet mined into a block, as well
   * as spent key image information for the tx pool.
   * 
   * @return transactions in the transaction pool
   */
  public List<MoneroTx> getTxPool();
  
  /**
   * Get hashes of transactions in the transaction pool.
   * 
   * @return hashes of transactions in the transaction pool
   */
  public List<String> getTxPoolHashes();
  
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
   * Flush transactions from the tx pool.
   * 
   * @param hashes are hashes of transactions to flush
   */
  public void flushTxPool(String... hashes);
  
  /**
   * Flush transactions from the tx pool.
   * 
   * @param hashes are hashes of transactions to flush
   */
  public void flushTxPool(Collection<String> hashes);
  
  /**
   * Get the spent status of the given key image.
   * 
   * @param keyImage is key image hex to get the status of
   * @return the status of the key image
   */
  public MoneroKeyImageSpentStatus getKeyImageSpentStatus(String keyImage);
  
  /**
   * Get the spent status of each given key image.
   * 
   * @param keyImages are hex key images to get the statuses of
   * @return the spent status for each key image
   */
  public List<MoneroKeyImageSpentStatus> getKeyImageSpentStatuses(List<String> keyImages);
  
  /**
   * Get outputs identified by a list of output amounts and indices as a binary
   * request.
   * 
   * @param outputs identify each output by amount and index
   * @return the identified outputs
   */
  public List<MoneroOutput> getOutputs(Collection<MoneroOutput> outputs);
  
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
  public List<MoneroOutputHistogramEntry> getOutputHistogram(Collection<BigInteger> amounts, Integer minCount, Integer maxCount, Boolean isUnlocked, Integer recentCutoff);
  
  /**
   * Creates an output distribution.
   * 
   * @param amounts are amounts of outputs to make the distribution with
   * @return output distribution entries meeting the parameters
   */
  public List<MoneroOutputDistributionEntry> getOutputDistribution(Collection<BigInteger> amounts);
  
  /**
   * Creates an output distribution.
   * 
   * @param amounts are amounts of outputs to make the distribution with
   * @param isCumulative specifies if the results should be cumulative (defaults to TODO)
   * @param startHeight is the start height lower bound inclusive (optional)
   * @param endHeight is the end height upper bound inclusive (optional)
   * @return output distribution entries meeting the parameters
   */
  public List<MoneroOutputDistributionEntry> getOutputDistribution(Collection<BigInteger> amounts, Boolean isCumulative, Long startHeight, Long endHeight);
  
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
   * Get known block hashes which are not on the main chain.
   * 
   * @return known block hashes which are not on the main chain
   */
  public List<String> getAltBlockHashes();
  
  /**
   * Get the download bandwidth limit.
   * 
   * @return is the download bandwidth limit
   */
  public int getDownloadLimit();
  
  /**
   * Set the download bandwidth limit.
   * 
   * @param limit is the download limit to set (-1 to reset to default)
   * @return int is the new download limit after setting
   */
  public int setDownloadLimit(int limit);
  
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
   * @param limit is the upload limit to set (-1 to reset to default)
   * @return int is the new upload limit after setting
   */
  public int setUploadLimit(int limit);
  
  /**
   * Reset the upload bandwidth limit.
   * 
   * @return the upload bandwidth limit after resetting
   */
  public int resetUploadLimit();
  
  /**
   * Get peers with active incoming or outgoing connections to the node.
   * 
   * @return the daemon's peers
   */
  public List<MoneroPeer> getPeers();
  
  /**
   * Get all known peers including their last known online status.
   * 
   * @return the daemon's known peers
   */
  public List<MoneroPeer> getKnownPeers();
  
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
  public void startMining(String address, Long numThreads, Boolean isBackground, Boolean ignoreBattery);
  
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
  public void submitBlocks(Collection<String> blockBlobs);

  /**
   * Prune the blockchain.
   * 
   * @param check specifies to check the pruning (default false)
   * @return the prune result
   */
  public MoneroPruneResult pruneBlockchain(boolean check);
  
  /**
   * Check for update.
   * 
   * @return the result of the update check
   */
  public MoneroDaemonUpdateCheckResult checkForUpdate();
  
  /**
   * Download an update.
   * 
   * @return the result of the update download
   */
  public MoneroDaemonUpdateDownloadResult downloadUpdate();
  
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
  public MoneroBlockHeader waitForNextBlockHeader();
  
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