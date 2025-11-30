

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import common.utils.JsonUtils;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroRpcError;
import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroConnectionSpan;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonListener;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroFeeEstimate;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroMinerTxSum;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroOutputHistogramEntry;
import monero.daemon.model.MoneroPeer;
import monero.daemon.model.MoneroPruneResult;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxPoolStats;
import monero.daemon.model.MoneroVersion;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroTxConfig;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Tests a Monero daemon.
 */
public class TestMoneroDaemonRpc {
  
  // classes to test
  private static MoneroDaemonRpc daemon;  // could test base interface if multiple daemon implementations come along
  private static MoneroWallet wallet;
  
  // test configuration
  private static final boolean LITE_MODE = false;
  private static boolean TEST_NON_RELAYS = true;
  private static boolean TEST_RELAYS = true; // creates and relays outgoing txs
  private static boolean TEST_NOTIFICATIONS = true;
  
  // config for testing binary blocks
  // TODO: binary blocks have inconsistent client-side pruning
  // TODO: get_blocks_by_height.bin does not return output indices (#5127)
  private static TestContext BINARY_BLOCK_CTX = new TestContext();
  {
    BINARY_BLOCK_CTX.hasHex = false;
    BINARY_BLOCK_CTX.headerIsFull = false;
    BINARY_BLOCK_CTX.hasTxs = true;
    BINARY_BLOCK_CTX.txContext = new TestContext();
    BINARY_BLOCK_CTX.txContext.isPruned = false;
    BINARY_BLOCK_CTX.txContext.isConfirmed = true;
    BINARY_BLOCK_CTX.txContext.fromGetTxPool = false;
    BINARY_BLOCK_CTX.txContext.hasOutputIndices = false;
    BINARY_BLOCK_CTX.txContext.fromBinaryBlock = true;
  }
  
  // logger
  //private static final Logger LOGGER = Logger.getLogger(TestMoneroDaemonRpc.class);
  
  @BeforeAll
  public static void beforeAll() throws Exception {
    daemon = TestUtils.getDaemonRpc();
    wallet = TestUtils.getWalletRpc();
  }
  
  @BeforeEach
  public void beforeEach() {
    
  }
  
  // -------------------------------- NON RELAYS ------------------------------
  
  // Can start and stop a daemon process
  @Test
  public void testStartDaemon() {
    
    // create command to start monerod process
    List<String> cmd = new ArrayList<String>(Arrays.asList(
        TestUtils.DAEMON_LOCAL_PATH,
        "--" + TestUtils.NETWORK_TYPE.toString().toLowerCase(),
        "--no-igd",
        "--hide-my-port",
        "--data-dir", TestUtils.MONERO_BINS_DIR + "/node1",
        "--p2p-bind-port", "58080",
        "--rpc-bind-port", "58081",
        "--rpc-login", "superuser:abctesting123",
        "--zmq-rpc-bind-port", "58082"));
    
    // start monerod process from command
    MoneroDaemonRpc daemon;
    try {
      daemon = new MoneroDaemonRpc(cmd);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    
    // query daemon
    MoneroRpcConnection connection = daemon.getRpcConnection();
    assertEquals("http://127.0.0.1:58081", connection.getUri());
    assertEquals("superuser", connection.getUsername());
    assertEquals("abctesting123", connection.getPassword());
    assertTrue(daemon.getHeight() > 0);
    MoneroDaemonInfo info = daemon.getInfo();
    testInfo(info);
    
    // add listeners
    daemon.addListener(new MoneroDaemonListener());
    daemon.addListener(new MoneroDaemonListener());
    
    // stop daemon
    daemon.stopProcess();
  }
  
  // Can get the daemon's version
  @Test
  public void testGetVersion() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroVersion version = daemon.getVersion();
    assertNotNull(version.getNumber());
    assertTrue(version.getNumber() > 0);
    assertNotNull(version.getIsRelease());
  }
  
  // Can indicate if it's trusted
  @Test
  public void testIsTrusted() {
    assumeTrue(TEST_NON_RELAYS);
    daemon.isTrusted();
  }
  
  // Can get the blockchain height
  @Test
  public void testGetHeight() {
    assumeTrue(TEST_NON_RELAYS);
    long height = daemon.getHeight();
    assertTrue(height > 0, "Height must be greater than 0");
  }
  
  // Can get a block hash by height
  @Test
  public void testGetBlockIdByHeight() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String hash = daemon.getBlockHash(lastHeader.getHeight());
    assertNotNull(hash);
    assertEquals(64, hash.length());
  }
  
  // Can get a block template
  @Test
  public void testGetBlockTemplate() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.ADDRESS, 2);
    testBlockTemplate(template);
  }
  
  // Can get the last block's header
  @Test
  public void testGetLastBlockHeader() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    testBlockHeader(lastHeader, true);
  }
  
  // Can get a block header by hash
  @Test
  public void testGetBlockHeaderByHash() {
    assumeTrue(TEST_NON_RELAYS);
    
    // retrieve by hash of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String hash = daemon.getBlockHash(lastHeader.getHeight());
    MoneroBlockHeader header = daemon.getBlockHeaderByHash(hash);
    testBlockHeader(header, true);
    assertEquals(lastHeader, header);
    
    // retrieve by hash of previous to last block
    hash = daemon.getBlockHash(lastHeader.getHeight() - 1);
    header = daemon.getBlockHeaderByHash(hash);
    testBlockHeader(header, true);
    assertEquals(lastHeader.getHeight() - 1, (long) header.getHeight());
  }
  
  // Can get a block header by height
  @Test
  public void testGetBlockHeaderByHeight() {
    assumeTrue(TEST_NON_RELAYS);
    
    // retrieve by height of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    MoneroBlockHeader header = daemon.getBlockHeaderByHeight(lastHeader.getHeight());
    testBlockHeader(header, true);
    assertEquals(lastHeader, header);
    
    // retrieve by height of previous to last block
    header = daemon.getBlockHeaderByHeight(lastHeader.getHeight() - 1);
    testBlockHeader(header, true);
    assertEquals(lastHeader.getHeight() - 1, (long) header.getHeight());
  }
  
  // Can get block headers by range
  // TODO: test start with no end, vice versa, inclusivity
  @Test
  public void testGetBlockHeadersByRange() {
    assumeTrue(TEST_NON_RELAYS);
    
    // determine start and end height based on number of blocks and how many blocks ago
    long numBlocks = 100;
    long numBlocksAgo = 100;
    long currentHeight = daemon.getHeight();
    long startHeight = currentHeight - numBlocksAgo;
    long endHeight = currentHeight - (numBlocksAgo - numBlocks) - 1;
    
    // fetch headers
    List<MoneroBlockHeader> headers = daemon.getBlockHeadersByRange(startHeight, endHeight);
    
    // test headers
    assertEquals(numBlocks, headers.size());
    for (int i = 0; i < numBlocks; i++) {
      MoneroBlockHeader header = headers.get(i);
      assertEquals(startHeight + i, (long) header.getHeight());
      testBlockHeader(header, true);
    }
  }
  
  // Can get a block by hash
  @Test
  public void testGetBlockByHash() {
    assumeTrue(TEST_NON_RELAYS);
    
    // test config
    TestContext ctx = new TestContext();
    ctx.hasHex = true;
    ctx.hasTxs = false;
    ctx.headerIsFull = true;
    
    // retrieve by hash of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String hash = daemon.getBlockHash(lastHeader.getHeight());
    MoneroBlock block = daemon.getBlockByHash(hash);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeight()), block);
    assertEquals(null, block.getTxs());
    
    // retrieve by hash of previous to last block
    hash = daemon.getBlockHash(lastHeader.getHeight() - 1);
    block = daemon.getBlockByHash(hash);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(lastHeader.getHeight() - 1), block);
    assertEquals(null, block.getTxs());
  }

  // Can get blocks by hash which includes transactions (binary)
  @Test
  public void testGetBlocksByHashBinary() {
    assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented");
  }

  // Can get a block by height
  @Test
  public void testGetBlockByHeight() {
    assumeTrue(TEST_NON_RELAYS);
    
    // config for testing blocks
    TestContext ctx = new TestContext();
    ctx.hasHex = true;
    ctx.headerIsFull = true;
    ctx.hasTxs = false;
    
    // retrieve by height of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    MoneroBlock block = daemon.getBlockByHeight(lastHeader.getHeight());
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeight()), block);
    
    // retrieve by height of previous to last block
    block = daemon.getBlockByHeight(lastHeader.getHeight() - 1);
    testBlock(block, ctx);
    assertEquals(lastHeader.getHeight() - 1, (long) block.getHeight());
  }
  
  // Can get blocks by height which includes transactions (binary)
  @Test
  public void testGetBlocksByHeightBinary() {
    assumeTrue(TEST_NON_RELAYS);
    
    // set number of blocks to test
    int numBlocks = 100;
    
    // select random heights  // TODO: this is horribly inefficient way of computing last 100 blocks if not shuffling
    long currentHeight = daemon.getHeight();
    List<Long> allHeights = new ArrayList<Long>();
    for (long i = 0; i < currentHeight - 1; i++) allHeights.add(i);
    //GenUtils.shuffle(allHeights);
    List<Long> heights = new ArrayList<Long>();
    for (int i = allHeights.size() - numBlocks; i < allHeights.size(); i++) heights.add(allHeights.get(i));
    
    // fetch blocks
    List<MoneroBlock> blocks = daemon.getBlocksByHeight(heights);
    
    // test blocks
    boolean txFound = false;
    assertEquals(numBlocks, blocks.size());
    for (int i = 0; i < heights.size(); i++) {
      MoneroBlock block = blocks.get(i);
      if (!block.getTxs().isEmpty()) txFound = true;
      testBlock(block, BINARY_BLOCK_CTX);
      assertEquals(block.getHeight(), heights.get(i));
    }
    assertTrue(txFound, "No transactions found to test");
  }
  
  // Can get blocks by range in a single request
  @Test
  public void testGetBlocksByRange() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get height range
    long numBlocks = 100;
    long numBlocksAgo = 190;
    assertTrue(numBlocks > 0);
    assertTrue(numBlocksAgo >= numBlocks);
    long height = daemon.getHeight();
    assertTrue(height - numBlocksAgo + numBlocks - 1 < height);
    long startHeight = height - numBlocksAgo;
    long endHeight = height - numBlocksAgo + numBlocks - 1;
    
    // test known start and end heights
    testGetBlocksRange(startHeight, endHeight, height, false);
    
    // test unspecified start
    testGetBlocksRange(null, numBlocks - 1, height, false);
    
    // test unspecified end
    testGetBlocksRange(height - numBlocks - 1, null, height, false);
  };
  
  // Can get blocks by range using chunked requests
  @Test
  public void testGetBlocksByRangeChunked() {
    assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get long height range
    long numBlocks = Math.min(daemon.getHeight() - 2, 1440); // test up to ~2 days of blocks
    assertTrue(numBlocks > 0);
    long height = daemon.getHeight();
    assertTrue(height - numBlocks - 1 < height);
    long startHeight = height - numBlocks;
    long endHeight = height - 1;
    
    // test known start and end heights
    testGetBlocksRange(startHeight, endHeight, height, true);
    
    // test unspecified start
    testGetBlocksRange(null, numBlocks - 1, height, true);
    
    // test unspecified end
    testGetBlocksRange(endHeight - numBlocks - 1, null, height, true);
  };
  
  // Can get block hashes (binary)
  @Test
  public void testGetBlockIdsBinary() {
    assumeTrue(TEST_NON_RELAYS);
    //get_hashes.bin
    throw new RuntimeException("Not implemented");
  }
  
  // Can get a transaction by hash with and without pruning
  @Test
  public void testGetTxByHash() {
    assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction hashes to test
    List<String> txHashes = getConfirmedTxHashes(daemon);
    
    // context for testing txs
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = true;
    ctx.fromGetTxPool = false;
    
    // fetch each tx by hash without pruning
    for (String txHash : txHashes) {
      MoneroTx tx = daemon.getTx(txHash);
      testTx(tx, ctx);
    }
    
    // fetch each tx by hash with pruning
    for (String txHash : txHashes) {
      MoneroTx tx = daemon.getTx(txHash, true);
      ctx.isPruned = true;
      testTx(tx, ctx);
    }
    
    // fetch invalid hash
    try {
      daemon.getTx("invalid tx hash");
      throw new RuntimeException("fail");
    } catch (MoneroError e) {
      assertEquals("Invalid transaction hash", e.getMessage());
    }
  }
  
  // Can get transactions by hashes with and without pruning
  @Test
  public void testGetTxsByHashes() {
    assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction hashes to test
    List<String> txHashes = getConfirmedTxHashes(daemon);
    assertTrue(txHashes.size() > 0);
    
    // context for testing txs
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = true;
    ctx.fromGetTxPool = false;
    
    // fetch txs by hash without pruning
    List<MoneroTx> txs = daemon.getTxs(txHashes);
    assertEquals(txHashes.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // fetch txs by hash with pruning
    txs = daemon.getTxs(txHashes, true);
    ctx.isPruned = true;
    assertEquals(txHashes.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // fetch missing hash
    MoneroTx tx = wallet.createTx(new MoneroTxConfig().setAccountIndex(0).addDestination(wallet.getPrimaryAddress(), TestUtils.MAX_FEE));
    assertNull(daemon.getTx(tx.getHash()));
    txHashes.add(tx.getHash());
    int numTxs = txs.size();
    txs = daemon.getTxs(txHashes);
    assertEquals(numTxs, txs.size());
    
    // fetch invalid hash
    txHashes.add("invalid tx hash");
    try {
      daemon.getTxs(txHashes);
      throw new RuntimeException("fail");
    } catch (MoneroError e) {
      assertEquals("Invalid transaction hash", e.getMessage());
    }
  }
  
  // Can get transactions by hashes that are in the transaction pool
  @Test
  public void testGetTxsByHashesInPool() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet); // wait for wallet's txs in the pool to clear to ensure reliable sync
    
    // submit txs to the pool but don't relay
    List<String> txHashes = new ArrayList<String>();
    for (int i = 1; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
      assertFalse(result.isRelayed());
      txHashes.add(tx.getHash());
    }
    
    // fetch txs by hash
    System.out.print("Fetching txs...");
    List<MoneroTx> txs = daemon.getTxs(txHashes);
    System.out.println("done");
    
    // context for testing tx
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = false;
    ctx.fromGetTxPool = false;
    
    // test fetched txs
    assertEquals(txHashes.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // clear txs from pool
    daemon.flushTxPool(txHashes);
    wallet.sync();
  }
  
  // Can get a transaction hex by hash with and without pruning
  @Test
  public void testGetTxHexByHash() {
    assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction hashes to test
    List<String> txHashes = getConfirmedTxHashes(daemon);
    
    // fetch each tx hex by hash with and without pruning
    List<String> hexes = new ArrayList<String>();
    List<String> hexesPruned = new ArrayList<String>();
    for (String txHash : txHashes) {
      hexes.add(daemon.getTxHex(txHash));
      hexesPruned.add(daemon.getTxHex(txHash, true));
    }
    
    // test results
    assertEquals(hexes.size(), txHashes.size());
    assertEquals(hexesPruned.size(), txHashes.size());
    for (int i = 0; i < hexes.size(); i++) {
      assertNotNull(hexes.get(i));
      assertNotNull(hexesPruned.get(i));
      assertFalse(hexesPruned.isEmpty());
      assertTrue(hexes.get(i).length() >= hexesPruned.get(i).length()); // pruned hex is shorter
    }
    
    // fetch invalid hash
    try {
      daemon.getTxHex("invalid tx hash");
      throw new RuntimeException("fail");
    } catch (MoneroError e) {
      assertEquals("Invalid transaction hash", e.getMessage());
    }
  }
  
  // Can get transaction hexes by hashes with and without pruning
  @Test
  public void testGetTxHexesByHashes() {
    assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction hashes to test
    List<String> txHashes = getConfirmedTxHashes(daemon);
    
    // fetch tx hexes by hash with and without pruning
    List<String> hexes = daemon.getTxHexes(txHashes);
    List<String> hexesPruned = daemon.getTxHexes(txHashes, true);
    
    // test results
    assertEquals(hexes.size(), txHashes.size());
    assertEquals(hexesPruned.size(), txHashes.size());
    for (int i = 0; i < hexes.size(); i++) {
      assertNotNull(hexes.get(i));
      assertNotNull(hexesPruned.get(i));
      assertFalse(hexesPruned.isEmpty());
      assertTrue(hexes.get(i).length() > hexesPruned.get(i).length()); // pruned hex is shorter
    }
    
    // fetch invalid hash
    txHashes.add("invalid tx hash");
    try {
      daemon.getTxHexes(txHashes);
      throw new RuntimeException("fail");
    } catch (MoneroError e) {
      assertEquals("Invalid transaction hash", e.getMessage());
    }
  }
  
  // Can get the miner transaction sum
  @Test
  public void testGetMinerTxSum() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroMinerTxSum sum = daemon.getMinerTxSum(0l, Math.min(50000l, daemon.getHeight()));
    testMinerTxSum(sum);
  }
  
  // Can get a fee estimate
  @Test
  public void testGetFeeEstimate() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroFeeEstimate feeEstimate = daemon.getFeeEstimate();
    TestUtils.testUnsignedBigInteger(feeEstimate.getFee(), true);
    assertTrue(feeEstimate.getFees().size() == 4); // slow, normal, fast, fastest
    for (int i = 0; i < 4; i++) TestUtils.testUnsignedBigInteger(feeEstimate.getFees().get(i), true);
    TestUtils.testUnsignedBigInteger(feeEstimate.getQuantizationMask(), true);
  }
  
  // Can get all transactions in the transaction pool
  @Test
  public void testGetTxsInPool() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // submit tx to pool but don't relay
    MoneroTx tx = getUnrelayedTx(wallet, 1);
    MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
    testSubmitTxResultGood(result);
    assertFalse(result.isRelayed());
    
    // fetch txs in pool
    List<MoneroTx> txs = daemon.getTxPool();
    
    // context for testing tx
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = false;
    ctx.fromGetTxPool = true;
    
    // test txs
    assertFalse(txs.isEmpty(), "Test requires an unconfirmed tx in the tx pool");
    for (MoneroTx aTx : txs) {
      testTx(aTx, ctx);
    }
    
    // flush the tx from the pool, gg
    daemon.flushTxPool(tx.getHash());
    wallet.sync();
  }
  
  // Can get hashes of transactions in the transaction pool (binary)
  @Test
  public void testGetIdsOfTxsInPoolBin() {
    assumeTrue(TEST_NON_RELAYS);
    // TODO: get_transaction_pool_hashes.bin
    throw new RuntimeException("Not implemented");
  }
  
  // Can get the transaction pool backlog (binary)
  @Test
  public void testGetTxPoolBacklogBin() {
    assumeTrue(TEST_NON_RELAYS);
    // TODO: get_txpool_backlog
    throw new RuntimeException("Not implemented");
  }
  
  // Can get transaction pool statistics
  @Test
  public void testGetTxPoolStatistics() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    Throwable err = null;
    Collection<String> txIds = new HashSet<String>();
    try {

      // submit txs to the pool but don't relay
      for (int i = 1; i < 3; i++) {
      
        // submit tx hex
        MoneroTx tx =  getUnrelayedTx(wallet, i);
        MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
        assertTrue(result.isGood(), JsonUtils.serialize(result));
        txIds.add(tx.getHash());
        
        // get tx pool stats
        MoneroTxPoolStats stats = daemon.getTxPoolStats();
        assertTrue(stats.getNumTxs() > i - 1);
        testTxPoolStats(stats);
      }
    } catch (Throwable e) {
      err = e;
    }

    // flush txs
    daemon.flushTxPool(txIds);
    if (err != null) throw new RuntimeException(err);
  }
  
  // Can flush all transactions from the pool
  @Test
  public void testFlushTxsFromPool() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // preserve original transactions in the pool
    List<MoneroTx> txPoolBefore = daemon.getTxPool();
    
    // submit txs to the pool but don't relay
    for (int i = 1; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
    }
    assertEquals(txPoolBefore.size() + 2, daemon.getTxPool().size());
    
    // flush tx pool
    daemon.flushTxPool();
    assertEquals(0, daemon.getTxPool().size());
    
    // re-submit original transactions
    for (MoneroTx tx : txPoolBefore) {
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), tx.isRelayed());
      testSubmitTxResultGood(result);
    }
    
    // pool is back to original state
    assertEquals(txPoolBefore.size(), daemon.getTxPool().size());
    
    // sync wallet for next test
    wallet.sync();
  }
  
  // Can flush a transaction from the pool by hash
  @Test
  public void testFlushTxFromPoolByHash() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // preserve original transactions in the pool
    List<MoneroTx> txPoolBefore = daemon.getTxPool();
    
    // submit txs to the pool but don't relay
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 1; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
      txs.add(tx);
    }
    
    // remove each tx from the pool by hash and test
    for (int i = 0; i < txs.size(); i++) {
      
      // flush tx from pool
      daemon.flushTxPool(txs.get(i).getHash());
      
      // test tx pool
      List<MoneroTx> poolTxs = daemon.getTxPool();
      assertEquals(txs.size() - i - 1, poolTxs.size());
    }
    
    // pool is back to original state
    assertEquals(txPoolBefore.size(), daemon.getTxPool().size());
    
    // sync wallet for next test
    wallet.sync();
  }
  
  // Can flush transactions from the pool by hashes
  @Test
  public void testFlushTxsFromPoolByHashes() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // preserve original transactions in the pool
    List<MoneroTx> txPoolBefore = daemon.getTxPool();
    
    // submit txs to the pool but don't relay
    List<String> txHashes = new ArrayList<String>();
    for (int i = 1; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
      txHashes.add(tx.getHash());
    }
    assertEquals(txPoolBefore.size() + txHashes.size(), daemon.getTxPool().size());
    
    // remove all txs by hashes
    daemon.flushTxPool(txHashes);
    
    // pool is back to original state
    assertEquals(txPoolBefore.size(), daemon.getTxPool().size());
    wallet.sync();
  }
  
  // Can get the spent status of key images
  @Test
  public void testGetSpentStatusOfKeyImages() {
    assumeTrue(TEST_NON_RELAYS);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // submit txs to the pool to collect key images then flush them
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 1; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
      txs.add(tx);
    }
    List<String> keyImages = new ArrayList<String>();
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTx tx : txs) txHashes.add(tx.getHash());
    for (MoneroTx tx : daemon.getTxs(txHashes)) {
      for (MoneroOutput input : tx.getInputs()) keyImages.add(input.getKeyImage().getHex());
    }
    daemon.flushTxPool(txHashes);
    
    // key images are not spent
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.NOT_SPENT);
    
    // submit txs to the pool but don't relay
    for (MoneroTx tx : txs) daemon.submitTxHex(tx.getFullHex(), true);
    
    // key images are in the tx pool
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.TX_POOL);
    
    // collect key images of confirmed txs
    keyImages = new ArrayList<String>();
    txs = getConfirmedTxs(daemon, 10);
    for (MoneroTx tx : txs) {
      for (MoneroOutput input : tx.getInputs()) keyImages.add(input.getKeyImage().getHex());
    }
    
    // key images are all spent
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.CONFIRMED);
    
    // flush this test's txs from pool
    daemon.flushTxPool(txHashes);
  }
  
  // Can get output indices given a list of transaction hashes (binary)
  @Test
  public void testGetOutputIndicesFromTxIdsBinary() {
    assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented"); // get_o_indexes.bin
  }
  
  // Can get outputs given a list of output amounts and indices (binary)
  @Test
  public void testGetOutputsFromAmountsAndIndicesBinary() {
    assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented"); // get_outs.bin
  }
  
  // Can get an output histogram (binary)
  @Test
  public void testGetOutputHistogramBinary() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroOutputHistogramEntry> entries = daemon.getOutputHistogram(null, null, null, null, null);
    assertFalse(entries.isEmpty());
    for (MoneroOutputHistogramEntry entry : entries) {
      testOutputHistogramEntry(entry);
    }
  }
  
  // Can get an output distribution (binary)
  @Test
  public void testGetOutputDistributionBinary() {
    assumeTrue(TEST_NON_RELAYS);
    List<BigInteger> amounts = new ArrayList<BigInteger>();
    amounts.add(BigInteger.valueOf(0));
    amounts.add(BigInteger.valueOf(1));
    amounts.add(BigInteger.valueOf(10));
    amounts.add(BigInteger.valueOf(100));
    amounts.add(BigInteger.valueOf(1000));
    amounts.add(BigInteger.valueOf(10000));
    amounts.add(BigInteger.valueOf(100000));
    amounts.add(BigInteger.valueOf(1000000));
    List<MoneroOutputDistributionEntry> entries = daemon.getOutputDistribution(amounts);
    for (MoneroOutputDistributionEntry entry : entries) {
      testOutputDistributionEntry(entry);
    }
  }
  
  // Can get general information
  @Test
  public void testGetGeneralInformation() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonInfo info = daemon.getInfo();
    testInfo(info);
  }
  
  // Can get sync information
  @Test
  public void testGetSyncInformation() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonSyncInfo syncInfo = daemon.getSyncInfo();
    testSyncInfo(syncInfo);
  }
  
  // Can get hard fork information
  @Test
  public void testGetHardForkInformation() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroHardForkInfo hardForkInfo = daemon.getHardForkInfo();
    testHardForkInfo(hardForkInfo);
  }
  
  // Can get alternative chains
  @Test
  public void testGetAlternativeChains() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroAltChain> altChains = daemon.getAltChains();
    for (MoneroAltChain altChain : altChains) {
      testAltChain(altChain);
    }
  }
  
  // Can get alternative block hashes
  @Test
  public void testGetAlternativeBlockIds() {
    assumeTrue(TEST_NON_RELAYS);
    List<String> altBlockIds = daemon.getAltBlockHashes();
    for (String altBlockId : altBlockIds) {
      assertNotNull(altBlockId);
      assertEquals(64, altBlockId.length());  // TODO: common validation
    }
  }
  
  // Can get, set, and reset a download bandwidth limit
  @Test
  public void testSetDownloadBandwidth() {
    assumeTrue(TEST_NON_RELAYS);
    int initVal = daemon.getDownloadLimit();
    assertTrue(initVal > 0);
    int setVal = initVal * 2;
    daemon.setDownloadLimit(setVal);
    assertEquals(setVal, daemon.getDownloadLimit());
    int resetVal = daemon.resetDownloadLimit();
    assertEquals(initVal, resetVal);
    
    // test invalid limits
    try {
      daemon.setDownloadLimit(0);
      fail("Should have thrown error on invalid input");
    } catch (MoneroError e) {
      assertEquals("Download limit must be an integer greater than 0", e.getMessage());
    }
    assertEquals(daemon.getDownloadLimit(), initVal);
  }
  
  // Can get, set, and reset an upload bandwidth limit
  @Test
  public void testSetUploadBandwidth() {
    assumeTrue(TEST_NON_RELAYS);
    int initVal = daemon.getUploadLimit();
    assertTrue(initVal > 0);
    int setVal = initVal * 2;
    daemon.setUploadLimit(setVal);
    assertEquals(setVal, daemon.getUploadLimit());
    int resetVal = daemon.resetUploadLimit();
    assertEquals(initVal, resetVal);
    
    // test invalid limits
    try {
      daemon.setUploadLimit(0);
      fail("Should have thrown error on invalid input");
    } catch (MoneroError e) {
      assertEquals("Upload limit must be an integer greater than 0", e.getMessage());
    }
    assertEquals(initVal, daemon.getUploadLimit());
  }
  
  // Can get peers with active incoming or outgoing connections
  @Test
  public void testGetPeers() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroPeer> peers = daemon.getPeers();
    assertFalse(peers.isEmpty(), "Daemon has no incoming or outgoing peers to test");
    for (MoneroPeer peer : peers) {
      testPeer(peer);
    }
  }
  
  // Can get all known peers which may be online or offline
  @Test
  public void testGetKnownPeers() {
    assumeTrue(TEST_NON_RELAYS);
    List<MoneroPeer> peers = daemon.getKnownPeers();
    assertFalse(peers.isEmpty(), "Daemon has no known peers to test");
    for (MoneroPeer peer : peers) {
      testKnownPeer(peer, false);
    }
  }
  
  // Can limit the number of outgoing peers
  @Test
  public void testSetOutgoingPeerLimit() {
    assumeTrue(TEST_NON_RELAYS);
    daemon.setOutgoingPeerLimit(0);
    daemon.setOutgoingPeerLimit(8);
    daemon.setOutgoingPeerLimit(10);
  }
  
  // Can limit the number of incoming peers
  @Test
  public void testSetIncomingPeerLimit() {
    assumeTrue(TEST_NON_RELAYS);
    daemon.setIncomingPeerLimit(0);
    daemon.setIncomingPeerLimit(8);
    daemon.setIncomingPeerLimit(10);
  }
  
  // Can ban a peer
  @Test
  public void testBanPeer() {
    assumeTrue(TEST_NON_RELAYS);
    
    // set ban
    MoneroBan ban = new MoneroBan();
    ban.setHost("192.168.1.51");
    ban.setIsBanned(true);
    ban.setSeconds((long) 60);
    daemon.setPeerBan(ban);
    
    // test ban
    List<MoneroBan> bans = daemon.getPeerBans();
    boolean found = false;
    for (MoneroBan aBan : bans) {
      testMoneroBan(aBan);
      if ("192.168.1.51".equals(aBan.getHost())) found = true;
    }
    assertTrue(found);
  }
  
  // Can ban peers
  @Test
  public void testBanPeers() {
    assumeTrue(TEST_NON_RELAYS);
    
    // set bans
    MoneroBan ban1 = new MoneroBan();
    ban1.setHost("192.168.1.52");
    ban1.setIsBanned(true);
    ban1.setSeconds((long) 60);
    MoneroBan ban2 = new MoneroBan();
    ban2.setHost("192.168.1.53");
    ban2.setIsBanned(true);
    ban2.setSeconds((long) 60);
    List<MoneroBan> bans = new ArrayList<MoneroBan>();
    bans.add(ban1);
    bans.add(ban2);
    daemon.setPeerBans(bans);
    
    // test bans
    bans = daemon.getPeerBans();
    boolean found1 = false;
    boolean found2 = false;
    for (MoneroBan aBan : bans) {
      testMoneroBan(aBan);
      if ("192.168.1.52".equals(aBan.getHost())) found1 = true;
      if ("192.168.1.53".equals(aBan.getHost())) found2 = true;
    }
    assertTrue(found1);
    assertTrue(found2);
  }
  
  // Can start and stop mining
  @Test
  public void testMining() {
    assumeTrue(TEST_NON_RELAYS);
    
    // stop mining at beginning of test
    try { daemon.stopMining(); }
    catch (MoneroError e) { }
    
    // generate address to mine to
    String address = wallet.getPrimaryAddress();
    
    // start mining
    daemon.startMining(address, 2l, false, true);
    
    // stop mining
    daemon.stopMining();
  }
  
  // Can get mining status
  @Test
  public void testGetMiningStatus() {
    assumeTrue(TEST_NON_RELAYS);
    
    try {
      
      // stop mining at beginning of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
      
      // test status without mining
      MoneroMiningStatus status = daemon.getMiningStatus();
      assertEquals(false, status.isActive());
      assertNull(status.getAddress());
      assertEquals(0, (long) status.getSpeed());
      assertEquals(0, (int) status.getNumThreads());
      assertNull(status.isBackground());
      
      // test status with mining
      String address = wallet.getPrimaryAddress();
      long threadCount = 3;
      boolean isBackground = false;
      daemon.startMining(address, threadCount, isBackground, true);
      status = daemon.getMiningStatus();
      assertEquals(true, status.isActive());
      assertEquals(address, status.getAddress());
      assertTrue(status.getSpeed() >= 0);
      assertEquals(threadCount, (int) status.getNumThreads());
      assertEquals(isBackground, status.isBackground());
    } catch (MoneroError e) {
      throw e;
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
    }
  }
  
  // Can submit a mined block to the network
  @Test
  public void testSubmitMinedBlock() {
    assumeTrue(TEST_NON_RELAYS);
    
    // get template to mine on
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.ADDRESS);
    
    // TODO monero rpc: way to get mining nonce when found in order to submit?
    
    // try to submit block hashing blob without nonce
    try {
      daemon.submitBlock(template.getBlockTemplateBlob());
      fail("Should have thrown error");
    } catch (MoneroRpcError e) {
      assertEquals(-7, (int) e.getCode());
      assertEquals("Block not accepted", e.getMessage());
    }
  }

  // Can prune the blockchain
  @Test
  public void testPruneBlockchain() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroPruneResult result = daemon.pruneBlockchain(true);
    if (result.isPruned()) {
      assertTrue(result.getPruningSeed() > 0);
    } else {
      assertEquals(0,  result.getPruningSeed());
    }
  }
  
  // Can check for an update
  @Test
  public void testCheckForUpdate() {
    assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonUpdateCheckResult result = daemon.checkForUpdate();
    testUpdateCheckResult(result);
  }
  
  // Can download an update
  @Test
  @Disabled
  public void testDownloadUpdate() {
    assumeTrue(TEST_NON_RELAYS);
    
    // download to default path
    MoneroDaemonUpdateDownloadResult result = daemon.downloadUpdate();
    testUpdateDownloadResult(result, null);
    
    // download to defined path
    String path = "test_download_" + +new Date().getTime() + ".tar.bz2";
    result = daemon.downloadUpdate(path);
    testUpdateDownloadResult(result, path);
    
    // test invalid path
    if (result.isUpdateAvailable()) {
      try {
        result = daemon.downloadUpdate("./ohhai/there");
        fail("Should have thrown error");
      } catch (MoneroRpcError e) {
        assertNotEquals(e.getMessage(), "Should have thrown error");
        assertEquals(500, (int) e.getCode());  // TODO monerod: this causes a 500 in daemon rpc
      }
    }
  }
  
  // Can be stopped
  @Test
  @Disabled // test is disabled to not interfere with other tests
  public void testStop() throws InterruptedException {
    assumeTrue(TEST_NON_RELAYS);
    
    // stop the daemon
    daemon.stop();
    
    // give the daemon time to shut down
    TimeUnit.MILLISECONDS.sleep(TestUtils.SYNC_PERIOD_IN_MS);
    
    // try to interact with the daemon
    try {
      daemon.getHeight();
      throw new RuntimeException("Should have thrown error");
    } catch (MoneroError e) {
      assertNotEquals("Should have thrown error", e.getMessage());
    }
  }
  
  // ----------------------------- RELAY TESTS -------------------------------
  
  // Can submit a tx in hex format to the pool and relay in one call
  @Test
  public void testSubmitAndRelayTxHex() {
    assumeTrue(TEST_RELAYS && !LITE_MODE);
    
    // wait one time for wallet txs in the pool to clear
    // TODO monero-project: update from pool does not prevent creating double spend tx
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    
    // create 2 txs, the second will double spend outputs of first
    MoneroTx tx1 = getUnrelayedTx(wallet, 2); // TODO: this test requires tx to be from/to different accounts else the occlusion issue (#4500) causes the tx to not be recognized by the wallet at all
    MoneroTx tx2 = getUnrelayedTx(wallet, 2);
    
    // submit and relay tx1
    MoneroSubmitTxResult result = daemon.submitTxHex(tx1.getFullHex());
    assertEquals(true, result.isRelayed());
    testSubmitTxResultGood(result);
    
    // tx1 is in the pool
    List<MoneroTx> txs = daemon.getTxPool();
    boolean found = false;
    for (MoneroTx aTx : txs) {
      if (aTx.getHash().equals(tx1.getHash())) {
        assertEquals(true, aTx.isRelayed());
        found = true;
        break;
      }
    }
    assertTrue(found, "Tx1 was not found after being submitted to the daemon's tx pool");
    
    // tx1 is recognized by the wallet
    wallet.sync();
    wallet.getTx(tx1.getHash());
    
    // submit and relay tx2 hex which double spends tx1
    result = daemon.submitTxHex(tx2.getFullHex());
    assertEquals(true, result.isRelayed());
    testSubmitTxResultDoubleSpend(result);
    
    // tx2 is in not the pool
    txs = daemon.getTxPool();
    found = false;
    for (MoneroTx aTx : txs) {
      if (aTx.getHash().equals(tx2.getHash())) {
        found = true;
        break;
      }
    }
    assertTrue(!found, "Tx2 should not be in the pool because it double spends tx1 which is in the pool");
  }
  
  // Can submit a tx in hex format to the pool then relay
  @Test
  public void testSubmitThenRelayTxHex() {
    assumeTrue(TEST_RELAYS && !LITE_MODE);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    MoneroTx tx = getUnrelayedTx(wallet, 1);
    testSubmitThenRelay(Arrays.asList(tx));
  }
  
  // Can submit txs in hex format to the pool then relay
  @Test
  public void testSubmitThenRelayTxHexes() {
    assumeTrue(TEST_RELAYS && !LITE_MODE);
    TestUtils.WALLET_TX_TRACKER.waitForTxsToClearPool(wallet);
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    txs.add(getUnrelayedTx(wallet, 1));
    txs.add(getUnrelayedTx(wallet, 2));  // TODO: accounts cannot be re-used across send tests else isRelayed is true; wallet needs to update?
    testSubmitThenRelay(txs);
  }
  
  private static void testSubmitThenRelay(List<MoneroTx> txs) {
    
    // submit txs hex but don't relay
    List<String> txHashes = new ArrayList<String>();
    for (MoneroTx tx : txs) {
      txHashes.add(tx.getHash());
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
      assertEquals(false, result.isRelayed());
      
      // ensure tx is in pool
      List<MoneroTx> poolTxs = daemon.getTxPool();
      boolean found = false;
      for (MoneroTx aTx : poolTxs) {
        if (aTx.getHash().equals(tx.getHash())) {
          assertEquals(false, aTx.isRelayed());
          found = true;
          break;
        }
      }
      assertTrue(found, "Tx was not found after being submitted to the daemon's tx pool");
      
      // fetch tx by hash and ensure not relayed
      MoneroTx fetchedTx = daemon.getTx(tx.getHash());
      assertFalse(fetchedTx.isRelayed());
    }
    
    // relay the txs
    try {
      if (txHashes.size() == 1) daemon.relayTxByHash(txHashes.get(0));
      else daemon.relayTxsByHash(txHashes);
    } catch (Exception e) {
      daemon.flushTxPool(txHashes); // flush txs when relay fails to prevent double spends in other tests
      throw e;
    }
    
    // wait for txs to be relayed // TODO (monero-project): all txs should be relayed: https://github.com/monero-project/monero/issues/8523
    try { Thread.sleep(1000); } catch (Exception e) { throw new RuntimeException(e); }
    
    // ensure txs are relayed
    List<MoneroTx> poolTxs = daemon.getTxPool();
    for (MoneroTx tx : txs) {
      boolean found = false;
      for (MoneroTx aTx : poolTxs) {
        if (aTx.getHash().equals(tx.getHash())) {
          assertEquals(true, aTx.isRelayed());
          found = true;
          break;
        }
      }
      assertTrue(found, "Tx was not found after being submitted to the daemon's tx pool");
    }
  }
  
  // -------------------------- NOTIFICATION TESTS ---------------------------
  
  // Can notify listeners when a new block is added to the chain
  @Test
  public void testBlockListener() {
    assumeTrue(!LITE_MODE && TEST_NOTIFICATIONS);
    
    try {
            
      // start mining if possible to help push the network along
      String address = wallet.getPrimaryAddress();
      try { daemon.startMining(address, 8l, false, true); }
      catch (MoneroError e) { }
      
      // register a listener
      MoneroDaemonListener listener = new MoneroDaemonListener();
      daemon.addListener(listener);
      
      // wait for next block notification
      MoneroBlockHeader header = daemon.waitForNextBlockHeader();
      daemon.removeListener(listener); // unregister listener so daemon does not keep polling
      testBlockHeader(header, true);
      
      // test that listener was called with equivalent header
      assertEquals(header, listener.getLastBlockHeader());
    } catch (MoneroError e) {
      throw e;
    } finally {
      
      // stop mining
      try { daemon.stopMining(); }
      catch (MoneroError e) { }
    }
  }
  
  // ------------------------------- PRIVATE ---------------------------------
  
  /**
   * Provides context or configuration for test methods to test a type.
   */
  public static class TestContext {
    Boolean hasJson;
    Boolean isPruned;
    Boolean isFull;
    Boolean isConfirmed;
    Boolean isMinerTx;
    Boolean fromGetTxPool;
    Boolean fromBinaryBlock;
    Boolean hasOutputIndices;
    Boolean doNotTestCopy;
    Boolean hasTxs;
    Boolean hasHex;
    Boolean headerIsFull;
    TestContext txContext;
    public TestContext() { }
    public TestContext(TestContext ctx) {
      this.hasJson = ctx.hasJson;
      this.isPruned = ctx.isPruned;
      this.isFull = ctx.isFull;
      this.isConfirmed = ctx.isConfirmed;
      this.isMinerTx = ctx.isMinerTx;
      this.fromGetTxPool = ctx.fromGetTxPool;
      this.fromBinaryBlock = ctx.fromBinaryBlock;
      this.hasOutputIndices = ctx.hasOutputIndices;
      this.doNotTestCopy = ctx.doNotTestCopy;
      this.hasTxs = ctx.hasTxs;
      this.hasHex = ctx.hasHex;
      this.headerIsFull = ctx.headerIsFull;
    }
  }
  
  private static void testBlockTemplate(MoneroBlockTemplate template) {
    assertNotNull(template);
    assertNotNull(template.getBlockTemplateBlob());
    assertNotNull(template.getBlockHashingBlob());
    assertNotNull(template.getDifficulty());
    assertNotNull(template.getExpectedReward());
    assertNotNull(template.getHeight());
    assertNotNull(template.getPrevHash());
    assertNotNull(template.getReservedOffset());
    assertNotNull(template.getSeedHeight());
    assertTrue(template.getSeedHeight() >= 0);
    assertNotNull(template.getSeedHash());
    assertFalse(template.getSeedHash().isEmpty());
    // next seed hash can be null or initialized  // TODO: test circumstances for each
  }
  
  private static void testBlockHeader(MoneroBlockHeader header, boolean isFull) {
    assertNotNull(header);
    assertTrue(header.getHeight() >= 0);
    assertTrue(header.getMajorVersion() > 0);
    assertTrue(header.getMinorVersion() >= 0);
    if (header.getHeight() == 0) assertTrue(header.getTimestamp() == 0);
    else assertTrue(header.getTimestamp() > 0);
    assertNotNull(header.getPrevHash());
    assertNotNull(header.getNonce());
    if (header.getNonce() == 0) System.err.println("WARNING: header nonce is 0 at height " + header.getHeight()); // TODO (monero-project): why is header nonce 0?
    else assertTrue(header.getNonce() > 0);
    assertNull(header.getPowHash());  // never seen defined
    if (isFull) {
      assertTrue(header.getSize() > 0);
      assertTrue(header.getDepth() >= 0);
      assertTrue(header.getDifficulty().compareTo(BigInteger.valueOf(0)) > 0);
      assertTrue(header.getCumulativeDifficulty().compareTo(BigInteger.valueOf(0)) > 0);
      assertEquals(64, header.getHash().length());
      assertEquals(64, header.getMinerTxHash().length());
      assertTrue(header.getNumTxs() >= 0);
      assertNotNull(header.getOrphanStatus());
      assertNotNull(header.getReward());
      assertNotNull(header.getWeight());
      assertTrue(header.getWeight() > 0);
    } else {
      assertNull(header.getSize());
      assertNull(header.getDepth());
      assertNull(header.getDifficulty());
      assertNull(header.getCumulativeDifficulty());
      assertNull(header.getHash());
      assertNull(header.getMinerTxHash());
      assertNull(header.getNumTxs());
      assertNull(header.getOrphanStatus());
      assertNull(header.getReward());
      assertNull(header.getWeight());
    }
  }
  
  // TODO: test block deep copy
  private static void testBlock(MoneroBlock block, TestContext ctx) {
    
    // test required fields
    assertNotNull(block);
    testMinerTx(block.getMinerTx());  // TODO: miner tx doesn't have as much stuff, can't call testTx?
    testBlockHeader(block, ctx.headerIsFull);
    
    if (ctx.hasHex) {
      assertNotNull(block.getHex());
      assertTrue(block.getHex().length() > 1);
    } else {
      assertNull(block.getHex());
    }
    
    if (ctx.hasTxs) {
      assertNotNull(ctx.txContext);
      for (MoneroTx tx : block.getTxs()) {
        assertTrue(block == tx.getBlock());
        testTx(tx, ctx.txContext);
      }
    } else {
      assertNull(ctx.txContext);
      assertNull(block.getTxs());
    }
  }
  
  private static void testMinerTx(MoneroTx minerTx) {
    assertNotNull(minerTx);
    assertNotNull(minerTx.isMinerTx());
    assertTrue(minerTx.getVersion() >= 0);
    assertNotNull(minerTx.getExtra());
    assertTrue(minerTx.getExtra().length > 0);
    assertTrue(minerTx.getUnlockTime().longValue() >= 0);

//    // TODO: miner tx does not have hashes in binary requests so this will fail, need to derive using prunable data
//    TestContext ctx = new TestContext();
//    ctx.hasJson = false;
//    ctx.isPruned = true;
//    ctx.isFull = false;
//    ctx.isConfirmed = true;
//    ctx.isMiner = true;
//    ctx.fromGetTxPool = true;
//    testTx(minerTx, ctx);
  }
  
  private static void testTx(MoneroTx tx, TestContext ctx) {
    
    // check inputs
    assertNotNull(tx);
    assertNotNull(ctx);
    assertNotNull(ctx.isPruned);
    assertNotNull(ctx.isConfirmed);
    assertNotNull(ctx.fromGetTxPool);
    
    // standard across all txs
    assertEquals(64, tx.getHash().length());
    if (tx.isRelayed() == null) assertTrue(tx.inTxPool());  // TODO monerod: add relayed to get_transactions
    else assertNotNull(tx.isRelayed());
    assertNotNull(tx.isConfirmed());
    assertNotNull(tx.inTxPool());
    assertNotNull(tx.isMinerTx());
    assertNotNull(tx.isDoubleSpendSeen());
    assertTrue(tx.getVersion() >= 0);
    assertTrue(tx.getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
    assertNotNull(tx.getInputs());
    assertNotNull(tx.getOutputs());
    assertTrue(tx.getExtra().length > 0);
    TestUtils.testUnsignedBigInteger(tx.getFee(), true);
    
    // test presence of output indices
    // TODO: change this over to outputs only
    if (tx.isMinerTx()) assertEquals(tx.getOutputIndices(), null); // TODO: how to get output indices for miner transactions?
    if (tx.inTxPool() || ctx.fromGetTxPool || Boolean.FALSE.equals(ctx.hasOutputIndices)) assertEquals(null, tx.getOutputIndices());
    else assertNotNull(tx.getOutputIndices());
    if (tx.getOutputIndices() != null) assertFalse(tx.getOutputIndices().isEmpty());
    
    // test confirmed ctx
    if (ctx.isConfirmed == true) assertEquals(true, tx.isConfirmed());
    if (ctx.isConfirmed == false) assertEquals(false, tx.isConfirmed());
    
    // test confirmed
    if (tx.isConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeight() > 0);
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeight() > 0);
      assertTrue(tx.getBlock().getTimestamp() > 0);
      assertEquals(true, tx.getRelay());
      assertEquals(true, tx.isRelayed());
      assertEquals(false, tx.isFailed());
      assertEquals(false, tx.inTxPool());
      assertEquals(false, tx.isDoubleSpendSeen());
      if (Boolean.TRUE.equals(ctx.fromBinaryBlock)) assertNull(tx.getNumConfirmations());
      else assertTrue(tx.getNumConfirmations() > 0);
    } else {
      assertEquals(null, tx.getBlock());
      assertEquals(0, (long) tx.getNumConfirmations());
    }
    
    // test in tx pool
    if (tx.inTxPool()) {
      assertEquals(tx.isConfirmed(), false);
      assertEquals(tx.isDoubleSpendSeen(), false);
      assertEquals(tx.getLastFailedHeight(), null);
      assertEquals(tx.getLastFailedHash(), null);
      assertTrue(tx.getReceivedTimestamp() > 0);
      if (ctx.fromGetTxPool) {
        assertTrue(tx.getSize() > 0);
        assertTrue(tx.getWeight() > 0);
        assertNotNull(tx.isKeptByBlock());
        assertTrue(tx.getMaxUsedBlockHeight() >= 0);
        assertNotNull(tx.getMaxUsedBlockHash());
      }
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedHash());
    } else {
      assertEquals(tx.getLastRelayedTimestamp(), null);
    }
    
    // test miner tx
    if (tx.isMinerTx()) {
      assertEquals(0, tx.getFee().equals(BigInteger.valueOf(0)));
      assertEquals(null, tx.getInputs());
      assertNull(tx.getSignatures());
    } else {
      if (tx.getSignatures() != null) assertFalse(tx.getSignatures().isEmpty());
    }
    
    // test failed  // TODO: what else to test associated with failed
    if (tx.isFailed()) {
      assertTrue(tx.getReceivedTimestamp() > 0);
    } else {
      if (tx.isRelayed() == null) assertEquals(null, tx.getRelay()); // TODO monerod: add relayed to get_transactions
      else if (tx.isRelayed()) assertEquals(false, tx.isDoubleSpendSeen());
      else {
        assertEquals(false, tx.isRelayed());
        if (ctx.fromGetTxPool) {
          assertEquals(false, tx.getRelay());
          assertNotNull(tx.isDoubleSpendSeen());
        }
      }
    }
    assertNull(tx.getLastFailedHeight());
    assertNull(tx.getLastFailedHash());
    
    // received time only for tx pool or failed txs
    if (tx.getReceivedTimestamp() != null) {
      assertTrue(tx.inTxPool() || tx.isFailed());
    }
    
    // test inputs and outputs
    if (!tx.isMinerTx()) assertFalse(tx.getInputs().isEmpty());
    for (MoneroOutput input : tx.getInputs()) {
      assertTrue(tx == input.getTx());
      testInput(input, ctx);
    }
    assertFalse(tx.getOutputs().isEmpty());
    for (MoneroOutput output : tx.getOutputs()) {
      assert(tx == output.getTx());
      testOutput(output, ctx);
    }
    
    // test pruned vs not pruned
    boolean isPruned = tx.getPrunedHex() != null; // tx might be pruned regardless of configuration
    if (ctx.isPruned) assertTrue(isPruned);
    if (ctx.fromGetTxPool || Boolean.TRUE.equals(ctx.fromBinaryBlock)) assertNull(tx.getPrunableHash());   // TODO monerod: tx pool txs do not have prunable hash, TODO: getBlocksByHeight() has inconsistent client-side pruning
    else assertNotNull(tx.getPrunableHash());
    if (isPruned) {
      assertNull(tx.getRctSigPrunable());
      assertNull(tx.getSize());
      assertNull(tx.getLastRelayedTimestamp());
      assertNull(tx.getReceivedTimestamp());
      assertNull(tx.getFullHex());
      assertNotNull(tx.getPrunedHex());
    } else {
      assertTrue(tx.getVersion() >= 0);
      assertTrue(tx.getUnlockTime().compareTo(BigInteger.ZERO) >= 0);
      assertTrue(tx.getExtra().length > 0);
      if (Boolean.TRUE.equals(ctx.fromBinaryBlock)) assertNull(tx.getFullHex());         // TODO: getBlocksByHeight() has inconsistent client-side pruning
      else assertFalse(tx.getFullHex().isEmpty());
      if (Boolean.TRUE.equals(ctx.fromBinaryBlock)) assertNull(tx.getRctSigPrunable());  // TODO: getBlocksByHeight() has inconsistent client-side pruning
      //else assertNotNull(tx.getRctSigPrunable()); // TODO: define and test this
      assertFalse(tx.isDoubleSpendSeen());
      if (tx.isConfirmed()) {
        assertNull(tx.getLastRelayedTimestamp());
        assertNull(tx.getReceivedTimestamp());
      } else {
        if (tx.isRelayed()) assert(tx.getLastRelayedTimestamp() > 0);
        else assertNull(tx.getLastRelayedTimestamp());
        assert(tx.getReceivedTimestamp() > 0);
      }
    }
    
    if (tx.isFailed()) {
      // TODO: implement this
    }
    
    // test deep copy
    if (!Boolean.TRUE.equals(ctx.doNotTestCopy)) testTxCopy(tx, ctx);
  }
  
  private static void testInput(MoneroOutput input, TestContext ctx) {
    testOutput(input);
    testKeyImage(input.getKeyImage(), ctx);
    assertFalse(input.getRingOutputIndices().isEmpty());
  }

  private static void testKeyImage(MoneroKeyImage image, TestContext ctx) {
    assertFalse(image.getHex().isEmpty());
    if (image.getSignature() != null) {
      assertNotNull(image.getSignature());
      assertFalse(image.getSignature().isEmpty());
    }
  }

  private static void testOutput(MoneroOutput output, TestContext ctx) {
    testOutput(output);
    if (output.getTx().inTxPool() || Boolean.FALSE.equals(ctx.hasOutputIndices)) assertEquals(null, output.getIndex());
    else assertTrue(output.getIndex() >= 0);
    assertEquals(64, output.getStealthPublicKey().length());
  }

  private static void testOutput(MoneroOutput output) {
    TestUtils.testUnsignedBigInteger(output.getAmount());
  }
  
  private static void testTxCopy(MoneroTx tx, TestContext ctx) {
    
    // copy tx and assert deep equality
    MoneroTx copy = tx.copy();
    assertTrue(copy instanceof MoneroTx);
    assertNull(copy.getBlock());
    if (tx.getBlock() != null) copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy)));
    assertEquals(tx.toString(), copy.toString());
    assertTrue(copy != tx);
    
    // test different input references
    if (copy.getInputs() == null) assertEquals(tx.getInputs(), null);
    else {
      assertFalse(copy.getInputs() == tx.getInputs());
      for (int i = 0; i < copy.getInputs().size(); i++) {
        assertEquals(0, tx.getInputs().get(i).getAmount().compareTo(copy.getInputs().get(i).getAmount()));
      }
    }
    
    // test different output references
    if (copy.getOutputs() == null) assertEquals(null, tx.getOutputs());
    else {
      assertTrue(copy.getOutputs() != tx.getOutputs());
      for (int i = 0; i < copy.getOutputs().size(); i++) {
        assertEquals(0, tx.getOutputs().get(i).getAmount().compareTo(copy.getOutputs().get(i).getAmount()));
      }
    }
    
    // test copied tx
    ctx = new TestContext(ctx);
    ctx.doNotTestCopy = true; // to prevent infinite recursion
    if (tx.getBlock() != null) copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy))); // copy block for testing
    testTx(copy, ctx);
    
    // test merging with copy
    MoneroTx merged = copy.merge(copy.copy());
    assertEquals(tx.toString(), merged.toString());
  }
  
  private static void testGetBlocksRange(Long startHeight, Long endHeight, Long chainHeight, boolean chunked) {
    
    // fetch blocks by range
    long realStartHeight = startHeight == null ? 0 : startHeight;
    long realEndHeight = endHeight == null ? chainHeight - 1 : endHeight;
    List<MoneroBlock> blocks = chunked ? daemon.getBlocksByRangeChunked(startHeight, endHeight) : daemon.getBlocksByRange(startHeight, endHeight);
    assertEquals(realEndHeight - realStartHeight + 1, blocks.size());
    
    // test each block
    for (int i = 0; i < blocks.size(); i++) {
      assertEquals(realStartHeight + i, (long) blocks.get(i).getHeight());
      testBlock(blocks.get(i), BINARY_BLOCK_CTX);
    }
  }
  
  private static List<String> getConfirmedTxHashes(MoneroDaemon daemon) {
    int numTxs = 5;
    List<String> txHashes = new ArrayList<String>();
    long height = daemon.getHeight();
    while (txHashes.size() < numTxs && height > 0) {
      MoneroBlock block = daemon.getBlockByHeight(--height);
      for (String txHash : block.getTxHashes()) txHashes.add(txHash);
    }
    return txHashes;
  }
  
  private static MoneroTx getUnrelayedTx(MoneroWallet wallet, Integer accountIdx) {
    assertTrue(accountIdx > 0, "Txs sent from/to same account are not properly synced from the pool");  // TODO monero-project
    MoneroTxConfig config = new MoneroTxConfig().setAccountIndex(accountIdx).setAddress(wallet.getPrimaryAddress()).setAmount(TestUtils.MAX_FEE);
    MoneroTx tx = wallet.createTx(config);
    assertFalse(tx.getFullHex().isEmpty());
    assertEquals(tx.getRelay(), false);
    return tx;
  }
  
  private static void testMinerTxSum(MoneroMinerTxSum txSum) {
    TestUtils.testUnsignedBigInteger(txSum.getEmissionSum(), true);
    TestUtils.testUnsignedBigInteger(txSum.getFeeSum(), true);
  }
  
  private static void testTxPoolStats(MoneroTxPoolStats stats) {
    assertNotNull(stats);
    assertTrue(stats.getNumTxs() >= 0);
    if (stats.getNumTxs() > 0) {
      if (stats.getNumTxs() == 1) assertNull(stats.getHisto());
      else {
        Map<Long, Integer> histo = stats.getHisto();
        assertNotNull(histo);
        assertTrue(histo.size() > 0);
        for (Long key : histo.keySet()) {
          assertTrue(histo.get(key) >= 0);
        }
      }
      assertTrue(stats.getBytesMax() > 0);
      assertTrue(stats.getBytesMed() > 0);
      assertTrue(stats.getBytesMin() > 0);
      assertTrue(stats.getBytesTotal() > 0);
      assertTrue(stats.getHisto98pc() == null || stats.getHisto98pc() > 0);
      assertTrue(stats.getOldestTimestamp() > 0);
      assertTrue(stats.getNum10m() >= 0);
      assertTrue(stats.getNumDoubleSpends() >= 0);
      assertTrue(stats.getNumFailing() >= 0);
      assertTrue(stats.getNumNotRelayed() >= 0);
    } else {
      assertNull(stats.getBytesMax());
      assertNull(stats.getBytesMed());
      assertNull(stats.getBytesMin());
      assertEquals(0, (long) stats.getBytesTotal());
      assertNull(stats.getHisto98pc());
      assertNull(stats.getOldestTimestamp());
      assertEquals(0, (int) stats.getNum10m());
      assertEquals(0, (int) stats.getNumDoubleSpends());
      assertEquals(0, (int) stats.getNumFailing());
      assertEquals(0, (int) stats.getNumNotRelayed());
      assertNull(stats.getHisto());
    }
  }
  
  // helper function to check the spent status of a key image or array of key images
  private static void testSpentStatuses(List<String> keyImages, MoneroKeyImageSpentStatus expectedStatus) {
    
    // test image
    for (String keyImage : keyImages) {
      assertEquals(expectedStatus, daemon.getKeyImageSpentStatus(keyImage));
    }
    
    // test array of images
    List<MoneroKeyImageSpentStatus> statuses = keyImages.isEmpty() ? new ArrayList<MoneroKeyImageSpentStatus>() : daemon.getKeyImageSpentStatuses(keyImages);
    assertEquals(keyImages.size(), statuses.size());
    for (MoneroKeyImageSpentStatus status : statuses) assertEquals(expectedStatus, status);
  }
  
  private static List<MoneroTx> getConfirmedTxs(MoneroDaemon daemon, int numTxs) {
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    int numBlocksPerReq = 50;
    for (long startIdx = daemon.getHeight() - numBlocksPerReq - 1; startIdx >= 0; startIdx -= numBlocksPerReq) {
      List<MoneroBlock> blocks = daemon.getBlocksByRange(startIdx, startIdx + numBlocksPerReq);
      for (MoneroBlock block : blocks) {
        if (block.getTxs() == null) continue;
        for (MoneroTx tx : block.getTxs()) {
          txs.add(tx);
          if (txs.size() == numTxs) return txs;
        }
      }
    }
    throw new RuntimeException("Could not get " + numTxs + " confirmed txs");
  }
  
  private static void testOutputDistributionEntry(MoneroOutputDistributionEntry entry) {
    TestUtils.testUnsignedBigInteger(entry.getAmount());
    assert(entry.getBase() >= 0);
    assertFalse(entry.getDistribution().isEmpty());
    assertTrue(entry.getStartHeight() >= 0);
  }
  
  private static void testInfo(MoneroDaemonInfo info) {
    assertNotNull(info.getVersion());
    assertTrue(info.getNumAltBlocks() >= 0);
    assertTrue(info.getBlockSizeLimit() > 0);
    assertTrue(info.getBlockSizeMedian() > 0);
    assertTrue(info.getBootstrapDaemonAddress() == null || !info.getBootstrapDaemonAddress().isEmpty());
    TestUtils.testUnsignedBigInteger(info.getCumulativeDifficulty());
    TestUtils.testUnsignedBigInteger(info.getFreeSpace());
    assertTrue(info.getNumOfflinePeers() >= 0);
    assertTrue(info.getNumOnlinePeers() >= 0);
    assertTrue(info.getHeight() >= 0);
    assertTrue(info.getHeightWithoutBootstrap() > 0);
    assertTrue(info.getNumIncomingConnections() >= 0);
    assertNotNull(info.getNetworkType());
    assertNotNull(info.isOffline());
    assertTrue(info.getNumOutgoingConnections() >= 0);
    assertTrue(info.getNumRpcConnections() >= 0);
    assertTrue(info.getStartTimestamp() > 0);
    assertTrue(info.getAdjustedTimestamp() > 0);
    assertTrue(info.getTarget() > 0);
    assertTrue(info.getTargetHeight() >= 0);
    assertTrue(info.getNumTxs() >= 0);
    assertTrue(info.getNumTxsPool() >= 0);
    assertNotNull(info.getWasBootstrapEverUsed());
    assertTrue(info.getBlockWeightLimit() > 0);
    assertTrue(info.getBlockWeightMedian() > 0);
    assertTrue(info.getDatabaseSize() > 0);
    assertNotNull(info.getUpdateAvailable());
    TestUtils.testUnsignedBigInteger(info.getCredits(), false); // 0 credits
    assertFalse(info.getTopBlockHash().isEmpty());
    assertNotNull(info.isBusySyncing());
    assertNotNull(info.isSynchronized());
  }

  private static void testSyncInfo(MoneroDaemonSyncInfo syncInfo) { // TODO: consistent naming, daemon in name?
    assertTrue(syncInfo instanceof MoneroDaemonSyncInfo);
    assertTrue(syncInfo.getHeight() >= 0);
    if (syncInfo.getPeers() != null) {
      assertTrue(syncInfo.getPeers().size() > 0);
      for (MoneroPeer connection : syncInfo.getPeers()) {
        testPeer(connection);
      }
    }
    if (syncInfo.getSpans() != null) {  // TODO: test that this is being hit, so far not used
      assertTrue(syncInfo.getSpans().size() > 0);
      for (MoneroConnectionSpan span : syncInfo.getSpans()) {
        testConnectionSpan(span);
      }
    }
    assertTrue(syncInfo.getNextNeededPruningSeed() >= 0);
    assertNull(syncInfo.getOverview());
    TestUtils.testUnsignedBigInteger(syncInfo.getCredits(), false); // 0 credits
    assertNull(syncInfo.getTopBlockHash());
  }
  
  private static void testConnectionSpan(MoneroConnectionSpan span) {
    assertNotNull(span);
    assertNotNull(span.getConnectionId());
    assertFalse(span.getConnectionId().isEmpty());
    assertTrue(span.getStartHeight() > 0);
    assertTrue(span.getNumBlocks() > 0);
    assertTrue(span.getRemoteAddress() == null || !span.getRemoteAddress().isEmpty());
    assertTrue(span.getRate() > 0);
    assertTrue(span.getSpeed() >= 0);
    assertTrue(span.getSize() > 0);
  }

  private static void testHardForkInfo(MoneroHardForkInfo hardForkInfo) {
    assertNotNull(hardForkInfo.getEarliestHeight());
    assertNotNull(hardForkInfo.isEnabled());
    assertNotNull(hardForkInfo.getState());
    assertNotNull(hardForkInfo.getThreshold());
    assertNotNull(hardForkInfo.getVersion());
    assertNotNull(hardForkInfo.getNumVotes());
    assertNotNull(hardForkInfo.getVoting());
    assertNotNull(hardForkInfo.getWindow());
    TestUtils.testUnsignedBigInteger(hardForkInfo.getCredits(), false); // 0 credits
    assertNull(hardForkInfo.getTopBlockHash());
  }

  private static void testMoneroBan(MoneroBan ban) {
    assertNotNull(ban.getHost());
    assertNotNull(ban.getIp());
    assertNotNull(ban.getSeconds());
  }
  
  private static void testAltChain(MoneroAltChain altChain) {
    assertNotNull(altChain);
    assertFalse(altChain.getBlockHashes().isEmpty());
    TestUtils.testUnsignedBigInteger(altChain.getDifficulty(), true);
    assertTrue(altChain.getHeight() > 0);
    assertTrue(altChain.getLength() > 0);
    assertEquals(64, altChain.getMainChainParentBlockHash().length());
  }

  private static void testPeer(MoneroPeer peer) {
    assertTrue(peer instanceof MoneroPeer);
    testKnownPeer(peer, true);
    assertFalse(peer.getHash().isEmpty());
    assertTrue(peer.getAvgDownload() >= 0);
    assertTrue(peer.getAvgUpload() >= 0);
    assertTrue(peer.getCurrentDownload() >= 0);
    assertTrue(peer.getCurrentUpload() >= 0);
    assertTrue(peer.getHeight() >= 0);
    assertTrue(peer.getLiveTime() >= 0);
    assertNotNull(peer.isLocalIp());
    assertNotNull(peer.isLocalHost());
    assertTrue(peer.getNumReceives() >= 0);
    assertTrue(peer.getReceiveIdleTime() >= 0);
    assertTrue(peer.getNumSends() >= 0);
    assertTrue(peer.getSendIdleTime() >= 0);
    assertNotNull(peer.getState());
    assertTrue(peer.getNumSupportFlags() >= 0);
    assertNotNull(peer.getType());
  }

  private static void testKnownPeer(MoneroPeer peer, boolean fromConnection) {
    assertNotNull(peer);
    assertFalse(peer.getId().isEmpty());
    assertFalse(peer.getHost().isEmpty());
    assertTrue(peer.getPort() > 0);
    assertTrue(peer.getRpcPort() == null || peer.getRpcPort() >= 0);
    assertNotNull(peer.isOnline());
    if (peer.getRpcCreditsPerHash() != null) TestUtils.testUnsignedBigInteger(peer.getRpcCreditsPerHash());
    if (fromConnection) assertNull(peer.getLastSeenTimestamp());
    else {
      if (peer.getLastSeenTimestamp() < 0) System.out.println("Last seen timestamp is invalid: " + peer.getLastSeenTimestamp());
      assertTrue(peer.getLastSeenTimestamp() >= 0);
    }
    assertTrue(peer.getPruningSeed() == null || peer.getPruningSeed() >= 0);
  }

  private static void testUpdateCheckResult(MoneroDaemonUpdateCheckResult result) {
    assertTrue(result instanceof MoneroDaemonUpdateCheckResult);
    assertNotNull(result.isUpdateAvailable());
    if (result.isUpdateAvailable()) {
      assertFalse(result.getAutoUri().isEmpty(), "No auto uri; is daemon online?");
      assertFalse(result.getUserUri().isEmpty());
      assertFalse(result.getVersion().isEmpty());
      assertFalse(result.getHash().isEmpty());
      assertEquals(64, result.getHash().length());
    } else {
      assertNull(result.getAutoUri());
      assertNull(result.getUserUri());
      assertNull(result.getVersion());
      assertNull(result.getHash());
    }
  }

  private static void testUpdateDownloadResult(MoneroDaemonUpdateDownloadResult result, String path) {
    testUpdateCheckResult(result);
    if (result.isUpdateAvailable()) {
      if (path != null) assertEquals(path, result.getDownloadPath());
      else assertNotNull(result.getDownloadPath());
    } else {
      assertNull(result.getDownloadPath());
    }
  }
  
  private static void testSubmitTxResultGood(MoneroSubmitTxResult result) {
    testSubmitTxResultCommon(result);
    try {
      assertEquals(false, result.isDoubleSpend(), "tx submission is double spend.");
      assertEquals(false, result.isFeeTooLow());
      assertEquals(false, result.isMixinTooLow());
      assertEquals(false, result.hasInvalidInput());
      assertEquals(false, result.hasInvalidOutput());
      assertEquals(false, result.hasTooFewOutputs());
      assertEquals(false, result.isOverspend());
      assertEquals(false, result.isTooBig());
      assertEquals(false, result.getSanityCheckFailed());
      TestUtils.testUnsignedBigInteger(result.getCredits(), false); // 0 credits
      assertNull(result.getTopBlockHash());
      assertEquals(false, result.isTxExtraTooBig());
      assertEquals(true, result.isGood());
      assertEquals(false, result.isNonzeroUnlockTime());
    } catch (AssertionError e) {
      System.out.println("Submit result is not good: " + JsonUtils.serialize(result));
      throw e;
    }
  }
  
  private static void testSubmitTxResultDoubleSpend(MoneroSubmitTxResult result) {
    testSubmitTxResultCommon(result);
    assertEquals(false, result.isGood());
    assertEquals(true, result.isDoubleSpend());
    assertEquals(false, result.isFeeTooLow());
    assertEquals(false, result.isMixinTooLow());
    assertEquals(false, result.hasInvalidInput());
    assertEquals(false, result.hasInvalidOutput());
    assertEquals(false, result.isOverspend());
    assertEquals(false, result.isTooBig());
  }

  private static void testSubmitTxResultCommon(MoneroSubmitTxResult result) {
    assertNotNull(result.isGood());
    assertNotNull(result.isRelayed());
    assertNotNull(result.isDoubleSpend());
    assertNotNull(result.isFeeTooLow());
    assertNotNull(result.isMixinTooLow());
    assertNotNull(result.hasInvalidInput());
    assertNotNull(result.hasInvalidOutput());
    assertNotNull(result.isOverspend());
    assertNotNull(result.isTooBig());
    assertNotNull(result.getSanityCheckFailed());
    assertTrue(result.getReason() == null || !result.getReason().isEmpty());
  }
  
  private static void testOutputHistogramEntry(MoneroOutputHistogramEntry entry) {
    TestUtils.testUnsignedBigInteger(entry.getAmount());
    assertTrue(entry.getNumInstances() >= 0);
    assertTrue(entry.getNumUnlockedInstances() >= 0);
    assertTrue(entry.getNumRecentInstances() >= 0);
  }
}
