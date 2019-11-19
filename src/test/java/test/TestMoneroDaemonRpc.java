package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import common.utils.JsonUtils;
import monero.daemon.MoneroDaemon;
import monero.daemon.MoneroDaemonRpc;
import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroMinerTxSum;
import monero.daemon.model.MoneroDaemonConnection;
import monero.daemon.model.MoneroDaemonConnectionSpan;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonListener;
import monero.daemon.model.MoneroDaemonPeer;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroOutputHistogramEntry;
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxPoolStats;
import monero.rpc.MoneroRpcException;
import monero.utils.MoneroException;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroSendRequest;
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
  
  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daemon = TestUtils.getDaemonRpc();
    wallet = TestUtils.getWalletRpc();
    TestUtils.TX_POOL_WALLET_TRACKER.reset(); // all wallets need to wait for txs to confirm to reliably sync
  }
  
  @Before
  public void before() {
    
  }
  
  // -------------------------------- NON RELAYS ------------------------------
  
  // Can indicate if it's trusted
  @Test
  public void testIsTrusted() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.isTrusted();
  }
  
  // Can get the blockchain height
  @Test
  public void testGetHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    long height = daemon.getHeight();
    assertTrue("Height must be greater than 0", height > 0);
  }
  
  // Can get a block id by height
  @Test
  public void testGetBlockIdByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    assertNotNull(id);
    assertEquals(64, id.length());
  }
  
  // Can get a block template
  @Test
  public void testGetBlockTemplate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.ADDRESS, 2);
    testBlockTemplate(template);
  }
  
  // Can get the last block's header
  @Test
  public void testGetLastBlockHeader() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    testBlockHeader(lastHeader, true);
  }
  
  // Can get a block header by id
  @Test
  public void testGetBlockHeaderById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // retrieve by id of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    MoneroBlockHeader header = daemon.getBlockHeaderById(id);
    testBlockHeader(header, true);
    assertEquals(lastHeader, header);
    
    // retrieve by id of previous to last block
    id = daemon.getBlockId(lastHeader.getHeight() - 1);
    header = daemon.getBlockHeaderById(id);
    testBlockHeader(header, true);
    assertEquals(lastHeader.getHeight() - 1, (long) header.getHeight());
  }
  
  // Can get a block header by height
  @Test
  public void testGetBlockHeaderByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
  
  // Can get a block by id
  @Test
  public void testGetBlockById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test config
    TestContext ctx = new TestContext();
    ctx.hasHex = true;
    ctx.hasTxs = false;
    ctx.headerIsFull = true;
    
    // retrieve by id of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    MoneroBlock block = daemon.getBlockById(id);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeight()), block);
    assertEquals(null, block.getTxs());
    
    // retrieve by id of previous to last block
    id = daemon.getBlockId(lastHeader.getHeight() - 1);
    block = daemon.getBlockById(id);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(lastHeader.getHeight() - 1), block);
    assertEquals(null, block.getTxs());
  }

  // Can get blocks by id which includes transactions (binary)
  @Test
  public void testGetBlocksByIdBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented");
  }

  // Can get a block by height
  @Test
  public void testGetBlockByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set number of blocks to test
    int numBlocks = 200;
    
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
    assertTrue("No transactions found to test", txFound);
  }
  
  // Can get blocks by range in a single request
  @Test
  public void testGetBlocksByRange() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS && !LITE_MODE);
    
    // get long height range
    long numBlocks = 2160; // test ~3 days of blocks
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
  
  // Can get block ids (binary)
  @Test
  public void testGetBlockIdsBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    //get_hashes.bin
    throw new RuntimeException("Not implemented");
  }
  
  // Can get a transaction by id with and without pruning
  @Test
  public void testGetTxById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // context for testing txs
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = true;
    ctx.fromGetTxPool = false;
    
    // fetch each tx by id without pruning
    for (String txId : txIds) {
      MoneroTx tx = daemon.getTx(txId);
      testTx(tx, ctx);
    }
    
    // fetch each tx by id with pruning
    for (String txId : txIds) {
      MoneroTx tx = daemon.getTx(txId, true);
      ctx.isPruned = true;
      testTx(tx, ctx);
    }
    
    // fetch invalid id
    try {
      daemon.getTx("invalid tx id");
      throw new RuntimeException("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  // Can get transactions by ids with and without pruning
  @Test
  public void testGetTxsByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // context for testing txs
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = true;
    ctx.fromGetTxPool = false;
    
    // fetch txs by id without pruning
    List<MoneroTx> txs = daemon.getTxs(txIds);
    assertEquals(txIds.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // fetch txs by id with pruning
    txs = daemon.getTxs(txIds, true);
    ctx.isPruned = true;
    assertEquals(txIds.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // fetch invalid id
    txIds.add("invalid tx id");
    try {
      daemon.getTxs(txIds);
      throw new RuntimeException("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  // Can get transactions by ids that are in the transaction pool
  //@Ignore // TODO re-enable, monero-core: fix daemon.getTxs() hanging occasionally
  @Test
  public void testGetTxsByIdsInPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet); // wait for wallet's txs in the pool to clear to ensure reliable sync
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 1; i < 3; i++) {
      System.out.print("Fetching unrelayed tx...");
      MoneroTx tx = getUnrelayedTx(wallet, i);
      System.out.println("done");
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      assertFalse(result.isDoubleSpend());
      assertTrue(result.isGood());
      txIds.add(tx.getId());
    }
    
    // fetch txs by id
    System.out.print("Fetching txs...");
    List<MoneroTx> txs = daemon.getTxs(txIds);
    System.out.println("done");
    
    // context for testing tx
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = false;
    ctx.fromGetTxPool = false;
    
    // test fetched txs
    assertEquals(txIds.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // clear txs from pool
    daemon.flushTxPool(txIds);
    wallet.sync();
  }
  
  // Can get a transaction hex by id with and without pruning
  @Test
  public void testGetTxHexById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // fetch each tx hex by id with and without pruning
    List<String> hexes = new ArrayList<String>();
    List<String> hexesPruned = new ArrayList<String>();
    for (String txId : txIds) {
      hexes.add(daemon.getTxHex(txId));
      hexesPruned.add(daemon.getTxHex(txId, true));
    }
    
    // test results
    assertEquals(hexes.size(), txIds.size());
    assertEquals(hexesPruned.size(), txIds.size());
    for (int i = 0; i < hexes.size(); i++) {
      assertNotNull(hexes.get(i));
      assertNotNull(hexesPruned.get(i));
      assertFalse(hexesPruned.isEmpty());
      assertTrue(hexes.get(i).length() > hexesPruned.get(i).length()); // pruned hex is shorter
    }
    
    // fetch invalid id
    try {
      daemon.getTxHex("invalid tx id");
      throw new RuntimeException("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  // Can get transaction hexes by ids with and without pruning
  @Test
  public void testGetTxHexesByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // fetch tx hexes by id with and without pruning
    List<String> hexes = daemon.getTxHexes(txIds);
    List<String> hexesPruned = daemon.getTxHexes(txIds, true);
    
    // test results
    assertEquals(hexes.size(), txIds.size());
    assertEquals(hexesPruned.size(), txIds.size());
    for (int i = 0; i < hexes.size(); i++) {
      assertNotNull(hexes.get(i));
      assertNotNull(hexesPruned.get(i));
      assertFalse(hexesPruned.isEmpty());
      assertTrue(hexes.get(i).length() > hexesPruned.get(i).length()); // pruned hex is shorter
    }
    
    // fetch invalid id
    txIds.add("invalid tx id");
    try {
      daemon.getTxHexes(txIds);
      throw new RuntimeException("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  // Can get the miner transaction sum
  @Test
  public void testGetMinerTxSum() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroMinerTxSum sum = daemon.getMinerTxSum(0l, 50000l);
    testMinerTxSum(sum);
  }
  
  // Can get a fee estimate
  @Test
  public void testGetFeeEstimate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    BigInteger fee = daemon.getFeeEstimate();
    TestUtils.testUnsignedBigInteger(fee, true);
  }
  
  // Can get all transactions in the transaction pool
  @Test
  public void testGetTxsInPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // submit tx to pool but don't relay
    MoneroTx tx = getUnrelayedTx(wallet, 1);
    MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
    assertTrue(result.isGood());
    assertFalse(result.isRelayed());
    
    // fetch txs in pool
    List<MoneroTx> txs = daemon.getTxPool();
    
    // context for testing tx
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = false;
    ctx.fromGetTxPool = true;
    
    // test txs
    assertFalse("Test requires an unconfirmed tx in the tx pool", txs.isEmpty());
    for (MoneroTx aTx : txs) {
      testTx(aTx, ctx);
    }
    
    // flush the tx from the pool, gg
    daemon.flushTxPool(tx.getId());
    wallet.sync();
  }
  
  // Can get ids of transactions in the transaction pool (binary)
  @Test
  public void testGetIdsOfTxsInPoolBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    // TODO: get_transaction_pool_hashes.bin
    throw new RuntimeException("Not implemented");
  }
  
  // Can get the transaction pool backlog (binary)
  @Test
  public void testGetTxPoolBacklogBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    // TODO: get_txpool_backlog
    throw new RuntimeException("Not implemented");
  }
  
  // Can get transaction pool statistics (binary)
  @Test
  public void testGetTxPoolStatisticsBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // submit txs to the pool but don't relay (multiple txs result in binary `histo` field)
    for (int i = 1; i < 3; i++) {
      
      // submit tx hex
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      assertTrue(result.isGood());
      
      // test stats
      try {
        MoneroTxPoolStats stats = daemon.getTxPoolStats();
        assertTrue(stats.getNumTxs() > i);
        testTxPoolStats(stats);
      } finally {
        daemon.flushTxPool(tx.getId());
        wallet.sync();
      }
    }
  }
  
  // Can flush all transactions from the pool
  @Test
  public void testFlushTxsFromPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
  
  // Can flush a transaction from the pool by id
  @Test
  public void testFlushTxFromPoolById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
    
    // remove each tx from the pool by id and test
    for (int i = 0; i < txs.size(); i++) {
      
      // flush tx from pool
      daemon.flushTxPool(txs.get(i).getId());
      
      // test tx pool
      List<MoneroTx> poolTxs = daemon.getTxPool();
      assertEquals(txs.size() - i - 1, poolTxs.size());
    }
    
    // pool is back to original state
    assertEquals(txPoolBefore.size(), daemon.getTxPool().size());
    
    // sync wallet for next test
    wallet.sync();
  }
  
  // Can flush transactions from the pool by ids
  @Test
  public void testFlushTxsFromPoolByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // preserve original transactions in the pool
    List<MoneroTx> txPoolBefore = daemon.getTxPool();
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 1; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
      txIds.add(tx.getId());
    }
    assertEquals(txPoolBefore.size() + txIds.size(), daemon.getTxPool().size());
    
    // remove all txs by ids
    daemon.flushTxPool(txIds);
    
    // pool is back to original state
    assertEquals(txPoolBefore.size(), daemon.getTxPool().size());
    wallet.sync();
  }
  
  // Can get the spent status of key images
  @Test
  @Ignore // TODO: hanging like getTxsByIds()
  public void testGetSpentStatusOfKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
    // submit txs to the pool to collect key images then flush them
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 1; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
      txs.add(tx);
    }
    List<String> keyImages = new ArrayList<String>();
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) txIds.add(tx.getId());
    for (MoneroTx tx : daemon.getTxs(txIds)) {
      for (MoneroOutput vin : tx.getVins()) keyImages.add(vin.getKeyImage().getHex());
    }
    daemon.flushTxPool(txIds);
    
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
      for (MoneroOutput vin : tx.getVins()) keyImages.add(vin.getKeyImage().getHex());
    }
    
    // key images are all spent
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.CONFIRMED);
    
    // flush this test's txs from pool
    daemon.flushTxPool(txIds);
  }
  
  // Can get output indices given a list of transaction ids (binary)
  @Test
  public void testGetOutputIndicesFromTxIdsBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented"); // get_o_indexes.bin
  }
  
  // Can get outputs given a list of output amounts and indices (binary)
  @Test
  public void testGetOutputsFromAmountsAndIndicesBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented"); // get_outs.bin
  }
  
  // Can get an output histogram (binary)
  @Test
  public void testGetOutputHistogramBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroOutputHistogramEntry> entries = daemon.getOutputHistogram(null, null, null, null, null);
    assertFalse(entries.isEmpty());
    for (MoneroOutputHistogramEntry entry : entries) {
      testOutputHistogramEntry(entry);
    }
  }
  
  // Can get an output distribution (binary)
  @Test
  public void testGetOutputDistributionBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonInfo info = daemon.getInfo();
    testInfo(info);
  }
  
  // Can get sync information
  @Test
  public void testGetSyncInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonSyncInfo syncInfo = daemon.getSyncInfo();
    testSyncInfo(syncInfo);
  }
  
  // Can get hard fork information
  @Test
  public void testGetHardForkInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroHardForkInfo hardForkInfo = daemon.getHardForkInfo();
    testHardForkInfo(hardForkInfo);
  }
  
  // Can get alternative chains
  @Test
  public void testGetAlternativeChains() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAltChain> altChains = daemon.getAltChains();
    for (MoneroAltChain altChain : altChains) {
      testAltChain(altChain);
    }
  }
  
  // Can get alternative block ids
  @Test
  public void testGetAlternativeBlockIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<String> altBlockIds = daemon.getAltBlockIds();
    for (String altBlockId : altBlockIds) {
      assertNotNull(altBlockId);
      assertEquals(64, altBlockId.length());  // TODO: common validation
    }
  }
  
  // Can get, set, and reset a download bandwidth limit
  @Test
  public void testSetDownloadBandwidth() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    } catch (MoneroException e) {
      assertEquals("Download limit must be an integer greater than 0", e.getMessage());
    }
    assertEquals(daemon.getDownloadLimit(), initVal);
  }
  
  // Can get, set, and reset an upload bandwidth limit
  @Test
  public void testSetUploadBandwidth() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
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
    } catch (MoneroException e) {
      assertEquals("Upload limit must be an integer greater than 0", e.getMessage());
    }
    assertEquals(initVal, daemon.getUploadLimit());
  }
  
  // Can get known peers which may be online or offline
  @Test
  public void testGetKnownPeers() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroDaemonPeer> peers = daemon.getKnownPeers();
    assertFalse("Daemon has no known peers to test", peers.isEmpty());
    for (MoneroDaemonPeer peer : peers) {
      testKnownPeer(peer, false);
    }
  }
  
  // Can get incoming and outgoing peer connections
  @Test
  public void testGetPeerConnections() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroDaemonConnection> connections = daemon.getConnections();
    assertFalse("Daemon has no incoming or outgoing connections to test", connections.isEmpty());
    for (MoneroDaemonConnection connection : connections) {
      testDaemonConnection(connection);
    }
  }
  
  // Can limit the number of outgoing peers
  @Test
  public void testSetOutgoingPeerLimit() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.setOutgoingPeerLimit(0);
    daemon.setOutgoingPeerLimit(8);
    daemon.setOutgoingPeerLimit(10);
  }
  
  // Can limit the number of incoming peers
  @Test
  public void testSetIncomingPeerLimit() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.setIncomingPeerLimit(0);
    daemon.setIncomingPeerLimit(8);
    daemon.setIncomingPeerLimit(10);
  }
  
  // Can ban a peer
  @Test
  public void testBanPeer() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // stop mining at beginning of test
    try { daemon.stopMining(); }
    catch (MoneroException e) { }
    
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
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    try {
      
      // stop mining at beginning of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
      
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
    } catch (MoneroException e) {
      throw e;
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
    }
  }
  
  // Can submit a mined block to the network
  @Test
  public void testSubmitMinedBlock() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get template to mine on
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.ADDRESS);
    
    // TODO monero rpc: way to get mining nonce when found in order to submit?
    
    // try to submit block hashing blob without nonce
    try {
      daemon.submitBlock(template.getBlockHashingBlob());
      fail("Should have thrown error");
    } catch (MoneroRpcException e) {
      assertEquals(-7, (int) e.getCode());
      assertEquals("Block not accepted", e.getMessage());
    }
  }
  
  // Can check for an update
  @Test
  public void testCheckForUpdate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonUpdateCheckResult result = daemon.checkForUpdate();
    testUpdateCheckResult(result);
  }
  
  // Can download an update
  @Test
  @Ignore
  public void testDownloadUpdate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
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
      } catch (MoneroRpcException e) {
        assertNotEquals("Should have thrown error", e.getMessage());
        assertEquals(500, (int) e.getCode());  // TODO monero-daemon-rpc: this causes a 500 in daemon rpc
      }
    }
  }
  
  // Can be stopped
  @Test
  @Ignore // test is disabled to not interfere with other tests
  public void testStop() throws InterruptedException {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // stop the daemon
    daemon.stop();
    
    // give the daemon time to shut down
    TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL);
    
    // try to interact with the daemon
    try {
      daemon.getHeight();
      throw new RuntimeException("Should have thrown error");
    } catch (MoneroException e) {
      assertNotEquals("Should have thrown error", e.getMessage());
    }
  }
  
  // ----------------------------- RELAY TESTS -------------------------------
  
  // Can submit a tx in hex format to the pool and relay in one call
  @Test
  public void testSubmitAndRelayTxHex() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && !LITE_MODE);
    
    // wait one time for wallet txs in the pool to clear
    // TODO monero core: update from pool does not prevent creating double spend tx
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    
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
      if (aTx.getId().equals(tx1.getId())) {
        assertEquals(true, aTx.isRelayed());
        found = true;
        break;
      }
    }
    assertTrue("Tx1 was not found after being submitted to the daemon's tx pool", found);
    
    // tx1 is recognized by the wallet
    wallet.sync();
    wallet.getTx(tx1.getId());
    
    // submit and relay tx2 hex which double spends tx1
    result = daemon.submitTxHex(tx2.getFullHex());
    assertEquals(true, result.isRelayed());
    testSubmitTxResultDoubleSpend(result);
    
    // tx2 is in not the pool
    txs = daemon.getTxPool();
    found = false;
    for (MoneroTx aTx : txs) {
      if (aTx.getId().equals(tx2.getId())) {
        found = true;
        break;
      }
    }
    assertTrue("Tx2 should not be in the pool because it double spends tx1 which is in the pool", !found);
    
    // all wallets will need to wait for tx to confirm in order to properly sync
    TestUtils.TX_POOL_WALLET_TRACKER.reset();
  }
  
  // Can submit a tx in hex format to the pool then relay
  @Test
  public void testSubmitThenRelayTxHex() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && !LITE_MODE);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    MoneroTx tx = getUnrelayedTx(wallet, 1);
    testSubmitThenRelay(Arrays.asList(tx));
  }
  
  // Can submit txs in hex format to the pool then relay
  @Test
  public void testSubmitThenRelayTxHexes() {
    org.junit.Assume.assumeTrue(TEST_RELAYS && !LITE_MODE);
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(wallet);
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    txs.add(getUnrelayedTx(wallet, 2));
    txs.add(getUnrelayedTx(wallet, 3));  // TODO: accounts cannot be re-used across send tests else isRelayed is true; wallet needs to update?
    testSubmitThenRelay(txs);
  }
  
  private static void testSubmitThenRelay(List<MoneroTx> txs) {
    
    // submit txs hex but don't relay
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) {
      txIds.add(tx.getId());
      MoneroSubmitTxResult result = daemon.submitTxHex(tx.getFullHex(), true);
      testSubmitTxResultGood(result);
      assertEquals(false, result.isRelayed());
      
      // ensure tx is in pool
      List<MoneroTx> poolTxs = daemon.getTxPool();
      boolean found = false;
      for (MoneroTx aTx : poolTxs) {
        if (aTx.getId().equals(tx.getId())) {
          assertEquals(false, aTx.isRelayed());
          found = true;
          break;
        }
      }
      assertTrue("Tx was not found after being submitted to the daemon's tx pool", found);
      
      // fetch tx by id and ensure not relayed
      MoneroTx fetchedTx = daemon.getTx(tx.getId());
      assertFalse(fetchedTx.isRelayed());
    }
    
    // relay the txs
    if (txIds.size() == 1) daemon.relayTxById(txIds.get(0));
    else daemon.relayTxsById(txIds);
    
    // ensure txs are relayed
    for (MoneroTx tx : txs) {
      List<MoneroTx> poolTxs = daemon.getTxPool();
      boolean found = false;
      for (MoneroTx aTx : poolTxs) {
        if (aTx.getId().equals(tx.getId())) {
          assertEquals(true, aTx.isRelayed());
          found = true;
          break;
        }
      }
      assertTrue("Tx was not found after being submitted to the daemon's tx pool", found);
    }
    
    // wallets will need to wait for tx to confirm in order to properly sync
    TestUtils.TX_POOL_WALLET_TRACKER.reset();
  }
  
  // -------------------------- NOTIFICATION TESTS ---------------------------
  
  // Can notify listeners when a new block is added to the chain
  @Test
  public void testBlockListener() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    
    try {
            
      // start mining if possible to help push the network along
      String address = wallet.getPrimaryAddress();
      try { daemon.startMining(address, 8l, false, true); }
      catch (MoneroException e) { }
      
      // register a listener
      MoneroDaemonListener listener = new MoneroDaemonListener();
      daemon.addListener(listener);
      
      // wait for next block notification
      MoneroBlockHeader header = daemon.getNextBlockHeader();
      testBlockHeader(header, true);
      
      // test that listener was called with equivalent header
      assertEquals(header, listener.getLastBlockHeader());
      
      // unregister listener so daemon does not keep polling
      daemon.removeListener(listener);
    } catch (MoneroException e) {
      throw e;
    } finally {
      
      // stop mining
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
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
    assertNotNull(template.getPrevId());
    assertNotNull(template.getReservedOffset());
  }
  
  private static void testBlockHeader(MoneroBlockHeader header, boolean isFull) {
    assertNotNull(header);
    assertTrue(header.getHeight() >= 0);
    assertTrue(header.getMajorVersion() >= 0);
    assertTrue(header.getMinorVersion() >= 0);
    assertTrue(header.getTimestamp() >= 0);
    assertNotNull(header.getPrevId());
    assertNotNull(header.getNonce());
    assertNull(header.getPowHash());  // never seen defined
    if (isFull) {
      assertTrue(header.getSize() > 0);
      assertTrue(header.getDepth() >= 0);
      assertTrue(header.getDifficulty().compareTo(BigInteger.valueOf(0)) > 0);
      assertTrue(header.getCumulativeDifficulty().compareTo(BigInteger.valueOf(0)) > 0);
      assertEquals(64, header.getId().length());
      assertEquals(64, header.getMinerTxId().length());
      assertTrue(header.getNumTxs() >= 0);
      assertNotNull(header.getOrphanStatus());
      assertNotNull(header.getReward());
      assertNotNull(header.getWeight());
    } else {
      assertNull(header.getSize());
      assertNull(header.getDepth());
      assertNull(header.getDifficulty());
      assertNull(header.getCumulativeDifficulty());
      assertNull(header.getId());
      assertNull(header.getMinerTxId());
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
    assertTrue(minerTx.getUnlockTime() >= 0);

//    // TODO: miner tx does not have ids in binary requests so this will fail, need to derive using prunable data
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
    assertEquals(64, tx.getId().length());
    if (tx.isRelayed() == null) assertTrue(tx.inTxPool());  // TODO monero-daemon-rpc: add relayed to get_transactions
    else assertNotNull(tx.isRelayed());
    assertNotNull(tx.isConfirmed());
    assertNotNull(tx.inTxPool());
    assertNotNull(tx.isMinerTx());
    assertNotNull(tx.isDoubleSpendSeen());
    assertTrue(tx.getVersion() >= 0);
    assertTrue(tx.getUnlockTime() >= 0);
    assertNotNull(tx.getVins());
    assertNotNull(tx.getVouts());
    assertTrue(tx.getExtra().length > 0);
    
    // test presence of output indices
    // TODO: change this over to vouts only
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
      assertEquals(true, tx.isRelayed());
      assertEquals(false, tx.isFailed());
      assertEquals(false, tx.inTxPool());
      assertEquals(false, tx.getDoNotRelay());
      assertEquals(false, tx.isDoubleSpendSeen());
      assertEquals(null, tx.getNumConfirmations()); // client must compute
    } else {
      assertEquals(null, tx.getBlock());
      assertEquals(0, (long) tx.getNumConfirmations());
    }
    
    // test in tx pool
    if (tx.inTxPool()) {
      assertEquals(tx.isConfirmed(), false);
      assertEquals(tx.isDoubleSpendSeen(), false);
      assertEquals(tx.getLastFailedHeight(), null);
      assertEquals(tx.getLastFailedId(), null);
      assertTrue(tx.getReceivedTimestamp() > 0);
      assertTrue(tx.getSize() > 0);
      assertTrue(tx.getWeight() > 0);
      assertNotNull(tx.isKeptByBlock());
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedId());
      assertTrue(tx.getMaxUsedBlockHeight() >= 0);
      assertNotNull(tx.getMaxUsedBlockId());
    } else {
      assertEquals(tx.getLastRelayedTimestamp(), null);
    }
    
    // test miner tx
    if (tx.isMinerTx()) {
      assertEquals(0, tx.getFee().equals(BigInteger.valueOf(0)));
      assertEquals(null, tx.getVins());
      assertNull(tx.getSignatures());
    } else {
      if (tx.getSignatures() != null) assertFalse(tx.getSignatures().isEmpty());
    }
    
    // test failed  // TODO: what else to test associated with failed
    if (tx.isFailed()) {
      assertTrue(tx.getReceivedTimestamp() > 0);
    } else {
      if (tx.isRelayed() == null) assertEquals(null, tx.getDoNotRelay()); // TODO monero-daemon-rpc: add relayed to get_transactions
      else if (tx.isRelayed()) assertEquals(false, tx.isDoubleSpendSeen());
      else {
        assertEquals(false, tx.isRelayed());
        assertEquals(true, tx.getDoNotRelay());
        assertNotNull(tx.isDoubleSpendSeen());
      }
    }
    assertNull(tx.getLastFailedHeight());
    assertNull(tx.getLastFailedId());
    
    // received time only for tx pool or failed txs
    if (tx.getReceivedTimestamp() != null) {
      assertTrue(tx.inTxPool() || tx.isFailed());
    }
    
    // test relayed tx
    // this is not strictly correct because a tx can be submitted then relayed
//    if (tx.isRelayed()) assertEquals(false, tx.getDoNotRelay());
//    if (tx.getDoNotRelay()) {
//      assertTrue(!tx.isRelayed());
//      assertTrue(!tx.isConfirmed());
//    }
    
    // test vins and vouts
    if (!tx.isMinerTx()) assertFalse(tx.getVins().isEmpty());
    for (MoneroOutput vin : tx.getVins()) {
      assertTrue(tx == vin.getTx());
      testVin(vin, ctx);
    }
    assertFalse(tx.getVouts().isEmpty());
    for (MoneroOutput vout : tx.getVouts()) {
      assert(tx == vout.getTx());
      testVout(vout, ctx);
    }
    
    // test pruned vs not pruned
    if (ctx.fromGetTxPool || Boolean.TRUE.equals(ctx.fromBinaryBlock)) assertNull(tx.getPrunableHash());   // TODO monero-daemon-rpc: tx pool txs do not have prunable hash, TODO: getBlocksByHeight() has inconsistent client-side pruning
    else assertNotNull(tx.getPrunableHash());
    if (ctx.isPruned) {
      assertNull(tx.getRctSigPrunable());
      assertNull(tx.getSize());
      assertNull(tx.getLastRelayedTimestamp());
      assertNull(tx.getReceivedTimestamp());
      assertNull(tx.getFullHex());
      assertNotNull(tx.getPrunedHex());
    } else {
      assertNull(tx.getPrunedHex());
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
  
  private static void testVin(MoneroOutput vin, TestContext ctx) {
    testOutput(vin);
    testKeyImage(vin.getKeyImage(), ctx);
    assertFalse(vin.getRingOutputIndices().isEmpty());
  }

  private static void testKeyImage(MoneroKeyImage image, TestContext ctx) {
    assertFalse(image.getHex().isEmpty());
    if (image.getSignature() != null) {
      assertNotNull(image.getSignature());
      assertFalse(image.getSignature().isEmpty());
    }
  }

  private static void testVout(MoneroOutput vout, TestContext ctx) {
    testOutput(vout);
    if (vout.getTx().inTxPool() || Boolean.FALSE.equals(ctx.hasOutputIndices)) assertEquals(null, vout.getIndex());
    else assertTrue(vout.getIndex() >= 0);
    assertEquals(64, vout.getStealthPublicKey().length());
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
    
    // test different vin references
    if (copy.getVins() == null) assertEquals(tx.getVins(), null);
    else {
      assertFalse(copy.getVins() == tx.getVins());
      for (int i = 0; i < copy.getVins().size(); i++) {
        assertEquals(0, tx.getVins().get(i).getAmount().compareTo(copy.getVins().get(i).getAmount()));
      }
    }
    
    // test different vout references
    if (copy.getVouts() == null) assertEquals(null, tx.getVouts());
    else {
      assertTrue(copy.getVouts() != tx.getVouts());
      for (int i = 0; i < copy.getVouts().size(); i++) {
        assertEquals(0, tx.getVouts().get(i).getAmount().compareTo(copy.getVouts().get(i).getAmount()));
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
  
  private static List<String> getConfirmedTxIds(MoneroDaemon daemon) {
    
    // get valid height range
    long height = daemon.getHeight();
    int numBlocks = 200;
    int numBlocksAgo = 200;
    assertTrue(numBlocks > 0);
    assertTrue(numBlocksAgo >= numBlocks);
    assertTrue(height - numBlocksAgo + numBlocks - 1 < height);
    long startHeight = height - numBlocksAgo;
    long endHeight = height - numBlocksAgo + numBlocks - 1;
    
    // get blocks
    List<MoneroBlock> blocks = daemon.getBlocksByRange(startHeight, endHeight);
    
    // collect tx ids
    List<String> txIds = new ArrayList<String>();
    for (MoneroBlock block : blocks) txIds.addAll(block.getTxIds());
    assertFalse("No transactions found in the range [" + startHeight + ", " + endHeight + "]", txIds.isEmpty());  // TODO: this fails if no txs in last 200 blocks
    return txIds;
  }
  
  private static MoneroTx getUnrelayedTx(MoneroWallet wallet, Integer accountIdx) {
    assertTrue("Txs sent from/to same account are not properly synced from the pool", accountIdx > 0);
    MoneroSendRequest req = new MoneroSendRequest(accountIdx, wallet.getPrimaryAddress(), TestUtils.MAX_FEE); 
    req.setDoNotRelay(true);
    MoneroTx tx = wallet.send(req).getTxs().get(0);
    assertFalse(tx.getFullHex().isEmpty());
    assertEquals(tx.getDoNotRelay(), true);
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
        assertNotNull(stats.getHisto());
        fail("Ready to test histogram");
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
    assertTrue(info.getTarget() > 0);
    assertTrue(info.getTargetHeight() >= 0);
    assertFalse(info.getTopBlockId().isEmpty());
    assertTrue(info.getNumTxs() >= 0);
    assertTrue(info.getNumTxsPool() >= 0);
    assertNotNull(info.getWasBootstrapEverUsed());
    assertTrue(info.getBlockWeightLimit() > 0);
    assertTrue(info.getBlockWeightMedian() > 0);
    assertTrue(info.getDatabaseSize() > 0);
    assertNotNull(info.getUpdateAvailable());
  }

  private static void testSyncInfo(MoneroDaemonSyncInfo syncInfo) { // TODO: consistent naming, daemon in name?
    assertTrue(syncInfo instanceof MoneroDaemonSyncInfo);
    assertTrue(syncInfo.getHeight() >= 0);
    if (syncInfo.getConnections() != null) {
      assertTrue(syncInfo.getConnections().size() > 0);
      for (MoneroDaemonConnection connection : syncInfo.getConnections()) {
        testDaemonConnection(connection);
      }
    }
    if (syncInfo.getSpans() != null) {  // TODO: test that this is being hit, so far not used
      assertTrue(syncInfo.getSpans().size() > 0);
      for (MoneroDaemonConnectionSpan span : syncInfo.getSpans()) {
        testDaemonConnectionSpan(span);
      }
    }
    assertTrue(syncInfo.getNextNeededPruningSeed() >= 0);
    assertNull(syncInfo.getOverview());
  }
  
  private static void testDaemonConnectionSpan(MoneroDaemonConnectionSpan span) {
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
  }

  private static void testMoneroBan(MoneroBan ban) {
    assertNotNull(ban.getHost());
    assertNotNull(ban.getIp());
    assertNotNull(ban.getSeconds());
  }
  
  private static void testAltChain(MoneroAltChain altChain) {
    assertNotNull(altChain);
    assertFalse(altChain.getBlockIds().isEmpty());
    TestUtils.testUnsignedBigInteger(altChain.getDifficulty(), true);
    assertTrue(altChain.getHeight() > 0);
    assertTrue(altChain.getLength() > 0);
    assertEquals(64, altChain.getMainChainParentBlockId().length());
  }

  private static void testDaemonConnection(MoneroDaemonConnection connection) {
    assertTrue(connection instanceof MoneroDaemonConnection);
    testKnownPeer(connection.getPeer(), true);
    assertFalse(connection.getId().isEmpty());
    assertTrue(connection.getAvgDownload() >= 0);
    assertTrue(connection.getAvgUpload() >= 0);
    assertTrue(connection.getCurrentDownload() >= 0);
    assertTrue(connection.getCurrentUpload() >= 0);
    assertTrue(connection.getHeight() >= 0);
    assertTrue(connection.getLiveTime() >= 0);
    assertNotNull(connection.isLocalIp());
    assertNotNull(connection.isLocalHost());
    assertTrue(connection.getNumReceives() >= 0);
    assertTrue(connection.getReceiveIdleTime() >= 0);
    assertTrue(connection.getNumSends() >= 0);
    assertTrue(connection.getSendIdleTime() >= 0);
    assertNotNull(connection.getState());
    assertTrue(connection.getNumSupportFlags() >= 0); 
  }

  private static void testKnownPeer(MoneroDaemonPeer peer, boolean fromConnection) {
    assertNotNull(peer);
    assertFalse(peer.getId().isEmpty());
    assertFalse(peer.getHost().isEmpty());
    assertTrue(peer.getPort() > 0);
    assertTrue(peer.getRpcPort() >= 0);
    assertNotNull(peer.isOnline());
    if (fromConnection) assertNull(peer.getLastSeenTimestamp());
    else {
      if (peer.getLastSeenTimestamp() < 0) System.out.println("Last seen timestamp is invalid: " + peer.getLastSeenTimestamp());
      assertTrue(peer.getLastSeenTimestamp() >= 0);
    }
    assertTrue(peer.getPruningSeed() >= 0);
  }

  private static void testUpdateCheckResult(MoneroDaemonUpdateCheckResult result) {
    assertTrue(result instanceof MoneroDaemonUpdateCheckResult);
    assertNotNull(result.isUpdateAvailable());
    if (result.isUpdateAvailable()) {
      assertFalse("No auto uri; is daemon online?", result.getAutoUri().isEmpty());
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
      assertEquals(false, result.isDoubleSpend());
      assertEquals(false, result.isFeeTooLow());
      assertEquals(false, result.isMixinTooLow());
      assertEquals(false, result.hasInvalidInput());
      assertEquals(false, result.hasInvalidOutput());
      assertEquals(true, result.isRct());
      assertEquals(false, result.isOverspend());
      assertEquals(false, result.isTooBig());
      assertEquals(false, result.getSanityCheckFailed());
      assertEquals(true, result.isGood());
    } catch (Exception e) {
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
    assertEquals(true, result.isRct());
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
    assertNotNull(result.isRct());
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
