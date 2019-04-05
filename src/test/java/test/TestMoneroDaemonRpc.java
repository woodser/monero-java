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
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockListener;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroCoinbaseTxSum;
import monero.daemon.model.MoneroDaemonConnection;
import monero.daemon.model.MoneroDaemonConnectionSpan;
import monero.daemon.model.MoneroDaemonInfo;
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
import monero.daemon.model.MoneroSubmitTxResult;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxPoolStats;
import monero.rpc.MoneroRpcException;
import monero.utils.MoneroException;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletLocal;
import monero.wallet.config.MoneroSendConfig;
import utils.TestUtils;

/**
 * Tests a Monero daemon.
 */
public class TestMoneroDaemonRpc {
  
  // classes to test
  private static MoneroDaemon daemon;
  private static MoneroWallet wallet;
  
  // test configuration
  private static boolean TEST_NON_RELAYS = true;
  private static boolean TEST_RELAYS = true; // creates and relays outgoing txs
  private static boolean TEST_NOTIFICATIONS = true;
  
  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daemon = TestUtils.getDaemonRpc();
    wallet = TestUtils.getWalletRpc();
  }
  
  @Before
  public void before() {
    
  }
  
  // -------------------------------- NON RELAYS ------------------------------
  
  @Test
  public void testIsTrusted() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.getIsTrusted();
  }
  
  @Test
  public void testGetHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int height = daemon.getHeight();
    assertTrue("Height must be greater than 0", height > 0);
  }
  
  @Test
  public void testGetBlockIdByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    assertNotNull(id);
    assertEquals(64, id.length());
  }
  
  @Test
  public void testGetBlockTemplate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.TEST_ADDRESS, 2);
    testBlockTemplate(template);
  }
  
  @Test
  public void testGetLastBlockHeader() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    testBlockHeader(lastHeader, true);
  }
  
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
    assertEquals(lastHeader.getHeight() - 1, (int) header.getHeight());
  }
  
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
    assertEquals(lastHeader.getHeight() - 1, (int) header.getHeight());
  }
  
  // TODO: test start with no end, vice versa, inclusivity
  @Test
  public void testGetBlockHeadersByRange() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // determine start and end height based on number of blocks and how many blocks ago
    int numBlocks = 100;
    int numBlocksAgo = 100;
    int currentHeight = daemon.getHeight();
    int startHeight = currentHeight - numBlocksAgo;
    int endHeight = currentHeight - (numBlocksAgo - numBlocks) - 1;
    
    // fetch headers
    List<MoneroBlockHeader> headers = daemon.getBlockHeadersByRange(startHeight, endHeight);
    
    // test headers
    assertEquals(numBlocks, headers.size());
    for (int i = 0; i < numBlocks; i++) {
      MoneroBlockHeader header = headers.get(i);
      assertEquals(startHeight + i, (int) header.getHeight());
      testBlockHeader(header, true);
    }
  }
  
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
    assertEquals(lastHeader.getHeight() - 1, (int) block.getHeight());
  }
  
  @Test
  public void testGetBlocksByHeightBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set number of blocks to test
    int numBlocks = 200;
    
    // select random heights  // TODO: this is horribly inefficient way of computing last 100 blocks if not shuffling
    int currentHeight = daemon.getHeight();
    List<Integer> allHeights = new ArrayList<Integer>();
    for (int i = 0; i < currentHeight - 1; i++) allHeights.add(i);
    //GenUtils.shuffle(allHeights);
    List<Integer> heights = new ArrayList<Integer>();
    for (int i = allHeights.size() - numBlocks; i < allHeights.size(); i++) heights.add(allHeights.get(i));
    
    // fetch blocks
    List<MoneroBlock> blocks = daemon.getBlocksByHeight(heights);
    
    // config for testing blocks
    // TODO: getBlocksByHeight() has inconsistent client-side pruning
    // TODO: get_blocks_by_height.bin does not return output indices (#5127)
    TestContext ctx  = new TestContext();
    ctx.hasHex = false;
    ctx.headerIsFull = false;
    ctx.hasTxs = true;
    ctx.txContext = new TestContext();
    ctx.txContext.isPruned = false;
    ctx.txContext.isConfirmed = true;
    ctx.txContext.fromGetTxPool = false;
    ctx.txContext.hasOutputIndices = false;
    ctx.txContext.fromGetBlocksByHeight = true;
    
    // test blocks
    boolean txFound = false;
    assertEquals(numBlocks, blocks.size());
    for (int i = 0; i < heights.size(); i++) {
      MoneroBlock block = blocks.get(i);
      if (!block.getTxs().isEmpty()) txFound = true;
      testBlock(block, ctx);
      assertEquals(block.getHeight(), heights.get(i));      
    }
    assertTrue("No transactions found to test", txFound);
  }
  
  @Test
  public void testGetBlocksByIdBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented");
  }
  
  @Test
  public void testGetBlocksByRange() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get current height
    int height = daemon.getHeight();
    
    // get valid height range
    int numBlocks = 1; // TODO: RequestError: Error: read ECONNRESET or  RequestError: Error: socket hang up if > 64 or (or > 1 if test getBlocksByHeight() runs first)
    int numBlocksAgo = 190;
    assertTrue(numBlocks > 0);
    assertTrue(numBlocksAgo >= numBlocks);
    assertTrue(height - numBlocksAgo + numBlocks - 1 < height);
    int startHeight = height - numBlocksAgo;
    int endHeight = height - numBlocksAgo + numBlocks - 1;
    
    // test known start and end heights
    //console.log("Height: " + height);
    //console.log("Fecthing " + (endHeight - startHeight + 1) + " blocks [" + startHeight + ", " + endHeight + "]");
    testRange(startHeight, endHeight, height);
    
    // test unspecified start
    testRange(null, numBlocks - 1, height);
    
    // test unspecified end
    testRange(height - numBlocks - 1, null, height);
    
    // test unspecified start and end 
    //testRange(null, null, height);  // TODO: RequestError: Error: socket hang up
  };
  
  @Test
  public void testGetBlockIdsBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    //get_hashes.bin
    throw new RuntimeException("Not implemented");
  }
  
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
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
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
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  @Test
  public void testGetTxsByIdsInPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
      txIds.add(tx.getId());
    }
    
    // fetch txs by id
    List<MoneroTx> txs = daemon.getTxs(txIds);
    
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
  }
  
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
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
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
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  @Test
  public void testGetCoinbaseTxSum() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroCoinbaseTxSum sum = daemon.getCoinbaseTxSum(0, 50000);
    testCoinbaseTxSum(sum);
  }
  
  @Test
  public void testGetFeeEstimate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    BigInteger fee = daemon.getFeeEstimate();
    TestUtils.testUnsignedBigInteger(fee, true);
  }
  
  @Test
  public void testGetTxsInPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit tx to pool but don't relay
    MoneroTx tx = getUnrelayedTx(wallet, null);
    daemon.submitTxHex(tx.getFullHex(), true);
    
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
  }
  
  @Test
  public void testGetIdsOfTransactionsInPoolBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    // TODO: get_transaction_pool_hashes.bin
    throw new Error("Not implemented");
  }
  
  @Test
  public void testGetTxPoolBacklogBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    // TODO: get_txpool_backlog
    throw new Error("Not implemented");
  }
  
  @Test
  public void testGetTxPoolStatisticsBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay (multiple txs result in binary `histo` field)
    for (int i = 0; i < 2; i++) {
      
      // submit tx hex
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
      
      // test stats
      MoneroTxPoolStats stats = daemon.getTxPoolStats();
      assertTrue(stats.getNumTxs() > i);
      testTxPoolStats(stats);
    }
  }
  
  @Test
  public void testFlushTxsFromPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // pool starts flushed for each test
    List<MoneroTx> txs = daemon.getTxPool();
    assertEquals(0, txs.size());
    
    // submit txs to the pool but don't relay
    for (int i = 0; i < 2; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
    }
    
    // txs are in pool
    txs = daemon.getTxPool();
    assertTrue(txs.size() >= 2);
    
    // flush tx pool
    daemon.flushTxPool();
    txs = daemon.getTxPool();
    assertTrue(txs.isEmpty());
  }
  
  @Test
  public void testFlushTxFromPoolById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
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
  }
  
  @Test
  public void testFlushTxsFromPoolByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getFullHex(), true);
      txIds.add(tx.getId());
    }
    
    // remove all txs by ids
    daemon.flushTxPool(txIds);
    
    // test tx pool
    List<MoneroTx> txs = daemon.getTxPool();
    assertEquals(0, txs.size());
  }
  
  @Test
  public void testGetSpentStatusOfKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool to collect key images then flush
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 0; i < 3; i++) {
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
  }
  
  @Test
  public void testGetOutputIndicesFromTxIdsBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new Error("Not implemented"); // get_o_indexes.bin
  }
  
  @Test
  public void testGetOutputsFromAmountsAndIndicesBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new Error("Not implemented"); // get_outs.bin
  }
  
  @Test
  public void testGetOutputHistogramBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new RuntimeException("Not implemented");
//    List<MoneroOutputHistogramEntry> entries = daemon.getOutputHistogram();
//    assertFalse(entries.isEmpty());
//    for (MoneroOutputHistogramEntry entry : entries) {
//      testOutputHistogramEntry(entry);
//    }
  }
  
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
  
  @Test
  public void testGetGeneralInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonInfo info = daemon.getInfo();
    testInfo(info);
  }
  
  @Test
  public void testGetSyncInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonSyncInfo syncInfo = daemon.getSyncInfo();
    testSyncInfo(syncInfo);
  }
  
  @Test
  public void testGetHardForkInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroHardForkInfo hardForkInfo = daemon.getHardForkInfo();
    testHardForkInfo(hardForkInfo);
  }
  
  @Test
  public void testGetAlternativeChains() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAltChain> altChains = daemon.getAltChains();
    for (MoneroAltChain altChain : altChains) {
      testAltChain(altChain);
    }
  }
  
  @Test
  public void testGetAlternativeBlockIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<String> altBlockIds = daemon.getAltBlockIds();
    for (String altBlockId : altBlockIds) {
      assertNotNull(altBlockId);
      assertEquals(64, altBlockId.length());  // TODO: common validation
    }
  }
  
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
  
  @Test
  public void testGetKnownPeers() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroDaemonPeer> peers = daemon.getKnownPeers();
    assertFalse("Daemon has no known peers to test", peers.isEmpty());
    for (MoneroDaemonPeer peer : peers) {
      testKnownPeer(peer, false);
    }
  }
  
  @Test
  public void testGetPeerConnections() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroDaemonConnection> connections = daemon.getConnections();
    assertFalse("Daemon has no incoming or outgoing connections to test", connections.isEmpty());
    for (MoneroDaemonConnection connection : connections) {
      testDaemonConnection(connection);
    }
  }
  
  @Test
  public void testSetOutgoingPeerLimit() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.setOutgoingPeerLimit(0);
    daemon.setOutgoingPeerLimit(8);
    daemon.setOutgoingPeerLimit(10);
  }
  
  @Test
  public void testSetIncomingPeerLimit() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.setIncomingPeerLimit(0);
    daemon.setIncomingPeerLimit(8);
    daemon.setIncomingPeerLimit(10);
  }
  
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
  
  @Test
  public void testMining() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // stop mining at beginning of test
    try { daemon.stopMining(); }
    catch (MoneroException e) { }
    
    // generate address to mine to
    MoneroWallet wallet = new MoneroWalletLocal(daemon);
    String address = wallet.getPrimaryAddress();
    
    // start mining
    daemon.startMining(address, 2, false, true);
    
    // stop mining
    daemon.stopMining();
  }
  
  @Test
  public void testGetMiningStatus() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    try {
      
      // stop mining at beginning of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
      
      // test status without mining
      MoneroMiningStatus status = daemon.getMiningStatus();
      assertEquals(false, status.getIsActive());
      assertNull(status.getAddress());
      assertEquals(0, (int) status.getSpeed());
      assertEquals(0, (int) status.getNumThreads());
      assertNull(status.getIsBackground());
      
      // test status with mining
      MoneroWallet wallet = new MoneroWalletLocal(daemon);
      String address = wallet.getPrimaryAddress();
      int threadCount = 3;
      boolean isBackground = false;
      daemon.startMining(address, threadCount, isBackground, true);
      status = daemon.getMiningStatus();
      assertEquals(true, status.getIsActive());
      assertEquals(address, status.getAddress());
      assertTrue(status.getSpeed() >= 0);
      assertEquals(threadCount, (int) status.getNumThreads());
      assertEquals(isBackground, status.getIsBackground());
    } catch (MoneroException e) {
      throw e;
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
    }
  }
  
  @Test
  public void testSubmitMinedBlock() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get template to mine on
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.TEST_ADDRESS);
    
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
  
  @Test
  public void testCheckForUpdate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonUpdateCheckResult result = daemon.checkForUpdate();
    testUpdateCheckResult(result);
  }
  
  @Test
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
    if (result.getIsUpdateAvailable()) {
      try {
        result = daemon.downloadUpdate("./ohhai/there");
        fail("Should have thrown error");
      } catch (MoneroRpcException e) {
        assertNotEquals("Should have thrown error", e.getMessage());
        assertEquals(500, (int) e.getCode());  // TODO monero-daemon-rpc: this causes a 500 in daemon rpc
      }
    }
  }
  
  // test is disabled to not interfere with other tests
  @Test
  public void testStop() throws InterruptedException {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // stop the daemon
    daemon.stop();
    
    // give the daemon 10 seconds to shut down
    TimeUnit.SECONDS.sleep(10);
    
    // try to interact with the daemon
    try {
      daemon.getHeight();
      throw new Error("Should have thrown error");
    } catch (MoneroException e) {
      assertNotEquals("Should have thrown error", e.getMessage());
    }
  }
  
  // ----------------------------- RELAY TESTS -------------------------------
  
  @Test
  public void testSubmitAndRelayTxHex() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    
    // create 2 txs, the second will double spend outputs of first
    MoneroTx tx1 = getUnrelayedTx(wallet, 0);
    MoneroTx tx2 = getUnrelayedTx(wallet, 0);
    
    // submit and relay tx1
    MoneroSubmitTxResult result = daemon.submitTxHex(tx1.getFullHex());
    assertEquals(result.getIsRelayed(), true);
    testSubmitTxResultGood(result);
    
    // tx1 is in the pool
    List<MoneroTx> txs = daemon.getTxPool();
    boolean found = false;
    for (MoneroTx aTx : txs) {
      if (aTx.getId().equals(tx1.getId())) {
        assertEquals(aTx.getIsRelayed(), true);
        found = true;
        break;
      }
    }
    assertTrue("Tx1 was not found after being submitted to the daemon's tx pool", found);
    
    // submit and relay tx2 hex which double spends tx1
    result = daemon.submitTxHex(tx2.getFullHex());
    assertEquals(result.getIsRelayed(), true);
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
  }
  
  @Test
  public void testSubmitThenRelayTxHex() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
    MoneroTx tx = getUnrelayedTx(wallet, 1);
    testSubmitThenRelay(Arrays.asList(tx));
  }
  
  @Test
  public void testSubmitThenRelayTxHexes() {
    org.junit.Assume.assumeTrue(TEST_RELAYS);
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
      assertEquals(result.getIsRelayed(), false);
      testSubmitTxResultGood(result);
      
      // ensure tx is in pool
      List<MoneroTx> poolTxs = daemon.getTxPool();
      boolean found = false;
      for (MoneroTx aTx : poolTxs) {
        if (aTx.getId().equals(tx.getId())) {
          assertEquals(aTx.getIsRelayed(), false);
          found = true;
          break;
        }
      }
      assertTrue("Tx was not found after being submitted to the daemon's tx pool", found);
      
      // fetch tx by id and ensure not relayed
      MoneroTx fetchedTx = daemon.getTx(tx.getId());
      assertNull(fetchedTx.getIsRelayed()); // TODO monero-daemon-rpc: add relayed to get_transactions
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
          assertEquals(aTx.getIsRelayed(), true);
          found = true;
          break;
        }
      }
      assertTrue("Tx was not found after being submitted to the daemon's tx pool", found);
    }
  }
  
  // -------------------------- NOTIFICATION TESTS ---------------------------
  
  @Test
  public void testBlockListener() {
    org.junit.Assume.assumeTrue(TEST_NOTIFICATIONS);
    
    try {
      
      // start mining if possible to help push the network along
      MoneroWallet wallet = new MoneroWalletLocal(daemon);
      String address = wallet.getPrimaryAddress();
      try { daemon.startMining(address, 8, false, true); }
      catch (MoneroException e) { }
      
      // register a listener
      MoneroBlockListener listener = new MoneroBlockListener();
      daemon.addBlockListener(listener);
      
      // wait for next block notification
      MoneroBlockHeader header = daemon.getNextBlockHeader();
      testBlockHeader(header, true);
      
      // test that listener was called with equivalent header
      assertEquals(header, listener.getLastBlockHeader());
      
      // unregister listener so daemon does not keep polling
      daemon.removeBlockListener(listener);
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
    Boolean isCoinbase;
    Boolean fromGetTxPool;
    Boolean fromGetBlocksByHeight;
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
      this.isCoinbase = ctx.isCoinbase;
      this.fromGetTxPool = ctx.fromGetTxPool;
      this.fromGetBlocksByHeight = ctx.fromGetBlocksByHeight;
      this.hasOutputIndices = ctx.hasOutputIndices;
      this.doNotTestCopy = ctx.doNotTestCopy;
      this.hasTxs = ctx.hasTxs;
      this.hasHex = ctx.hasHex;
      this.headerIsFull = ctx.headerIsFull;
    }
  }
  
//  public static class MoneroBlockTestConfig {
//    boolean hasTxs;
//    boolean hasHex;
//    boolean headerIsFull;
//    MoneroTxCtx txConfig;
//    public MoneroBlockTestConfig(boolean hasTxs, boolean hasHex, boolean headerIsFull, MoneroTxCtx txConfig) {
//      super();
//      this.hasTxs = hasTxs;
//      this.hasHex = hasHex;
//      this.headerIsFull = headerIsFull;
//      this.txConfig = txConfig;
//    }
//  }
//  
//  public static class MoneroTxCtx {
//    boolean hasJson;
//    boolean isPruned;
//    boolean isFull;
//    boolean isConfirmed;
//    boolean isCoinbase;
//    boolean fromGetTxPool;
//    boolean fromGetBlocksByHeight;
//    boolean hasOutputIndices;
//    boolean doNotTestCopy;
//  }
//  
//  public static class MoneroOuptutCtx {
//    boolean hasJson;
//    boolean isPruned;
//    boolean isFull;
//    boolean isConfirmed;
//    boolean isCoinbase;
//    boolean fromGetTxPool;
//    boolean fromGetBlocksByHeight;
//    boolean hasOutputIndices;
//    boolean doNotTestCopy;
//  }
  
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
    testCoinbaseTx(block.getCoinbaseTx());  // TODO: coinbase tx doesn't have as much stuff, can't call testTx?
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
  
  private static void testCoinbaseTx(MoneroTx coinbaseTx) {
    assertNotNull(coinbaseTx);
    assertNotNull(coinbaseTx.getIsCoinbase());
    assertTrue(coinbaseTx.getVersion() >= 0);
    assertNotNull(coinbaseTx.getExtra());
    assertTrue(coinbaseTx.getExtra().length > 0);
    assertTrue(coinbaseTx.getUnlockTime() >= 0);

//    // TODO: coinbase tx does not have ids in binary requests so this will fail, need to derive using prunable data
//    TestContext ctx = new TestContext();
//    ctx.hasJson = false;
//    ctx.isPruned = true;
//    ctx.isFull = false;
//    ctx.isConfirmed = true;
//    ctx.isCoinbase = true;
//    ctx.fromGetTxPool = true;
//    testTx(coinbaseTx, ctx);
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
    if (tx.getIsRelayed() == null) assertTrue(tx.getInTxPool());  // TODO monero-daemon-rpc: add relayed to get_transactions
    else assertNotNull(tx.getIsRelayed());
    assertNull(tx.getSignatures());
    assertNotNull(tx.getIsConfirmed());
    assertNotNull(tx.getInTxPool());
    assertNotNull(tx.getIsCoinbase());
    assertNotNull(tx.getIsDoubleSpend());
    
    // test presence of output indices
    // TODO: change this over to vouts only
    if (tx.getIsCoinbase()) assertEquals(tx.getOutputIndices(), null); // TODO: how to get output indices for coinbase transactions?
    if (tx.getInTxPool() || ctx.fromGetTxPool || Boolean.FALSE.equals(ctx.hasOutputIndices)) assertEquals(null, tx.getOutputIndices());
    else assertFalse(tx.getOutputIndices().isEmpty());
    
    // test confirmed ctx
    if (ctx.isConfirmed == true) assertEquals(true, tx.getIsConfirmed());
    if (ctx.isConfirmed == false) assertEquals(false, tx.getIsConfirmed());
    
    // test confirmed
    if (tx.getIsConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeight() > 0);
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeight() > 0);
      assertTrue(tx.getBlock().getTimestamp() > 0);
      assertEquals(true, tx.getIsRelayed());
      assertEquals(false, tx.getIsFailed());
      assertEquals(false, tx.getInTxPool());
      assertEquals(false, tx.getDoNotRelay());
      assertEquals(false, tx.getIsDoubleSpend());
      assertEquals(null, tx.getNumConfirmations()); // client must compute
    } else {
      assertEquals(null, tx.getBlock());
      assertEquals(0, (int) tx.getNumConfirmations());
    }
    
    // test in tx pool
    if (tx.getInTxPool()) {
      assertEquals(tx.getIsConfirmed(), false);
      assertEquals(tx.getIsDoubleSpend(), false);
      assertEquals(tx.getLastFailedHeight(), null);
      assertEquals(tx.getLastFailedId(), null);
      assertTrue(tx.getReceivedTimestamp() > 0);
      if (tx.getIsRelayed()) assertTrue(tx.getNumEstimatedBlocksUntilConfirmed() > 0);
      else assertNull(tx.getNumEstimatedBlocksUntilConfirmed());
    } else {
      assertEquals(tx.getNumEstimatedBlocksUntilConfirmed(), null);
      assertEquals(tx.getLastRelayedTimestamp(), null);
    }
    
    // test coinbase tx
    if (tx.getIsCoinbase()) {
      assertEquals(0, tx.getFee().equals(BigInteger.valueOf(0)));
      assertEquals(null, tx.getVins());
    }
    
    // test failed  // TODO: what else to test associated with failed
    if (tx.getIsFailed()) {
      assertTrue(tx.getReceivedTimestamp() > 0);
    } else {
      if (tx.getIsRelayed() == null) assertEquals(null, tx.getDoNotRelay()); // TODO monero-daemon-rpc: add relayed to get_transactions
      else if (tx.getIsRelayed()) assertEquals(false, tx.getIsDoubleSpend());
      else {
        assertEquals(false, tx.getIsRelayed());
        assertEquals(true, tx.getDoNotRelay());
        assertNotNull(tx.getIsDoubleSpend());
      }
    }
    assertEquals(tx.getLastFailedHeight(), null);
    assertEquals(tx.getLastFailedId(), null);
    
    // received time only for tx pool or failed txs
    if (tx.getReceivedTimestamp() != null) {
      assertTrue(tx.getInTxPool() || tx.getIsFailed());
    }
    
    // test relayed tx
    if (tx.getIsRelayed()) assertEquals(tx.getDoNotRelay(), false);
    if (tx.getDoNotRelay()) {
      assertTrue(!tx.getIsRelayed());
      assertTrue(!tx.getIsConfirmed());
    }
    
    // test pruned vs not pruned
    if (ctx.fromGetTxPool || Boolean.TRUE.equals(ctx.fromGetBlocksByHeight)) assertNull(tx.getPrunableHash());   // TODO monero-daemon-rpc: tx pool txs do not have prunable hash, TODO: getBlocksByHeight() has inconsistent client-side pruning
    else assertNotNull(tx.getPrunableHash());
    if (ctx.isPruned) {
      assertNull(tx.getRctSigPrunable());
      assertNull(tx.getSize());
      assertNull(tx.getLastRelayedTimestamp());
      assertNull(tx.getReceivedTimestamp());
      assertNull(tx.getVersion());
      assertNull(tx.getUnlockTime());
      assertNull(tx.getVins());
      assertNull(tx.getVouts());
      assertNull(tx.getExtra());
      assertNull(tx.getFullHex());
      assertNotNull(tx.getPrunedHex());
    } else {
      assertNull(tx.getPrunedHex());
      assertTrue(tx.getVersion() >= 0);
      assertTrue(tx.getUnlockTime() >= 0);
      assertTrue(tx.getExtra().length > 0);
      if (Boolean.TRUE.equals(ctx.fromGetBlocksByHeight)) assertNull(tx.getFullHex());         // TODO: getBlocksByHeight() has inconsistent client-side pruning
      else assertFalse(tx.getFullHex().isEmpty());
      if (Boolean.TRUE.equals(ctx.fromGetBlocksByHeight)) assertNull(tx.getRctSigPrunable());  // TODO: getBlocksByHeight() has inconsistent client-side pruning
      else assertNotNull(tx.getRctSigPrunable()); // TODO: define and test this
      assertFalse(tx.getIsDoubleSpend());
      if (tx.getIsConfirmed()) {
        assertNull(tx.getLastRelayedTimestamp());
        assertNull(tx.getReceivedTimestamp());
      } else {
        if (tx.getIsRelayed()) assert(tx.getLastRelayedTimestamp() > 0);
        else assertNull(tx.getLastRelayedTimestamp());
        assert(tx.getReceivedTimestamp() > 0);
      }
      
      // test vins and vouts
      assertNotNull(tx.getVins());
      assertNotNull(tx.getVouts());
      if (!tx.getIsCoinbase()) assertFalse(tx.getVins().isEmpty());
      for (MoneroOutput vin : tx.getVins()) {
        assertTrue(tx == vin.getTx());
        testVin(vin, ctx);
      }
      assertFalse(tx.getVouts().isEmpty());
      for (MoneroOutput vout : tx.getVouts()) {
        assert(tx == vout.getTx());
        testVout(vout, ctx);
      }
    }
    
    // test fields from tx pool
    if (ctx.fromGetTxPool) {
      assertTrue(tx.getSize() > 0);
      assertTrue(tx.getWeight() > 0);
      assertNotNull(tx.getIsKeptByBlock());
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedId());
      assertTrue(tx.getMaxUsedBlockHeight() >= 0);
      assertNotNull(tx.getMaxUsedBlockId());
    } else {
      assertNull(tx.getWeight());
      assertNull(tx.getIsKeptByBlock());
      assertFalse(tx.getIsFailed());
      assertNull(tx.getLastFailedHeight());
      assertNull(tx.getLastFailedId());
      assertNull(tx.getMaxUsedBlockHeight());
      assertNull(tx.getMaxUsedBlockId());
    }
    
    if (tx.getIsFailed()) {
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
    if (vout.getTx().getInTxPool() || Boolean.FALSE.equals(ctx.hasOutputIndices)) assertEquals(null, vout.getIndex());
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
    copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy)));
    assertEquals(tx.toString(), copy.toString());
    assertTrue(copy != tx);
    
    // test different vin references
    if (copy.getVins() == null) assertEquals(tx.getVins(), null);
    else {
      assertFalse(copy.getVins() == tx.getVins());
      for (int i = 0; i < copy.getVins().size(); i++) {
        if (tx.getVins().get(i).getAmount().equals(copy.getVins().get(i).getAmount())) assertTrue(tx.getVins().get(i).getAmount().equals(BigInteger.valueOf(0)));
      }
    }
    
    // test different vout references
    if (copy.getVouts() == null) assertEquals(null, tx.getVouts());
    else {
      assertTrue(copy.getVouts() != tx.getVouts());
      for (int i = 0; i < copy.getVouts().size(); i++) {
        if (tx.getVouts().get(i).getAmount() == copy.getVouts().get(i).getAmount()) assertTrue(tx.getVouts().get(i).getAmount().equals(BigInteger.valueOf(0)));
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
  
  private static void testRange(Integer startHeight, Integer endHeight, Integer chainHeight) {
    int realStartHeight = startHeight == null ? 0 : startHeight;
    int realEndHeight = endHeight == null ? chainHeight - 1 : endHeight;
    List<MoneroBlock> blocks = daemon.getBlocksByRange(startHeight, endHeight);
    assertEquals(realEndHeight - realStartHeight + 1, blocks.size());
    for (int i = 0; i < blocks.size(); i++) {
      assertEquals(realStartHeight + i, (int) blocks.get(i).getHeight());
    }
  }
  
  private static List<String> getConfirmedTxIds(MoneroDaemon daemon) {
    
    // get valid height range
    int height = daemon.getHeight();
    int numBlocks = 200;
    int numBlocksAgo = 200;
    assertTrue(numBlocks > 0);
    assertTrue(numBlocksAgo >= numBlocks);
    assertTrue(height - numBlocksAgo + numBlocks - 1 < height);
    int startHeight = height - numBlocksAgo;
    int endHeight = height - numBlocksAgo + numBlocks - 1;
    
    // get blocks
    List<MoneroBlock> blocks = daemon.getBlocksByRange(startHeight, endHeight);
    
    // collect tx ids
    List<String> txIds = new ArrayList<String>();
    for (MoneroBlock block : blocks) txIds.addAll(block.getTxIds());
    assertFalse("No transactions found in the range [" + startHeight + ", " + endHeight + "]", txIds.isEmpty());  // TODO: this fails if no txs in last 100 blocks
    return txIds;
  }
  
  private static MoneroTx getUnrelayedTx(MoneroWallet wallet, Integer accountIdx) {
    MoneroSendConfig sendConfig = new MoneroSendConfig(wallet.getPrimaryAddress(), TestUtils.MAX_FEE); 
    sendConfig.setDoNotRelay(true);
    sendConfig.setAccountIndex(accountIdx);
    MoneroTx tx = wallet.send(sendConfig);
    assertFalse(tx.getFullHex().isEmpty());
    assertEquals(tx.getDoNotRelay(), true);
    return tx;
  }
  
  private static void testCoinbaseTxSum(MoneroCoinbaseTxSum txSum) {
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
      assertEquals(0, (int) stats.getBytesTotal());
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
      assertEquals(expectedStatus, daemon.getSpentStatus(keyImage));
    }
    
    // test array of images
    List<MoneroKeyImageSpentStatus> statuses = daemon.getSpentStatuses(keyImages);
    assertEquals(keyImages.size(), statuses.size());
    for (MoneroKeyImageSpentStatus status : statuses) assertEquals(expectedStatus, status);
  }
  
  private static List<MoneroTx> getConfirmedTxs(MoneroDaemon daemon, int numTxs) {
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    int numBlocksPerReq = 50;
    for (int startIdx = daemon.getHeight() - numBlocksPerReq - 1; startIdx >= 0; startIdx -= numBlocksPerReq) {
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
    assertFalse(info.getBootstrapDaemonAddress().isEmpty());
    TestUtils.testUnsignedBigInteger(info.getCumulativeDifficulty());
    TestUtils.testUnsignedBigInteger(info.getFreeSpace());
    assertTrue(info.getNumOfflinePeers() >= 0);
    assertTrue(info.getNumOnlinePeers() >= 0);
    assertTrue(info.getHeight() >= 0);
    assertTrue(info.getHeightWithoutBootstrap() > 0);
    assertTrue(info.getNumIncomingConnections() >= 0);
    assertNotNull(info.getNetworkType());
    assertNotNull(info.getIsOffline());
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

  private static void testHardForkInfo(MoneroHardForkInfo hardForkInfo) {
    assertNotNull(hardForkInfo.getEarliestHeight());
    assertNotNull(hardForkInfo.getIsEnabled());
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
    assertNotNull(connection.getIsLocalIp());
    assertNotNull(connection.getIsLocalHost());
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
    assertNotNull(peer.getIsOnline());
    if (fromConnection) assertNull(peer.getLastSeenTimestamp());
    else assertTrue(peer.getLastSeenTimestamp() > 0);
    assertEquals(0, (int) peer.getPruningSeed());
  }

  private static void testUpdateCheckResult(MoneroDaemonUpdateCheckResult result) {
    assertTrue(result instanceof MoneroDaemonUpdateCheckResult);
    assertNotNull(result.getIsUpdateAvailable());
    if (result.getIsUpdateAvailable()) {
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
    if (result.getIsUpdateAvailable()) {
      if (path != null) assertEquals(path, result.getDownloadPath());
      else assertNotNull(result.getDownloadPath());
    } else {
      assertNull(result.getDownloadPath());
    }
  }
  
  private static void testDaemonConnectionSpan(MoneroDaemonConnectionSpan span) {
    throw new RuntimeException("Not implemented");
  }
  
  private static void testSubmitTxResultGood(MoneroSubmitTxResult result) {
    testSubmitTxResultCommon(result);
    assertEquals(true, result.getIsGood());
    assertEquals(false, result.getIsDoubleSpend());
    assertEquals(false, result.getIsFeeTooLow());
    assertEquals(false, result.getIsMixinTooLow());
    assertEquals(false, result.getHasInvalidInput());
    assertEquals(false, result.getHasInvalidOutput());
    assertEquals(true, result.getIsRct());
    assertEquals(false, result.getIsOverspend());
    assertEquals(false, result.getIsTooBig());
  }
  
  private static void testSubmitTxResultDoubleSpend(MoneroSubmitTxResult result) {
    testSubmitTxResultCommon(result);
    assertEquals(false, result.getIsGood());
    assertEquals(true, result.getIsDoubleSpend());
    assertEquals(false, result.getIsFeeTooLow());
    assertEquals(false, result.getIsMixinTooLow());
    assertEquals(false, result.getHasInvalidInput());
    assertEquals(false, result.getHasInvalidOutput());
    assertEquals(true, result.getIsRct());
    assertEquals(false, result.getIsOverspend());
    assertEquals(false, result.getIsTooBig());
  }

  private static void testSubmitTxResultCommon(MoneroSubmitTxResult result) {
    assertNull(result.getIsGood());
    assertNull(result.getIsRelayed());
    assertNull(result.getIsDoubleSpend());
    assertNull(result.getIsFeeTooLow());
    assertNull(result.getIsMixinTooLow());
    assertNull(result.getHasInvalidInput());
    assertNull(result.getHasInvalidOutput());
    assertNull(result.getIsRct());
    assertNull(result.getIsOverspend());
    assertNull(result.getIsTooBig());
  }
}
