package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroCoinbaseTxSum;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.utils.MoneroException;
import monero.wallet.MoneroWallet;
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
    
    // test config
    TestContext ctx = new TestContext();
    ctx.hasHex = false;
    ctx.hasJson = true;
    ctx.headerIsFull = true;
    
    // retrieve by id of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    MoneroBlock block = daemon.getBlockById(id);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeader().getHeight()), block);
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
    
    // config for testing blocks
    TestContext ctx = new TestContext();
    ctx.hasHex = true;
    ctx.headerIsFull = true;
    ctx.hasTxs = false;
    
    // retrieve by height of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    MoneroBlock block = daemon.getBlockByHeight(lastHeader.getHeight());
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeader().getHeight()), block);
    
    // retrieve by height of previous to last block
    block = daemon.getBlockByHeight(lastHeader.getHeight() - 1);
    testBlock(block, ctx);
    assertEquals(lastHeader.getHeight() - 1, (int) block.getHeader().getHeight());
  }
  
  @Test
  public void testGetBlocksByHeightBinary() {
    
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
    ctx.txContext.fromGetBlocksByHeight = false;
    
    // test blocks
    boolean txFound = false;
    assertEquals(numBlocks, blocks.size());
    for (int i = 0; i < heights.size(); i++) {
      MoneroBlock block = blocks.get(i);
      if (!block.getTxs().isEmpty()) txFound = true;
      testBlock(block, ctx);
      assertEquals(block.getHeader().getHeight(), heights.get(i));      
    }
    assertTrue("No transactions found to test", txFound);
  }
  
  @Test
  public void testGetBlocksByIdBinary() {
    fail("Not implemented");
  }
  
  @Test
  public void testGetBlocksByRange() {
    
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
    //get_hashes.bin
    fail("Not implemented");
  }
  
  @Test
  public void testGetTransactionById() {
    
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
  public void testGetTransactionsByIds() {
    
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
  
  public void testGetTransactionsByIdsInPool() {
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
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
  public void testGetTransactionHexById() {
    
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
  public void testGetTransactionHexesByIds() {
    
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
    MoneroCoinbaseTxSum sum = daemon.getCoinbaseTxSum(0, 50000);
    testCoinbaseTxSum(sum);
  }
  
  @Test
  public void testGetFeeEstimate() {
    BigInteger fee = daemon.getFeeEstimate();
    TestUtils.testUnsignedBigInteger(fee, true);
  }
  
  @Test
  public void testGetTransactionsInPool() {
    
    // submit tx to pool but don't relay
    MoneroTx tx = getUnrelayedTx(wallet, null);
    daemon.submitTxHex(tx.getHex(), true);
    
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
  
  // ------------------------------- PRIVATE ---------------------------------
  
  /**
   * Provides context or configuration for test methods to test a type.
   */
  public static class TestContext {
    boolean hasJson;
    boolean isPruned;
    boolean isFull;
    boolean isConfirmed;
    boolean isCoinbase;
    boolean fromGetTxPool;
    boolean fromGetBlocksByHeight;
    boolean hasOutputIndices;
    boolean doNotTestCopy;
    boolean hasTxs;
    boolean hasHex;
    boolean headerIsFull;
    TestContext txContext;
    public TestContext() { }
    public TestContext(TestContext ctx) {
      throw new RuntimeException("Not implemented");
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
      assertTrue(header.getDifficulty() > 0);
      assertTrue(header.getCumulativeDifficulty() > 0);
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
    assertFalse(block.getTxIds().isEmpty());
    testCoinbaseTx(block.getCoinbaseTx());  // TODO: coinbase tx doesn't have as much stuff, can't call testTx?
    testBlockHeader(block.getHeader(), ctx.headerIsFull);
    
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

    // TODO: coinbase tx does not have ids in binary requests so this will fail, need to derive using prunable data
    TestContext ctx = new TestContext();
    ctx.hasJson = false;
    ctx.isPruned = true;
    ctx.isFull = false;
    ctx.isConfirmed = true;
    ctx.isCoinbase = true;
    ctx.fromGetTxPool = true;
    testTx(coinbaseTx, ctx);
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
    assertNull(tx.getPrunableHex());
    assertTrue(tx.getVersion() >= 0);
    assertTrue(tx.getUnlockTime() >= 0);
    assertTrue(tx.getVins().size() >= 0);
    assertTrue(tx.getVouts().size() >= 0);
    for (MoneroOutput vin : tx.getVins()) assertTrue(tx == vin.getTx());
    for (MoneroOutput vout : tx.getVouts()) assertTrue(tx == vout.getTx());
    assertTrue(tx.getExtra().length > 0);
    assertNotNull(tx.getRctSignatures()); // TODO: model and return 
    if (ctx.fromGetBlocksByHeight) assertEquals(tx.getHex(), null);  // TODO: getBlocksByHeight() has inconsistent client-side pruning
    else assertFalse(tx.getHex().isEmpty());
    
    // test presence of output indices
    // TODO: change this over to vouts only
    if (tx.getIsCoinbase()) assertEquals(tx.getOutputIndices(), null); // TODO: how to get output indices for coinbase transactions?
    if (tx.getInTxPool() || ctx.fromGetTxPool || ctx.hasOutputIndices == false) assertEquals(null, tx.getOutputIndices());
    else assertFalse(tx.getOutputIndices().isEmpty());
    
    // test confirmed ctx
    if (ctx.isConfirmed == true) assertEquals(true, tx.getIsConfirmed());
    if (ctx.isConfirmed == false) assertEquals(false, tx.getIsConfirmed());
    
    // test confirmed
    if (tx.getIsConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeader().getHeight() > 0);
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeader().getHeight() > 0);
      assertTrue(tx.getBlock().getHeader().getTimestamp() > 0);
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
      assertTrue(tx.getNumEstimatedBlocksUntilConfirmed() > 0);
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
    
    // test vins and vouts
    if (!tx.getIsCoinbase()) assertFalse(tx.getVins().isEmpty());
    assertFalse(tx.getVouts().isEmpty());
    if (tx.getVins() != null) for (MoneroOutput vin : tx.getVins()) testVin(vin, ctx);
    if (tx.getVouts() != null) for (MoneroOutput vout : tx.getVouts()) testVout(vout, ctx);
    
    // test pruned vs not pruned
    if (ctx.isPruned) {
      assertEquals(null, tx.getRctSigPrunable());
      assertEquals(null, tx.getSize());
      assertEquals(null, tx.getLastRelayedTimestamp());
      assertEquals(null, tx.getReceivedTimestamp());
      assertEquals(null, tx.getPrunedHex());
      assertFalse(tx.getPrunedHex().isEmpty());
    } else {
      if (ctx.fromGetBlocksByHeight) assertEquals(null, tx.getRctSigPrunable());  // TODO: getBlocksByHeight() has inconsistent client-side pruning
      else assertNotNull(tx.getRctSigPrunable());
      assertEquals(null, tx.getPrunedHex());
      assertEquals(false, tx.getIsDoubleSpend());
      if (tx.getIsConfirmed()) {
        assertEquals(null, tx.getLastRelayedTimestamp());
        assertEquals(null, tx.getReceivedTimestamp());
      } else {
        if (tx.getIsRelayed()) assertTrue(tx.getLastRelayedTimestamp() > 0);
        else assertEquals(null, tx.getLastRelayedTimestamp());
        assertTrue(tx.getReceivedTimestamp() > 0);
      }
      assertEquals(null, tx.getPrunableHash());
      
//      if (ctx.fromGetTxPool || ctx.fromGetBlocksByHeight) assertEquals(tx.getPrunableHash(), null);  // TODO: getBlocksByHeight() has inconsistent client-side pruning
//      else assertFalse(tx.getPrunableHash().isEmpty());
//      
//      if (ctx.isPruned) assertNull(tx.getPrunableHash()); // TODO: tx may or may not have prunable hash, need to know when it's expected
//      else assertFalse(tx.getPrunableHash().isEmpty());
    }
    
    // test fields from tx pool
    if (ctx.fromGetTxPool) {
      assertTrue(tx.getSize() > 0);
      assertTrue(tx.getWeight() > 0);
      assertNotNull(tx.getIsKeptByBlock());
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedId());
      assertTrue(tx.getMaxUsedBlockHeight() >= 0);
      assertTrue(tx.getMaxUsedBlockId() > 0);
    } else {
      assertEquals(null, tx.getWeight());
      assertEquals(null, tx.getIsKeptByBlock());
      assertEquals(null ,tx.getIsFailed());
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedId());
      assertEquals(null, tx.getMaxUsedBlockHeight());
      assertEquals(null, tx.getMaxUsedBlockId());
    }
    
    if (tx.getIsFailed()) {
      // TODO: implement this
    }
    
    // test deep copy
    if (!ctx.doNotTestCopy) testTxCopy(tx, ctx);
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
    if (vout.getTx().getInTxPool() || ctx.hasOutputIndices == false) assertEquals(null, vout.getIndex());
    else assertTrue(vout.getIndex() >= 0);
    assertEquals(64, vout.getStealthPublicKey().length());
  }

  private static void testOutput(MoneroOutput output) {
    TestUtils.testUnsignedBigInteger(output.getAmount());
  }
  
  private static void testTxCopy(MoneroTx tx, TestContext ctx) {
    
    // copy tx and assert deep equality
    MoneroTx copy = tx.copy();
    assert(copy instanceof MoneroTx);
    assertEquals(tx, copy);
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
      assertEquals(realStartHeight + i, (int) blocks.get(i).getHeader().getHeight());
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
    assertFalse(tx.getHex().isEmpty());
    assertEquals(tx.getDoNotRelay(), true);
    return tx;
  }
  
  private static void testCoinbaseTxSum(MoneroCoinbaseTxSum txSum) {
    TestUtils.testUnsignedBigInteger(txSum.getEmissionSum(), true);
    TestUtils.testUnsignedBigInteger(txSum.getFeeSum(), true);
  }
}
