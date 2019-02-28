package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.List;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroTransfer;
import utils.TestUtils;

/**
 * Tests a Monero daemon.
 */
public class TestMoneroDaemon {
  
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
  public void testGetBlockchainHeight() {
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
    MoneroBlockTestConfig config = new MoneroBlockTestConfig(false, true, true, null);
    
    // retrieve by id of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    MoneroBlock block = daemon.getBlockById(id);
    testBlock(block, config);
    assertEquals(daemon.getBlockByHeight(block.getHeader().getHeight()), block);
    assertEquals(null, block.getTxs());
    
    // retrieve by id of previous to last block
    id = daemon.getBlockId(lastHeader.getHeight() - 1);
    block = daemon.getBlockById(id);
    testBlock(block, config);
    assertEquals(daemon.getBlockByHeight(lastHeader.getHeight() - 1), block);
    assertEquals(null, block.getTxs());
  }
  
  // ------------------------------- PRIVATE ---------------------------------
  
  public static class MoneroBlockTestConfig {
    boolean hasTxs;
    boolean hasHex;
    boolean headerIsFull;
    MoneroTxCtx txConfig;
    public MoneroBlockTestConfig(boolean hasTxs, boolean hasHex, boolean headerIsFull, MoneroTxCtx txConfig) {
      super();
      this.hasTxs = hasTxs;
      this.hasHex = hasHex;
      this.headerIsFull = headerIsFull;
      this.txConfig = txConfig;
    }
  }
  
  public static class MoneroTxCtx {
    boolean hasJson;
    boolean isPruned;
    boolean isFull;
    boolean isConfirmed;
    boolean isCoinbase;
    boolean fromGetTxPool;
    boolean fromGetBlocksByHeight;
    boolean hasOutputIndices;
    boolean doNotTestCopy;
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
  private static void testBlock(MoneroBlock block, MoneroBlockTestConfig config) {
    
    // test required fields
    assertNotNull(block);
    assertFalse(block.getTxIds().isEmpty());
    testCoinbaseTx(block.getCoinbaseTx());  // TODO: coinbase tx doesn't have as much stuff, can't call testTx?
    testBlockHeader(block.getHeader(), config.headerIsFull);
    
    if (config.hasHex) {
      assertNotNull(block.getHex());
      assertTrue(block.getHex().length() > 1);
    } else {
      assertNull(block.getHex());
    }
    
    if (config.hasTxs) {
      assertNotNull(config.txConfig);
      for (MoneroTx tx : block.getTxs()) {
        assertTrue(block == tx.getBlock());
        testTx(tx, config.txConfig);
      }
    } else {
      assertNull(config.txConfig);
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
    MoneroTxCtx config = new MoneroTxCtx();
    config.hasJson = false;
    config.isPruned = true;
    config.isFull = false;
    config.isConfirmed = true;
    config.isCoinbase = true;
    config.fromGetTxPool = true;
    testTx(coinbaseTx, config);
  }
  
  private static void testTx(MoneroTx tx, MoneroTxCtx ctx) {
    
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
}
