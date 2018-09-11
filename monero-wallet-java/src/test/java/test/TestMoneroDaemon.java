package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.BeforeClass;
import org.junit.Test;

import daemon.MoneroDaemon;
import daemon.model.MoneroBlockCount;
import daemon.model.MoneroDaemonModel;
import utils.TestUtils;

/**
 * Tests a Monero daemon.
 */
public class TestMoneroDaemon {
  
  private static MoneroDaemon daemon;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daemon = TestUtils.getDaemon();
  }

  @Test
  public void testGetBlockCount() {
    MoneroBlockCount blockCount = daemon.getBlockCount();
    testDaemonStatus(blockCount);
    //testing
    assertNotNull(blockCount.getCount());
  }

  @Test
  public void testGetBlockHash() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBlockTemplate() {
    fail("Not yet implemented");
  }

  @Test
  public void testSubmitBlock() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetLastBlockHeader() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBlockHeaderString() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBlockHeaderInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBlockHeaders() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBlockString() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBlockInt() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetConnections() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetInfo() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetSyncInfo() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetHardForkInfo() {
    fail("Not yet implemented");
  }

  @Test
  public void testSetBan() {
    fail("Not yet implemented");
  }

  @Test
  public void testSetBans() {
    fail("Not yet implemented");
  }

  @Test
  public void testFlushTxPool() {
    fail("Not yet implemented");
  }

  @Test
  public void testFlushTxPoolCollectionOfString() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetOutputHistogram() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetOutputDistribution() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetCoinbaseTxSum() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetFeeEstimate() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetAlternativeChains() {
    fail("Not yet implemented");
  }

  @Test
  public void testRelayTx() {
    fail("Not yet implemented");
  }

  @Test
  public void testRelayTxs() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetTxPoolBacklog() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetAltBlockHashes() {
    fail("Not yet implemented");
  }

  @Test
  public void testIsKeyImageSpent() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetTxs() {
    fail("Not yet implemented");
  }

  @Test
  public void testStartMining() {
    fail("Not yet implemented");
  }

  @Test
  public void testStopMining() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetMiningStatus() {
    fail("Not yet implemented");
  }

  @Test
  public void testSetBandwidthLimit() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetBandwidthLimit() {
    fail("Not yet implemented");
  }

  @Test
  public void testSetNumOutgoingLimit() {
    fail("Not yet implemented");
  }

  @Test
  public void testSetNumIncomingLimit() {
    fail("Not yet implemented");
  }

  private static void testDaemonStatus(MoneroDaemonModel model) {
    assertTrue(model.getStatus() != null || model.isTrusted() != null);
    if (model.getStatus() != null) {
      assertEquals("OK", model.getStatus());
    }
  }
}
