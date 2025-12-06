package utils;

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;

import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;

public class WalletSyncTester extends SyncProgressTester {
  
  private Long walletTesterPrevHeight;  // renamed from prevHeight to not interfere with super's prevHeight
  private MoneroOutputWallet prevOutputReceived;
  private MoneroOutputWallet prevOutputSpent;
  private BigInteger incomingTotal;
  private BigInteger outgoingTotal;
  private Boolean onNewBlockAfterDone;
  private BigInteger prevBalance;
  private BigInteger prevUnlockedBalance;
  
  public WalletSyncTester(MoneroWallet wallet, long startHeight, long endHeight) {
    super(wallet, startHeight, endHeight);
    assertTrue(startHeight >= 0);
    assertTrue(endHeight >= 0);
    incomingTotal = BigInteger.valueOf(0);
    outgoingTotal = BigInteger.valueOf(0);
  }
  
  @Override
  public synchronized void onNewBlock(long height) {
    if (isDone) {
      assertTrue(wallet.getListeners().contains(this), "Listener has completed and is not registered so should not be called again");
      onNewBlockAfterDone = true;
    }
    if (walletTesterPrevHeight != null) {
      assertEquals(walletTesterPrevHeight + 1, height);
    }
    assertTrue(height >= super.startHeight);
    walletTesterPrevHeight = height;
  }
  
  @Override
  public void onBalancesChanged(BigInteger newBalance, BigInteger newUnlockedBalance) {
    if (this.prevBalance != null) assertTrue(!newBalance.equals(this.prevBalance) || !newUnlockedBalance.equals(this.prevUnlockedBalance));
    this.prevBalance = newBalance;
    this.prevUnlockedBalance = newUnlockedBalance;
  }

  @Override
  public void onOutputReceived(MoneroOutputWallet output) {
    assertNotNull(output);
    prevOutputReceived = output;
    
    // test output
    assertNotNull(output.getAmount());
    assertTrue(output.getAccountIndex() >= 0);
    assertTrue(output.getSubaddressIndex() >= 0);
    
    // test output's tx
    assertNotNull(output.getTx());
    assertNotNull(output.getTx().getHash());
    assertEquals(64, output.getTx().getHash().length());
    assertTrue(output.getTx().getVersion() >= 0);
    assertTrue(output.getTx().getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
    assertNull(output.getTx().getInputs());
    assertEquals(1, output.getTx().getOutputs().size());
    assertTrue(output.getTx().getOutputs().get(0) == output);
    
    // extra is not sent over the jni bridge
    assertNull(output.getTx().getExtra());
    
    // add incoming amount to running total
    if (output.isLocked()) incomingTotal = incomingTotal.add(output.getAmount()); // TODO: only add if not unlocked, test unlocked received
  }

  @Override
  public void onOutputSpent(MoneroOutputWallet output) {
    assertNotNull(output);
    prevOutputSpent = output;
    
    // test output
    assertNotNull(output.getAmount());
    assertTrue(output.getAccountIndex() >= 0);
    if (output.getSubaddressIndex() != null) assertTrue(output.getSubaddressIndex() >= 0); // TODO (monero-project): can be undefined because inputs not provided so one created from outgoing transfer
    
    // test output's tx
    assertNotNull(output.getTx());
    assertNotNull(output.getTx().getHash());
    assertEquals(64, output.getTx().getHash().length());
    assertTrue(output.getTx().getVersion() >= 0);
    assertTrue(output.getTx().getUnlockTime().compareTo(BigInteger.valueOf(0)) >= 0);
    assertEquals(1, output.getTx().getInputs().size());
    assertTrue(output.getTx().getInputs().get(0) == output);
    assertNull(output.getTx().getOutputs());
    
    // extra is not sent over the jni bridge
    assertNull(output.getTx().getExtra());
    
    // add outgoing amount to running total
    if (output.isLocked()) outgoingTotal = outgoingTotal.add(output.getAmount());
  }
  
  @Override
  public void onDone(long chainHeight) {
    super.onDone(chainHeight);
    assertNotNull(walletTesterPrevHeight);
    assertNotNull(prevOutputReceived);
    assertNotNull(prevOutputSpent);
    BigInteger expectedBalance = incomingTotal.subtract(outgoingTotal);

    // output notifications do not include pool fees or outgoing amount
    BigInteger poolSpendAmount = BigInteger.ZERO;
    for (MoneroTxWallet poolTx : wallet.getTxs(new MoneroTxQuery().setInTxPool(true))) {
      poolSpendAmount = poolSpendAmount.add(poolTx.getFee()).add(poolTx.getOutgoingAmount() == null ? BigInteger.ZERO : poolTx.getOutgoingAmount());
    }
    expectedBalance = expectedBalance.subtract(poolSpendAmount);

    assertEquals(expectedBalance, wallet.getBalance());
    assertEquals(prevBalance, wallet.getBalance());
    assertEquals(prevUnlockedBalance, wallet.getUnlockedBalance());
  }
  
  public Boolean getOnNewBlockAfterDone() {
    return onNewBlockAfterDone;
  }
}