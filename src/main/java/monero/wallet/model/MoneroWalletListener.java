package monero.wallet.model;

/**
 * Provides default handling for wallet notifications.
 */
public class MoneroWalletListener implements MoneroWalletListenerI {

  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) { }

  @Override
  public void onNewBlock(long height) { }

  @Override
  public void onIncomingTransfer(MoneroIncomingTransfer transfer) { }

  @Override
  public void onOutgoingTransfer(MoneroOutgoingTransfer transfer) { }
}
