package monero.wallet.model;

import monero.daemon.model.MoneroBlockHeader;

/**
 * Provides default handling for wallet notifications.
 */
public class MoneroWalletListenerDefault implements MoneroWalletListener {

  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) { }

  @Override
  public void onNewBlock(MoneroBlockHeader header) { }

  @Override
  public void onNewTx(MoneroTxWallet tx) { }

  @Override
  public void onMoneyReceived(MoneroTxWallet tx) { }

  @Override
  public void onMoneySent(MoneroTxWallet tx) { }
}
