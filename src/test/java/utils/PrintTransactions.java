package utils;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.config.MoneroTxFilter;
import monero.wallet.model.MoneroTxWallet;

/**
 * Prints transactions in a wallet.
 */
public class PrintTransactions {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWalletRpc();
    MoneroTxFilter filter = new MoneroTxFilter();
    //filter.setIsOutgoing(false);
    //filter.setIsIncoming(false);
    Set<String> ids = new HashSet<String>();
    ids.add("d16e603efed00a3d2e14085d0ebe96734145d9eba851f6afd02ab76f9d5b62c0");
    filter.setTxIds(ids);
    List<MoneroTxWallet> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      System.out.println(tx);
    }
  }
}
