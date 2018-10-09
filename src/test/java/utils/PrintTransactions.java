package utils;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroTx;
import monero.wallet.model.MoneroTxFilter;

/**
 * Prints transactions in a wallet.
 */
public class PrintTransactions {

  public static void main(String[] args) {
    MoneroWallet wallet = TestUtils.getWallet();
    MoneroTxFilter filter = new MoneroTxFilter();
//    filter.setOutgoing(false);
//    filter.setIncoming(false);
    Set<String> ids = new HashSet<String>();
    ids.add("727de264a4a3278b3bab5cf6c4ccfab2f595543d70da907e924025c3082882ee");
    filter.setTxIds(ids);
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      System.out.println(tx);
      //TestUtils.testGetTx(tx, hasOutgoingPayments, wallet);
    }
  }
}
