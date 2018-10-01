package utils;

import java.util.List;

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
    filter.setOutgoing(false);
    filter.setIncoming(true);
    //filter.setAccountIndex(0);
//    Set<String> ids = new HashSet<String>();
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      if (tx.getPayments() != null && tx.getPayments().size() > 1) {
        System.out.println(tx);
      }
//      System.out.println(tx);
    }
  }
}
