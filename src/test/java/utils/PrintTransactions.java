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
    filter.setIncoming(false);
    Set<String> ids = new HashSet<String>();
    ids.add("e9699640fb3253f21c6a3b247b13d713b3dd5f89f3c2ab8b771748c4d562e406");
    filter.setTxIds(ids);
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      System.out.println(tx);
      //TestUtils.testGetTx(tx, hasOutgoingPayments, wallet);
    }
  }
}
