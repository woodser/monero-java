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
    ids.add("d37bd0ef59f5dd40249b4aab6a23ec0fbb4432da68cf7a496212fc78a25afcba");
    filter.setTxIds(ids);
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      System.out.println(tx);
      //TestUtils.testGetTx(tx, hasOutgoingPayments, wallet);
    }
  }
}
