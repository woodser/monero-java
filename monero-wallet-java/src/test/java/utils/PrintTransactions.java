package utils;

import java.util.Arrays;
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
    filter.setTxIds(Arrays.asList("4d31a222d8e4e7f5bb9f8e2917af04745dc38549741709364aff5483b3567905"));
    filter.setOutgoing(true);
    filter.setIncoming(false);
    //filter.setAccountIndex(0);
    Set<String> ids = new HashSet<String>();
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      ids.add(tx.getId());
      System.out.println(tx);
    }
  }
}
