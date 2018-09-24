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
    filter.setTxIds(Arrays.asList("ec43d5000bbcb0b43d2cf03fb24e8d90a93d4d66625fb8e92db92613a177d41b"));
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
