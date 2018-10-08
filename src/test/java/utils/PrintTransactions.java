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
    ids.add("6f081caf6508e9c47f2fabf6f32db0f177a25920eedabc2ca1557450998c214d");
    filter.setTxIds(ids);
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      System.out.println(tx);
    }
  }
}
