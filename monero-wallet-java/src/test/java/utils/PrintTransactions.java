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
    filter.setTxIds(Arrays.asList("409778b3e5abbbeb3b35c9b532001e8b1c84b8743c40728219eeeb95c1c283bf"));
    filter.setOutgoing(false);
    filter.setIncoming(true);
    //filter.setAccountIndex(0);
    Set<String> ids = new HashSet<String>();
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      ids.add(tx.getId());
      System.out.println(tx);
    }
  }
}
