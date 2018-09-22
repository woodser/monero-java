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
    //filter.setTxIds(Arrays.asList("0b93b598552f843e6ada0e2dc0dd9f8ed47d1228f0092d8728c8b8c30c1bf5d5"));
    filter.setIncoming(false);
    //filter.setAccountIndex(1);
    Set<String> ids = new HashSet<String>();
    List<MoneroTx> txs = wallet.getTxs(filter);
    for (MoneroTx tx : txs) {
      ids.add(tx.getId());
      System.out.println(tx);
    }
    
    System.out.println(ids.size());
    System.out.println(txs.size());
  }
}
