package utils;

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
    //filter.setTxIds(Arrays.asList("c5ae93ceb9751734dc361bbeb0aba8a7a18b7d383e6c4f26713319d312a7f34c"));
    //filter.setAccountIdx(1);
    //filter.setAccountIdx(1);
    for (MoneroTx tx : wallet.getTxs(filter)) {
      System.out.println(tx);
    }
  }
}
