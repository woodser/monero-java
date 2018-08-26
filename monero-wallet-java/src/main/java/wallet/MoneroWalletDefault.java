package wallet;

import java.util.Arrays;
import java.util.List;

import model.MoneroException;
import model.MoneroSubaddress;

/**
 * Default implementation of a Monero Wallet.
 */
public abstract class MoneroWalletDefault implements MoneroWallet {

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx) {
    return getSubaddresses(accountIdx, null);
  }
  
  @Override
  public MoneroSubaddress getSubaddress(int accountIdx, int subaddressIdx) {
    List<MoneroSubaddress> subaddresses = getSubaddresses(accountIdx, Arrays.asList(subaddressIdx));
    if (subaddresses.size() != 1) throw new MoneroException("Subaddress at index " + subaddressIdx + " does not exist");
    return subaddresses.get(0);
  }
}
