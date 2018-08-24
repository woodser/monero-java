package service;

import java.util.List;

import model.MoneroSubaddress;

/**
 * Default implementation of a Monero Wallet.
 */
public abstract class MoneroWalletDefault implements MoneroWallet {

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx) {
    // TODO Auto-generated method stub
    return null;
  }
  
  

//  @Override
//  public List<MoneroSubaddress> getSubaddresses() {
//    return getSubaddresses(null);
//  }
//  
//  public MoneroSubaddress getSubaddress(int index) {
//    List<MoneroSubaddress> subaddresses = getSubaddresses(Arrays.asList(index));
//    if (subaddresses.size() != 1) throw new MoneroException("Subaddress at index " + index + " does not exist");
//    return subaddresses.get(0);
//  }
}
