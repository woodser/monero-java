package service;

import java.util.Arrays;
import java.util.List;

import model.MoneroException;

public abstract class MoneroAccountDefault implements MoneroAccount {

  public List<MoneroSubAddress> getSubAddresses() {
    return getSubAddresses(null);
  }
  
  public MoneroSubAddress getSubAddress(int index) {
    List<MoneroSubAddress> subaddresses = getSubAddresses(Arrays.asList(index));
    if (subaddresses.size() != 1) throw new MoneroException("Subaddress at index " + index + " does not exist");
    return subaddresses.get(0);
  }
}
