package service;

import java.util.Arrays;
import java.util.List;

public abstract class MoneroAccountDefault implements MoneroAccount {

  public List<MoneroSubAddress> getSubAddresses() {
    return getSubAddresses(null);
  }
  
  public MoneroSubAddress getSubAddress(int index) {
    List<MoneroSubAddress> subAddresses = getSubAddresses(Arrays.asList(index));
    if (subAddresses.size() != 1) throw new MoneroException("Sub-address at index " + index + " does not exist");
    return subAddresses.get(0);
  }
}
