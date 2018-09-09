package daemon;

import daemon.model.MoneroBlockTemplate;

public interface MoneroDaemon {

  public void getBlockCount();
  
  public void getBlockHash(int height);
  
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize);
  
  public String submitBlock(String blockBlob);
}
