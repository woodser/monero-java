package daemon;

import java.util.List;

import daemon.model.MoneroBlock;
import daemon.model.MoneroBlockHeader;
import daemon.model.MoneroBlockTemplate;
import daemon.model.MoneroDaemonConnection;
import daemon.model.MoneroDaemonInfo;

/**
 * Monero daemon interface.
 */
public interface MoneroDaemon {

  public void getBlockCount();
  
  public void getBlockHash(int height);
  
  public MoneroBlockTemplate getBlockTemplate(String walletAddress, int reserveSize);
  
  public String submitBlock(String blockBlob);
  
  public MoneroBlockHeader getLastBlockHeader();  
  
  public MoneroBlockHeader getBlockHeader(String hash);
  
  public MoneroBlockHeader getBlockHeader(int height);
  
  public List<MoneroBlockHeader> getBlockHeaders(int startHeight, int endHeight);
  
  public MoneroBlock getBlock(String hash);
  
  public MoneroBlock getBlock(int height);
  
  public List<MoneroDaemonConnection> getConnections();
  
  public MoneroDaemonInfo getInfo();
}
