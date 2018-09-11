package daemon.model;

import java.util.List;

/**
 * Monero block.
 */
public class MoneroBlock extends MoneroDaemonModel {

  private String blob;
  private String header;
  private MoneroMinerTx minerTx;
  private List<String> txHashes;
}
