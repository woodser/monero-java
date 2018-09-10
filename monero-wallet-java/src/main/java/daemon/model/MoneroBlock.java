package daemon.model;

import java.util.List;

/**
 * Monero block.
 */
public class MoneroBlock {

  private String blob;
  private String header;
  private MoneroMinerTx minerTx;
  private List<String> txHashes;
  private String status;
  private Boolean isUntrusted;
}
