package daemon.model;

/**
 * Monero daemon info.
 */
public class MoneroDaemonInfo extends MoneroDaemonModel {
  
  private Integer altBlocksCount;
  private Integer blockSizeLimit;
  private Integer blockSizeMedian;
  private String bootstrapDaemonAddress;
  private Integer cumulativeDifficulty;
  private Integer difficulty;
  private Integer freeSpace;
  private Integer greyPeerlistSize;
  private Integer whitePeerlistSize;
  private Integer height;
  private Integer heightWithoutBootstrap;
  private Boolean isMainnet;
  private Boolean isStagenet;
  private Boolean isTestnet;
  private Boolean isOffline;
  private Integer incomingConnectionsCount;
  private Integer outgoingConnectionsCount;
  private Integer rpcConnectionsCount;
  private Long startTime;
  private Integer target;
  private Integer targetHeight;
  private String topBlockHash;
  private Integer txCount;
  private Integer txPoolSize;
  private Boolean wasBootstrapEverUsed;
  private Integer version;
}
