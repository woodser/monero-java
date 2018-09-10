package daemon.model;

import java.util.List;

/**
 * Models daemon synchronization information.
 */
public class MoneroSyncInfo extends MoneroDaemonStatus {

  private Integer height;
  private List<MoneroDaemonConnection> peers;
  private List<MoneroDaemonConnectionSpan> spans; // TODO: incorporate span info into daemon connection?
  private Integer targetHeight;
}
