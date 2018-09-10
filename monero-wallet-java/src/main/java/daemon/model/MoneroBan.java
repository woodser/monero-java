package daemon.model;

/**
 * Monero banhammer.
 */
public class MoneroBan extends MoneroDaemonStatus {
  
  private String host;  // e.g. 192.168.1.100
  private Integer ip;   // integer formatted IP
  private Boolean isBanned;
  private Integer seconds;
}
