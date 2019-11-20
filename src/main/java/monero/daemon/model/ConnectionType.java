package monero.daemon.model;

/**
 * Enumerates connection types.
 * 
 * Based on enums.h in monero-project.
 */
public enum ConnectionType {
  INVALID,
  IPV4,
  IPV6,
  TOR,
  I2P
}
