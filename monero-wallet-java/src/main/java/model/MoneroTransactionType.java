package model;

/**
 * Enumerates Monero transaction types.
 */
public enum MoneroTransactionType {
  INCOMING,
  OUTGOING,
  PENDING,
  FAILED,
  MEMPOOL
}