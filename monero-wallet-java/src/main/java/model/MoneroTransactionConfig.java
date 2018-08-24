package model;

import java.math.BigInteger;
import java.util.Collection;

import model.MoneroTransaction.MoneroTransactionPriority;

/**
 * Configuration for sending a transaction.
 */
public class MoneroTransactionConfig {

  private Collection<MoneroPayment> destinations;
  private MoneroAccount account;
  private Collection<MoneroSubaddress> subaddresses;
  private BigInteger fee;
  private int mixin;
  private int unlockTime;
  private String paymentId;
  private MoneroTransactionPriority priority;
  private boolean doNotRelay;
}
