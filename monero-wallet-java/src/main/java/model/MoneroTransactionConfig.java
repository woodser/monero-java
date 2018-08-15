package model;

import java.math.BigInteger;
import java.util.Collection;

import service.MoneroAccount;
import service.MoneroSubAddress;

/**
 * Configuration for sending a transaction.
 */
public class MoneroTransactionConfig {

  private Collection<MoneroPayment> destinations;
  private MoneroAccount account;
  private Collection<MoneroSubAddress> subaddresses;
  private BigInteger fee;
  private int mixin;
  private int unlockTime;
  private String paymentId;
  private MoneroTransactionPriority priority;
  private boolean doNotRelay;
}
