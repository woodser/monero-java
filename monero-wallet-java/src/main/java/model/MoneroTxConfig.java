package model;

import java.math.BigInteger;
import java.util.Collection;

import model.MoneroTx.MoneroTxPriority;

/**
 * Configuration for sending a transaction.
 */
public class MoneroTxConfig {

  private Collection<MoneroPayment> destinations;
  private MoneroAccount account;
  private Collection<MoneroSubaddress> subaddresses;
  private BigInteger fee;
  private int mixin;
  private int unlockTime;
  private String paymentId;
  private MoneroTxPriority priority;
  private boolean doNotRelay;
}
