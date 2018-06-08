package api;

import java.math.BigInteger;
import java.util.Collection;

/**
 * Send transaction configuration.
 */
public class MoneroTransactionConfig {

  private Collection<MoneroPayment> destinations;
  private Collection<MoneroSubAddress> subAddresses;
  private BigInteger fee;
  private int mixin;
  private int unlockTime;
  private String paymentId;
  private int priority;
  private boolean doNotRelay;
}
