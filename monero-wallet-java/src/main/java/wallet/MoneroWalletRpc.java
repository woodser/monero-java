package wallet;

import java.net.URI;
import java.util.Set;

import org.jooq.types.UInteger;

/**
 * Implements a MoneroWallet backed by a monero wallet RPC endpoint.
 * 
 * @author woodser
 */
public class MoneroWalletRpc implements MoneroWallet {
  
  private String domain;
  private int port;
  
  public MoneroWalletRpc(String domain, int port) {
    this.domain = domain;
    this.port = port;
  }

  public String getDomain() {
    return domain;
  }

  public int getPort() {
    return port;
  }

  public UInteger getBalance() {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroAddress getAddress() {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(MoneroAddress address, UInteger amount, UInteger fee, int mixin, int unlockTime) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(MoneroPayment payment) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroTransaction sendTransaction(Set<MoneroPayment> payments, UInteger fee, int mixin, int unlockTime) {
    throw new RuntimeException("Not yet implemented.");
  }

  public Set<MoneroTransaction> sweepDust() {
    throw new RuntimeException("Not yet implemented.");
  }

  public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight) {
    throw new RuntimeException("Not yet implemented.");
  }

  public String getSpendKey() {
    throw new RuntimeException("Not yet implemented.");
  }

  public String getViewKey() {
    throw new RuntimeException("Not yet implemented.");
  }

  public void save() {
    throw new RuntimeException("Not yet implemented.");
  }

  public URI getUri(MoneroUri uri) {
    throw new RuntimeException("Not yet implemented.");
  }

  public MoneroUri parseUri(URI uri) {
    throw new RuntimeException("Not yet implemented.");
  }
	
}
