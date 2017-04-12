package wallet;

import java.net.URI;
import java.util.Set;

import org.jooq.types.UInteger;

/**
 * Defines a Monero wallet interface.
 * 
 * @author woodser
 */
public interface MoneroWallet {

	public UInteger getBalance();
	
	public MoneroAddress getAddress();
	
	public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
	
	public MoneroTransaction sendTransaction(MoneroAddress address, UInteger amount, UInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(MoneroPayment payment);
	
	public MoneroTransaction sendTransaction(Set<MoneroPayment> payments, UInteger fee, int mixin, int unlockTime);
	
	public Set<MoneroTransaction> sweepDust();
	
	public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight);
	
	public String getSpendKey();
	
	public String getViewKey();
	
	public void save();
	
	public URI getUri(MoneroUri uri);
	
	public MoneroUri parseUri(URI uri);
}
