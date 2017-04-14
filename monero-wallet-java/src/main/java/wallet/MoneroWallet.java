package wallet;

import java.net.URI;
import java.util.Set;

import com.google.common.primitives.UnsignedInteger;

/**
 * Defines a Monero wallet interface.
 * 
 * @author woodser
 */
public interface MoneroWallet {

	public UnsignedInteger getBalance();
	
  public UnsignedInteger getUnlockedBalance();
	
	public String getStandardAddress();
	
	public String getIntegratedAddress(String paymentId);
	
	public MoneroTransaction sendTransaction(String address, UnsignedInteger amount, UnsignedInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(MoneroPayment payment);
	
	public MoneroTransaction sendTransaction(Set<MoneroPayment> payments, UnsignedInteger fee, int mixin, int unlockTime);
	
	public Set<MoneroTransaction> sweepDust();
	
	public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight);
	
	public String getSpendKey();
	
	public String getViewKey();
	
	public void save();
	
	public URI getUri(MoneroUri uri);
	
	public MoneroUri parseUri(URI uri);
}
