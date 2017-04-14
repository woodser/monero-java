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
  
  public int getHeight();

	public UnsignedInteger getBalance();
	
  public UnsignedInteger getUnlockedBalance();
	
	public MoneroAddress getStandardAddress();
	
	public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
	
	public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress);
	
	public MoneroTransaction sendTransaction(String address, UnsignedInteger amount, UnsignedInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(MoneroPayment payment, UnsignedInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(Set<MoneroPayment> payments, UnsignedInteger fee, int mixin, int unlockTime);
	
	public Set<MoneroTransaction> sweepDust();
	
	public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight);
	
	public String getMnemonicSeed();
	
	public String getViewKey();
	
	public URI getUri(MoneroUri uri);
	
	public MoneroUri parseUri(URI uri);
	 
  public void saveBlockchain();
  
  public void stopWallet();
}
