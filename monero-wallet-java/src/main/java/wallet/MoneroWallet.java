package wallet;

import java.math.BigInteger;
import java.net.URI;
import java.util.Set;

/**
 * Defines a Monero wallet interface.
 * 
 * @author woodser
 */
public interface MoneroWallet {
  
  public int getHeight();

	public BigInteger getBalance();
	
  public BigInteger getUnlockedBalance();
	
	public MoneroAddress getStandardAddress();
	
	public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
	
	public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress);
	
	public MoneroTransaction sendTransaction(String address, BigInteger amount, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(MoneroPayment payment, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(Set<MoneroPayment> payments, BigInteger fee, int mixin, int unlockTime);
	
	public Set<MoneroTransaction> sweepDust();
	
	public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight);
	
	public String getMnemonicSeed();
	
	public String getViewKey();
	
	public URI toUri(MoneroUri uri);
	
	public MoneroUri fromUri(URI uri);
	 
  public void saveBlockchain();
  
  public void stopWallet();
}
