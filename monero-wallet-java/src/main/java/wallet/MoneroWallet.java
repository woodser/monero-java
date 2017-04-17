package wallet;

import java.math.BigInteger;
import java.net.URI;
import java.util.List;
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
	
	public MoneroTransaction sendTransaction(String address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(MoneroAddress address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction sendTransaction(MoneroPayment payment, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public List<MoneroTransaction> sendTransactions(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime, Boolean newAlgorithm);
	
	public Set<MoneroTransaction> sweepDust();
	
	public Set<MoneroTransaction> getTransactions(Set<MoneroTransactionType> includeTypes, Integer minHeight, Integer maxHeight);
	
	public String getMnemonicSeed();
	
	public String getViewKey();
	
	public URI toUri(MoneroUri uri);
	
	public MoneroUri fromUri(URI uri);
	 
  public void saveBlockchain();
  
  public void stopWallet();
}
