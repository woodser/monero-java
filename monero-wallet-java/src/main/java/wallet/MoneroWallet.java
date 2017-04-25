package wallet;

import java.math.BigInteger;
import java.net.URI;
import java.util.Collection;
import java.util.List;

/**
 * Monero wallet interface.
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
	
	public MoneroTransaction send(String address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction send(MoneroAddress address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction send(MoneroPayment payment, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction send(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public List<MoneroTransaction> sendSplit(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime, Boolean newAlgorithm);
	
	public List<MoneroTransaction> sweepDust();
	
	public List<MoneroTransaction> getAllTransactions();
	
	public List<MoneroTransaction> getAllTransactions(boolean getIncoming, boolean getOutgoing, boolean getPending, boolean getFailed, boolean getMemPool, Collection<String> paymentIds, Integer minHeight, Integer maxHeight);
	
	public String getMnemonicSeed();
	
	public String getViewKey();
	
	public URI toUri(MoneroUri uri);
	
	public MoneroUri fromUri(URI uri);
	 
  public void saveBlockchain();
  
  public void stopWallet();
}
