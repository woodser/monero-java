package wallet;

import java.math.BigInteger;
import java.net.URI;
import java.util.List;

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
	
	public MoneroTransaction transfer(String address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction transfer(MoneroAddress address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction transfer(MoneroPayment payment, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public MoneroTransaction transfer(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime);
	
	public List<MoneroTransaction> transferSplit(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime, Boolean newAlgorithm);
	
	public List<MoneroTransaction> sweepDust();
	
	public List<MoneroTransaction> getTransactions();
	
  public List<MoneroTransaction> getTransactions(Integer minHeight, Integer maxHeight);
		
	public List<MoneroTransaction> getTransactions(boolean getIn, boolean getOut, boolean getPending, boolean getFailed, boolean getMemPool, Integer minHeight, Integer maxHeight);
	
	public String getMnemonicSeed();
	
	public String getViewKey();
	
	public URI toUri(MoneroUri uri);
	
	public MoneroUri fromUri(URI uri);
	 
  public void saveBlockchain();
  
  public void stopWallet();
}
