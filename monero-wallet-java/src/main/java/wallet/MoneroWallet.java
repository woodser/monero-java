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
  
  /**
   * Gets the current height of the blockchain.
   * 
   * @return int is the height of the wallet's blockchain
   */
  public int getHeight();

  /**
   * Gets the wallet's balance.
   * 
   * @return BigInteger is the wallet's balance
   */
	public BigInteger getBalance();
	
	/**
	 * Gets the wallet's unlocked balance.
	 * 
	 * @return BigInteger is the wallet's unlocked balance
	 */
  public BigInteger getUnlockedBalance();
	
  /**
   * Returns the wallet's standard address.
   * 
   * @return MoneroAddress is the wallet's standard address
   */
	public MoneroAddress getStandardAddress();
	
	/**
	 * Returns an integrated address based on this wallet's standard address and the given payment id.
	 * 
	 * @param paymentId is the payment id to generate an integrated address from
	 * @return MoneroIntegratedAddress is the integrated address with standard address and payment id components
	 */
	public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
	
	/**
	 * Splits an integrated address into its standard address and payment id components.
	 * 
	 * @param integratedAddress is a string representation of the integrated address
	 * @return MoneroIntegratedAddress contains the integrated address, standard address, and payment id
	 */
	public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress);
	 
	/**
	 * Gets the wallet's mnemonic seed.
	 * 
	 * @return String is the wallet's mnemonic seed
	 */
  public String getMnemonicSeed();
  
  /**
   * Gets the wallet's view key.
   * 
   * @return String is the wallet's view key
   */
  public String getViewKey();
  
  /**
   * Converts a MoneroUri to a standard URI.
   * 
   * @param moneroUri is the MoneroUri to convert to a standard URI
   * @return URI is the MoneroUri converted to a standard URI
   */
  public URI toUri(MoneroUri moneroUri);
  
  /**
   * Converts a standard URI to a Monero URI.
   * 
   * @param uri is the standard URI to convert
   * @return MoneroUri is the URI converted to a Monero URI
   */
  public MoneroUri toMoneroUri(URI uri);
   
  /**
   * Saves the current state of the blockchain.
   */
  public void saveBlockchain();
  
  /**
   * Stops the wallet.
   */
  public void stopWallet();
	
  /**
   * Sends a payment.
   * 
   * @param address is the recipient public address as a string
   * @param amount is the amount to send in atomic units
   * @param paymentId specifies the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @return MoneroTransaction is the resulting transaction from sending the payment
   */
	public MoneroTransaction send(String address, BigInteger amount, String paymentId, int mixin, int unlockTime);
	
  /**
   * Sends a payment.
   * 
   * @param address is the recipient public address
   * @param amount is the amount to send in atomic units
   * @param paymentId specifies the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @return MoneroTransaction is the resulting transaction from sending the payment
   */
	public MoneroTransaction send(MoneroAddress address, BigInteger amount, String paymentId, int mixin, int unlockTime);
	
  /**
   * Sends a payment.
   * 
   * @param payment specifies the recipient public address and amount to send
   * @param paymentId is the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @return MoneroTransaction is the resulting transaction from sending the payment
   */
	public MoneroTransaction send(MoneroPayment payment, String paymentId, int mixin, int unlockTime);
	
	/**
	 * Sends a list of payments in one transaction.
	 * 
	 * @param payments are the payments to send, each specifying a recipient public address and amount to send
	 * @param paymentId specifies the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @return MoneroTransaction is the resulting transaction from sending the payment
	 */
	public MoneroTransaction send(List<MoneroPayment> payments, String paymentId, int mixin, int unlockTime);
	
	/**
	 * Sends a list of payments which can be split into more than one transaction if necessary.
	 * 
   * @param payments are the payments to send, each specifying a recipient public address and amount to send
   * @param paymentId specifies the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @param newAlgorithm specifies if the "new" construction algorithm should be used; defaults to false
   * @return List<MoneroTransaction> are the resulting transaction from sending the payments
	 */
	public List<MoneroTransaction> sendSplit(List<MoneroPayment> payments, String paymentId, int mixin, int unlockTime, Boolean newAlgorithm);
	
	/**
	 * Send all dust outputs back to the wallet to make them easier to spend and mix.
	 * 
	 * @return List<MoneroTransaction> are the resulting transactions from sweeping dust
	 */
	public List<MoneroTransaction> sweepDust();
	
	/**
	 * Returns all wallet transactions, each containing payments, outputs, and other metadata
	 * depending on the transaction type.
	 * 
	 * @return List<MoneroTransaction> are all of the wallet's transactions
	 */
	public List<MoneroTransaction> getAllTransactions();
	
	/**
	 * Returns all wallet transactions specified, each containing payments, outputs, and other metadata
	 * depending on the transaction type.
	 * 
	 * @param getIncoming specifies if incoming transactions should be retrieved
	 * @param getOutgoing specifies if outgoing transactions should be retrieved
	 * @param getPending specifies if pending transactions should be retrieved
	 * @param getFailed specifies if failed transactions should be retrieved
	 * @param getMemPool specifies if mempool transactions should be retrieved
	 * @param paymentIds allows transactions with specific transaction ids to be retrieved (optional)
	 * @param minHeight allows transactions with a mininum block height to be retrieved (optional)
	 * @param maxHeight allows transactions with a maximum block height to be retrieved (optional)
	 * @return List<MoneroTransaction> are the retrieved transactions
	 */
	public List<MoneroTransaction> getTransactions(boolean getIncoming, boolean getOutgoing, boolean getPending, boolean getFailed, boolean getMemPool, Collection<String> paymentIds, Integer minHeight, Integer maxHeight);
}
