package api;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Monero account interface.
 */
public abstract class MoneroAccount {
  
  /**
   * Gets the account's index.
   * 
   * @return int is the account's index in the wallet
   */
  public abstract int getIndex();

  /**
   * Gets the account's balance.
   * 
   * @return BigInteger is the account's balance
   */
  public abstract BigInteger getBalance();

  /**
   * Gets the account's unlocked balance.
   * 
   * @return BigInteger is the account's unlocked balance
   */
  public abstract BigInteger getUnlockedBalance();
  
  /**
   * Indicates if importing multisig data is needed for returning a correct balance.
   * 
   * @return true if importing multisig data is needed for returning a correct balance, false otherwise
   */
  public abstract boolean isMultisigImportNeeded();
  
  /**
   * Gets the account's sub-addresses.
   * 
   * @return List<MoneroSubAddress> are the account's sub-addresses
   */
  public List<MoneroSubAddress> getSubAddresses() {
    return getSubAddresses(null);
  }
  
  /**
   * Gets a sub-address.
   * 
   * @param index is the index of the sub-address to get
   * @return MoneroSubAddress is the sub-address at the given index
   */
  public MoneroSubAddress getSubAddress(int index) {
    List<MoneroSubAddress> subAddresses = getSubAddresses(Arrays.asList(index));
    return subAddresses.isEmpty() ? null : subAddresses.get(0);
  }
  
  /**
   * Gets the account's sub-addresses.
   * 
   * @param indices are indices of sub-addresses to get
   * @return List<MoneroSubAddress> are the account's sub-addresses at the given indices
   */
  public abstract List<MoneroSubAddress> getSubAddresses(Collection<Integer> indices);
  
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
  public MoneroTransaction send(String address, BigInteger amount, String paymentId, int mixin, int unlockTime) {
    return send(new MoneroPayment(null, address, amount), paymentId, mixin, unlockTime);
  }

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
  public MoneroTransaction send(MoneroAddress address, BigInteger amount, String paymentId, int mixin, int unlockTime) {
    return send(address.toString(), amount, paymentId, mixin, unlockTime);
  }

  /**
   * Sends a payment.
   * 
   * @param payment specifies the recipient public address and amount to send
   * @param paymentId is the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @return MoneroTransaction is the resulting transaction from sending the payment
   */
  public MoneroTransaction send(MoneroPayment payment, String paymentId, int mixin, int unlockTime) {
    List<MoneroPayment> payments = new ArrayList<MoneroPayment>();
    payments.add(payment);
    return send(payments, paymentId, mixin, unlockTime);
  }

  /**
   * Sends a list of payments in one transaction.
   * 
   * @param payments are the payments to send, each specifying a recipient public address and amount to send
   * @param paymentId specifies the payment id (optional)
   * @param mixin is the number of outputs from the blockchain to mix with
   * @param unlockTime is the number of blocks before the funds can be spent
   * @return MoneroTransaction is the resulting transaction from sending the payment
   */
  public MoneroTransaction send(List<MoneroPayment> payments, Collection<Integer> subAddressIndices, BigInteger fee, String paymentId, int mixin, int unlockTime);

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
   * Returns all wallet transactions, each containing payments, outputs, and other metadata depending on the transaction type.
   * 
   * @return List<MoneroTransaction> are all of the wallet's transactions
   */
  public List<MoneroTransaction> getAllTransactions();

  /**
   * Returns all wallet transactions specified, each containing payments, outputs, and other metadata depending on the transaction type.
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
