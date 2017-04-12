package wallet_old;

import java.util.List;

import org.jooq.types.UInteger;

import common.Pair;

/**
 * Defines a Monero wallet interface.
 * 
 * @author woodser
 */
public interface MoneroWallet {

	public UInteger getBalance();
	
	public UInteger getUnlockedBalance();
	
	public MoneroAddress getAddress();
	
	public UInteger getHeight();
	
	public MoneroTransaction send(MoneroAddress address, UInteger amount, UInteger fee, UInteger mixin, UInteger unlockTime, String paymentId);
	
	public List<MoneroTransaction> send(Pair<MoneroAddress, UInteger> addressAmountPairs, UInteger fee, UInteger mixin, UInteger unlockTime, String paymentId, Boolean newAlgorithm);
	
	public MoneroTransaction send(MoneroTransfer transfer);
	
	public List<String> sweepDust();
	
	public void store();
	
	public List<MoneroPayment> getPayments(String paymentId);
	
	public List<MoneroPayment> getBulkPayments(List<String> paymentIds, UInteger minBlockHeight);
	
	
}
