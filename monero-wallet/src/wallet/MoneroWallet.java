package wallet;

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
	
	public MoneroResponse transfer(Pair<MoneroAddress, UInteger> destinations, UInteger fee, UInteger mixin, UInteger unlockTime, String paymentId);
	
	public MoneroResponse sweepDust();
}
