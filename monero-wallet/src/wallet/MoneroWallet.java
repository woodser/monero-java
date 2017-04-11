package wallet;

import org.jooq.types.UInteger;

/**
 * Defines a Monero wallet interface.
 * 
 * @author woodser
 */
public interface MoneroWallet {

	public UInteger getBalance();
	
	public UInteger getUnlockedBalance();
}
