package wallet;

import org.jooq.types.UInteger;

public class MoneroPayment {

	private MoneroAddress address;
	private UInteger amount;
	private int blockHeight;
	private MoneroTransaction transaction;
	private boolean isAvailableToSpend;
}
