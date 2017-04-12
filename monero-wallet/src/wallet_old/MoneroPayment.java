package wallet_old;

import org.jooq.types.UInteger;

public class MoneroPayment {

	private String paymentId;
	private String hxHash;
	private UInteger amount;
	private UInteger blockHeight;
	private UInteger unlockTime;
}
