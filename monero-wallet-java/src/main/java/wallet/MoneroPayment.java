package wallet;

import com.google.common.primitives.UnsignedInteger;

public class MoneroPayment {

	private String address;
	private UnsignedInteger amount;
	private int blockHeight;
	private MoneroTransaction transaction;
	private boolean isAvailableToSpend;
}
