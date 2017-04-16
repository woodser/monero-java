package wallet;

import java.math.BigInteger;

public class MoneroPayment {

	private String address;
	private BigInteger amount;
	private int blockHeight;
	private MoneroTransaction transaction;
	private boolean isAvailableToSpend;
}
