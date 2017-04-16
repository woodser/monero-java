package wallet;

import java.math.BigInteger;
import java.util.List;

public class MoneroTransaction {

	private List<MoneroPayment> payments;
	private BigInteger fee;
	private int mixin;
	private String txKey;
	private String txHash;
	private int size;
	private MoneroTransactionType type;
}
