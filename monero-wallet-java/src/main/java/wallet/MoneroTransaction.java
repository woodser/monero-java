package wallet;

import java.util.List;

import com.google.common.primitives.UnsignedInteger;

public class MoneroTransaction {

	private List<MoneroPayment> payments;
	private UnsignedInteger fee;
	private int mixin;
	private String txKey;
	private String txHash;
	private int size;
	private MoneroTransactionType type;
}
