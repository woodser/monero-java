package wallet;

import java.util.List;

import org.jooq.types.UInteger;

public class MoneroTransaction {

	private List<MoneroPayment> payments;
	private UInteger fee;
	private int mixin;
	private String txKey;
	private String txHash;
	private int size;
	private MoneroTransactionType type;
}
