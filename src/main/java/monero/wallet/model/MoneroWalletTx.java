package monero.wallet.model;

import java.math.BigInteger;
import java.util.List;

import monero.daemon.model.MoneroTx;

public class MoneroWalletTx extends MoneroTx {

  private BigInteger incomingAmount;
  private BigInteger outgoingAmount;
  private List<MoneroTransfer> incomingTransfers;
  private MoneroTransfer outgoingTransfer;
  private String note;
}
