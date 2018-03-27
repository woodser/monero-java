# Monero Wallet Java Interface

This is a Java interface and implementation of a Monero wallet.

The implementation uses a running instance of monero-wallet-rpc.

This project is licensed under Apache 2.0 and MIT.

# Supported Methods
```
public int getHeight();
public BigInteger getBalance();
public BigInteger getUnlockedBalance();
public MoneroAddress getStandardAddress();
public MoneroIntegratedAddress getIntegratedAddress(String paymentId);
public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress);
public String getMnemonicSeed();
public String getViewKey();
public URI toUri(MoneroUri uri);
public MoneroUri fromUri(URI uri);
public void saveBlockchain();
public void stopWallet();
public MoneroTransaction send(String address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
public MoneroTransaction send(MoneroAddress address, BigInteger amount, String paymentId, BigInteger fee, int mixin, int unlockTime);
public MoneroTransaction send(MoneroPayment payment, String paymentId, BigInteger fee, int mixin, int unlockTime);
public MoneroTransaction send(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime);
public List<MoneroTransaction> sendSplit(List<MoneroPayment> payments, String paymentId, BigInteger fee, int mixin, int unlockTime, Boolean newAlgorithm);
public List<MoneroTransaction> sweepDust();
public List<MoneroTransaction> getAllTransactions();
public List<MoneroTransaction> getTransactions(boolean getIncoming, boolean getOutgoing, boolean getPending, boolean getFailed, boolean getMemPool, Collection<String> paymentIds, Integer minHeight, Integer maxHeight);
```
