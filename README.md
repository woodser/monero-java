# Monero Wallet Java Interface

This project is a Java wrapper for [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html).

Monero Wallet RPC manages a Monero wallet using remote procedure calls and it depends on a full node to consult the Monero blockchain.

This project is licensed under Apache 2.0 and MIT.

# Getting started

1. Download and extract the latest [Monero CLI](https://getmonero.org/downloads/) for your platform.
2. `cd` to the extracted directory in a terminal
3. Start monero-wallet-rpc
  Example, start RPC on testnet with wallet 'testnet', user 'rpc_user', password 'abc123': `./monero-wallet-rpc --testnet --daemon-address http://node.xmrbackb.one:28081 --rpc-bind-port 18082 --wallet-file /Applications/monero-v0.11.1.0/testnet --rpc-login rpc_user:abc123`

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
