# Monero Java

## Introduction

This project provides Java interfaces for a Monero wallet and daemon.

The interfaces currently rely on running instances of [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html) and [Monero Daemon RPC](https://getmonero.org/resources/developer-guides/daemon-rpc.html).

A primary goal of this project make the Monero Core C++ wallet accessible through a Java interface.

Main Features

- General-purpose library with focus on ease-of-use
- Clear object-oriented models to formalize Monero types and their relationships to each other
- Powerful API to query transactions, transfers, and vouts by their attributes
- Extensive test suite (130+ passing tests)
- Fetch and process binary data from the daemon in Java using JNI to bridge C++ utilities

A quick reference of the wallet and daemon data models can be found [here](monero-model.pdf).

## Wallet Sample Code

See the [tests](tests) for the most complete examples of using this library.

```java
// create a wallet that uses a monero-wallet-rpc endpoint with authentication
MoneroWallet wallet = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");

// get wallet balance as BigInteger
BigInteger balance = wallet.getBalance();  // e.g. 533648366742

// get wallet primary address
String primaryAddress = wallet.getPrimaryAddress();  // e.g. 59aZULsUF3YNSKGiHz4J...

// get incoming and outgoing transfers
List<MoneroTransfer> transfers = wallet.getTransfers();
for (MoneroTransfer transfer : transfers) {
  boolean isIncoming = transfer.getIsIncoming();
  BigInteger amount = transfer.getAmount();
  int accountIdx = transfer.getAccountIndex();
  Integer height = transfer.getTx().getHeight();  // can be null if unconfirmed
}

// get address and balance of subaddress [1, 0]
MoneroSubaddress subaddress = wallet.getSubaddress(1, 0);
BigInteger subaddressBalance = subaddress.getBalance();
String subaddressAddress = subaddress.getAddress();

// send to an address
MoneroTxWallet sentTx = wallet.send("74oAtjgE2dfD1bJBo4DWW3E6qXCAwUDMgNqUurnX9b2xUvDTwMwExiXDkZskg7Vct37tRGjzHRqL4gH4H3oag3YyMYJzrNp", new BigInteger("50000"));

// send to multiple destinations from subaddress 1, 0 which can be split into multiple transactions
// see MoneroSendConfig.java for all config options or to build a config object
List<MoneroTxWallet> sentTxs = wallet.sendSplit(new MoneroSendConfig()
    .setAccountIndex(1)
    .setSubaddressIndices(0, 1)
    .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
        .setDestinations(
                new MoneroDestination("7BV7iyk9T6kfs7cPfmn7vPZPyWRid7WEwecBkkVr8fpw9MmUgXTPtvMKXuuzqKyr2BegWMhEcGGEt5vNkmJEtgnRFUAvf29", new BigInteger("50000")),
                new MoneroDestination("78NWrWGgyZeYgckJhuxmtDMqo8Kzq5r9j1kV8BQXGq5CDnECz2KjQeBDc3KKvdMQmR6TWtfbRaedgbSGmmwr1g8N1rBMdvW", new BigInteger("50000"))));
                
// get all confirmed wallet transactions
for (MoneroTxWallet tx : wallet.getTxs(new MoneroTxFilter().setIsConfirmed(true))) {
  String txId = tx.getId();                   // e.g. f8b2f0baa80bf6b...
  BigInteger txFee = tx.getFee();             // e.g. 750000
  boolean isConfirmed = tx.getIsConfirmed();  // e.g. true
}

// get a wallet transaction by id
MoneroTxWallet tx = wallet.getTx("c936b213b236a8ff60da84067c39409db6934faf6c0acffce752ac5a0d53f6b6");
String txId = tx.getId();                   // e.g. c936b213b236a8ff6...
BigInteger txFee = tx.getFee();             // e.g. 750000
boolean isConfirmed = tx.getIsConfirmed();  // e.g. true
```

## Daemon Sample Code

```java
// create a daemon that uses a monero-daemon-rpc endpoint
MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "admin", "password");

// get daemon info
int height = daemon.getHeight();                  // e.g. 1523651
BigInteger feeEstimate = daemon.getFeeEstimate(); // e.g. 750000

// get last block's header
MoneroBlockHeader lastBlockHeader = daemon.getLastBlockHeader();
long lastBlockSize = lastBlockHeader.getSize();

// get first 100 blocks as a binary request
List<MoneroBlock> blocks = daemon.getBlocksByRange(0, 100);

// get block info
for (MoneroBlock block : blocks) {
  int blockHeight = block.getHeight();
  String blockId = block.getId();
  List<MoneroTx> txs = block.getTxs();
  
  // get tx ids and keys
  for (MoneroTx tx : txs) {
    String txId = tx.getId();
    String txKey = tx.getKey();
  }
}

// start mining to an address with 4 threads, not in the background, and ignoring the battery
String address = "59aZULsUF3YNSKGiHz4JPMfjGYk...";
int numThreads = 4;
boolean isBackground = false;
boolean ignoreBattery = false;
daemon.startMining(address, numThreads, isBackground, ignoreBattery);

// wait for the header of the next block added to the chain
MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
long nextNumTxs = nextBlockHeader.getNumTxs();

// stop mining
daemon.stopMining();
```

## Running Tests

1. Set up running instances of [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html) and [Monero Daemon RPC](https://getmonero.org/resources/developer-guides/daemon-rpc.html).  See [Monero RPC Setup](#monero-rpc-setup).
2. `git clone --recurse-submodules https://github.com/woodser/monero-javascript.git`
3. `npm install`
4. Configure the appropriate RPC endpoints and authentication by modifying `WALLET_RPC_CONFIG` and `DAEMON_RPC_CONFIG` in [TestUtils.js](tests/TestUtils.js).
5. `npm test`

Note: some tests are failing as not all functionality is implemented.

## Monero RPC Setup

1. Download and extract the latest [Monero CLI](https://getmonero.org/downloads/) for your platform.
2. Start Monero daemon locally: `./monerod --stagenet` (or use a remote daemon).
3. Create a wallet file if one does not exist.  This is only necessary one time.
	- Create new / open existing: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet`
	- Restore from mnemonic seed: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet --restore-deterministic-wallet`
4. Start monero-wallet-rpc (requires --wallet-dir to run tests):
	
	e.g. For wallet name `test_wallet_1`, user `rpc_user`, password `abc123`, stagenet: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir /Applications/monero-v0.13.0.2`

## Interfaces and Types

- [Monero daemon (MoneroDaemon.js)](src/daemon/MoneroDaemon.js)
- [Monero daemon rpc implementation (MoneroDaemonRpc.js)](src/daemon/MoneroDaemonRpc.js)
- [Monero daemon model (src/daemon/model)](src/daemon/model)
- [Monero wallet (src/wallet/MoneroWallet.js)](src/wallet/MoneroWallet.js)
- [Monero wallet rpc implementation (src/wallet/MoneroWalletRpc.js)](src/wallet/MoneroWalletRpc.js)
- [Monero wallet model (src/wallet/model)](src/wallet/model)

## Project Goals

- Offer consistent terminology and APIs for Monero's developer ecosystem
- Build a wallet adapter for a local wallet which uses client-side crypto and a daemon
- Build a wallet adapter for a MyMonero wallet which shares the view key with a 3rd party to scan the blockchain

## License

This project is licensed under MIT.

## Donate

<p align="center">
	<img src="donate.png" width="150" height="150"/>
</p>

`46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz`

Thank you supporting this project.