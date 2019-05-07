# Monero Java

**Compatible with Monero Core version 14.0.3**

## Introduction

This project provides Java interfaces for a Monero wallet and daemon.

The interfaces currently rely on running instances of [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html) and [Monero Daemon RPC](https://getmonero.org/resources/developer-guides/daemon-rpc.html).

A primary goal of this project make the Monero Core C++ wallet accessible through a Java interface.

Main Features

- General-purpose library to access a Monero wallet and daemon with focus on ease-of-use
- Clear object-oriented models to formalize Monero types and their relationships to each other
- Powerful API to query transactions, transfers, and vouts by their attributes
- Fetch and process binary data from the daemon (e.g. raw blocks) in Java using JNI to bridge C++ utilities
- Extensive test suite (130+ passing tests)

A quick reference of the wallet and daemon data models can be found [here](https://github.com/monero-ecosystem/monero-javascript/blob/master/monero-model.pdf).

## Wallet Sample Code

See the [src/test/java/](src/test/java) for the most complete examples of using this library.

```java
// create a wallet that uses a monero-wallet-rpc endpoint with authentication
MoneroWallet wallet = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");

// get wallet balance as BigInteger
BigInteger balance = wallet.getBalance();  // e.g. 533648366742

// get wallet primary address
String primaryAddress = wallet.getPrimaryAddress();  // e.g. 59aZULsUF3YNSKGiHz4J...

// get address and balance of subaddress [1, 0]
MoneroSubaddress subaddress = wallet.getSubaddress(1, 0);
BigInteger subaddressBalance = subaddress.getBalance();
String subaddressAddress = subaddress.getAddress();

// get incoming and outgoing transfers
List<MoneroTransfer> transfers = wallet.getTransfers();
for (MoneroTransfer transfer : transfers) {
  boolean isIncoming = transfer.getIsIncoming();
  BigInteger amount = transfer.getAmount();
  int accountIdx = transfer.getAccountIndex();
  Integer height = transfer.getTx().getHeight();  // will be null if unconfirmed
}

// get incoming transfers to account 0
transfers = wallet.getTransfers(new MoneroTransferRequest().setAccountIndex(0).setIsIncoming(true));
for (MoneroTransfer transfer : transfers) {
  assertTrue(transfer.getIsIncoming());
  assertEquals(0, (int) transfer.getAccountIndex());
  BigInteger amount = transfer.getAmount();
  Integer height = transfer.getTx().getHeight();  // will be null if unconfirmed
}

// send to an address
MoneroTxWallet sentTx = wallet.send("74oAtjgE2dfD1bJBo4DWW3E6qXCAwUDMgNqUurnX9b2xUvDTwMwExiXDkZskg7Vct37tRGjzHRqL4gH4H3oag3YyMYJzrNp", new BigInteger("50000"));

// send to multiple destinations from subaddress 1, 0 which can be split into multiple transactions
// see MoneroSendRequest.java for all request options
List<MoneroTxWallet> sentTxs = wallet.sendSplit(new MoneroSendRequest()
        .setAccountIndex(1)
        .setSubaddressIndices(0, 1)
        .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
        .setDestinations(
                new MoneroDestination("7BV7iyk9T6kfs7cPfmn7vPZPyWRid7WEwecBkkVr8fpw9MmUgXTPtvMKXuuzqKyr2BegWMhEcGGEt5vNkmJEtgnRFUAvf29", new BigInteger("50000")),
                new MoneroDestination("78NWrWGgyZeYgckJhuxmtDMqo8Kzq5r9j1kV8BQXGq5CDnECz2KjQeBDc3KKvdMQmR6TWtfbRaedgbSGmmwr1g8N1rBMdvW", new BigInteger("50000"))));

// get all confirmed wallet transactions
for (MoneroTxWallet tx : wallet.getTxs(new MoneroTxRequest().setIsConfirmed(true))) {
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
String address = "74oAtjgE2dfD1bJBo4DWW3E6qXCAwUDMgNqUurnX9b2xUvDTwMwExiXDkZskg7Vct37tRGjzHRqL4gH4H3oag3YyMYJzrNp";
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

## API Documentation

This library follows the wallet and daemon interfaces and models defined [here](https://github.com/monero-ecosystem/monero-javascript/blob/master/monero-model.pdf).

Javadoc is provided in the [doc](doc) folder (best viewed opening [doc/index.html](doc/index.html) in a browser).

The main interfaces are [MoneroWallet.java](src/main/java/monero/wallet/MoneroWallet.java) and [MoneroDaemon.java](src/main/java/monero/daemon/MoneroDaemon.java).

Here is the source code to the main interfaces, implementations, and models:

- [Monero daemon (MoneroDaemon.java)](src/main/java/monero/daemon/MoneroDaemon.java)
- [Monero daemon rpc implementation](src/main/java/monero/daemon/MoneroDaemonRpc.java)
- [Monero daemon models](src/main/java/monero/daemon/model)
- [Monero wallet (MoneroWallet.java)](src/main/java/monero/wallet/MoneroWallet.java)
- [Monero wallet rpc implementation](src/main/java/monero/wallet/MoneroWalletRpc.java)
- [Monero wallet models](src/main/java/monero/wallet/model)

## Monero RPC Setup

1. Download and extract the latest [Monero CLI](https://getmonero.org/downloads/) for your platform.
2. Start Monero daemon locally: `./monerod --stagenet` (or use a remote daemon).
3. Create a wallet file if one does not exist.  This is only necessary one time.
	- Create new / open existing: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet`
	- Restore from mnemonic seed: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet --restore-deterministic-wallet`
4. Start monero-wallet-rpc (requires --wallet-dir to run tests):
	
	e.g. For wallet name `test_wallet_1`, user `rpc_user`, password `abc123`, stagenet: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir /Applications/monero-v0.14.0.3`

## Running Tests

1. Set up running instances of [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html) and [Monero Daemon RPC](https://getmonero.org/resources/developer-guides/daemon-rpc.html) with two test wallets named `test_wallet_1` and `test_wallet_2`.  The mnemonic phrase and public address of `test_wallet_1` must match `TestUtils.TEST_MNEMONIC` and `TestUtils.TEST_ADDRESS`, respectively.  Both wallets must be encrypted with a password which matches `TestUtils.WALLET_RPC_PW` ("supersecretpassword123").  See [Monero RPC Setup](#monero-rpc-setup).
2. Clone the Java repository: `git clone --recurse-submodules https://github.com/monero-ecosystem/monero-java-rpc.git`
3. Install project dependencies: `maven install`
4. Configure the appropriate RPC endpoints and authentication by modifying `WALLET_RPC_CONFIG` and `DAEMON_RPC_CONFIG` in [src/test/main/test/TestUtils.java](src/test/main/TestUtils.java).
5.  [Build a dynamic library from Monero C++ for your platform](#building-platform-specific-monero-binaries)
6. Run all *.java files in src/main/test as JUnits.

Note: some tests are failing as not all functionality is implemented.

## Building platform-specific Monero binaries

In order to fetch and process binary data from the daemon, C++ source code is built as a dynamic library for Java to access using JNI.  The C++ code being built is contained in src/main/cpp/submodules.  A fork of MyMonero is currently used with added utilities to convert binary data between JSON and Monero's portable storage format.

The dynamic library is platform-specific so it must be built from source for the specific platform it is running on (e.g. Linux, Mac, Windows, etc).

For convenience, a pre-built library for MacOSX is included with this project.  **This executable is suitable for development and testing only and should be re-built from source to verify its integrity for any other purpose.**

### Build Steps

1. Build boost for your system
2. Copy boost_system.a, boost_thread.a, and boost_chrono.a into src/main/cpp/build/boost/lib
3. `export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/` (change as appropriate)
4. Build libmonero.dylib to src/main/cpp/build/: `cd src/main/cpp && ./bin/build-libmonero.sh`
5. Run TestMoneroCppUtils.java JUnit tests to verify the dynamic library is working with Java JNI

## Project Goals

- Expose a Monero daemon and wallet in Java using Monero Core RPC.
- Expose a Monero wallet in Java by binding to Monero Core's wallet in C++.
- Expose a Monero wallet in Java backed by a MyMonero-compatible endpoint which shares the view key with a 3rd party to scan the blockchain.
- Offer consistent terminology and APIs for Monero's developer ecosystem with a working reference implementation.

## License

This project is licensed under MIT.

## Donate

<p align="center">
	<img src="donate.png" width="150" height="150"/>
</p>

`46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz`

Thank you supporting this project.