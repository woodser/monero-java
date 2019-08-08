# Monero Java Library

This project is a library for working with Monero wallets and daemons in Java using RPC and JNI bindings to [Monero Core](https://github.com/monero-project/monero).

In addition, this project offers an [API specification](monero-spec.pdf) for wallet and daemon methods and types.  The specification is intended to be intuitive, robust, and for long-term use in the Monero project.

## Main Features

- Manage a Monero daemon using RPC
- Manage a Monero wallet using RPC and JNI
- Object-oriented model with rigorous focus on ease-of-use
- Fetch and process binary data from the daemon (e.g. raw blocks)
- Query wallet transactions, transfers, and outputs by their many attributes
- Over 220 passing JUnit tests

## Sample Code

This code demonstrates the API.  See the [Javadoc](https://moneroecosystem.org/monero-java/), [specification PDF](monero-spec.pdf), or [JUnit tests](src/test/java) for full details.

```java
// connect to a daemon
MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081");
long height = daemon.getHeight();                 // 1523651
BigInteger feeEstimate = daemon.getFeeEstimate(); // 1014313512

// get transactions in the pool
List<MoneroTx> txsInPool = daemon.getTxPool();
for (MoneroTx tx : txsInPool) {
  String id = tx.getId();
  BigInteger fee = tx.getFee();
  boolean isDoubleSpendSeen = tx.isDoubleSpendSeen();
}

// get last 100 blocks as a binary request
List<MoneroBlock> blocks = daemon.getBlocksByRange(height - 100, height);
for (MoneroBlock block : blocks) {
  int numTxs = block.getTxs().size();
}

// mine with 2 threads in the background
String address = "74oAtjgE2dfD1bJBo4DW...";
int numThreads = 2;
boolean isBackground = true;
boolean ignoreBattery = false;
daemon.startMining(address, numThreads, isBackground, ignoreBattery);
daemon.stopMining();

// wait for the next block to be added to the chain
MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
long nextNumTxs = nextBlockHeader.getNumTxs();

// connect to a wallet using RPC
MoneroWallet walletRPC = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
BigInteger balance = walletRPC.getBalance();           // 533648366742
String primaryAddress = walletRPC.getPrimaryAddress(); // 59aZULsUF3YNSKGiHz4J...
MoneroSubaddress subaddress = walletRPC.getSubaddress(1, 0);
BigInteger subaddressBalance = subaddress.getBalance();

// query a transaction
MoneroTxWallet tx = walletRPC.getTx("3276252c5a545b90c8e147fcde45d3e1917726470a8f7d4c8977b527a44dfd15");
List<MoneroIncomingTransfer> incomingTransfers = tx.getIncomingTransfers();
List<MoneroDestination> destinations = tx.getOutgoingTransfer().getDestinations();

// query incoming transfers to account 1
MoneroTransferQuery transferQuery = new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(1);
List<MoneroTransfer> transfers = walletRPC.getTransfers(transferQuery);

// query unspent outputs
MoneroOutputQuery outputQuery = new MoneroOutputQuery().setIsSpent(false);
List<MoneroOutputWallet> outputs = walletRPC.getOutputs(outputQuery);

// create a new wallet using native Java binding to Monero Core
MoneroWalletJni walletJNI = MoneroWalletJni.createWalletRandom("MyWallet", "supersecretpassword123", MoneroNetworkType.MAINNET);
walletJNI.startSyncing();

// listen for incoming funds to the JNI wallet
walletJNI.addListener(new MoneroWalletListener() {
  
  @Override
  public void onOutputReceived(MoneroOutputWallet output) {
    System.out.println("Funds received!"); 
    int accountIdx = output.getAccountIndex();
    int subaddressIdx = output.getSubaddressIndex();
    MoneroKeyImage keyImage = output.getKeyImage();
  }
});

// send funds from the RPC wallet to the JNI wallet
MoneroTxWallet sentTx = walletRPC.send(0, walletJNI.getPrimaryAddress(), new BigInteger("50000"));

// create and relay a tx which sends funds to multiple destinations in the JNI wallet
MoneroSendRequest request = new MoneroSendRequest()
        .setAccountIndex(1)                           // send from account 1
        .setSubaddressIndices(0, 1)                   // send from subaddreses in account 1
        .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
        .setDestinations(
                new MoneroDestination(walletJNI.getAddress(1, 0), new BigInteger("50000")),
                new MoneroDestination(walletJNI.getAddress(2, 0), new BigInteger("50000")));
MoneroTxWallet createdTx = walletRPC.createTx(request);
BigInteger fee = createdTx.getFee();  // "Are you sure you want to send ...?"
walletRPC.relayTx(createdTx);
```

## How to Use This Library

1. Clone the Java repository: `git clone --recurse-submodules https://github.com/monero-ecosystem/monero-java-rpc.git`
2. Install project dependencies: `maven install`

You are now ready to use this library with [monero-daemon-rpc](https://getmonero.org/resources/developer-guides/daemon-rpc.html) and [monero-wallet-rpc](https://getmonero.org/resources/developer-guides/wallet-rpc.html) endpoints.

If you want to process binary data or use a Monero wallet using JNI instead of RPC, a dynamic library must be built for your specific platform for this Java library to use.  This project uses a [C++ counterpart library](https://github.com/woodser/monero-cpp-library) (included as a submodule in ./external/monero-cpp-library) to support JNI.

1. [Build the counterpart C++ library as a dynamic library](https://github.com/woodser/monero-cpp-library#building-a-dynamic--shared-library)
2. Copy the built libmonero-cpp.dylib in step 1 to ./external-libs
3. `export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/` (change as appropriate)
4. Build libmonero-java.dylib to ./build: `./bin/build-libmonero-java.sh`
5. Copy ./build/libmonero-java.dylib to ./lib
6. Run TestMoneroCppUtils.java JUnit tests to verify the dynamic library is working with Java JNI

## How to Set Up Monero RPC

1. Download and extract the latest [Monero CLI](https://getmonero.org/downloads/) for your platform.
2. Start Monero daemon locally: `./monerod --stagenet` (or use a remote daemon).
3. Create a wallet file if one does not exist.
	- Create new / open existing: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet`
	- Restore from mnemonic seed: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet --restore-deterministic-wallet`
4. Start monero-wallet-rpc (requires --wallet-dir to run tests):
	
	e.g. For wallet name `test_wallet_1`, user `rpc_user`, password `abc123`, stagenet: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir /Applications/monero-v0.14.0.3`

## How to Run Tests

1. [Set up this library with JNI support](how-to-use-this-library)
2. Set up running instances of [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html) and [Monero Daemon RPC](https://getmonero.org/resources/developer-guides/daemon-rpc.html).  See [Monero RPC Setup](#how-to-set-up-monero-rpc). 
3. Configure the desired RPC endpoints, authentication, and test wallet in [TestUtils.java](src/test/java/utils/TestUtils.java).
4. Run all *.java files in src/main/test as JUnits.

## See Also

These libraries conform to the same [API specification](monero-spec.pdf).

[JavaScript reference implementation](https://github.com/monero-ecosystem/monero-javascript)

[C++ reference implementation](https://github.com/woodser/monero-cpp-library)

## License

This project is licensed under MIT.

## Donate

Donations are gratefully accepted.  Thank you for your support!

<p align="center">
	<img src="donate.png" width="125" height="125"/>
</p>

`46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz`