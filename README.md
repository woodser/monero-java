**Compatible with [Monero Core v0.15.0.1](https://web.getmonero.org/downloads/) Carbon Chameleon**

# Monero Java Library

This project is a library for using a Monero wallet and daemon in Java using RPC and JNI bindings to [Monero Core](https://github.com/monero-project/monero).

In addition, this project conforms to an [API specification](http://moneroecosystem.org/monero-java/monero-spec.pdf) intended to be intuitive, robust, and suitable for long-term use in the Monero project.

## Main Features

- Cohesive APIs with rigorous focus on ease-of-use
- Manage a Monero daemon using RPC
- Manage a Monero wallet using RPC and JNI
- Fetch and process binary data from the daemon (e.g. raw blocks)
- Query wallet transactions, transfers, and outputs by their many attributes
- Be notified when blocks are added to the chain, as the wallet synchronizes, or when the wallet sends or receives funds
- Validate addresses in native Java
- Full multisig support
- Over 200 passing JUnit test cases

## Sample Code

This code introduces the API.  See the [Javadoc](https://moneroecosystem.org/monero-java/), [specification PDF](http://moneroecosystem.org/monero-java/monero-spec.pdf), or [JUnit tests](src/test/java) for more details.

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
List<MoneroBlock> blocks = daemon.getBlocksByRange(height - 100, height - 1);
for (MoneroBlock block : blocks) {
  int numTxs = block.getTxs().size();
}

// connect to a monero-wallet-rpc endpoint with authentication
MoneroWalletRpc walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
    
// open a wallet on the server
walletRpc.openWallet("test_wallet_1", "supersecretpassword123");
String primaryAddress = walletRpc.getPrimaryAddress(); // 59aZULsUF3YNSKGiHz4J...
BigInteger balance = walletRpc.getBalance();           // 533648366742
MoneroSubaddress subaddress = walletRpc.getSubaddress(1, 0);
BigInteger subaddressBalance = subaddress.getBalance();

// query a transaction by id
MoneroTxWallet tx = walletRpc.getTx("314a0f1375db31cea4dac4e0a51514a6282b43792269b3660166d4d2b46437ca");
long txHeight = tx.getHeight();
List<MoneroIncomingTransfer> incomingTransfers = tx.getIncomingTransfers();
List<MoneroDestination> destinations = tx.getOutgoingTransfer().getDestinations();

// query incoming transfers to account 1
MoneroTransferQuery transferQuery = new MoneroTransferQuery().setIsIncoming(true).setAccountIndex(1);
List<MoneroTransfer> transfers = walletRpc.getTransfers(transferQuery);

// query unspent outputs
MoneroOutputQuery outputQuery = new MoneroOutputQuery().setIsSpent(false);
List<MoneroOutputWallet> outputs = walletRpc.getOutputs(outputQuery);

// create a wallet from a mnemonic phrase using Java native bindings to Monero Core
MoneroWalletJni walletJni = MoneroWalletJni.createWalletFromMnemonic("MyWallet", "supersecretpassword123", MoneroNetworkType.STAGENET, "hefty value ...", new MoneroRpcConnection("http://localhost:38081"), 384151l);

// synchronize the wallet and receive progress notifications
walletJni.sync(new MoneroSyncListener() {
  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    // feed a progress bar?
  }
});

// start syncing the wallet continuously in the background
walletJni.startSyncing();

// be notified when the JNI wallet receives funds
walletJni.addListener(new MoneroWalletListener() {
  
  @Override
  public void onOutputReceived(MoneroOutputWallet output) {
    System.out.println("Wallet received funds!");
    String txId = output.getTx().getId();
    int accountIdx = output.getAccountIndex();
    int subaddressIdx = output.getSubaddressIndex();
    JNI_OUTPUT_RECEIVED = true;
  }
});

// send funds from the RPC wallet to the JNI wallet
MoneroTxWallet sentTx = walletRpc.send(0, walletJni.getPrimaryAddress(), new BigInteger("50000"));
assertTrue(sentTx.inTxPool());

// mine with 7 threads to push the network along
int numThreads = 7;
boolean isBackground = false;
boolean ignoreBattery = false;
walletRpc.startMining(numThreads, isBackground, ignoreBattery);

// wait for the next block to be added to the chain
MoneroBlockHeader nextBlockHeader = daemon.getNextBlockHeader();
long nextNumTxs = nextBlockHeader.getNumTxs();

// stop mining
walletRpc.stopMining();

// the transaction is (probably) confirmed
TimeUnit.SECONDS.sleep(10); // wait 10s for auto refresh
boolean isConfirmed = walletRpc.getTx(sentTx.getId()).isConfirmed();

// create a request to send funds from the RPC wallet to multiple destinations in the JNI wallet
MoneroSendRequest request = new MoneroSendRequest()
        .setAccountIndex(1)                           // send from account 1
        .setSubaddressIndices(0, 1)                   // send from subaddreses in account 1
        .setPriority(MoneroSendPriority.UNIMPORTANT)  // no rush
        .setDestinations(
                new MoneroDestination(walletJni.getAddress(1, 0), new BigInteger("50000")),
                new MoneroDestination(walletJni.getAddress(2, 0), new BigInteger("50000")));

// create the transaction, confirm with the user, and relay to the network
MoneroTxWallet createdTx = walletRpc.createTx(request).getTxs().get(0);
BigInteger fee = createdTx.getFee();  // "Are you sure you want to send ...?"
walletRpc.relayTx(createdTx); // submit the transaction which will notify the JNI wallet

// JNI wallet will receive notification of incoming output after a moment
TimeUnit.SECONDS.sleep(10);
assertTrue(JNI_OUTPUT_RECEIVED);

// save and close the JNI wallet
walletJni.close(true);
```

## How to Run This Library

1. Clone the project repository: `git clone https://github.com/monero-ecosystem/monero-java.git`
2. `cd monero-java`
3. Install dependencies using Maven: `maven install`

You are now ready to use this library with [monero-daemon-rpc](https://getmonero.org/resources/developer-guides/daemon-rpc.html) and [monero-wallet-rpc](https://getmonero.org/resources/developer-guides/wallet-rpc.html) endpoints.

If you want to process binary data or use a Monero wallet using JNI instead of RPC, a dynamic library must be built for your specific platform for this Java library to use.  This project uses a [C++ counterpart library](https://github.com/woodser/monero-cpp-library) to support JNI, which is included as a submodule in ./external/monero-cpp-library.

1. Update submodules: `./bin/update_submodules`
2. [Build monero-cpp-library submodule as a dynamic library](https://github.com/woodser/monero-cpp-library#building-a-dynamic--shared-library)
4. `export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/` (change as appropriate)
5. Build dylib to ./build/libmonero-java.dylib: `./bin/build-libmonero-java.sh`
6. Run TestMoneroCppUtils.java JUnit tests to verify the dynamic library is working with Java JNI

## How to Run Monero RPC

1. Download and extract the latest [Monero CLI](https://getmonero.org/downloads/) for your platform.
2. Start Monero daemon locally: `./monerod --stagenet` (or use a remote daemon).
3. Create a wallet file if one does not exist.
	- Create new / open existing: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet`
	- Restore from mnemonic seed: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet --restore-deterministic-wallet`
4. Start monero-wallet-rpc (requires --wallet-dir to run tests):
	
	e.g. For wallet name `test_wallet_1`, user `rpc_user`, password `abc123`, stagenet: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir ./`

## How to Run JUnit Tests

1. [Set up this library with JNI support](#how-to-run-this-library)
2. Run monero-wallet-rpc and monero-daemon-rpc.  See [How to Run Monero RPC](#how-to-run-monero-rpc). 
3. Configure the appropriate RPC endpoints, authentication, and test wallet in [TestUtils.java](src/test/java/utils/TestUtils.java).
4. Run all *.java files in src/main/test as JUnits.

## See Also

[API specification](http://moneroecosystem.org/monero-java/monero-spec.pdf)

[Java reference implementation (lite version)](https://github.com/woodser/monero-java-lite)

[JavaScript reference implementation](https://github.com/monero-ecosystem/monero-javascript)

[C++ reference implementation](https://github.com/woodser/monero-cpp-library)

## License

This project is licensed under MIT.

## Donate

Donations are gratefully accepted.  Thank you for your support!

<p align="center">
	<img src="donate.png" width="115" height="115"/>
</p>

`46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz`