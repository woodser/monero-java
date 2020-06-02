# Monero Java Library

A Java library for creating Monero applications using RPC or JNI bindings to [monero v0.16.0.0 'Nitrogen Nebula'](https://github.com/monero-project/monero/tree/v0.16.0.0).

* Supports wallet and daemon RPC clients.
* Supports client-side wallets using JNI bindings.
* Supports multisig, view-only, and offline wallets.
* Wallet types are interchangeable by conforming to a [common interface](https://moneroecosystem.org/monero-java/index.html?monero/wallet/MoneroWallet.html).
* Uses a clearly defined [data model and API specification](https://moneroecosystem.org/monero-java/monero-spec.pdf) intended to be intuitive and robust.
* Query wallet transactions, transfers, and outputs by their many attributes.
* Fetch and process binary data from the daemon (e.g. raw blocks).
* Receive notifications when blocks are added to the chain or when wallets sync, send, or receive.
* Over 270 passing JUnit tests.

## Table of contents

* [Architecture](#architecture)
* [Sample code](#sample-code)
* [Using monero-java in your project](#using-monero-java-in-your-project)
* [Developer guide](#developer-guide)
* [Building JNI shared libraries from source](#building-jni-shared-libraries-from-source)
* [Running JUnit tests](#running-junit-tests)
* [See also](#see-also)
* [License](#license)
* [Donations](#donations)

## Architecture

<p align="center">
	<img width="80%" height="auto" src="docs/architecture.png"/><br>
	<i>Build Java applications using RPC or JNI bindings to <a href="https://github.com/monero-project/monero">monero-project/monero</a>.  Wallet implementations are interchangeable by conforming to a common interface, <a href="https://moneroecosystem.org/monero-java/index.html?monero/wallet/MoneroWallet.html">MoneroWallet.java</a>.</i>
</p>

## Sample code

This code introduces the API used in monero-java.  See the [Javadoc](https://moneroecosystem.org/monero-java/), [specification PDF](http://moneroecosystem.org/monero-java/monero-spec.pdf), or [JUnit tests](src/test/java) for more detail.

```java
// connect to a daemon
MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "superuser", "abctesting123");
long height = daemon.getHeight();                       // 1523651
BigInteger feeEstimate = daemon.getFeeEstimate();       // 1014313512
List<MoneroTx> txsInPool = daemon.getTxPool();          // get transactions in the pool

// open wallet on monero-wallet-rpc
MoneroWalletRpc walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
walletRpc.openWallet("sample_wallet_rpc", "supersecretpassword123");
String primaryAddress = walletRpc.getPrimaryAddress();  // 555zgduFhmKd2o8rPUz...
BigInteger balance = walletRpc.getBalance();            // 533648366742
List<MoneroTxWallet> txs = walletRpc.getTxs();          // get transactions containing transfers to/from the wallet

// create wallet from mnemonic phrase using JNI bindings to Monero Core
MoneroWalletJni walletJni = MoneroWalletJni.createWallet(new MoneroWalletConfig()
        .setPath("sample_wallet_jni")
        .setPassword("supersecretpassword123")
        .setNetworkType(MoneroNetworkType.STAGENET)
        .setServerUri("http://localhost:38081")
        .setServerUsername("superuser")
        .setServerPassword("abctesting123")
        .setMnemonic("hefty value scenic...")
        .setRestoreHeight(573936l));

// synchronize the wallet and receive progress notifications
walletJni.sync(new MoneroWalletListener() {
  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    // feed a progress bar?
  }
});

// synchronize in the background
walletJni.startSyncing();

// listen for incoming transfers
walletJni.addListener(new MoneroWalletListener() {
  @Override
  public void onOutputReceived(MoneroOutputWallet output) {
    BigInteger amount = output.getAmount();
    String txHash = output.getTx().getHash();
    JNI_OUTPUT_RECEIVED = true;
  }
});

// send funds from RPC wallet to JNI wallet
MoneroTxWallet createdTx = walletRpc.createTx(new MoneroTxConfig()
        .setAccountIndex(0)
        .setAddress(walletJni.getAddress(1, 0))
        .setAmount(new BigInteger("50000"))
        .setRelay(false)); // create transaction and relay to the network if true
BigInteger fee = createdTx.getFee(); // "Are you sure you want to send... ?"
walletRpc.relayTx(createdTx); // relay the transaction

// recipient receives unconfirmed funds within 10 seconds
TimeUnit.SECONDS.sleep(10);
assertTrue(JNI_OUTPUT_RECEIVED);

// save and close JNI wallet
walletJni.close(true);
```

## Using monero-java in your project

#### For Maven, add to pom.xml:

```xml
<dependency>
  <groupId>io.github.monero-ecosystem</groupId>
  <artifactId>monero-java</artifactId>
  <version>0.3.0</version>
</dependency>
```

#### For Gradle, add to build.gradle:

`compile 'io.github.monero-ecosystem:monero-java:0.3.0'`

You are now ready to use this library with [monero-daemon-rpc](https://getmonero.org/resources/developer-guides/daemon-rpc.html) and [monero-wallet-rpc](https://getmonero.org/resources/developer-guides/wallet-rpc.html) endpoints.  If you want to use client-side wallets via native JNI bindings, first [build the JNI shared libraries](#building-jni-shared-libraries-from-source).

#### If using RPC servers:

1. Download and install [Monero CLI](https://web.getmonero.org/downloads/).
2. Start monero-daemon-rpc, e.g.: `./monerod --stagenet` (or use a remote daemon).
3. Start monero-wallet-rpc, e.g.: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir ./`

## Developer guide

Please refer to [monero-javascript's developer guide](https://github.com/monero-ecosystem/monero-javascript#developer-guide) which mostly translates to monero-java.

## Building JNI shared libraries from source

If you want to process binary data or use a client-side wallet instead of RPC, shared libraries must be built for your specific platform for this Java library to use.  This project uses a C++ counterpart library, [monero-cpp-library](https://github.com/woodser/monero-cpp-library), to support JNI, which is included as a submodule in ./external/monero-cpp-library.

1. Clone the project repository: `git clone https://github.com/monero-ecosystem/monero-java.git`
2. `cd monero-java`
3. Install dependencies using Maven: `mvn install`
4. Update submodules: `./bin/update_submodules.sh`
5. [Build ./external/monero-cpp-library as a shared library.](https://github.com/woodser/monero-cpp-library#how-to-run-this-library)
6. `export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/` (change as appropriate)
7. Build shared libraries to ./build/: `./bin/build_libmonero_java.sh`
8. Run TestMoneroCppUtils.java JUnit tests to verify the shared libraries are working with Java JNI.
9. Add the shared libraries within ./build/ to your application's classpath.

## Running JUnit tests

1. Clone the project repository: `git clone https://github.com/monero-ecosystem/monero-java.git`
2. `cd monero-java`
3. [Build JNI shared libraries from source.](#building-jni-shared-libraries-from-source)
3. [Run monero-wallet-rpc and monero-daemon-rpc.](#if-using-rpc-servers)
4. Configure the appropriate RPC endpoints, authentication, and test wallet in [TestUtils.java](src/test/java/utils/TestUtils.java).
4. Run all *.java files in src/main/test as JUnits.

## See also

* [API specification](http://moneroecosystem.org/monero-java/monero-spec.pdf)
* [monero-javascript](https://github.com/monero-ecosystem/monero-javascript)
* [monero-cpp-library](https://github.com/woodser/monero-cpp-library)

## License

This project is licensed under MIT.

## Donations

If this library brings you value, please consider donating.

<p align="center">
	<img src="donate.png" width="115" height="115"/><br>
	<code>46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz</code>
</p>