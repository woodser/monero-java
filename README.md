# Monero Java Library

A Java library for creating Monero applications using RPC and JNI bindings to [monero v0.18.3.3 'Fluorine Fermi'](https://github.com/monero-project/monero/tree/v0.18.3.3).

* Supports wallet and daemon RPC clients.
* Supports client-side wallets using JNI bindings.
* Supports multisig, view-only, and offline wallets.
* Wallet types are interchangeable by conforming to a [common interface](https://woodser.github.io/monero-java/javadocs/monero/wallet/MoneroWallet.html).
* Uses a clearly defined [data model and API specification](https://woodser.github.io/monero-java/monero-spec.pdf) intended to be intuitive and robust.
* Query wallet transactions, transfers, and outputs by their properties.
* Fetch and process binary data from the daemon (e.g. raw blocks).
* Receive notifications when blocks are added to the chain or when wallets sync, send, or receive.
* Over 300 passing JUnit tests.

## Table of contents

* [Architecture](#architecture)
* [Sample code](#sample-code)
* [Documentation](#documentation)
* [Using monero-java in your project](#using-monero-java-in-your-project)
* [Building JNI shared libraries from source](#building-jni-shared-libraries-from-source)
* [Running JUnit tests](#running-junit-tests)
* [Related projects](#related-projects)
* [License](#license)
* [Donations](#donations)

## Architecture

<p align="center">
	<img width="85%" height="auto" src="docs/architecture.png"/><br>
	<i>Build Java applications using RPC or JNI bindings to <a href="https://github.com/monero-project/monero">monero-project/monero</a>.  Wallet implementations are interchangeable by conforming to a common interface, <a href="https://woodser.github.io/monero-java/javadocs/monero/wallet/MoneroWallet.html">MoneroWallet.java</a>.</i>
</p>

## Sample code

```java
// connect to daemon
MoneroDaemon daemon = new MoneroDaemonRpc("http://localhost:38081", "superuser", "abctesting123");
long height = daemon.getHeight();                       // 1523651
List<MoneroTx> txsInPool = daemon.getTxPool();          // get transactions in the pool

// create wallet from mnemonic phrase using JNI bindings to monero-project
MoneroWalletFull walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPath("sample_wallet_full")
        .setPassword("supersecretpassword123")
        .setNetworkType(MoneroNetworkType.STAGENET)
        .setServerUri("http://localhost:38081")
        .setServerUsername("superuser")
        .setServerPassword("abctesting123")
        .setSeed("hefty value scenic...")
        .setRestoreHeight(573936l));

// synchronize the wallet and receive progress notifications
walletFull.sync(new MoneroWalletListener() {
  @Override
  public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
    // feed a progress bar?
  }
});

// synchronize in the background every 5 seconds
walletFull.startSyncing(5000l);

// receive notifications when funds are received, confirmed, and unlocked
walletFull.addListener(new MoneroWalletListener() {
  @Override
  public void onOutputReceived(MoneroOutputWallet output) {
    BigInteger amount = output.getAmount();
    String txHash = output.getTx().getHash();
    Boolean isConfirmed = output.getTx().isConfirmed();
    Boolean isLocked = output.getTx().isLocked();
    FUNDS_RECEIVED = true;
  }
});

// connect to wallet RPC and open wallet
MoneroWalletRpc walletRpc = new MoneroWalletRpc("http://localhost:38083", "rpc_user", "abc123");
walletRpc.openWallet("sample_wallet_rpc", "supersecretpassword123");
String primaryAddress = walletRpc.getPrimaryAddress();  // 555zgduFhmKd2o8rPUz...
BigInteger balance = walletRpc.getBalance();            // 533648366742
List<MoneroTxWallet> txs = walletRpc.getTxs();          // get transactions containing transfers to/from the wallet

// send funds from RPC wallet to full wallet
MoneroTxWallet createdTx = walletRpc.createTx(new MoneroTxConfig()
        .setAccountIndex(0)
        .setAddress(walletFull.getAddress(1, 0))
        .setAmount("250000000000") // send 0.25 XMR (denominated in atomic units)
        .setRelay(false)); // create transaction and relay to the network if true
BigInteger fee = createdTx.getFee(); // "Are you sure you want to send... ?"
walletRpc.relayTx(createdTx); // relay the transaction

// recipient receives unconfirmed funds within 5 seconds
TimeUnit.SECONDS.sleep(5);
assertTrue(FUNDS_RECEIVED);

// save and close wallet
walletFull.close(true);
```

## Documentation

* [Javadoc](https://woodser.github.io/monero-java/allclasses.html)
* [API and model overview with visual diagrams](https://woodser.github.io/monero-java/monero-spec.pdf)
* [JUnit tests](src/test/java)
* [Using TOR](docs/tor.md)
* [monero-ts documentation](https://github.com/woodser/monero-ts#documentation) provides additional documentation which translates to monero-java

## Using monero-java in your project

#### For Maven, add to pom.xml:

```xml
<dependency>
  <groupId>io.github.woodser</groupId>
  <artifactId>monero-java</artifactId>
  <version>0.8.14</version>
</dependency>
```

#### For Gradle, add to build.gradle:

`compile 'io.github.woodser:monero-java:0.8.14'`

You are now ready to use this library with [monerod](https://getmonero.org/resources/developer-guides/daemon-rpc.html) and [monero-wallet-rpc](https://getmonero.org/resources/developer-guides/wallet-rpc.html) endpoints.

If you want to use client-side wallets, first [build the JNI shared libraries](#building-jni-shared-libraries-from-source).

#### If using RPC servers:

1. Download and install [Monero CLI](https://web.getmonero.org/downloads/).
2. Start monerod, e.g.: `./monerod --stagenet` (or use a remote daemon).
3. Start monero-wallet-rpc, e.g.: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir ./`

## Building JNI shared libraries from source

If you want to process binary data or use a client-side wallet instead of RPC, shared libraries must be built for your specific platform for this Java library to use.  This project uses a C++ counterpart library, [monero-cpp](https://github.com/woodser/monero-cpp), to support JNI, which is included as a submodule in ./external/monero-cpp.

### macOS & Linux

1. Install [maven](https://maven.apache.org/download.cgi) for your system.
2. Install a Java JDK for your system, for example:<br>
    ```
    curl -s "https://get.sdkman.io" | bash
    sdk install java 21.0.2.fx-librca
    ```
3. Clone the project repository: `git clone https://github.com/woodser/monero-java.git`
4. `cd ./monero-java`
5. Install Maven dependencies: `mvn install`
6. Update submodules: `./bin/update_submodules.sh`
7. Build the monero-cpp submodule (located at ./external/monero-cpp) as a shared library by following [instructions](https://github.com/woodser/monero-cpp#using-monero-cpp-in-your-project) for your system.
8. Build shared libraries to ./build/: `./bin/build_libmonero_java.sh`
9. Run TestMoneroUtils.java JUnit tests to verify the shared libraries are working with Java JNI.
10. Add the shared libraries within ./build/ to your application's classpath.


### Windows

1. Download and install [Java JDK](https://adoptium.net/temurin/). During installation, enable the option to set the $JAVA_HOME environment variable if possible.
2. Install [MSYS2](https://www.msys2.org/).
3. Install Maven:

    a. Download binary zip archive from https://maven.apache.org/download.cgi<br>
    b. Unpack to C:\msys64\usr\local
4. Start MSYS2 MINGW64 or MSYS MINGW32 depending on your system.
5. Update packages: `pacman -Syu` and confirm at the prompts.
6. Install dependencies. During installation, use default=all by leaving the input blank and pressing enter.

    64-bit: `pacman -S mingw-w64-x86_64-toolchain make mingw-w64-x86_64-cmake git`

    32-bit: `pacman -S mingw-w64-i686-toolchain make mingw-w64-i686-cmake git`

7. Set environment variables:

    `export JAVA_HOME=/path/to/jdk/` (if not set during installation)<br>
    `export MAVEN_HOME=/usr/local/apache-maven-3.x.x/`<br>
    `export PATH=$PATH:$JAVA_HOME/bin/:$MAVEN_HOME/bin/`<br>
8. Clone the project repository: `git clone https://github.com/woodser/monero-java.git`
9. `cd ./monero-java`
10. Install Maven dependencies: `mvn install`
11. Update submodules: `./bin/update_submodules.sh`
12. Build the monero-cpp submodule (located at ./external/monero-cpp) as a shared library by following [instructions](https://github.com/woodser/monero-cpp#windows) for Windows.
13. Build shared libraries to ./build/: `./bin/build_libmonero_java.sh`
15. Run TestMoneroUtils.java JUnit tests to verify the shared libraries are working with Java JNI.
16. Add the shared libraries within ./build/ to your application's classpath.

### Memory Growth

If you see unrestricted memory growth using native bindings, consider applying [jemalloc](https://jemalloc.net/) to improve memory management with `malloc`. In many cases, this can completely resolve the memory growth.

For example: `export LD_PRELOAD=/path/to/libjemalloc.a` then run your app.

## Running JUnit tests

1. Clone the project repository: `git clone https://github.com/woodser/monero-java.git`
2. `cd monero-java`
3. If using JNI, [build JNI shared libraries from source](#building-jni-shared-libraries-from-source).
3. Start RPC servers:
	1. Download and install [Monero CLI](https://web.getmonero.org/downloads/).
	2. Start monerod, e.g.: `./monerod --stagenet` (or use a remote daemon).
	3. Start monero-wallet-rpc, e.g.: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir ./`
4. Configure the appropriate RPC endpoints, authentication, and other settings in [TestUtils.java](src/test/java/utils/TestUtils.java).
4. Run all *.java files in src/test/java as JUnits.

## Related projects

* [monero-ts](https://github.com/woodser/monero-ts)
* [monero-cpp](https://github.com/woodser/monero-cpp)
* [Haveno](https://github.com/haveno-dex/haveno)

## License

This project is licensed under MIT.

## Donations

If this library brings you value, please consider donating.

<p align="center">
	<img src="donate.png" width="115" height="115"/><br>
	<code>46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz</code>
</p>
