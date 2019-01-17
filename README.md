# Introduction

This project provides Java interfaces for a Monero wallet and daemon.

The interfaces rely on running instances of [Monero Wallet RPC](https://getmonero.org/resources/developer-guides/wallet-rpc.html) and [Monero Daemon RPC](https://getmonero.org/resources/developer-guides/daemon-rpc.html).

## Getting started

1. Download and extract the latest [Monero CLI](https://getmonero.org/downloads/) for your platform.
2. Start Monero daemon locally: `./monerod --stagenet` (or use a remote daemon).
3. Create a wallet file if one does not exist.  This is only necessary one time.
	- Create new / open existing: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet`
	- Restore from mnemonic seed: `./monero-wallet-cli --daemon-address http://localhost:38081 --stagenet --restore-deterministic-wallet`
4. Start monero-wallet-rpc (requires --wallet-dir to run tests):
	
	e.g. For wallet name `test_wallet_1`, user `rpc_user`, password `abc123`, stagenet: `./monero-wallet-rpc --daemon-address http://localhost:38081 --stagenet --rpc-bind-port 38083 --rpc-login rpc_user:abc123 --wallet-dir /Applications/monero-v0.13.0.2 --log-level 4`
5. Download the latest Java code from this GitHub repository:
	1. Create a directory to hold project assets: `mkdir monero_wallet_java && cd monero_wallet_java`
	2. Check out the code: `git clone https://github.com/monero-ecosystem/monero-java-rpc.git`
	3. Optionally import the Java projects into an IDE like Eclipse (.project is included)
6. To run tests, configure the default wallet for running JUnit tests within TestUtils.java.  The RPC domains, ports, and authentication credentials can be configured.
7. Run JUnits under src/test/java to verify the setup.  Note that `TestMoneroWalletSends.java` requires sufficient funds to be available in the wallet to test sending payments (hence why stagenet mode is recommended, so it's not real XMR).

## API Documentation

The main interfaces are [MoneroWallet.java](src/main/java/monero/wallet/MoneroWallet.java) and [MoneroDaemon.java](src/main/java/monero/daemon/MoneroDaemon.java).

Javadoc is provided in the [doc](doc) folder (best viewed opening [doc/index.html](doc/index.html) in a browser).

## License

This project is licensed under Apache 2.0 and MIT.

## Donate

<p align="center">
	<img src="donate.png" width="175" height="175"/>
</p>

`46FR1GKVqFNQnDiFkH7AuzbUBrGQwz2VdaXTDD4jcjRE8YkkoTYTmZ2Vohsz9gLSqkj5EM6ai9Q7sBoX4FPPYJdGKQQXPVz`

Thank you for supporting this project.