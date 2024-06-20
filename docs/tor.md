# Using TOR

First start a TOR node and get its local proxy URI (outside the scope of monero-java).

Then use `monerod.setProxyUri("<tor proxy uri>")` or `wallet.setProxyUri("<tor proxy uri>")`.

## Using TOR with monerod

```java
// connect to monerod with tor address
MoneroDaemonRpc monerod = new MoneroDaemonRpc("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081"); // can add username and password

// set tor's proxy uri
monerod.setProxyUri("<tor proxy uri>");
```

## Using TOR with monero-wallet-rpc

```java
// create client connected to your monero-wallet-rpc instance
MoneroWallet walletRpc = new MoneroWalletRpc("http://localhost:38084"); // can add username and password

// open or create a wallet with tor address
// ...

// set tor's proxy uri
walletRpc.setProxyUri("<tor proxy uri>");
```

## Using TOR with client-side wallet

```java
// create full wallet with tor address
MoneroWallet walletFull = MoneroWalletFull.createWallet(new MoneroWalletConfig()
        .setPath("sample_wallet_full")
        .setPassword("supersecretpassword123")
        .setNetworkType(MoneroNetworkType.MAINNET)
        .setServerUri("http://kyaklhqp4yyza4fmtbvs6z4lrzr5cljxwa7o2d42sfelfhczsmbwzfad.onion:18081")
        .setServerUsername("superuser")
        .setServerPassword("abctesting123")
        .setMnemonic("hefty value scenic...")
        .setRestoreHeight(573936l));

// set tor's proxy uri
walletFull.setProxyUri("<tor proxy uri>");
```