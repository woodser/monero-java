package utils;

import static org.junit.Assert.assertFalse;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import monero.daemon.model.MoneroNetworkType;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroSendRequest;
import monero.wallet.model.MoneroTxSet;
import monero.wallet.model.MoneroWalletConfig;

/**
 * Utility to fund test wallets.
 */
public class FundWallets {
  
  public static final int NUM_WALLETS = 10;
  public static final int NUM_ACCOUNTS = 5;
  public static final int NUM_SUBADDRESSES_PER_ACCOUNT = 3; 
  
  public static void main(String[] args) {
    fundWallets(TestUtils.getWalletJni(), NUM_WALLETS, NUM_ACCOUNTS, NUM_SUBADDRESSES_PER_ACCOUNT);
  }
  
  /**
   * Utility to create and fund test wallets.
   * 
   * @param srcWallet is the source wallet to fund test wallets with
   * @param numWallets is the number of wallets to fund
   * @param numAccounts is the number of accounts to fund in each test wallet
   * @param numSubaddressesPerAccount is the number of subaddresses to fund in each account (num accounts * num subaddresses must be <= 16) // TODO: support funding more subaddresses
   */
  public static void fundWallets(MoneroWallet srcWallet, int numWallets, int numAccounts, int numSubaddressesPerAccount) {
    TestUtils.TX_POOL_WALLET_TRACKER.waitForWalletTxsToClearPool(srcWallet);
    for (int i = 0; i < NUM_WALLETS; i++) {
      
      // create wallet and create and collect subaddresses
      List<String> subaddresses = new ArrayList<String>();
      String path = TestUtils.TEST_WALLETS_DIR + "/funded_" + UUID.randomUUID().toString();
      MoneroWalletJni fundedWallet = MoneroWalletJni.createWallet(new MoneroWalletConfig().setPath(path).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(MoneroNetworkType.STAGENET));
      for (int accountIdx = 0; accountIdx < NUM_ACCOUNTS; accountIdx++) {
        fundedWallet.createAccount();
        for (int subaddressIdx = 0; subaddressIdx < NUM_SUBADDRESSES_PER_ACCOUNT; subaddressIdx++) {
          fundedWallet.createSubaddress(accountIdx);
          subaddresses.add(fundedWallet.getAddress(accountIdx, subaddressIdx));
        }
      }
      fundedWallet.close(true);
      
      // wait for unlocked funds
      while (srcWallet.getUnlockedBalance(0).compareTo(TestUtils.MAX_FEE) < 0) {
        System.out.println("Waiting...");
        try { TimeUnit.MILLISECONDS.sleep(MoneroUtils.WALLET2_REFRESH_INTERVAL); }
        catch (InterruptedException e) { throw new RuntimeException(e.getMessage()); }
      }
      
      // transfer funds to subaddresses
      List<MoneroDestination> destinations = new ArrayList<MoneroDestination>();
      for (String address : subaddresses) destinations.add(new MoneroDestination(address, TestUtils.MAX_FEE.multiply(BigInteger.valueOf(2))));
      System.out.println("Transferring....");
      MoneroTxSet txSet = srcWallet.sendTxs(new MoneroSendRequest().setDestinations(destinations).setAccountIndex(0));
      System.out.println("Tx set has " + txSet.getTxs().size() + " transactions");
      assertFalse(txSet.getTxs().isEmpty());
      //for (MoneroTxWallet tx : txSet.getTxs()) System.out.println(tx);
    }
    
    System.out.println("Wallets funded successfully");
  }
}
