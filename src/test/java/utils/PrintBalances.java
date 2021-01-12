package utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import monero.wallet.MoneroWallet;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroSubaddress;

/**
 * Prints the balances of the wallet.
 */
public class PrintBalances {

  public static void main(String[] args) {
    printBalances();
  }
  
  public static void printBalances() {
    printBalances(null);
  }
  
  public static void printBalances(MoneroWallet wallet) {
    
    // collect info about subaddresses
    List<Pair<String, List<Object>>> pairs = new ArrayList<Pair<String, List<Object>>>();
    if (wallet == null) wallet = TestUtils.getWalletRpc();
    //if (wallet == null) wallet = TestUtils.getWalletJni();
    BigInteger balance = wallet.getBalance();
    BigInteger unlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);
    System.out.println("Wallet balance: " + balance);
    System.out.println("Wallet unlocked balance: " + unlockedBalance);
    for (MoneroAccount account : accounts) {
      add(pairs, "ACCOUNT", account.getIndex());
      add(pairs, "SUBADDRESS", "");
      add(pairs, "LABEL", "");
      add(pairs, "ADDRESS", "");
      add(pairs, "BALANCE", account.getBalance());
      add(pairs, "UNLOCKED", account.getUnlockedBalance());
      for (MoneroSubaddress subaddress : account.getSubaddresses()) {
        add(pairs, "ACCOUNT", account.getIndex());
        add(pairs, "SUBADDRESS", subaddress.getIndex());
        add(pairs, "LABEL", subaddress.getLabel());
        add(pairs, "ADDRESS", subaddress.getAddress());
        add(pairs, "BALANCE", subaddress.getBalance());
        add(pairs, "UNLOCKED", subaddress.getUnlockedBalance());
      }
    }
    
    // convert info to csv
    Integer length = null;
    for (Pair<String, List<Object>> pair : pairs) {
      if (length == null) length = pair.getSecond().size();
      else assertEquals((int) length, (int) pair.getSecond().size());
    }
    
    System.out.println(pairsToCsv(pairs));
  }
  
  private static void add(List<Pair<String, List<Object>>> pairs, String header, Object value) {
    if (value == null) value = "";
    Pair<String, List<Object>> pair = null;
    for (Pair<String, List<Object>> aPair : pairs) {
      if (aPair.getFirst().equals(header)) {
        pair = aPair;
        break;
      }
    }
    if (pair == null) {
      List<Object> vals = new ArrayList<Object>();
      pair = new Pair<String, List<Object>>(header, vals);
      pairs.add(pair);
    }
    assertNotNull(value);
    pair.getSecond().add(value);
  }
  
  private static String pairsToCsv(List<Pair<String, List<Object>>> pairs) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < pairs.size(); i++) {
      sb.append(pairs.get(i).getFirst());
      if (i < pairs.size() - 1) sb.append(',');
      else sb.append('\n');
    }
    for (int i = 0; i < pairs.get(0).getSecond().size(); i++) {
      for (int j = 0; j < pairs.size(); j++) {
        sb.append(pairs.get(j).getSecond().get(i));
        if (j < pairs.size() - 1) sb.append(',');
        else sb.append('\n');
      }
    }
    return sb.toString();
  }
}
