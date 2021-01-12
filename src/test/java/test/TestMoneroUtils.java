package test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import monero.common.MoneroError;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroNetworkType;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletJni;
import monero.wallet.model.MoneroWalletConfig;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Tests static Monero utilities.
 */
public class TestMoneroUtils {
  
  // Can serialize heights with small numbers
  @Test
  public void testSerializeHeightsSmall() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("heights", Arrays.asList(111, 222, 333));
    byte[] binary = MoneroUtils.mapToBinary(map);
    assertTrue(binary.length > 0);
    //for (int i = 0; i < binary.length; i++) System.out.println(binary[i]);
    Map<String, Object> map2 = MoneroUtils.binaryToMap(binary);
    assertEquals(map, map2);
  };
  
  // Can serialize heights with big numbers
  @Test
  public void testSerializeHeightsBig() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("heights", Arrays.asList(123456, 1234567, 870987));
    byte[] binary = MoneroUtils.mapToBinary(map);
    assertTrue(binary.length > 0);
    Map<String, Object> map2 = MoneroUtils.binaryToMap(binary);
    assertEquals(map, map2);
  }
  
  // Can serialize map with text
  @Test
  public void testSerializeTextShort() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("msg", "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
    byte[] binary = MoneroUtils.mapToBinary(map);
    assertTrue(binary.length > 0);
    Map<String, Object> map2 = MoneroUtils.binaryToMap(binary);
    assertEquals(map, map2);
  }
  
  // Can serialize json with long text
  @Test
  public void testSerializeTextLong() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("msg", "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" +
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
    byte[] binary = MoneroUtils.mapToBinary(map);
    assertTrue(binary.length > 0);
    Map<String, Object> map2 = MoneroUtils.binaryToMap(binary);
    assertEquals(map, map2);
  }
  
  @Test
  public void testAddressValidation() {
    
    // test mainnet primary address validation
    MoneroUtils.validateAddress("42U9v3qs5CjZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKS3rvM3L", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("48ZxX3Y2y5s4nJ8fdz2w65TrTEp9PRsv5J8iHSShkHQcE2V31FhnWptioNst1K9oeDY4KpWZ7v8V2BZNVa4Wdky89iqmPz2", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("48W972Fx1SQMCHVKENnPpM7tRcL5oWMgpMCqQDbhH8UrjDFg2H9i5AQWXuU1qacJgUUCVLTsgDmZKXGz1vPLXY8QB5ypYqG", MoneroNetworkType.MAINNET);
    
    // test mainnet integrated address validation
    MoneroUtils.validateAddress("4CApvrfMgUFZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKeGLQ9vfRBRKFKnBtVH", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("4JGdXrMXaMP4nJ8fdz2w65TrTEp9PRsv5J8iHSShkHQcE2V31FhnWptioNst1K9oeDY4KpWZ7v8V2BZNVa4Wdky8DvDyXvDZXvE9jTQwom", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("4JCp7q5SchvMCHVKENnPpM7tRcL5oWMgpMCqQDbhH8UrjDFg2H9i5AQWXuU1qacJgUUCVLTsgDmZKXGz1vPLXY8QFySJXARQWju8AuRN2z", MoneroNetworkType.MAINNET);
    
    // test mainnet subaddress validation
    MoneroUtils.validateAddress("891TQPrWshJVpnBR4ZMhHiHpLx1PUnMqa3ccV5TJFBbqcJa3DWhjBh2QByCv3Su7WDPTGMHmCKkiVFN2fyGJKwbM1t6G7Ea", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("88fyq3t8Gxn1QWMG189EufHtMHXZXkfJtJKFJXqeA4GpSiuyfjVwVyp47PeQJnD7Tc8iK8TDvvhcmEmfh8nx7Va2ToP8wAo", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("88hnoBiX3TPjbFaQE8RxgyBcf3DtMKZWWQMoArBjQfn37JJwtm568mPX6ipcCuGKDnLCzgjmpLSqce4aBDyapJJAFtNxUMb", MoneroNetworkType.MAINNET);
    
    // test testnet primary address validation
    MoneroUtils.validateAddress("9tUBnNCkC3UKGygHCwYvAB1FscpjUuq5e9MYJd2rXuiiTjjfVeSVjnbSG5VTnJgBgy9Y7GTLfxpZNMUwNZjGfdFr1z79eV1", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("9xZmQa1kYakGoHcfXeBgcsLf622NCpChcACwXxfdgY9uAa9hXSPCV9cLvUsAShfDcFKDdPzCNJ1n5cFGKw5GVM722pjuGPd", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("A2TXS6QFQ4wEsp8U7C2Y4B7wBtiML8aDG7mdCbRvDQmRaRNj1YSSgJE46fSzUkwgpMUCXFqscvrQuN7oKpP6eDyQ7XuYsuf", MoneroNetworkType.TESTNET);
    
    // test testnet integrated address validation
    MoneroUtils.validateAddress("A4AroB2EoJzKGygHCwYvAB1FscpjUuq5e9MYJd2rXuiiTjjfVeSVjnbSG5VTnJgBgy9Y7GTLfxpZNMUwNZjGfdFr2QY5Ba2aHhTEdQa2ra", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("A8GSRNqF9rGGoHcfXeBgcsLf622NCpChcACwXxfdgY9uAa9hXSPCV9cLvUsAShfDcFKDdPzCNJ1n5cFGKw5GVM723iPoCEF1Fs9BcPYxTW", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("ACACSuDk1LTEsp8U7C2Y4B7wBtiML8aDG7mdCbRvDQmRaRNj1YSSgJE46fSzUkwgpMUCXFqscvrQuN7oKpP6eDyQAdgDoT3UnMYKQz7SHC", MoneroNetworkType.TESTNET);
    
    // test testnet subaddress validation
    MoneroUtils.validateAddress("BgnKzHPJQDcg7xiP7bMN9MfPv9Z8ciT71iEMYnCdgBRBFETWgu9nKTr8fnzyGfU9h9gyNA8SFzYYzHfTS9KhqytSU943Nu1", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("BZwiuKkoNP59zgPHTxpNw3PM4DW2xiAVQJWqfFRrGyeZ7afVdQqoiJg3E2dDL3Ja8BV4ov2LEoHx9UjzF3W4ihPBSZvWwTx", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("Bhf1DEYrentcehUvNreLK5gxosnC2VStMXNCCs163RTxQq4jxFYvpw7LrQFmrMwWW2KsXLhMRtyho6Lq11ci3Fb246bxYmi", MoneroNetworkType.TESTNET);
    
    // test stagenet primary address validation
    MoneroUtils.validateAddress("5B8s3obCY2ETeQB3GNAGPK2zRGen5UeW1WzegSizVsmf6z5NvM2GLoN6zzk1vHyzGAAfA8pGhuYAeCFZjHAp59jRVQkunGS", MoneroNetworkType.STAGENET);
    MoneroUtils.validateAddress("57VfotUbSZLG82UkKhWXDjS5ZEK9ZCDcmjdk4gpVq2fbKdEgwRCFrGTLZ2MMdSHphRWJDWVBi5qS8T7dz13JTCWtC228zyn", MoneroNetworkType.STAGENET);
    MoneroUtils.validateAddress("52FysgWJYmAG73QUQZRULJj2Dv2C2mceUMB5zHqNzMn8WBtfPWQrSUFSQUKTX9r7bUMmVSGbrau976xYLynR8jTWLdA7rfp", MoneroNetworkType.STAGENET);
    
    // test stagenet integrated address validation
    MoneroUtils.validateAddress("5LqY4cQh9HkTeQB3GNAGPK2zRGen5UeW1WzegSizVsmf6z5NvM2GLoN6zzk1vHyzGAAfA8pGhuYAeCFZjHAp59jRj6LZRFrjuGK8Whthg2", MoneroNetworkType.STAGENET);
    MoneroUtils.validateAddress("5HCLphJ63prG82UkKhWXDjS5ZEK9ZCDcmjdk4gpVq2fbKdEgwRCFrGTLZ2MMdSHphRWJDWVBi5qS8T7dz13JTCWtHETX8zcUhDjVKcynf6", MoneroNetworkType.STAGENET);
    MoneroUtils.validateAddress("5BxetVKoA2gG73QUQZRULJj2Dv2C2mceUMB5zHqNzMn8WBtfPWQrSUFSQUKTX9r7bUMmVSGbrau976xYLynR8jTWVwQwpHNg5fCLgtA2Dv", MoneroNetworkType.STAGENET);
    
    // test stagenet subaddress validation
    MoneroUtils.validateAddress("778B5D2JmMh5TJVWFbygJR15dvio5Z5B24hfSrWDzeroM8j8Lqc9sMoFE6324xg2ReaAZqHJkgfGFRugRmYHugHZ4f17Gxo", MoneroNetworkType.STAGENET);
    MoneroUtils.validateAddress("73U97wGEH9RCVUf6bopo45jSgoqjMzz4mTUsvWs5EusmYAmFcBYFm7wKMVmgtVKCBhMQqXrcMbHvwck2md63jMZSFJxUhQ2", MoneroNetworkType.STAGENET);
    MoneroUtils.validateAddress("747wPpaPKrjDPZrF48jAfz9pRRUHLMCWfYu2UanP4ZfTG8NrmYrSEWNW8gYoadU8hTiwBjV14e6DLaC5xfhyEpX5154aMm6", MoneroNetworkType.STAGENET);
    
    // test invalid addresses on mainnet
    testInvalidAddress(null, MoneroNetworkType.MAINNET);
    testInvalidAddress("", MoneroNetworkType.MAINNET);
    testInvalidAddress("42ZxX3Y2y5s4nJ8fdz2w65TrTEp9PRsv5J8iHSShkHQcE2V31FhnWptioNst1K9oeDY4KpWZ7v8V2BZNVa4Wdky89iqmPz2", MoneroNetworkType.MAINNET);
    testInvalidAddress("41ApvrfMgUFZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKeGLQ9vfRBRKFKnBtVH", MoneroNetworkType.MAINNET);
    testInvalidAddress("81fyq3t8Gxn1QWMG189EufHtMHXZXkfJtJKFJXqeA4GpSiuyfjVwVyp47PeQJnD7Tc8iK8TDvvhcmEmfh8nx7Va2ToP8wAo", MoneroNetworkType.MAINNET);
    
    // test invalid addresses on testnet
    testInvalidAddress(null, MoneroNetworkType.TESTNET);
    testInvalidAddress("", MoneroNetworkType.TESTNET);
    testInvalidAddress("91UBnNCkC3UKGygHCwYvAB1FscpjUuq5e9MYJd2rXuiiTjjfVeSVjnbSG5VTnJgBgy9Y7GTLfxpZNMUwNZjGfdFr1z79eV1", MoneroNetworkType.TESTNET);
    testInvalidAddress("A1AroB2EoJzKGygHCwYvAB1FscpjUuq5e9MYJd2rXuiiTjjfVeSVjnbSG5VTnJgBgy9Y7GTLfxpZNMUwNZjGfdFr2QY5Ba2aHhTEdQa2ra", MoneroNetworkType.TESTNET);
    testInvalidAddress("B1nKzHPJQDcg7xiP7bMN9MfPv9Z8ciT71iEMYnCdgBRBFETWgu9nKTr8fnzyGfU9h9gyNA8SFzYYzHfTS9KhqytSU943Nu1", MoneroNetworkType.TESTNET);
    
    // test invalid addresses on stagenet
    testInvalidAddress(null, MoneroNetworkType.STAGENET);
    testInvalidAddress("", MoneroNetworkType.STAGENET);
    testInvalidAddress("518s3obCY2ETeQB3GNAGPK2zRGen5UeW1WzegSizVsmf6z5NvM2GLoN6zzk1vHyzGAAfA8pGhuYAeCFZjHAp59jRVQkunGS", MoneroNetworkType.STAGENET);
    testInvalidAddress("51qY4cQh9HkTeQB3GNAGPK2zRGen5UeW1WzegSizVsmf6z5NvM2GLoN6zzk1vHyzGAAfA8pGhuYAeCFZjHAp59jRj6LZRFrjuGK8Whthg2", MoneroNetworkType.STAGENET);
    testInvalidAddress("718B5D2JmMh5TJVWFbygJR15dvio5Z5B24hfSrWDzeroM8j8Lqc9sMoFE6324xg2ReaAZqHJkgfGFRugRmYHugHZ4f17Gxo", MoneroNetworkType.STAGENET);
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  private static void testInvalidAddress(String address, MoneroNetworkType networkType) {
    try {
      MoneroUtils.validateAddress(address, networkType);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertFalse(e.getMessage().isEmpty());
    }
  }
  
  public static void main(String[] args) {
    printAddressValidationTestCode();
  }
  
  /**
   * Prints code to test address validation.
   */
  private static void printAddressValidationTestCode() {
    int repeat = 3;
    
    // test each network
    for (MoneroNetworkType networkType : MoneroNetworkType.values()) {
      
      // create test wallets
      List<MoneroWallet> wallets = new ArrayList<MoneroWallet>();
      for (int i = 0; i < repeat; i++) {
        wallets.add(MoneroWalletJni.createWallet(new MoneroWalletConfig().setPath(TestUtils.TEST_WALLETS_DIR + "/temp_" + UUID.randomUUID().toString() + "_" + i).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(networkType)));
      }
      
      // output primary address tests
      System.out.println("\n// test " + networkType.toString().toLowerCase() + " primary address validation");
      for (int i = 0; i < repeat; i++) {
        System.out.println("MoneroUtils.validateAddress(\"" + wallets.get(i).getPrimaryAddress() + "\", MoneroNetworkType." + networkType + ");");
      }
      
      // output integrated address tests
      System.out.println("\n// test " + networkType.toString().toLowerCase() + " integrated address validation");
      for (int i = 0; i < repeat; i++) {
        System.out.println("MoneroUtils.validateAddress(\"" + wallets.get(i).getIntegratedAddress() + "\", MoneroNetworkType." + networkType + ");");
      }
      
      // output subaddress tests
      System.out.println("\n// test " + networkType.toString().toLowerCase() + " subaddress validation");
      for (int i = 0; i < repeat; i++) {
        System.out.println("MoneroUtils.validateAddress(\"" + wallets.get(i).getAddress(0, 1) + "\", MoneroNetworkType." + networkType + ");");
      }
    }
  }
}
