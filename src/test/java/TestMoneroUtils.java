

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigInteger;
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
import monero.wallet.MoneroWalletFull;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroWalletConfig;
import org.junit.jupiter.api.Test;
import utils.TestUtils;

/**
 * Tests static Monero utilities.
 */
public class TestMoneroUtils {
  
  // Can get integrated addresses
  @Test
  public void testGetIntegratedAddresses() {
    String primaryAddress = "58qRVVjZ4KxMX57TH6yWqGcH5AswvZZS494hWHcHPt6cDkP7V8AqxFhi3RKXZueVRgUnk8niQGHSpY5Bm9DjuWn16GDKXpF";
    String subaddress = "7B9w2xieXjhDumgPX39h1CAYELpsZ7Pe8Wqtr3pVL9jJ5gGDqgxjWt55gTYUCAuhahhM85ajEp6VbQfLDPETt4oT2ZRXa6n";
    String paymentId = "03284e41c342f036";
    MoneroNetworkType networkType = MoneroNetworkType.STAGENET;
    
    // get integrated address with randomly generated payment id
    MoneroIntegratedAddress integratedAddress = MoneroUtils.getIntegratedAddress(networkType, primaryAddress, null);
    assertEquals(primaryAddress, integratedAddress.getStandardAddress());
    assertEquals(16, integratedAddress.getPaymentId().length());
    assertEquals(106, integratedAddress.getIntegratedAddress().length());
    
    // get integrated address with specific payment id
    integratedAddress = MoneroUtils.getIntegratedAddress(networkType, primaryAddress, paymentId);
    assertEquals(primaryAddress, integratedAddress.getStandardAddress());
    assertEquals(paymentId, integratedAddress.getPaymentId());
    assertEquals(106, integratedAddress.getIntegratedAddress().length());
    
    // get integrated address with subaddress
    integratedAddress = MoneroUtils.getIntegratedAddress(networkType, subaddress, paymentId);
    assertEquals(subaddress, integratedAddress.getStandardAddress());
    assertEquals(paymentId, integratedAddress.getPaymentId());
    assertEquals(106, integratedAddress.getIntegratedAddress().length());
    
    // get integrated address with invalid payment id
    try {
      MoneroUtils.getIntegratedAddress(networkType, primaryAddress, "123");
      fail("Getting integrated address with invalid payment id should have failed");
    } catch (MoneroError err) {
      assertEquals("Invalid payment id", err.getMessage());
    }
  }
  
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
  
  // Can validate addresses
  @Test
  public void testAddressValidation() {
    
    // test mainnet primary address validation
    assertTrue(MoneroUtils.isValidAddress("42U9v3qs5CjZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKS3rvM3L", MoneroNetworkType.MAINNET));
    assertTrue(MoneroUtils.isValidAddress("48ZxX3Y2y5s4nJ8fdz2w65TrTEp9PRsv5J8iHSShkHQcE2V31FhnWptioNst1K9oeDY4KpWZ7v8V2BZNVa4Wdky89iqmPz2", MoneroNetworkType.MAINNET));
    assertTrue(MoneroUtils.isValidAddress("48W972Fx1SQMCHVKENnPpM7tRcL5oWMgpMCqQDbhH8UrjDFg2H9i5AQWXuU1qacJgUUCVLTsgDmZKXGz1vPLXY8QB5ypYqG", MoneroNetworkType.MAINNET));
    
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
  
  // Can validate keys
  @Test
  public void testKeyValidation() {

    // test private view key validation
    assertTrue(MoneroUtils.isValidPrivateViewKey("86cf351d10894769feba29b9e201e12fb100b85bb52fc5825c864eef55c5840d"));
    testInvalidPrivateViewKey("");
    testInvalidPrivateViewKey(null);
    testInvalidPrivateViewKey("5B8s3obCY2ETeQB3GNAGPK2zRGen5UeW1WzegSizVsmf6z5NvM2GLoN6zzk1vHyzGAAfA8pGhuYAeCFZjHAp59jRVQkunGS");
    
    // test public view key validation
    assertTrue(MoneroUtils.isValidPublicViewKey("99873d76ca874ff1aad676b835dd303abcb21c9911ca8a3d9130abc4544d8a0a"));
    testInvalidPublicViewKey("");
    testInvalidPublicViewKey(null);
    testInvalidPublicViewKey("z86cf351d10894769feba29b9e201e12fb100b85bb52fc5825c864eef55c5840d");
    
    // test private spend key validation
    assertTrue(MoneroUtils.isValidPrivateSpendKey("e9ba887e93620ef9fafdfe0c6d3022949f1c5713cbd9ef631f18a0fb00421dee"));
    testInvalidPrivateSpendKey("");
    testInvalidPrivateSpendKey(null);
    testInvalidPrivateSpendKey("z86cf351d10894769feba29b9e201e12fb100b85bb52fc5825c864eef55c5840d");
    
    // test public spend key validation
    assertTrue(MoneroUtils.isValidPublicSpendKey("3e48df9e9d8038dbf6f5382fac2becd8686273cda5bd87187e45dca7ec5af37b"));
    testInvalidPublicSpendKey("");
    testInvalidPublicSpendKey(null);
    testInvalidPublicSpendKey("z86cf351d10894769feba29b9e201e12fb100b85bb52fc5825c864eef55c5840d");
  }
  
  // Can convert between XMR and atomic units
  @Test
  public void testAtomicUnitConversion() {
    assertEquals(new BigInteger("1000000000000").toString(), MoneroUtils.xmrToAtomicUnits(1).toString());
    assertEquals(1, MoneroUtils.atomicUnitsToXmr(new BigInteger("1000000000000")));
    assertEquals(new BigInteger("1000000000").toString(), MoneroUtils.xmrToAtomicUnits(0.001).toString());
    assertEquals(.001, MoneroUtils.atomicUnitsToXmr(new BigInteger("1000000000")));
    assertEquals(new BigInteger("250000000000").toString(), MoneroUtils.xmrToAtomicUnits(.25).toString());
    assertEquals(.25, MoneroUtils.atomicUnitsToXmr(new BigInteger("250000000000")));
    assertEquals(new BigInteger("1250000000000").toString(), MoneroUtils.xmrToAtomicUnits(1.25).toString());
    assertEquals(1.25, MoneroUtils.atomicUnitsToXmr(new BigInteger("1250000000000")));
  };

  @Test
  public void testGetPaymentUri() {
    MoneroTxConfig config = new MoneroTxConfig()
        .setAddress("42U9v3qs5CjZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKS3rvM3L")
        .setAmount(MoneroUtils.xmrToAtomicUnits(0.25))
        .setRecipientName("John Doe")
        .setNote("My transfer to wallet");
    String paymentUri = MoneroUtils.getPaymentUri(config);
    assertEquals("monero:42U9v3qs5CjZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKS3rvM3L?tx_amount=0.25&recipient_name=John%20Doe&tx_description=My%20transfer%20to%20wallet", paymentUri);
  }
  
  // ---------------------------- PRIVATE HELPERS -----------------------------
  
  private static void testInvalidAddress(String address, MoneroNetworkType networkType) {
    assertFalse(MoneroUtils.isValidAddress(address, networkType));
    try {
      MoneroUtils.validateAddress(address, networkType);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertFalse(e.getMessage().isEmpty());
    }
  }
  
  private static void testInvalidPrivateViewKey(String privateViewKey) {
    assertFalse(MoneroUtils.isValidPrivateViewKey(privateViewKey));
    try {
      MoneroUtils.validatePrivateViewKey(privateViewKey);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertFalse(e.getMessage().isEmpty());
    }
  }
  
  private static void testInvalidPublicViewKey(String publicViewKey) {
    assertFalse(MoneroUtils.isValidPublicViewKey(publicViewKey));
    try {
      MoneroUtils.validatePublicViewKey(publicViewKey);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertFalse(e.getMessage().isEmpty());
    }
  }
  
  private static void testInvalidPrivateSpendKey(String privateSpendKey) {
    try {
      assertFalse(MoneroUtils.isValidPrivateSpendKey(privateSpendKey));
      MoneroUtils.validatePrivateSpendKey(privateSpendKey);
      fail("Should have thrown exception");
    } catch (MoneroError e) {
      assertFalse(e.getMessage().isEmpty());
    }
  }
  
  private static void testInvalidPublicSpendKey(String publicSpendKey) {
    assertFalse(MoneroUtils.isValidPublicSpendKey(publicSpendKey));
    try {
      MoneroUtils.validatePublicSpendKey(publicSpendKey);
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
        wallets.add(MoneroWalletFull.createWallet(new MoneroWalletConfig().setPath(TestUtils.TEST_WALLETS_DIR + "/temp_" + UUID.randomUUID().toString() + "_" + i).setPassword(TestUtils.WALLET_PASSWORD).setNetworkType(networkType)));
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
