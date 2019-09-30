package test;

import java.util.UUID;

import org.junit.Test;

import monero.daemon.model.MoneroNetworkType;
import monero.utils.MoneroUtils;
import monero.wallet.MoneroWalletJni;
import utils.TestUtils;

public class TestMoneroUtils {
  
  // TODO: make tests more thorough
  @Test
  public void testAddressValidation() {
    
    // test each network
    for (MoneroNetworkType networkType : MoneroNetworkType.values()) {
      
      // repeat test
      for (int i = 0; i < 3; i++) {
        
        // create a random wallet
        MoneroWalletJni wallet = MoneroWalletJni.createWalletRandom(TestUtils.TEST_WALLETS_DIR + "/temp_" + UUID.randomUUID().toString() + "_" + i, TestUtils.WALLET_PASSWORD, networkType);

        // test standard address
        MoneroUtils.validateAddress(wallet.getPrimaryAddress(), networkType);
        
        // test integrated address
        MoneroUtils.validateAddress(wallet.getIntegratedAddress().toString(), networkType);
        
        // test subaddress
        System.out.println(networkType);
        System.out.println(wallet.getAddress(0, 1));
        MoneroUtils.validateAddress(wallet.getAddress(0, 1), networkType);
      }
    }
    
    
//    // test valid standard addresses
//    MoneroUtils.validateAddress("46oTQPpoxoxQ6XGWwPqEK1YvADH8X91rFC6brLWRHokoAC5qwbYKA2e9jzQyapENw4V2w5Tz1d4LiMSuDhhFCCf77mQgLRA", MoneroNetworkType.MAINNET);
//    MoneroUtils.validateAddress("A2be3UvzMtkJtxRYgcCbQt2y7Rp2eLVGqNTWfZeankrWimSMM4y7uMP6B9oAZaHsXTj8KFSerkSkkVRuEuEca9QM8VhxCNU", MoneroNetworkType.TESTNET);
//    
//    
//    // test stagenet primary addresses
//    MoneroUtils.validateAddress("579iMXGRmLvW77gsthcF7Ph5znBQ8wFq3fxu5o7FVFSVYrEGyEcYgh9bfM5kXeWpxMMpfXQ56kmeNjfb5YsyQPmfCmkCWti", MoneroNetworkType.STAGENET);
//    MoneroUtils.validateAddress("58zFTpaxjMVSRRouSoT4tURVnUv53S9v97NTXzQmQuMSGx3yNbPKG1yKNvm93VurBGFU58Bp6FLfUhii3BH3SBZLTEAE6JR", MoneroNetworkType.STAGENET);
//    MoneroUtils.validateAddress("53qweb3pcJEEyKfnDsqguha9boV3CruA8EJidbHY7FLkWZH7zHwCdx4b4YFfL9R1xeSWnoh2xrJ8JjjATAr7VwFmF8wbN8q", MoneroNetworkType.STAGENET);
//    
//    // test stagenet subaddresses
//    MoneroUtils.validateAddress("72yHyo523cCPJNfR94jB5jJVru8nn5yxaBidWX5yK8PSR9swPfyq7S6dn5o8L5DNKB26Kiz8jMHY272dXjoteV4SNs9Vp2K", MoneroNetworkType.STAGENET);
//    MoneroUtils.validateAddress("7BayZgRdCoANK9eLGrjqWw3zWjCoT5c23Ub8PFGSgdv74hs5zhWYGD7VxWh2hPMGNA6gyv6ZGXVpL9Xxhi1HK3JuVkqtrPE", MoneroNetworkType.STAGENET);
//    MoneroUtils.validateAddress("78doSssgGPS859xsiL2NtkCzkTX3xXoZShQHj7sSWMvoURzBuEkGovdMndohGAQWJnWyVf5Ga6CMojUee6NVMQtA1h6KDFz", MoneroNetworkType.STAGENET);
//
//    // test valid integrated addresses
//    MoneroUtils.validateAddress("4Gd4DLiXzBmbVX2FZZ3Cvu6fUaWACup1qDowprUCje1kSP4FmbftiJMSfV8kWZXNqmVwj4m52xqtgFNUudVmsmGkGvkLcCibWfVUfUFVB7", MoneroNetworkType.MAINNET);
//    MoneroUtils.validateAddress("4J5sF94AzXgFgx8LuWc9dcWkJkGkD3cL3L2AuhX6QA9jFvSxxj6QhHqHXqM2b2Go7G8RyDzEbHxYd9G26XUUbuJChipEyBz9fENMU2Ua9b", MoneroNetworkType.MAINNET);
  }
}
