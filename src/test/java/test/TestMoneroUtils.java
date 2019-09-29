package test;

import org.junit.Test;

import monero.daemon.model.MoneroNetworkType;
import monero.utils.MoneroUtils;

public class TestMoneroUtils {
  
  // TODO: make tests more thorough
  @Test
  public void testAddressValidation() {
    
    // test valid standard addresses
    MoneroUtils.validateAddress("46oTQPpoxoxQ6XGWwPqEK1YvADH8X91rFC6brLWRHokoAC5qwbYKA2e9jzQyapENw4V2w5Tz1d4LiMSuDhhFCCf77mQgLRA", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("A2be3UvzMtkJtxRYgcCbQt2y7Rp2eLVGqNTWfZeankrWimSMM4y7uMP6B9oAZaHsXTj8KFSerkSkkVRuEuEca9QM8VhxCNU", MoneroNetworkType.TESTNET);
    MoneroUtils.validateAddress("528qdm2pXnYYesCy5VdmBneWeaSZutEijFVAKjpVHeVd4unsCSM55CjgViQsK9WFNHK1eZgcCuZ3fRqYpzKDokqSKp4yp38", MoneroNetworkType.STAGENET);
    
    // test valid integrated addresses
    MoneroUtils.validateAddress("4Gd4DLiXzBmbVX2FZZ3Cvu6fUaWACup1qDowprUCje1kSP4FmbftiJMSfV8kWZXNqmVwj4m52xqtgFNUudVmsmGkGvkLcCibWfVUfUFVB7", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("4J5sF94AzXgFgx8LuWc9dcWkJkGkD3cL3L2AuhX6QA9jFvSxxj6QhHqHXqM2b2Go7G8RyDzEbHxYd9G26XUUbuJChipEyBz9fENMU2Ua9b", MoneroNetworkType.MAINNET);
  }
}
