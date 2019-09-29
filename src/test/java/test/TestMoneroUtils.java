package test;

import org.junit.Test;

import monero.daemon.model.MoneroNetworkType;
import monero.utils.MoneroUtils;

public class TestMoneroUtils {

  @Test
  public void testAddressValidation() {
    
    // Regular addresses
    MoneroUtils.validateAddress("46oTQPpoxoxQ6XGWwPqEK1YvADH8X91rFC6brLWRHokoAC5qwbYKA2e9jzQyapENw4V2w5Tz1d4LiMSuDhhFCCf77mQgLRA");
    MoneroUtils.validateAddress("46oTQPpoxoxQ6XGWwPqEK1YvADH8X91rFC6brLWRHokoAC5qwbYKA2e9jzQyapENw4V2w5Tz1d4LiMSuDhhFCCf77mQgLRA", MoneroNetworkType.MAINNET);
    MoneroUtils.validateAddress("47zQ5LAivg6hNCgijXSEFVLX7mke1bgM6YGLFaANDoJbgXDymcAAZvvMNt2PmMpqEe5qRy2zyfMYXdwpmdyitiFh84xnPG2");
    MoneroUtils.validateAddress("48bWuoDG75CXMDHbmPEvUF2hm1vLDic7ZJ7hqRkL65QR9p13AQAX4eEACXNk4YP115Q4KRVZnAvmMBHrcGfv9FvKPZnH6vH");
    MoneroUtils.validateAddress("A2be3UvzMtkJtxRYgcCbQt2y7Rp2eLVGqNTWfZeankrWimSMM4y7uMP6B9oAZaHsXTj8KFSerkSkkVRuEuEca9QM8VhxCNU");
    MoneroUtils.validateAddress("A2be3UvzMtkJtxRYgcCbQt2y7Rp2eLVGqNTWfZeankrWimSMM4y7uMP6B9oAZaHsXTj8KFSerkSkkVRuEuEca9QM8VhxCNU", MoneroNetworkType.TESTNET);
    
    // Integrated addresses
    MoneroUtils.validateAddress("4Gd4DLiXzBmbVX2FZZ3Cvu6fUaWACup1qDowprUCje1kSP4FmbftiJMSfV8kWZXNqmVwj4m52xqtgFNUudVmsmGkGvkLcCibWfVUfUFVB7");
    MoneroUtils.validateAddress("4J5sF94AzXgFgx8LuWc9dcWkJkGkD3cL3L2AuhX6QA9jFvSxxj6QhHqHXqM2b2Go7G8RyDzEbHxYd9G26XUUbuJChipEyBz9fENMU2Ua9b");
  }
  
}
