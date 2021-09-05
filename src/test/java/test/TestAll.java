package test;

import org.junit.platform.runner.JUnitPlatform;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.runner.RunWith;

/**
 * Run all tests in a custom order.
 */
@RunWith(JUnitPlatform.class)
@SelectClasses({
  TestSampleCode.class,
  TestSerialization.class,
  TestMoneroUtils.class,
  TestMoneroDaemonRpc.class,
  TestMoneroWalletFull.class,
  TestMoneroWalletRpc.class,
  TestMoneroConnectionManager.class
})
public class TestAll {}