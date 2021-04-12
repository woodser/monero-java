package test;

import org.junit.platform.runner.JUnitPlatform;
import org.junit.platform.suite.api.IncludeTags;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.runner.RunWith;

/**
 * Run notification tests.
 */
@RunWith(JUnitPlatform.class)
@IncludeTags("NotificationTest")
@SelectClasses({
  TestMoneroWalletFull.class,
  TestMoneroWalletRpc.class
})
public class TestNotifications {}