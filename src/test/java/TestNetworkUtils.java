import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import monero.common.NetworkUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests generic network/transport addressing utilities.
 */
public class TestNetworkUtils {

  @Test
  public void testParseUriPrependsHttpScheme() {
    assertEquals("http://example.com", NetworkUtils.parseUri("example.com").toString());
    assertEquals("http://localhost:8080", NetworkUtils.parseUri("localhost:8080").toString());
    assertEquals("http://127.0.0.1:18081", NetworkUtils.parseUri("127.0.0.1:18081").toString());
  }

  @Test
  public void testParseUriPreservesExistingScheme() {
    assertEquals("https://example.com", NetworkUtils.parseUri("https://example.com").toString());
    assertEquals("http://[::1]:18081", NetworkUtils.parseUri("http://[::1]:18081").toString());
    assertEquals("socks5://127.0.0.1:9050", NetworkUtils.parseUri("socks5://127.0.0.1:9050").toString());
  }

  @Test
  public void testParseUriSupportsIpv6WithoutScheme() {
    assertEquals("[::1]", NetworkUtils.parseUri("[::1]:18081").getHost());
    assertEquals(18081, NetworkUtils.parseUri("[::1]:18081").getPort());
    assertEquals("[2607:3c40:1900:33e0::1]", NetworkUtils.parseUri("2607:3c40:1900:33e0::1").getHost());
    assertEquals("[2607:3c40:1900:33e0::1]", NetworkUtils.parseUri("[2607:3c40:1900:33e0::1]:18089").getHost());
    assertEquals(18089, NetworkUtils.parseUri("[2607:3c40:1900:33e0::1]:18089").getPort());
  }

  @Test
  public void testParseUriBracketsBareIpv6Literal() {
    assertEquals("http://[fe80::1]", NetworkUtils.parseUri("fe80::1").toString());
    assertEquals("http://[::ffff:192.168.1.1]", NetworkUtils.parseUri("::ffff:192.168.1.1").toString());
  }

  @Test
  public void testParseUriThrowsOnInvalid() {
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseUri("http://[localhost]:9050"));
  }

  @Test
  public void testParseHostAndPortParsesBracketedIpv6() {
    NetworkUtils.HostAndPort hostAndPort = NetworkUtils.parseHostAndPort("[2607:3c40:1900:33e0::1]:18089", -1);
    assertEquals("2607:3c40:1900:33e0::1", hostAndPort.getHost());
    assertEquals(18089, hostAndPort.getPort());
    assertTrue(hostAndPort.hasPort());
  }

  @Test
  public void testParseHostAndPortAppliesDefaultPort() {
    NetworkUtils.HostAndPort hostAndPort = NetworkUtils.parseHostAndPort("feder8.me", 18081);
    assertEquals("feder8.me", hostAndPort.getHost());
    assertEquals(18081, hostAndPort.getPort());
  }

  @Test
  public void testParseHostAndPortRejectsBracketedNonIpv6() {
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseHostAndPort("[localhost]:9050", -1));
  }

  @Test
  public void testParseHostAndPortRejectsUnbracketedIpv6WithPort() {
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseHostAndPort("2a0b:f4c2:2::63:18081", -1, true));
  }

  @Test
  public void testParseHostAndPortRejectsMissingPortWhenRequired() {
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseHostAndPort("127.0.0.1", -1, true));
  }

  @Test
  public void testParseHostAndPortRejectsPortOutOfRange() {
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseHostAndPort("[::1]:65536", -1));
  }

  @Test
  public void testParseHostAndPortRejectsNullAndEmpty() {
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseHostAndPort(null, -1));
    assertThrows(IllegalArgumentException.class, () -> NetworkUtils.parseHostAndPort("   ", -1));
  }

  @Test
  public void testFormatHostAndPort() {
    assertEquals("127.0.0.1:9050", NetworkUtils.formatHostAndPort("127.0.0.1", 9050));
    assertEquals("[::1]:9050", NetworkUtils.formatHostAndPort("::1", 9050));
    assertEquals("[::1]:9050", NetworkUtils.formatHostAndPort("[::1]", 9050));
    assertEquals("feder8.me:18089", NetworkUtils.formatHostAndPort("feder8.me", 18089));
  }

  @Test
  public void testIsIpv6Literal() {
    assertTrue(NetworkUtils.isIpv6Literal("::1"));
    assertTrue(NetworkUtils.isIpv6Literal("::"));
    assertTrue(NetworkUtils.isIpv6Literal("[::1]"));
    assertTrue(NetworkUtils.isIpv6Literal("2607:3c40:1900:33e0::1"));
    assertTrue(NetworkUtils.isIpv6Literal("1:2:3:4:5:6:7:8"));
    assertTrue(NetworkUtils.isIpv6Literal("::ffff:192.168.1.1"));
    assertTrue(NetworkUtils.isIpv6Literal("fe80::1%eth0"));
    assertFalse(NetworkUtils.isIpv6Literal(null));
    assertFalse(NetworkUtils.isIpv6Literal("localhost"));
    assertFalse(NetworkUtils.isIpv6Literal("127.0.0.1"));
    assertFalse(NetworkUtils.isIpv6Literal("1:2:3:4:5:6:7:8:9"));
    assertFalse(NetworkUtils.isIpv6Literal("1::2::3"));
    assertFalse(NetworkUtils.isIpv6Literal("2a0b:f4c2:2::63:18081"));
  }

  @Test
  public void testIsLocalHost() {
    assertTrue(NetworkUtils.isLocalHost("localhost"));
    assertTrue(NetworkUtils.isLocalHost("127.0.0.1:18081"));
    assertTrue(NetworkUtils.isLocalHost("http://127.0.0.1:18081"));
    assertTrue(NetworkUtils.isLocalHost("[::1]:18081"));
    assertTrue(NetworkUtils.isLocalHost("http://[::1]:18081"));
    assertFalse(NetworkUtils.isLocalHost("example.com"));
    assertFalse(NetworkUtils.isLocalHost("http://192.168.1.1:18081"));
    assertFalse(NetworkUtils.isLocalHost("[fe80::1]:18081"));
  }

  @Test
  public void testIsPrivateIp() {
    // loopback / localhost
    assertTrue(NetworkUtils.isPrivateIp("127.0.0.1"));
    assertTrue(NetworkUtils.isPrivateIp("localhost"));
    // RFC 1918 site-local
    assertTrue(NetworkUtils.isPrivateIp("192.168.1.1"));
    assertTrue(NetworkUtils.isPrivateIp("10.0.0.1:18081"));
    assertTrue(NetworkUtils.isPrivateIp("http://172.16.0.1:18081"));
    // IPv6 link-local and unique local (fc00::/7)
    assertTrue(NetworkUtils.isPrivateIp("fe80::1"));
    assertTrue(NetworkUtils.isPrivateIp("[fe80::1]:18081"));
    assertTrue(NetworkUtils.isPrivateIp("http://[fe80::1]:18081"));
    assertTrue(NetworkUtils.isPrivateIp("fd12:3456:789a::1"));
    assertTrue(NetworkUtils.isPrivateIp("fc00::1"));
    assertTrue(NetworkUtils.isPrivateIp("http://[fdaa:bbbb:cccc::1]:18089"));
    // public addresses and hostnames are not private
    assertFalse(NetworkUtils.isPrivateIp(""));
    assertFalse(NetworkUtils.isPrivateIp(null));
    assertFalse(NetworkUtils.isPrivateIp("8.8.8.8"));
    assertFalse(NetworkUtils.isPrivateIp("example.com"));
    assertFalse(NetworkUtils.isPrivateIp("[2607:3c40:1900:33e0::1]:18089"));
  }

  @Test
  public void testIsIpv6Uri() {
    assertTrue(NetworkUtils.isIpv6Uri("http://[2607:3c40:1900:33e0::1]:18089"));
    assertTrue(NetworkUtils.isIpv6Uri("[::1]:18081"));
    assertFalse(NetworkUtils.isIpv6Uri("http://127.0.0.1:18081"));
    assertFalse(NetworkUtils.isIpv6Uri("http://example.com:18081"));
  }

  @Test
  public void testIsIpv4Literal() {
    assertTrue(NetworkUtils.isIpv4Literal("127.0.0.1"));
    assertTrue(NetworkUtils.isIpv4Literal("255.255.255.255"));
    assertFalse(NetworkUtils.isIpv4Literal(null));
    assertFalse(NetworkUtils.isIpv4Literal("256.0.0.1"));
    assertFalse(NetworkUtils.isIpv4Literal("192.168.1"));
    assertFalse(NetworkUtils.isIpv4Literal("01.2.3.4")); // no ambiguous leading zeros
    assertFalse(NetworkUtils.isIpv4Literal("::1"));
  }
}
