package monero.common;

import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.Locale;

/**
 * Collection of generic network/transport addressing utilities.
 *
 * No DNS resolution or other I/O is performed: IPv6 and IPv4 literals are
 * validated by hand, and the address-classification helpers only ever resolve
 * pre-validated IP literals (which {@link InetAddress#getByName(String)} parses
 * without consulting a name service). This avoids heavier dependencies.
 */
public class NetworkUtils {

  public static final String LOOPBACK_HOST = "127.0.0.1"; // local loopback address
  public static final String LOCALHOST = "localhost";

  private NetworkUtils() {
  }

  /**
   * Convert a string to a URI, defaulting to the http scheme when none is given.
   *
   * Unbracketed IPv6 literals are wrapped in brackets so the resulting URI is
   * well-formed (e.g. "::1" becomes "http://[::1]").
   *
   * @param uriString is the string to convert to a URI
   * @return the initialized URI
   * @throws IllegalArgumentException if the string cannot be parsed as a URI
   */
  public static URI parseUri(String uriString) {
    if (uriString != null && uriString.length() > 0 && !uriString.toLowerCase(Locale.ROOT).matches("^[a-z][a-z0-9+.-]*://.+")) { // assume http if scheme not given
      String trimmedUriString = uriString.trim();
      if (trimmedUriString.startsWith("[") || trimmedUriString.indexOf(':') != trimmedUriString.lastIndexOf(':')) {
        try {
          HostAndPort hostAndPort = parseHostAndPort(trimmedUriString, -1);
          if (isIpv6Literal(hostAndPort.getHost())) {
            uriString = "http://" + (hostAndPort.hasPort() ? formatHostAndPort(hostAndPort.getHost(), hostAndPort.getPort()) : formatHost(hostAndPort.getHost()));
          } else {
            uriString = "http://" + uriString;
          }
        } catch (IllegalArgumentException e) {
          uriString = "http://" + uriString;
        }
      } else {
        uriString = "http://" + uriString;
      }
    }
    try {
      return new URI(uriString);
    } catch (Exception e) {
      throw new IllegalArgumentException("Invalid URI: " + uriString, e);
    }
  }

  /**
   * Parse an address of the form "host", "host:port", "[ipv6]" or "[ipv6]:port".
   *
   * @param address is the address to parse
   * @param defaultPort is the port to use when the address has none
   * @return the parsed host and port
   * @throws IllegalArgumentException if the address is malformed
   */
  public static HostAndPort parseHostAndPort(String address, int defaultPort) {
    return parseHostAndPort(address, defaultPort, false);
  }

  /**
   * Parse an address of the form "host", "host:port", "[ipv6]" or "[ipv6]:port".
   *
   * @param address is the address to parse
   * @param defaultPort is the port to use when the address has none
   * @param portRequired throws if true and the address has no port
   * @return the parsed host and port
   * @throws IllegalArgumentException if the address is malformed
   */
  public static HostAndPort parseHostAndPort(String address, int defaultPort, boolean portRequired) {
    if (address == null) throw new IllegalArgumentException("Address must not be null");
    String trimmedAddress = address.trim();
    if (trimmedAddress.isEmpty()) throw new IllegalArgumentException("Address must not be empty");

    String host;
    int port = defaultPort;
    if (trimmedAddress.startsWith("[")) {
      int closingBracketIndex = trimmedAddress.indexOf("]");
      if (closingBracketIndex <= 0) throw new IllegalArgumentException("Invalid bracketed IPv6 address");
      host = trimmedAddress.substring(1, closingBracketIndex);
      if (!isIpv6Literal(host)) throw new IllegalArgumentException("Invalid bracketed IPv6 address");

      String remainder = trimmedAddress.substring(closingBracketIndex + 1);
      if (remainder.isEmpty()) {
        if (portRequired) throw new IllegalArgumentException("Missing port");
      } else {
        if (!remainder.startsWith(":") || remainder.length() == 1) throw new IllegalArgumentException("Missing port");
        port = parsePort(remainder.substring(1));
      }
    } else {
      int colonCount = countChars(trimmedAddress, ':');
      if (colonCount == 0) {
        host = trimmedAddress;
        if (portRequired) throw new IllegalArgumentException("Missing port");
      } else if (colonCount == 1) {
        int colonIndex = trimmedAddress.lastIndexOf(':');
        host = trimmedAddress.substring(0, colonIndex);
        if (host.isEmpty()) throw new IllegalArgumentException("Address format not recognised");
        port = parsePort(trimmedAddress.substring(colonIndex + 1));
      } else if (isIpv6Literal(trimmedAddress)) {
        host = trimmedAddress;
        if (portRequired) throw new IllegalArgumentException("Missing port");
      } else {
        throw new IllegalArgumentException("Invalid IPv6 address");
      }
    }

    if (host.isEmpty()) throw new IllegalArgumentException("Address format not recognised");
    return new HostAndPort(stripIpv6Brackets(host), port);
  }

  /**
   * Format a host and port, bracketing the host if it is an IPv6 literal.
   *
   * @param host is the host (with or without IPv6 brackets)
   * @param port is the port
   * @return the formatted "host:port"
   */
  public static String formatHostAndPort(String host, int port) {
    if (port < 0 || port > 65535) throw new IllegalArgumentException("Invalid port: " + port);
    return formatHost(host) + ":" + port;
  }

  /**
   * Strip surrounding brackets from an IPv6 literal, if present.
   *
   * @param host is the host to strip
   * @return the host without surrounding brackets
   */
  public static String stripIpv6Brackets(String host) {
    return host != null && host.startsWith("[") && host.endsWith("]") ? host.substring(1, host.length() - 1) : host;
  }

  /**
   * Determine if the host is an IPv6 literal (e.g. "::1" or "fe80::1%eth0").
   *
   * Surrounding brackets are tolerated. Pure text validation; performs no DNS
   * resolution or other I/O.
   *
   * @param host is the host to check
   * @return true if the host is an IPv6 literal, false otherwise
   */
  public static boolean isIpv6Literal(String host) {
    host = stripIpv6Brackets(host);
    if (host == null || host.indexOf(':') < 0) return false; // ipv6 requires a colon

    int zone = host.indexOf('%'); // strip optional zone id (e.g. "%eth0")
    if (zone >= 0) {
      if (zone == 0 || zone == host.length() - 1) return false;
      host = host.substring(0, zone);
    }

    int compress = host.indexOf("::"); // "::" stands in for one or more zero groups, at most once
    if (compress != host.lastIndexOf("::")) return false;

    // count 16-bit units on each side of an optional "::"; an embedded ipv4 is only valid as the final group
    if (compress < 0) return ipv6Units(host, true) == 8; // a full address is exactly 8 units
    int head = ipv6Units(host.substring(0, compress), false);
    int tail = ipv6Units(host.substring(compress + 2), true);
    return head >= 0 && tail >= 0 && head + tail < 8; // compression fills the remaining zero units
  }

  /**
   * Determine if the host is a dotted-quad IPv4 literal (e.g. "192.168.1.1").
   *
   * @param host is the host to check
   * @return true if the host is an IPv4 literal, false otherwise
   */
  public static boolean isIpv4Literal(String host) {
    if (host == null) return false;
    String[] octets = host.split("\\.", -1);
    if (octets.length != 4) return false;
    for (String octet : octets) {
      if (octet.isEmpty() || octet.length() > 3) return false;
      if (octet.length() > 1 && octet.charAt(0) == '0') return false; // no ambiguous leading zeros
      for (int i = 0; i < octet.length(); i++) if (octet.charAt(i) < '0' || octet.charAt(i) > '9') return false;
      if (Integer.parseInt(octet) > 255) return false;
    }
    return true;
  }

  /**
   * Determine if the given URI's host is the local host (loopback or "localhost").
   *
   * @param uriString is the URI to check (scheme optional)
   * @return true if the host is local, false otherwise or if it cannot be parsed
   */
  public static boolean isLocalHost(String uriString) {
    try {
      String host = stripIpv6Brackets(parseUri(uriString).getHost());
      if (host == null) return false;
      if (LOOPBACK_HOST.equals(host) || LOCALHOST.equals(host)) return true;
      return isLiteralIp(host) && toInetAddress(host).isLoopbackAddress();
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * Determine if the given URI's host is local or a private (non-routable) IP address.
   *
   * Covers loopback, wildcard, link-local, RFC 1918 site-local and IPv6 unique
   * local addresses. Hostnames (non-literals) are treated as not private.
   *
   * @param uriString is the URI to check (scheme optional)
   * @return true if the host is local or a private IP, false otherwise
   */
  public static boolean isPrivateIp(String uriString) {
    if (uriString == null || uriString.isEmpty()) return false;
    if (isLocalHost(uriString)) return true;
    try {
      String host = stripIpv6Brackets(parseUri(uriString).getHost());
      if (host == null || !isLiteralIp(host)) return false;
      InetAddress addr = toInetAddress(host);
      return addr.isAnyLocalAddress()
          || addr.isLoopbackAddress()
          || addr.isLinkLocalAddress()
          || addr.isSiteLocalAddress()
          || isUniqueLocalAddress(addr);
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * Determine if the given URI's host is an IPv6 literal.
   *
   * @param uriString is the URI to check (scheme optional)
   * @return true if the host is an IPv6 literal, false otherwise
   */
  public static boolean isIpv6Uri(String uriString) {
    try {
      return isIpv6Literal(parseUri(uriString).getHost());
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * Determine if the host is an IPv4 or IPv6 literal (brackets tolerated).
   */
  private static boolean isLiteralIp(String host) {
    host = stripIpv6Brackets(host);
    return host != null && (isIpv4Literal(host) || isIpv6Literal(host));
  }

  /**
   * Resolve a pre-validated IP literal to an {@link InetAddress}. The literal is
   * never a hostname, so {@link InetAddress#getByName(String)} parses it without
   * a name-service lookup. Any IPv6 zone id is stripped, since it is irrelevant
   * to address-range classification and would otherwise trigger interface resolution.
   */
  private static InetAddress toInetAddress(String host) throws UnknownHostException {
    host = stripIpv6Brackets(host);
    int zone = host.indexOf('%');
    if (zone >= 0) host = host.substring(0, zone);
    return InetAddress.getByName(host);
  }

  /**
   * Determine if the address is an IPv6 unique local address (fc00::/7), the IPv6
   * equivalent of an RFC 1918 private network. Java's {@link InetAddress#isSiteLocalAddress()}
   * only recognizes the deprecated fec0::/10 range, so ULAs must be checked explicitly.
   */
  private static boolean isUniqueLocalAddress(InetAddress addr) {
    return addr instanceof Inet6Address && (addr.getAddress()[0] & 0xfe) == 0xfc;
  }

  /**
   * Counts the 16-bit units in a colon-delimited IPv6 segment (an embedded trailing IPv4
   * counts as two), or returns -1 if any group is malformed. Empty segments count as zero.
   */
  private static int ipv6Units(String segment, boolean ipv4Allowed) {
    if (segment.isEmpty()) return 0;
    if (segment.startsWith(":") || segment.endsWith(":")) return -1; // dangling colon (e.g. ":1" or "1:")
    int units = 0;
    String[] groups = segment.split(":", -1);
    for (int i = 0; i < groups.length; i++) {
      String group = groups[i];
      if (group.indexOf('.') >= 0) {
        if (!ipv4Allowed || i != groups.length - 1 || !isIpv4Literal(group)) return -1;
        units += 2;
      } else {
        if (group.isEmpty() || group.length() > 4) return -1;
        for (int j = 0; j < group.length(); j++) if (Character.digit(group.charAt(j), 16) < 0) return -1;
        units += 1;
      }
    }
    return units;
  }

  private static String formatHost(String host) {
    host = stripIpv6Brackets(host);
    return isIpv6Literal(host) ? "[" + host + "]" : host;
  }

  private static int parsePort(String portString) {
    try {
      int port = Integer.parseInt(portString);
      if (port < 0 || port > 65535) throw new IllegalArgumentException("Invalid port: " + portString);
      return port;
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException("Invalid port: " + portString, e);
    }
  }

  private static int countChars(String value, char character) {
    int count = 0;
    for (int i = 0; i < value.length(); i++) {
      if (value.charAt(i) == character) count++;
    }
    return count;
  }

  /**
   * Immutable host and port pair.
   */
  public static class HostAndPort {
    private final String host;
    private final int port;

    private HostAndPort(String host, int port) {
      this.host = host;
      this.port = port;
    }

    public String getHost() {
      return host;
    }

    public int getPort() {
      return port;
    }

    public boolean hasPort() {
      return port >= 0;
    }
  }
}
