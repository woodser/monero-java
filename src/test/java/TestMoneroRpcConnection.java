import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsServer;
import java.io.File;
import java.net.InetSocketAddress;
import javax.net.ssl.SSLContext;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import org.apache.hc.core5.ssl.SSLContexts;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test the Monero RPC connection.
 */
@Tag("CI")
public class TestMoneroRpcConnection {

  // Can connect to a certificate which does not match the address if ssl verification is disabled
  @Test
  public void testSslVerifyDisabledAllowsMismatchedCertificate(@TempDir File dir) throws Exception {

    // create self signed certificate for another host
    File keyStore = new File(dir, "keystore.p12");
    char[] password = "password".toCharArray();
    String keytool = new File(System.getProperty("java.home"), "bin/keytool").getPath();
    Process process = new ProcessBuilder(keytool, "-genkeypair", "-keyalg", "EC", "-dname", "CN=example.invalid", "-keystore", keyStore.getPath(), "-storetype", "PKCS12", "-storepass", "password", "-keypass", "password").inheritIO().start();
    assertEquals(0, process.waitFor());
    SSLContext sslContext = SSLContexts.custom().loadKeyMaterial(keyStore, password, password).build();

    // start https server which answers each request with an empty rpc result
    HttpsServer server = HttpsServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    server.setHttpsConfigurator(new HttpsConfigurator(sslContext));
    server.createContext("/", exchange -> {
      byte[] response = "{\"jsonrpc\":\"2.0\",\"id\":\"0\",\"result\":{}}".getBytes();
      exchange.sendResponseHeaders(200, response.length);
      exchange.getResponseBody().write(response);
      exchange.close();
    });
    server.start();

    // certificate is rejected unless ssl verification is disabled
    try {
      MoneroRpcConnection connection = new MoneroRpcConnection("https://127.0.0.1:" + server.getAddress().getPort()).setTimeout(10000l);
      assertThrows(MoneroError.class, () -> connection.sendJsonRequest("get_version"));
      connection.setSslVerify(false).sendJsonRequest("get_version");
    } finally {
      server.stop(0);
    }
  }
}
