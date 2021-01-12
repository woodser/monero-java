package utils;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZThread;

/**
 * Subscribes to ZMQ publications.
 */
public class RunZmqSubscriber implements ZThread.IDetachedRunnable {

  private String connection;
  private ZContext context;
  private ZMQ.Socket subscriber;

  public static void main(String... args) {
    new RunZmqSubscriber("tcp://127.0.0.1:58083").run(null);
  }

  public RunZmqSubscriber(String connection) {
    this.connection = connection;
  }

  @Override
  public void run(Object[] args) {
    open();

    ZMQ.Poller poller = context.createPoller(1);
    poller.register(subscriber, ZMQ.Poller.POLLIN);
    while (!Thread.currentThread().isInterrupted()) {
      poller.poll();
      if (poller.pollin(0)) {
        String content = subscriber.recvStr();
        System.out.println("Received content! " + content);
      } else {
        System.out.println("No content received...");
      }
    }

    close();
  }

  private void open() {
    context = new ZContext();
    subscriber = context.createSocket(SocketType.SUB);

    subscriber.connect(connection);
    subscriber.subscribe("json-full".getBytes());
  }

  void close() {
    subscriber.close();
    context.close();
  }
}
