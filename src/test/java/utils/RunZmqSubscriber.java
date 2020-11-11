package utils;

import org.zeromq.ZMQ;
import org.zeromq.ZThread;

public class RunZmqSubscriber implements ZThread.IDetachedRunnable {

  private String connection;
  private ZMQ.Context context;
  private ZMQ.Socket subscriber;
  
//  public static void main(String[] args){
//    ZMQ.Context context=ZMQ.context(1);
//    ZMQ.Socket subscriber=context.socket(ZMQ.SUB);
//    subscriber.connect("tcp://localhost:5563");
//    subscriber.subscribe("bhs_queue".getBytes());
//    while (true) {
//      String msg = new String(subscriber.recv(0));
//      System.out.println("Received notification!: " + msg);
//    }
//  }

  public static void main(String... args) {
    new RunZmqSubscriber("tcp://127.0.0.1:58083").run(null);
  }

  public RunZmqSubscriber(String connection) {
    this.connection = connection;
  }

  @Override
  public void run(Object[] args) {
    open();

    ZMQ.Poller poller = context.poller(1);
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
    context = ZMQ.context(1);
    subscriber = context.socket(ZMQ.SUB);

    subscriber.connect(connection);
    subscriber.subscribe("json-full".getBytes());
  }

  void close() {
    subscriber.close();
    context.close();
  }
}
