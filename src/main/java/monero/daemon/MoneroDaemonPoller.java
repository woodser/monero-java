package monero.daemon;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroDaemonListener;
import monero.utils.MoneroException;

/**
 * Polls a Monero daemon for updates and notifies listeners as they occur.
 */
public class MoneroDaemonPoller {
  
  private MoneroDaemon daemon;
  private MoneroDaemonPollerRunnable runnable;
  private List<MoneroDaemonListener> listeners;
  private static final long POLL_INTERVAL = 5000; // poll every X ms  TODO: poll interval should come from configuration
  
  public MoneroDaemonPoller(MoneroDaemon daemon) {
    this.daemon = daemon;
    this.listeners = new ArrayList<MoneroDaemonListener>();
  }

  public void addListener(MoneroDaemonListener listener) {
    
    // register listener
    listeners.add(listener);
    
    // start polling thread
    if (runnable == null) {
      runnable = new MoneroDaemonPollerRunnable(daemon, POLL_INTERVAL);
      Thread thread = new Thread(runnable);
      thread.setDaemon(true); // daemon thread does not prevent JVM from halting
      thread.start();
    }
  }
  
  public void removeListener(MoneroDaemonListener listener) {
    boolean found = listeners.remove(listener);
    if (!found) throw new MoneroException("Listener is not registered");
    if (listeners.isEmpty()) {
      runnable.terminate();
      runnable = null;
    }
  }
  
  private class MoneroDaemonPollerRunnable implements Runnable {
    
    private MoneroDaemon daemon;
    private long interval;
    private boolean isTerminated;
    
    public MoneroDaemonPollerRunnable(MoneroDaemon daemon, long interval) {
      this.daemon = daemon;
      this.interval = interval;
      this.isTerminated = false;
    }

    @Override
    public void run() {
      
      // get header to detect changes while polling
      MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
      
      // poll until stopped
      while (!isTerminated) {
        
        // pause for interval ms
        try {
          TimeUnit.MILLISECONDS.sleep(interval);
        } catch (InterruptedException e) {
          e.printStackTrace();
          terminate();
        }
        
        // fetch and compare latest block header
        MoneroBlockHeader header = daemon.getLastBlockHeader();
        if (!header.getId().equals(lastHeader.getId())) {
          lastHeader = header;
          for (MoneroDaemonListener listener : listeners) {
            listener.onBlockHeader(header); // notify listener
          }
        }
      }
    }
    
    public void terminate() {
      isTerminated = true;
    }
  }
}
