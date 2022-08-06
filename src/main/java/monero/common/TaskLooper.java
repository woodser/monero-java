package monero.common;

import java.util.concurrent.TimeUnit;

/**
 * Run a task in a fixed period loop.
 */
public class TaskLooper {
  
  private Runnable task;
  private long periodInMs;
  private boolean isStarted;
  private boolean isLooping;

  /**
   * Build the looper with a task to invoke on a fixed period loop.
   * 
   * @param task is the task to invoke
   */
  public TaskLooper(Runnable task) {
    this.task = task;
  }
  
  /**
   * Start the task loop.
   * 
   * @param periodInMs the loop period in milliseconds
   */
  public synchronized void start(long periodInMs) {
    synchronized (this) {
      this.periodInMs = periodInMs;
      if (isStarted) return;
      isStarted = true;
      
      // start looping
      if (isLooping) return;
      isLooping = true;
      TaskLooper that = this;
      Thread loop = new Thread(new Runnable() {
        @Override
        public void run() {
          while (isStarted && !Thread.currentThread().isInterrupted()) {
            
            // run the task
            long startTime = System.currentTimeMillis();
            task.run();
            
            // wait remaining period
            if (isStarted) {
              try { TimeUnit.MILLISECONDS.sleep(that.periodInMs - (System.currentTimeMillis() - startTime)); } // target fixed period by accounting for run time
              catch (Exception e) {
                isLooping = false;
                if (isStarted) throw new RuntimeException(e);
              }
            }
          }
          isLooping = false;
        }
      });
      loop.start();
    }
  }
  
  /**
   * Stop the task loop.
   */
  public void stop() {
    synchronized (this) {
      isStarted = false;
    }
  }
  
  /**
   * Set the loop period in milliseconds.
   * 
   * @param periodInMs the loop period in milliseconds
   */
  public void setPeriodInMs(long periodInMs) {
    synchronized (this) {
      this.periodInMs = periodInMs;
    }
  }
}
