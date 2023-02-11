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
   * Get the runnable task to invoke on a fixed period loop.
   * 
   * @return the runnable task
   */
  public Runnable getTask() {
    return task;
  }
  
  /**
   * Start the task loop.
   * 
   * @param periodInMs the loop period in milliseconds
   * @return this instance for chaining
   */
  public synchronized TaskLooper start(long periodInMs) {
    synchronized (this) {
      this.periodInMs = periodInMs;
      if (isStarted) return this;
      isStarted = true;
      
      // start looping
      if (isLooping) return this;
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
    return this;
  }

  /**
   * Indicates if looping.
   * 
   * @return true if looping, false otherwise
   */
  public boolean isStarted() {
    synchronized(this) {
      return isStarted;
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
