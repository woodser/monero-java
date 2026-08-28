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
    start(periodInMs, false);
    return this;
  }
  
  /**
   * Start the task loop.
   * 
   * @param periodInMs the loop period in milliseconds
   * @param targetFixedPeriod specifies if the task should target a fixed period by accounting for run time
   * @return this instance for chaining
   */
  public synchronized TaskLooper start(long periodInMs, boolean targetFixedPeriod) {
    synchronized (this) {
      setPeriodInMs(periodInMs);
      if (isStarted) return this;
      isStarted = true;
      
      // reuse a live loop, which observes isStarted under lock and continues
      if (isLooping) return this;
      isLooping = true;
      TaskLooper that = this;
      Thread loop = new Thread(new Runnable() {
        @Override
        public void run() {
          while (true) {

            // decide to exit and clear isLooping atomically so a restart cannot reuse a dead loop
            synchronized (that) {
              if (!isStarted || Thread.currentThread().isInterrupted()) {
                isLooping = false;
                return;
              }
            }

            // run the task
            long startTime = System.currentTimeMillis();
            task.run();
            
            // wait period
            if (isStarted) {
              try { TimeUnit.MILLISECONDS.sleep(that.periodInMs - (targetFixedPeriod ? System.currentTimeMillis() - startTime : 0)); } // target fixed period by accounting for run time
              catch (Exception e) {
                synchronized (that) { isLooping = false; }
                if (isStarted) throw new RuntimeException(e);
                return;
              }
            }
          }
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
    if (periodInMs <= 0) throw new RuntimeException("Looper period must be greater than 0 ms");
    synchronized (this) {
      this.periodInMs = periodInMs;
    }
  }
}
