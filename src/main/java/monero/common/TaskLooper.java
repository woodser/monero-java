package monero.common;

import java.util.concurrent.TimeUnit;

/**
 * Run a task in a fixed period loop.
 */
public class TaskLooper {
  
  private Runnable task;
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
    isStarted = true;
    if (isLooping) return;
    
    // start looping
    isLooping = true;
    Thread loop = new Thread(new Runnable() {
      @Override
      public void run() {
        while (isStarted && !Thread.currentThread().isInterrupted()) {
          
          // run the task
          long startTime = System.currentTimeMillis();
          task.run();
          
          // wait remaining period
          if (isStarted) {
            try { TimeUnit.MILLISECONDS.sleep(periodInMs - (System.currentTimeMillis() - startTime)); } // target fixed period by accounting for run time
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
  
  /**
   * Stop the task loop.
   */
  public void stop() {
    isStarted = false;
  }
}
