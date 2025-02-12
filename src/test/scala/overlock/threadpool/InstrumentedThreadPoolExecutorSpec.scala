package overlock.threadpool

import org.specs2.mutable._;
import java.util.concurrent.atomic._

class InstrumentedThreadPoolExecutorSpec extends SpecificationWithJUnit {
  "InstrumentedThreadPoolExecutor" should {
    "basically work" in {
      val pool = ThreadPool.instrumentedFixed("threadpool", "1", 1).asInstanceOf[InstrumentedThreadPoolExecutor]
      val counter = new AtomicInteger(0)
      pool.execute(new Runnable {
        def run: Unit = {
          counter.getAndIncrement
        }
      })
      Thread.sleep(100)
      counter.get must be_==(1)
      pool.threadGauge.value must be_==(1)
      pool.queueGauge.value must be_==(0)
      pool.requestRate.count must be_==(1)
      pool.rejectedRate.count must be_==(0)
      pool.executionTimer.count must be_==(1)
      pool.activeThreadGauge.value must be_==(0)
    }
  }
}
