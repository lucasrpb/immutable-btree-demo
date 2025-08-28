package demo

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

object TimeProfiler {

  val timer = new AtomicLong(0L)
  var t0: Option[Long] = None

  def snap(): Unit = synchronized {
    if(t0.isEmpty){
      t0 = Some(System.nanoTime())
    } else {
      timer.addAndGet(System.nanoTime() - t0.get)
      t0 = None
    }
  }

  def calculate(): Long = {
    timer.get()
  }
}
